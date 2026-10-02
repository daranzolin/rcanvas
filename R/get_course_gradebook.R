#' Get a course gradebook
#'
#' Download a full gradebook or incrementally update a previous snapshot.
#' Includes published assignments by default, using the course-wide endpoint.
#' Unpublished assignment rows are removed from legacy caches unless requested.
#' Only reads Canvas; never uploads grades.
#' @importFrom magrittr %>%
#' @param course_id A valid course id.
#' @param progress Print concise page progress (default FALSE).
#' @param previous A previous gradebook from this function. NULL performs a full
#'   import. A legacy snapshot can be supplied with an explicit `since`.
#' @param since UTC ISO 8601 timestamp or POSIXt value. Defaults to the start time
#'   stored on `previous`; required for legacy snapshots without a sync timestamp.
#' @param overlap Non-negative seconds to look back before `since` (default 60).
#' @param refresh_metadata Refresh enrollment metadata during incremental
#'   updates? Default FALSE reuses the cached roster where available.
#'   Assignment metadata is always refreshed to detect publishing changes.
#'   Full imports always fetch both.
#' @param include_unpublished Include unpublished assignments? Default FALSE.
#'   When TRUE, these are fetched individually on both full and incremental
#'   imports because Canvas's bulk endpoint excludes them. This can be slower.
#' @details Incremental updates independently request newly submitted and newly
#'   graded work and replace cached rows by assignment/user id. Regrades are
#'   included. The watermark is the request start, not completion time.
#'   Save with `saveRDS()` or `save()` to retain cache attributes.
#'   `canvas_full_synced_at` records the last full import separately from the
#'   incremental watermark, allowing callers to schedule full reconciliation.
#'
#'   Timestamp filters cannot capture every change (resets to unsubmitted,
#'   deletions, enrollment changes, or some status-only edits). Run periodic full
#'   imports and a full import before final grading. `refresh_metadata = TRUE`
#'   updates labels/roster metadata but does not replace full reconciliation.
#'   Snapshots must belong to the same course and Canvas instance.
#' @return A gradebook in long format, with sync and metadata cache attributes.
#'   API failures raise errors rather than returning a partial gradebook.
#' @examples
#' \dontrun{
#' grades <- get_course_gradebook(20)
#' grades <- get_course_gradebook(20, previous = grades)
#' saveRDS(grades, "gradebook.rds")
#' }
#' @export
get_course_gradebook <- function(course_id, progress = FALSE, previous = NULL,
                                 since = NULL, overlap = 60,
                                 refresh_metadata = FALSE, include_unpublished = FALSE) {
  started <- Sys.time()
  if (!is.logical(include_unpublished) || length(include_unpublished) != 1L || is.na(include_unpublished)) {
    stop("include_unpublished must be TRUE or FALSE", call. = FALSE)
  }
  if (length(overlap) != 1L || !is.numeric(overlap) || is.na(overlap) ||
      !is.finite(overlap) || overlap < 0) {
    stop("overlap must be one finite, non-negative number of seconds", call. = FALSE)
  }
  if (is.null(previous) && !is.null(since)) stop("since requires a previous gradebook", call. = FALSE)
  if (!is.null(previous)) {
    if (!is.data.frame(previous) || !all(c("assignment_id", "user_id") %in% names(previous))) {
      stop("previous must be a gradebook with assignment_id and user_id", call. = FALSE)
    }
    cached_course <- attr(previous, "canvas_course_id")
    cached_domain <- attr(previous, "canvas_domain")
    if ((!is.null(cached_course) && !identical(as.character(cached_course), as.character(course_id))) ||
        ("course_id" %in% names(previous) &&
         any(!is.na(previous$course_id) & as.character(previous$course_id) != as.character(course_id)))) {
      stop("previous belongs to a different course", call. = FALSE)
    }
    if (!is.null(cached_domain) && !identical(cached_domain, canvas_url())) {
      stop("previous belongs to a different Canvas instance", call. = FALSE)
    }
    if (anyNA(previous$assignment_id) || anyNA(previous$user_id) ||
        anyDuplicated(previous[c("assignment_id", "user_id")])) {
      stop("previous contains missing or duplicate submission keys", call. = FALSE)
    }
    if (is.null(since)) since <- attr(previous, "canvas_synced_at")
    if (is.null(since)) stop("A legacy snapshot requires an explicit since timestamp", call. = FALSE)
    cutoff <- as.POSIXct(.submission_timestamp(since), format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC") - overlap
    if (cutoff > started) stop("since cannot be in the future", call. = FALSE)
  }
  cached_assignments <- attr(previous, "canvas_assignments")
  course_assignments <- .gradebook_course_items(course_id, "assignments")
  if (!"published" %in% names(course_assignments)) {
    stop("Assignment metadata is missing published status", call. = FALSE)
  }
  published_ids <- course_assignments$id[!is.na(course_assignments$published) & course_assignments$published]
  if (!include_unpublished) {
    course_assignments <- course_assignments %>% dplyr::filter(!is.na(published), published)
  }
  students <- attr(previous, "canvas_students")
  if (is.null(previous) || refresh_metadata || is.null(students)) {
    students <- .gradebook_course_items(course_id, "enrollments") %>%
      dplyr::filter(role == "StudentEnrollment", user.name != "Test Student") %>%
      dplyr::select(user.name, user_id, grades.final_score, course_id) %>%
      dplyr::distinct(user_id, .keep_all = TRUE)
  }
  if (is.null(previous)) {
    submissions <- get_course_submissions(course_id, enrollment_state = "active", progress = progress)
  } else {
    submitted <- get_course_submissions(course_id, submitted_since = cutoff,
                                        enrollment_state = "active", progress = progress)
    graded <- get_course_submissions(course_id, graded_since = cutoff,
                                     enrollment_state = "active", progress = progress)
    updates <- dplyr::bind_rows(submitted, graded) %>%
      dplyr::group_by(assignment_id, user_id) %>%
      dplyr::slice_tail(n = 1) %>% dplyr::ungroup()
    # Newly published assignments need a complete baseline, including old work
    # and unsubmitted rows that timestamp filters intentionally omit.
    cached_published_ids <- cached_assignments$id[cached_assignments$published %in% TRUE]
    new_ids <- setdiff(published_ids, cached_published_ids)
    if (length(new_ids)) {
      baseline <- get_course_submissions(course_id, assignment_ids = new_ids,
                                         enrollment_state = "active", progress = progress)
      updates <- dplyr::bind_rows(updates, baseline) %>%
        dplyr::group_by(assignment_id, user_id) %>%
        dplyr::slice_tail(n = 1) %>% dplyr::ungroup()
    }
    retained <- previous %>%
      dplyr::select(-dplyr::any_of(c("user.name", "grades.final_score", "course_id", "assignment_name"))) %>%
      dplyr::anti_join(updates, by = c("assignment_id", "user_id"))
    submissions <- dplyr::bind_rows(retained, updates)
  }
  if (include_unpublished) {
    unpublished_ids <- course_assignments$id[!is.na(course_assignments$published) & !course_assignments$published]
    extra <- dplyr::bind_rows(purrr::map(unpublished_ids, function(id) {
      .course_submission_pages(
        make_canvas_url("courses", course_id, "assignments", id, "submissions"),
        list(per_page = 100), progress
      )
    }))
    if (length(unpublished_ids)) {
      submissions <- submissions %>% dplyr::filter(!assignment_id %in% unpublished_ids)
      submissions <- dplyr::bind_rows(submissions, extra)
    }
  }
  submissions <- submissions %>% dplyr::filter(assignment_id %in% course_assignments$id)
  # Metadata columns are attached once.
  submissions <- submissions %>%
    dplyr::select(-dplyr::any_of(c("user.name", "grades.final_score", "course_id", "assignment_name")))
  gradebook <- submissions %>%
    dplyr::left_join(students, by = "user_id") %>%
    dplyr::left_join(course_assignments %>% dplyr::select(id, assignment_name = name),
                     by = c("assignment_id" = "id"))
  attr(gradebook, "canvas_course_id") <- course_id
  attr(gradebook, "canvas_domain") <- canvas_url()
  attr(gradebook, "canvas_synced_at") <- started
  attr(gradebook, "canvas_full_synced_at") <- if (is.null(previous)) started else attr(previous, "canvas_full_synced_at")
  attr(gradebook, "canvas_assignments") <- course_assignments
  attr(gradebook, "canvas_students") <- students
  gradebook
}

# Keep roster/assignment fetching separate from submission pagination.
.gradebook_course_items <- function(course_id, item) get_course_items(course_id, item)

# Retained for callers using the old internal helper.
get_assignment_submissions <- function(course_id, assignment_id, page) {
  url <- make_canvas_url("courses", course_id, "assignments", assignment_id, "submissions")
  response <- canvas_query(url, args = list(per_page = 100, page = page))
  jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"), flatten = TRUE)
}
