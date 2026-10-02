#' List submissions across a course
#'
#' Uses Canvas's course-wide endpoint, grouping by student for efficient paging
#' and flattening the result back to one row per submission. Follows next links
#' (including opaque bookmarks) without additional HEAD requests.
#' @param course_id A valid course id.
#' @param assignment_ids Optional assignment ids; NULL requests all assignments.
#' @param student_ids Student ids, or "all" (the default).
#' @param submitted_since Optional UTC ISO 8601 timestamp or POSIXt value.
#'   Returns submissions submitted after this time, excluding unsubmitted work.
#' @param graded_since Optional UTC ISO 8601 timestamp or POSIXt value.
#'   Returns submissions graded after this time, including regrades.
#' @param enrollment_state Optional enrollment filter, "active" or "concluded".
#'   NULL uses Canvas's default (non-deleted enrollments).
#' @param progress Print a concise message after each downloaded page.
#' @details Supplying both timestamps applies both restrictions. For newly
#'   submitted OR newly graded work, make separate calls;
#'   [get_course_gradebook()] does this automatically.
#'   Canvas's course-wide endpoint includes only published assignments, even
#'   for instructors. Requesting an unpublished assignment id can return 403.
#' @return A data frame. Empty results retain id, assignment_id and user_id
#'   columns. API failures raise an error.
#' @examples
#' \dontrun{
#' get_course_submissions(20, assignment_ids = c(101, 102))
#' get_course_submissions(20, graded_since = "2026-10-02T12:00:00Z")
#' }
#' @export
get_course_submissions <- function(course_id, assignment_ids = NULL,
                                   student_ids = "all", submitted_since = NULL,
                                   graded_since = NULL, enrollment_state = NULL,
                                   progress = FALSE) {
  if (!length(student_ids) || anyNA(student_ids)) {
    stop("student_ids must contain at least one non-missing id", call. = FALSE)
  }
  if (!is.null(assignment_ids) && (!length(assignment_ids) || anyNA(assignment_ids))) {
    stop("assignment_ids must be NULL or non-missing ids", call. = FALSE)
  }
  if (!is.null(enrollment_state) &&
      (length(enrollment_state) != 1L || is.na(enrollment_state) ||
       !enrollment_state %in% c("active", "concluded"))) {
    stop("enrollment_state must be NULL, active, or concluded", call. = FALSE)
  }
  args <- c(list(per_page = 100, grouped = TRUE,
                 submitted_since = .submission_timestamp(submitted_since),
                 graded_since = .submission_timestamp(graded_since),
                 enrollment_state = enrollment_state),
            iter_args_list(student_ids, "student_ids[]"),
            iter_args_list(assignment_ids, "assignment_ids[]"))
  url <- make_canvas_url("courses", course_id, "students", "submissions")
  .course_submission_pages(url, args, progress)
}

.course_submission_pages <- function(url, args, progress = FALSE) {
  pages <- list()
  visited <- character()
  repeat {
    if (url %in% visited) stop("Canvas returned a repeated pagination link", call. = FALSE)
    visited <- c(visited, url)
    response <- canvas_query(url, args, "GET")
    httr::stop_for_status(response)
    body <- httr::content(response, "text", encoding = "UTF-8")
    if (!stringr::str_detect(body, "^\\s*\\[")) {
      stop("Canvas returned an unexpected submissions response", call. = FALSE)
    }
    page <- jsonlite::fromJSON(body, flatten = TRUE)
    if (is.data.frame(page) && all(c("user_id", "submissions") %in% names(page))) {
      page <- dplyr::bind_rows(purrr::map2(page$submissions, page$user_id, function(rows, user_id) {
        if (!length(rows)) return(NULL)
        if (!is.data.frame(rows)) stop("Canvas returned unexpected grouped submissions", call. = FALSE)
        dplyr::mutate(rows, user_id = user_id)
      }))
    }
    if (length(page)) {
      if (!is.data.frame(page) || !all(c("assignment_id", "user_id") %in% names(page))) {
        stop("Canvas returned an unexpected submissions response", call. = FALSE)
      }
      pages[[length(pages) + 1L]] <- page
    }
    if (progress) message("Course submissions: page ", length(visited), " downloaded")
    link <- httr::headers(response)$link
    if (is.null(link) || !has_rel(link, "next")) break
    url <- get_page(response, "next")
    if (length(url) != 1L || is.na(url)) stop("Invalid next-page link", call. = FALSE)
    expected <- httr::parse_url(canvas_url())
    next_parts <- httr::parse_url(url)
    if (!identical(next_parts$hostname, expected$hostname) ||
        !identical(next_parts$scheme, expected$scheme) ||
        !identical(next_parts$port, expected$port)) {
      stop("Canvas next-page link points to a different origin", call. = FALSE)
    }
    args <- NULL # the next URL already contains filters and bookmark
  }
  if (!length(pages)) return(data.frame(id = numeric(), assignment_id = numeric(),
                                      user_id = numeric()))
  dplyr::bind_rows(pages)
}

.submission_timestamp <- function(value) {
  if (is.null(value)) return(NULL)
  if (length(value) != 1L || anyNA(value)) {
    stop("Timestamp must be one non-missing UTC ISO 8601 string or POSIXt value", call. = FALSE)
  }
  if (inherits(value, "POSIXt")) return(format(value, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  if (!is.character(value) ||
      !stringr::str_detect(value, "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$")) {
    stop("Timestamp must be UTC ISO 8601, e.g. 2026-10-02T12:00:00Z", call. = FALSE)
  }
  parsed <- as.POSIXct(value, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  if (is.na(parsed) || format(parsed, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC") != value) {
    stop("Invalid timestamp", call. = FALSE)
  }
  value
}
