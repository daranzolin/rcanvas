#' Post or hide assignment grades for selected students
#'
#' Release existing grades to students, or hide them again, without changing
#' scores, publishing the assignment, or changing its posting policy. Explicit
#' student ids are required: an empty vector never means the whole class.
#' Canvas's GraphQL mutation starts a Progress job; these functions wait and
#' verify the resulting submission visibility by default.
#'
#' Verification also checks that non-target submissions' visibility and the
#' assignment's manual-posting policy did not change. A verification failure
#' signals `rcanvas_grade_visibility_error`, with `results`, `progress`,
#' `non_target_changed`, and `policy_changed` fields. The operation is not
#' rolled back. A timeout or transport error may also occur after Canvas has
#' accepted the request; inspect submissions before retrying.
#'
#' @param course_id A Canvas course id.
#' @param assignment_id A Canvas assignment id.
#' @param user_id Unique, non-missing Canvas student ids. Required.
#' @param wait Wait for the Progress job. If FALSE, no verification is performed.
#' @param poll_interval Seconds between Progress checks.
#' @param timeout Maximum seconds to wait for the Progress job.
#' @param verify Verify visibility after the job completes.
#' @param retry_429 Number of retries for HTTP 429 responses.
#' @return Invisibly, a data frame with user_id, posted_at_before,
#'   posted_at_after and verified (NA when verification is disabled).
#'   With wait = FALSE, the Progress object instead.
#' @export
#' @examples
#' \dontrun{
#' post_grades(10, 20, user_id = c(101, 102))
#' hide_grades(10, 20, user_id = c(101, 102))
#' }
post_grades <- function(course_id, assignment_id, user_id, wait = TRUE,
                        poll_interval = 2, timeout = 300, verify = TRUE,
                        retry_429 = 3) {
  .rcanvas_grade_visibility(course_id, assignment_id, user_id, TRUE,
                            wait, poll_interval, timeout, verify, retry_429)
}

#' @rdname post_grades
#' @export
hide_grades <- function(course_id, assignment_id, user_id, wait = TRUE,
                        poll_interval = 2, timeout = 300, verify = TRUE,
                        retry_429 = 3) {
  .rcanvas_grade_visibility(course_id, assignment_id, user_id, FALSE,
                            wait, poll_interval, timeout, verify, retry_429)
}

.rcanvas_visibility_ids <- function(x, name) {
  if (!length(x) || anyNA(x) || !is.atomic(x))
    stop(name, " must contain positive Canvas ids", call. = FALSE)
  if (is.numeric(x)) x <- format(x, scientific = FALSE, trim = TRUE, digits = 15)
  x <- as.character(x)
  if (any(!grepl("^[1-9][0-9]*$", x)) || anyDuplicated(x))
    stop(name, " must contain unique positive Canvas ids", call. = FALSE)
  x
}

.rcanvas_visibility_submissions <- function(course_id, assignment_id, retry_429) {
  url <- make_canvas_url("courses", course_id, "assignments", assignment_id, "submissions")
  rows <- list()
  while (!is.null(url)) {
    response <- canvas_query(url, list(per_page = 100), "GET", retry_429)
    rows <- c(rows, .rcanvas_json(response))
    url <- .rcanvas_next_link(response)
  }
  data.frame(user_id = vapply(rows, function(x) as.character(x$user_id), character(1)),
             posted_at = vapply(rows, function(x) {
               if (is.null(x$posted_at)) NA_character_ else as.character(x$posted_at)
             }, character(1)), stringsAsFactors = FALSE)
}

.rcanvas_grade_visibility <- function(course_id, assignment_id, user_id, post,
                                      wait, poll_interval, timeout, verify, retry_429) {
  course_id <- .rcanvas_visibility_ids(course_id, "course_id")
  assignment_id <- .rcanvas_visibility_ids(assignment_id, "assignment_id")
  if (length(course_id) != 1 || length(assignment_id) != 1)
    stop("course_id and assignment_id must be scalar", call. = FALSE)
  user_id <- .rcanvas_visibility_ids(user_id, "user_id")
  .rcanvas_check_flag(wait, "wait")
  .rcanvas_check_flag(verify, "verify")
  .rcanvas_check_count(retry_429, "retry_429", 0)
  .rcanvas_check_seconds(poll_interval, "poll_interval")
  .rcanvas_check_seconds(timeout, "timeout")
  assignment_url <- make_canvas_url("courses", course_id, "assignments", assignment_id)
  assignment <- .rcanvas_json(canvas_query(assignment_url, list(), "GET", retry_429))
  .rcanvas_check_flag(assignment$post_manually, "assignment post_manually")
  if (post && !isTRUE(assignment$published))
    stop("Cannot post grades for an unpublished assignment", call. = FALSE)
  before <- .rcanvas_visibility_submissions(course_id, assignment_id, retry_429)
  if (any(!user_id %in% before$user_id))
    stop("Some user_id values have no submission in this assignment", call. = FALSE)
  mutation <- if (post) "postAssignmentGrades" else "hideAssignmentGrades"
  input <- if (post) "PostAssignmentGradesInput!" else "HideAssignmentGradesInput!"
  query <- paste0("mutation($input: ", input, ") { ", mutation,
                  "(input: $input) { errors { message } progress { _id state } } }")
  variables <- list(input = list(assignmentId = assignment_id,
                                 onlyStudentIds = as.list(user_id)))
  response <- .rcanvas_json(canvas_query(sub("/v1$", "/graphql", canvas_url()),
                       list(query = query, variables = variables), "POST", retry_429,
                       encode = "json"))
  payload <- response$data[[mutation]]
  errors <- c(response$errors, payload$errors)
  if (length(errors)) {
    stop("Canvas GraphQL: ", paste(vapply(errors, function(e) {
      if (is.null(e$message)) "Unknown error" else paste(e$message, collapse = "; ")
    }, character(1)), collapse = "; "), call. = FALSE)
  }
  job <- payload$progress
  progress <- .rcanvas_check_progress(list(id = job$`_id`, workflow_state = job$state))
  if (!wait) return(invisible(progress))
  progress <- .rcanvas_wait_progress(progress, poll_interval, timeout, retry_429)
  result <- data.frame(user_id = user_id,
                       posted_at_before = before$posted_at[match(user_id, before$user_id)],
                       posted_at_after = NA_character_, verified = NA, stringsAsFactors = FALSE)
  if (!verify) return(invisible(result))
  for (attempt in seq_len(3)) {
    after <- .rcanvas_visibility_submissions(course_id, assignment_id, retry_429)
    idx <- match(user_id, after$user_id)
    result$posted_at_after <- after$posted_at[idx]
    result$verified <- !is.na(idx) & if (post) !is.na(result$posted_at_after) else is.na(result$posted_at_after)
    if (all(result$verified) || attempt == 3) break
    .rcanvas_sleep(poll_interval)
  }
  other <- before[!before$user_id %in% user_id, , drop = FALSE]
  other_after <- after$posted_at[match(other$user_id, after$user_id)]
  equal <- (is.na(other$posted_at) & is.na(other_after)) |
    (!is.na(other$posted_at) & !is.na(other_after) & other$posted_at == other_after)
  changed <- other$user_id[!equal | !other$user_id %in% after$user_id]
  final_assignment <- .rcanvas_json(canvas_query(assignment_url, list(), "GET", retry_429))
  policy_changed <- !identical(assignment$post_manually, final_assignment$post_manually)
  if (!all(result$verified) || length(changed) || policy_changed) {
    stop(structure(list(message = "Canvas grade visibility did not verify; inspect results before retrying",
                         call = NULL, results = result, progress = progress,
                         non_target_changed = changed, policy_changed = policy_changed),
                    class = c("rcanvas_grade_visibility_error", "error", "condition")))
  }
  invisible(result)
}
