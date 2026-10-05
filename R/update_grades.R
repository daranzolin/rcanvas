#' Grade many submissions for one assignment
#'
#' Posts grades through Canvas's bulk endpoint
#' (`POST /courses/:course_id/assignments/:assignment_id/submissions/update_grades`),
#' waits for each asynchronous Progress job, and by default re-reads the
#' assignment's submissions to confirm every grade landed. A Progress job can
#' report `"completed"` while skipping individual students, so verification is
#' on by default.
#'
#' Grades are sent in chunks of `chunk_size` students per request. Each value
#' is passed as `posted_grade`, so it may be points (`"13.5"`), a percentage
#' (`"40%"`), or a letter grade, exactly as in the Canvas gradebook. Canvas
#' does not grade unpublished assignments in the gradebook; publish the
#' assignment first.
#'
#' @param course_id A Canvas course id.
#' @param assignment_id A Canvas assignment id.
#' @param user_id Canvas user ids of the students to grade.
#' @param grade Grades, parallel to `user_id`. Numbers are sent as points.
#' @param chunk_size Students per request.
#' @param wait If `FALSE`, return the Progress objects without waiting or
#'   verifying.
#' @param poll_interval Seconds between Progress checks.
#' @param timeout Seconds to wait for each Progress job before stopping.
#' @param verify Re-read the submissions after the jobs complete and report
#'   whether each grade matches.
#'
#' @return Invisibly, a data frame with `user_id`, `grade` (as sent), and, when
#'   `verify = TRUE`, `score` and `entered_grade` as Canvas now reports them and
#'   a logical `verified`. A warning names how many grades did not verify. With
#'   `wait = FALSE`, a list of Progress objects.
#' @export
#' @md
#'
#' @examples
#' \dontrun{
#' update_grades(1350207, 5681164, user_id = c(101, 102), grade = c(88.5, 92))
#' }
update_grades <- function(course_id, assignment_id, user_id, grade, chunk_size = 100,
                          wait = TRUE, poll_interval = 2, timeout = 300, verify = TRUE) {
  if (length(user_id) != length(grade)) stop("user_id and grade must have the same length", call. = FALSE)
  if (!length(user_id)) stop("No grades to send", call. = FALSE)
  if (anyNA(user_id) || anyDuplicated(user_id)) stop("user_id must be unique and non-missing", call. = FALSE)
  if (anyNA(grade)) stop("grade contains missing values", call. = FALSE)
  if (length(chunk_size) != 1 || chunk_size < 1) stop("chunk_size must be a positive number", call. = FALSE)
  user_id <- as.character(user_id)
  sent <- if (is.numeric(grade)) format(grade, trim = TRUE, scientific = FALSE, drop0trailing = TRUE) else as.character(grade)

  url <- make_canvas_url("courses", course_id, "assignments", assignment_id, "submissions", "update_grades")
  progress <- list()
  for (start in seq(1, length(user_id), by = chunk_size)) {
    idx <- start:min(length(user_id), start + chunk_size - 1)
    args <- stats::setNames(as.list(sent[idx]), sprintf("grade_data[%s][posted_grade]", user_id[idx]))
    job <- .rcanvas_json(canvas_query(url, args, "POST"))
    progress[[length(progress) + 1]] <- if (wait) .rcanvas_wait_progress(job, poll_interval, timeout) else job
  }
  if (!wait) return(invisible(progress))

  result <- data.frame(user_id = user_id, grade = sent, stringsAsFactors = FALSE)
  if (!verify) return(invisible(result))
  now <- process_response(make_canvas_url("courses", course_id, "assignments", assignment_id, "submissions"),
                          list(per_page = 100))
  now <- data.frame(user_id = as.character(now$user_id),
                    score = if ("score" %in% names(now)) suppressWarnings(as.numeric(now$score)) else NA_real_,
                    entered_grade = if ("entered_grade" %in% names(now)) as.character(now$entered_grade)
                                    else if ("grade" %in% names(now)) as.character(now$grade) else NA_character_,
                    stringsAsFactors = FALSE)
  result <- merge(result, now, by = "user_id", all.x = TRUE, sort = FALSE)
  result <- result[match(user_id, result$user_id), , drop = FALSE]
  as_number <- suppressWarnings(as.numeric(result$grade))
  result$verified <- ifelse(!is.na(as_number),
                            !is.na(result$score) & abs(result$score - as_number) < 0.005,
                            !is.na(result$entered_grade) & result$entered_grade == result$grade)
  rownames(result) <- NULL
  if (!all(result$verified)) {
    warning(sum(!result$verified), " of ", nrow(result), " grades did not verify in Canvas", call. = FALSE)
  }
  invisible(result)
}

.rcanvas_json <- function(response) {
  jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

.rcanvas_sleep <- function(seconds) Sys.sleep(seconds)

.rcanvas_wait_progress <- function(progress, poll_interval, timeout) {
  waited <- 0
  repeat {
    state <- progress$workflow_state
    if (identical(state, "completed")) return(progress)
    if (identical(state, "failed")) {
      stop("Canvas job ", progress$id, " failed",
           if (!is.null(progress$message)) paste0(": ", progress$message), call. = FALSE)
    }
    if (waited >= timeout) stop("Canvas job ", progress$id, " still ", state, " after ", timeout, " seconds", call. = FALSE)
    .rcanvas_sleep(poll_interval)
    waited <- waited + poll_interval
    progress <- .rcanvas_json(canvas_query(make_canvas_url("progress", progress$id), list(), "GET"))
  }
}
