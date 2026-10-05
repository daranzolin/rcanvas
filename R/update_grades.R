#' Grade many submissions for one assignment
#'
#' Posts grades through Canvas's bulk endpoint
#' (`POST /courses/:course_id/assignments/:assignment_id/submissions/update_grades`),
#' waits for each asynchronous Progress job, and by default re-reads the
#' assignment's submissions to confirm every grade landed. A Progress job can
#' report `"completed"` while skipping individual students, so verification is
#' on by default.
#'
#' Grades are sent in chunks of `chunk_size` students, one request at a time.
#' Each value is passed to Canvas as `posted_grade`, so it may be points
#' (`88.5`), a percentage (`"40%"`), or a letter grade, exactly as typed in the
#' gradebook. Numbers are always sent with a `.` decimal mark.
#'
#' **Publishing and visibility.** Canvas rejects grades for an unpublished
#' assignment, so `update_grades()` checks first and stops; it never publishes.
#' It also never posts or releases grades. Under a *manual* posting policy the
#' new grades stay hidden from students until you post them; under an
#' *automatic* policy they are visible as soon as Canvas saves them.
#'
#' **Verification** compares what Canvas recorded as entered (`entered_score`,
#' before any late-policy deduction) with what was sent: numbers as points,
#' percentages converted to points using the assignment's `points_possible`,
#' within 0.005. Other grades (letters, complete/incomplete) are compared with
#' `entered_grade`. `score` is returned too and can be lower than
#' `entered_score` when a late policy applies.
#'
#' **Failures part-way through.** Earlier chunks are not rolled back. If a
#' request or Progress job fails, `update_grades()` signals an error of class
#' `rcanvas_update_grades_error` whose fields record what happened:
#' `chunks` (each chunk's job id and status), `completed_user_ids`,
#' `uncertain_user_ids` (the chunk that failed, which Canvas may or may not
#' have applied), `unsent_user_ids`, and `results` (a best-effort verification
#' of the completed chunks, or `NULL`). Requests Canvas refuses with HTTP 429
#' (throttling) are retried up to `retry_429` times, honouring `Retry-After`;
#' nothing else is retried.
#'
#' @param course_id A Canvas course id.
#' @param assignment_id A Canvas assignment id.
#' @param user_id Canvas user ids of the students to grade.
#' @param grade Grades, parallel to `user_id`. Numbers are sent as points.
#' @param chunk_size Students per request (a positive whole number).
#' @param wait If `FALSE`, return the Progress objects without waiting or
#'   verifying.
#' @param poll_interval Seconds between Progress checks.
#' @param timeout Seconds to wait for each Progress job before stopping.
#' @param verify Re-read the submissions after the jobs complete and report
#'   whether each grade matches.
#' @param retry_429 How many times to retry a request Canvas throttles (429).
#'
#' @return Invisibly, a data frame with `user_id`, `grade` (as sent), and, when
#'   `verify = TRUE`, `expected_points`, `entered_score`, `score`,
#'   `entered_grade` as Canvas now reports them, and a logical `verified`. A
#'   warning names how many grades did not verify. With `wait = FALSE`, a list
#'   of Progress objects.
#' @export
#' @md
#'
#' @examples
#' \dontrun{
#' update_grades(1350207, 5681164, user_id = c(101, 102), grade = c(88.5, 92))
#' }
update_grades <- function(course_id, assignment_id, user_id, grade, chunk_size = 100,
                          wait = TRUE, poll_interval = 2, timeout = 300, verify = TRUE,
                          retry_429 = 3) {
  if (length(user_id) != length(grade)) stop("user_id and grade must have the same length", call. = FALSE)
  if (!length(user_id)) stop("No grades to send", call. = FALSE)
  if (anyNA(user_id) || anyDuplicated(user_id)) stop("user_id must be unique and non-missing", call. = FALSE)
  if (anyNA(grade)) stop("grade contains missing values", call. = FALSE)
  .rcanvas_check_count(chunk_size, "chunk_size", min = 1)
  .rcanvas_check_count(retry_429, "retry_429", min = 0)
  .rcanvas_check_seconds(poll_interval, "poll_interval")
  .rcanvas_check_seconds(timeout, "timeout")
  .rcanvas_check_flag(wait, "wait")
  .rcanvas_check_flag(verify, "verify")
  user_id <- as.character(user_id)
  sent <- .rcanvas_posted_grade(grade)

  assignment <- .rcanvas_json(canvas_query(
    make_canvas_url("courses", course_id, "assignments", assignment_id), list(), "GET", retry_429))
  if (!isTRUE(assignment$published)) {
    stop("Assignment ", assignment_id, " is unpublished; Canvas does not accept grades for it. ",
         "Publish it first.", call. = FALSE)
  }

  url <- make_canvas_url("courses", course_id, "assignments", assignment_id, "submissions", "update_grades")
  chunks <- split(seq_along(user_id), ceiling(seq_along(user_id) / chunk_size))
  log <- data.frame(chunk = seq_along(chunks), students = lengths(chunks),
                    job_id = NA_character_, status = "unsent", stringsAsFactors = FALSE)
  progress <- list()
  for (k in seq_along(chunks)) {
    idx <- chunks[[k]]
    args <- stats::setNames(as.list(sent[idx]), sprintf("grade_data[%s][posted_grade]", user_id[idx]))
    outcome <- tryCatch({
      job <- .rcanvas_check_progress(.rcanvas_json(canvas_query(url, args, "POST", retry_429)))
      log$job_id[k] <- as.character(job$id)
      if (wait) .rcanvas_wait_progress(job, poll_interval, timeout, retry_429) else job
    }, error = function(e) e)
    if (inherits(outcome, "error")) {
      log$status[k] <- "failed"
      .rcanvas_partial_failure(outcome, log, chunks, user_id, sent, course_id, assignment_id,
                               assignment, verify && wait, retry_429)
    }
    log$status[k] <- if (wait) "completed" else outcome$workflow_state
    progress[[k]] <- outcome
  }
  if (!wait) return(invisible(progress))

  result <- data.frame(user_id = user_id, grade = sent, stringsAsFactors = FALSE)
  if (!verify) return(invisible(result))
  result <- .rcanvas_verify_grades(course_id, assignment_id, assignment, user_id, sent, retry_429)
  if (!all(result$verified)) {
    warning(sum(!result$verified), " of ", nrow(result), " grades did not verify in Canvas", call. = FALSE)
  }
  invisible(result)
}

# ---- helpers -----------------------------------------------------------------

.rcanvas_json <- function(response) {
  jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

.rcanvas_now <- function() Sys.time()

.rcanvas_next_link <- function(response) {
  link <- httr::headers(response)$link
  if (is.null(link)) return(NULL)
  nxt <- regmatches(link, regexpr("<[^>]+>; rel=\"next\"", link))
  if (!length(nxt)) NULL else sub("^<([^>]+)>.*$", "\\1", nxt)
}

.rcanvas_check_count <- function(x, name, min) {
  if (length(x) != 1 || !is.numeric(x) || !is.finite(x) || x < min || x != round(x)) {
    stop(name, " must be a whole number of at least ", min, call. = FALSE)
  }
}

.rcanvas_check_seconds <- function(x, name) {
  if (length(x) != 1 || !is.numeric(x) || !is.finite(x) || x <= 0) {
    stop(name, " must be a positive number of seconds", call. = FALSE)
  }
}

.rcanvas_check_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1 || is.na(x)) stop(name, " must be TRUE or FALSE", call. = FALSE)
}

# Text Canvas receives as posted_grade: numbers with a "." decimal mark at full
# precision regardless of the session's OutDec/digits; text trimmed.
.rcanvas_posted_grade <- function(grade) {
  if (is.numeric(grade)) {
    if (any(!is.finite(grade))) stop("grade must be finite", call. = FALSE)
    return(vapply(grade, function(x) format(x, digits = 15, scientific = FALSE, trim = TRUE,
                                            decimal.mark = ".", drop0trailing = TRUE), character(1)))
  }
  grade <- trimws(as.character(grade))
  if (any(!nzchar(grade))) stop("grade contains empty values", call. = FALSE)
  grade
}

# Points Canvas should record as entered: plain numbers as points, "40%" as a
# share of points_possible, anything else NA (compared as text instead).
.rcanvas_expected_points <- function(sent, points_possible) {
  pct <- grepl("^-?[0-9]*\\.?[0-9]+%$", sent)
  points <- suppressWarnings(as.numeric(ifelse(pct, NA, sent)))
  possible <- suppressWarnings(as.numeric(points_possible))
  if (length(possible) != 1) possible <- NA_real_
  points[pct] <- as.numeric(sub("%$", "", sent[pct])) / 100 * possible
  points
}

.rcanvas_check_progress <- function(progress) {
  if (!is.list(progress) || is.null(progress$id) || !is.character(progress$workflow_state) ||
      !progress$workflow_state %in% c("queued", "running", "completed", "failed")) {
    stop("Canvas did not return a valid Progress object", call. = FALSE)
  }
  progress
}

.rcanvas_wait_progress <- function(progress, poll_interval, timeout, retry_429) {
  started <- .rcanvas_now()
  repeat {
    state <- progress$workflow_state
    if (identical(state, "completed")) return(progress)
    if (identical(state, "failed")) {
      stop("Canvas job ", progress$id, " failed",
           if (!is.null(progress$message)) paste0(": ", progress$message), call. = FALSE)
    }
    if (as.numeric(difftime(.rcanvas_now(), started, units = "secs")) >= timeout) {
      stop("Canvas job ", progress$id, " still ", state, " after ", timeout, " seconds", call. = FALSE)
    }
    .rcanvas_sleep(poll_interval)
    progress <- .rcanvas_check_progress(.rcanvas_json(
      canvas_query(make_canvas_url("progress", progress$id), list(), "GET", retry_429)))
  }
}

.rcanvas_submission_scores <- function(course_id, assignment_id, retry_429) {
  url <- make_canvas_url("courses", course_id, "assignments", assignment_id, "submissions")
  args <- list(per_page = 100)
  subs <- list()
  repeat {
    response <- canvas_query(url, args, "GET", retry_429)
    subs <- c(subs, .rcanvas_json(response))
    url <- .rcanvas_next_link(response)
    if (is.null(url)) break
    args <- list()
  }
  field <- function(s, name) if (is.null(s[[name]])) NA else s[[name]]
  data.frame(
    user_id = vapply(subs, function(s) as.character(field(s, "user_id")), character(1)),
    entered_score = vapply(subs, function(s) suppressWarnings(as.numeric(field(s, "entered_score"))), numeric(1)),
    score = vapply(subs, function(s) suppressWarnings(as.numeric(field(s, "score"))), numeric(1)),
    entered_grade = vapply(subs, function(s) as.character(field(s, "entered_grade")), character(1)),
    stringsAsFactors = FALSE)
}

.rcanvas_verify_grades <- function(course_id, assignment_id, assignment, user_id, sent, retry_429) {
  now <- .rcanvas_submission_scores(course_id, assignment_id, retry_429)
  row <- match(user_id, now$user_id)
  result <- data.frame(user_id = user_id, grade = sent,
                       expected_points = .rcanvas_expected_points(sent, assignment$points_possible),
                       entered_score = now$entered_score[row], score = now$score[row],
                       entered_grade = now$entered_grade[row], stringsAsFactors = FALSE)
  entered <- ifelse(is.na(result$entered_score), result$score, result$entered_score)
  result$verified <- ifelse(
    !is.na(result$expected_points),
    !is.na(entered) & abs(entered - result$expected_points) <= 0.005 + 1e-9,
    !is.na(result$entered_grade) & toupper(trimws(result$entered_grade)) == toupper(result$grade))
  result
}

.rcanvas_partial_failure <- function(error, log, chunks, user_id, sent, course_id, assignment_id,
                                     assignment, verify, retry_429) {
  done <- unlist(chunks[log$status == "completed"], use.names = FALSE)
  failed <- unlist(chunks[log$status == "failed"], use.names = FALSE)
  unsent <- unlist(chunks[log$status == "unsent"], use.names = FALSE)
  results <- NULL
  if (verify && length(done)) {
    results <- tryCatch(.rcanvas_verify_grades(course_id, assignment_id, assignment,
                                               user_id[done], sent[done], retry_429),
                        error = function(e) NULL)
  }
  message <- paste0(
    "Grade upload stopped at chunk ", which(log$status == "failed"), " of ", nrow(log), ": ",
    conditionMessage(error), ". ", length(done), " grades were sent in completed jobs and are not ",
    "rolled back; ", length(failed), " in the failed chunk may or may not have been applied; ",
    length(unsent), " were not sent. See the condition's fields for details.")
  stop(structure(class = c("rcanvas_update_grades_error", "error", "condition"), list(
    message = message, call = NULL, chunks = log,
    completed_user_ids = user_id[done], uncertain_user_ids = user_id[failed],
    unsent_user_ids = user_id[unsent], results = results, parent = error)))
}
