#' Update settings for a classic quiz
#'
#' Only non-NULL fields are sent to Canvas, so omitted settings remain
#' unchanged. Boolean values such as \code{FALSE} are preserved. To clear a
#' date or access code, pass an empty string.
#'
#' @param course_id A valid course id.
#' @param quiz_id A valid classic quiz id.
#' @param title Quiz title.
#' @param description Quiz description as text or HTML.
#' @param quiz_type One of \code{"practice_quiz"}, \code{"assignment"},
#' \code{"graded_survey"}, or \code{"survey"}.
#' @param assignment_group_id Assignment group id for the quiz.
#' @param time_limit Time limit in minutes.
#' @param shuffle_answers Whether Canvas shuffles answers.
#' @param hide_results When students may see their responses. Canvas accepts
#' \code{"always"}, \code{"until_after_last_attempt"}, or an empty string.
#' @param show_correct_answers Whether students can see correct answers.
#' @param show_correct_answers_last_attempt Whether correct answers appear
#' only after the final attempt.
#' @param show_correct_answers_at Optional ISO 8601 timestamp when correct
#' answers become visible.
#' @param hide_correct_answers_at Optional ISO 8601 timestamp when correct
#' answers stop being visible.
#' @param allowed_attempts Number of attempts; \code{-1} allows unlimited
#' attempts.
#' @param scoring_policy Which attempt counts when multiple attempts are
#' allowed: \code{"keep_highest"} or \code{"keep_latest"}.
#' @param one_question_at_a_time Whether Canvas displays one question per page.
#' @param cant_go_back Whether students cannot return to earlier questions.
#' @param access_code Optional access code.
#' @param ip_filter Optional IP-address filter.
#' @param due_at Optional ISO 8601 due date.
#' @param lock_at Optional ISO 8601 lock date.
#' @param unlock_at Optional ISO 8601 unlock date.
#' @param published Whether the quiz is published.
#' @param one_time_results Whether students may view results only once.
#' @param only_visible_to_overrides Whether the quiz is visible only through
#' assignment overrides.
#' @param notify_of_update Whether Canvas should notify students of the update.
#'
#' @return The httr response, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' update_quiz_settings(20, 123, shuffle_answers = TRUE)
#' update_quiz_settings(20, 123, published = FALSE, allowed_attempts = 1)
#' }
update_quiz_settings <- function(
    course_id, quiz_id, title = NULL, description = NULL, quiz_type = NULL,
    assignment_group_id = NULL, time_limit = NULL, shuffle_answers = NULL,
    hide_results = NULL, show_correct_answers = NULL,
    show_correct_answers_last_attempt = NULL,
    show_correct_answers_at = NULL, hide_correct_answers_at = NULL,
    allowed_attempts = NULL, scoring_policy = NULL,
    one_question_at_a_time = NULL, cant_go_back = NULL, access_code = NULL,
    ip_filter = NULL, due_at = NULL, lock_at = NULL, unlock_at = NULL,
    published = NULL, one_time_results = NULL,
    only_visible_to_overrides = NULL, notify_of_update = NULL) {
  stopifnot(length(course_id) == 1, length(quiz_id) == 1)

  args <- list(
    title = title,
    description = description,
    quiz_type = quiz_type,
    assignment_group_id = assignment_group_id,
    time_limit = time_limit,
    shuffle_answers = shuffle_answers,
    hide_results = hide_results,
    show_correct_answers = show_correct_answers,
    show_correct_answers_last_attempt = show_correct_answers_last_attempt,
    show_correct_answers_at = show_correct_answers_at,
    hide_correct_answers_at = hide_correct_answers_at,
    allowed_attempts = allowed_attempts,
    scoring_policy = scoring_policy,
    one_question_at_a_time = one_question_at_a_time,
    cant_go_back = cant_go_back,
    access_code = access_code,
    ip_filter = ip_filter,
    due_at = due_at,
    lock_at = lock_at,
    unlock_at = unlock_at,
    published = published,
    one_time_results = one_time_results,
    only_visible_to_overrides = only_visible_to_overrides,
    notify_of_update = notify_of_update
  ) %>%
    purrr::discard(is.null)
  if (length(args) == 0) {
    stop("Provide at least one quiz setting to update.", call. = FALSE)
  }
  names(args) <- stringr::str_c("quiz[", names(args), "]")

  url <- make_canvas_url("courses", course_id, "quizzes", quiz_id)
  resp <- canvas_query(url, args, "PUT")

  message(stringr::str_c(
    "Settings updated for quiz ", quiz_id, " in course ", course_id
  ))
  invisible(resp)
}
