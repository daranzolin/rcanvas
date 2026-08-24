#' List questions in a classic quiz
#'
#' @param course_id A valid course id.
#' @param quiz_id A valid classic quiz id.
#'
#' @return A data frame of quiz questions.
#' @export
#'
#' @examples
#' \dontrun{get_quiz_questions(20, 123)}
get_quiz_questions <- function(course_id, quiz_id) {
  stopifnot(length(course_id) == 1, length(quiz_id) == 1)
  url <- make_canvas_url("courses", course_id, "quizzes", quiz_id,
                         "questions")
  process_response(url, list(per_page = 100))
}

#' Create a question in a classic quiz
#'
#' @param question_text The question prompt as text or HTML.
#' @param answers A data frame or list of answer records. Each record must
#' contain \code{answer_text} and \code{answer_weight}; correct answers normally
#' have a weight of 100 and incorrect answers a weight of 0. Existing answer
#' \code{id} values may also be supplied when updating a question.
#' @param question_name Optional name shown to course staff.
#' @param question_type Canvas question type. Defaults to a multiple-choice
#' question.
#' @param position Optional display position in the quiz.
#' @param points_possible Points available for the question.
#' @param correct_comments Optional feedback for a correct response.
#' @param incorrect_comments Optional feedback for an incorrect response.
#' @param neutral_comments Optional feedback shown for any response.
#' @inheritParams get_quiz_questions
#'
#' @return The httr response, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' create_quiz_question(
#'   20, 123, "Where are exams taken?",
#'   data.frame(
#'     answer_text = c("At CASA", "At home"),
#'     answer_weight = c(100, 0)
#'   )
#' )
#' }
create_quiz_question <- function(course_id, quiz_id, question_text, answers,
                                 question_name = "Question",
                                 question_type = "multiple_choice_question",
                                 position = NULL, points_possible = 1,
                                 correct_comments = NULL,
                                 incorrect_comments = NULL,
                                 neutral_comments = NULL) {
  stopifnot(length(course_id) == 1, length(quiz_id) == 1,
            length(question_text) == 1)
  args <- quiz_question_args(
    question_text = question_text, answers = answers,
    question_name = question_name, question_type = question_type,
    position = position, points_possible = points_possible,
    correct_comments = correct_comments,
    incorrect_comments = incorrect_comments,
    neutral_comments = neutral_comments
  )
  url <- make_canvas_url("courses", course_id, "quizzes", quiz_id,
                         "questions")
  resp <- canvas_query(url, args, "POST")
  message(stringr::str_c("Question created in quiz ", quiz_id,
                         " in course ", course_id))
  invisible(resp)
}

#' Update a question in a classic quiz
#'
#' Only non-NULL fields are sent to Canvas. If \code{answers} is supplied,
#' provide the complete answer set that should remain on the question.
#'
#' @param question_id A valid quiz question id.
#' @inheritParams create_quiz_question
#'
#' @return The httr response, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' update_quiz_question(
#'   20, 123, 456,
#'   question_text = "Where are exams taken?",
#'   answers = data.frame(
#'     answer_text = c("At CASA", "At home"),
#'     answer_weight = c(100, 0)
#'   )
#' )
#' }
update_quiz_question <- function(course_id, quiz_id, question_id,
                                 question_text = NULL, answers = NULL,
                                 question_name = NULL, question_type = NULL,
                                 position = NULL, points_possible = NULL,
                                 correct_comments = NULL,
                                 incorrect_comments = NULL,
                                 neutral_comments = NULL) {
  stopifnot(length(course_id) == 1, length(quiz_id) == 1,
            length(question_id) == 1)
  args <- quiz_question_args(
    question_text = question_text, answers = answers,
    question_name = question_name, question_type = question_type,
    position = position, points_possible = points_possible,
    correct_comments = correct_comments,
    incorrect_comments = incorrect_comments,
    neutral_comments = neutral_comments
  )
  if (length(args) == 0) {
    stop("Provide at least one quiz question field to update.", call. = FALSE)
  }
  url <- make_canvas_url("courses", course_id, "quizzes", quiz_id,
                         "questions", question_id)
  resp <- canvas_query(url, args, "PUT")
  message(stringr::str_c("Question ", question_id, " updated in quiz ",
                         quiz_id, " in course ", course_id))
  invisible(resp)
}

#' Delete a question from a classic quiz
#'
#' @inheritParams update_quiz_question
#'
#' @return The httr response, invisibly.
#' @export
#'
#' @examples
#' \dontrun{delete_quiz_question(20, 123, 456)}
delete_quiz_question <- function(course_id, quiz_id, question_id) {
  stopifnot(length(course_id) == 1, length(quiz_id) == 1,
            length(question_id) == 1)
  url <- make_canvas_url("courses", course_id, "quizzes", quiz_id,
                         "questions", question_id)
  resp <- canvas_query(url, type = "DELETE")
  message(stringr::str_c("Question ", question_id, " deleted from quiz ",
                         quiz_id, " in course ", course_id))
  invisible(resp)
}

quiz_question_args <- function(question_text = NULL, answers = NULL,
                               question_name = NULL, question_type = NULL,
                               position = NULL, points_possible = NULL,
                               correct_comments = NULL,
                               incorrect_comments = NULL,
                               neutral_comments = NULL) {
  fields <- list(
    question_name = question_name,
    question_text = question_text,
    question_type = question_type,
    position = position,
    points_possible = points_possible,
    correct_comments = correct_comments,
    incorrect_comments = incorrect_comments,
    neutral_comments = neutral_comments
  ) %>% purrr::discard(is.null)
  names(fields) <- stringr::str_c("question[", names(fields), "]")
  c(fields, quiz_answer_args(answers))
}

quiz_answer_args <- function(answers) {
  if (is.null(answers)) return(list())
  if (is.data.frame(answers)) answers <- purrr::pmap(answers, list)
  if (!is.list(answers) || length(answers) == 0 ||
      !purrr::every(answers, is.list) ||
      !purrr::every(answers, ~ all(c("answer_text", "answer_weight") %in%
                                     names(.x)))) {
    stop("Each answer must contain answer_text and answer_weight.",
         call. = FALSE)
  }
  allowed <- c("id", "answer_text", "answer_weight", "answer_comments",
               "text_after_answers", "answer_match_left",
               "answer_match_right", "matching_answer_incorrect_matches",
               "numerical_answer_type", "exact", "margin", "approximate",
               "precision", "start", "end", "blank_id")
  answers %>%
    purrr::map(~ .x[names(.x) %in% allowed] %>% purrr::discard(is.null)) %>%
    purrr::map(~ {
      names(.x) <- stringr::str_c("question[answers][][", names(.x), "]")
      .x
    }) %>%
    purrr::flatten()
}
