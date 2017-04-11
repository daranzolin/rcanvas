#' @importFrom magrittr %>%
#'
#' @title Get quiz or assignment submissions
#'
#' @param course_id A valid canvas course id
#' @param type Either "quizzes" or "assignments"
#' @param type_id A valid quiz or assignment id
#'
#' @return Student submissions
#' @export
#'
#' @examples
#' get_submissions(27, "quizzes", 2915)
#' get_submissions(27, "assignments, 254)
get_submissions <- function(course_id, type, type_id) {
  if (!type %in% c("quizzes", "assignments")) stop("type must be 'quizzes' or 'assignments'")
  url <- sprintf("%scourses/%s/%s/%s/submissions", canvas_url(), course_id, type, type_id)
  args <- list(access_token = check_token(),
               per_page = 100)
  process_response(url, args) %>%
    dplyr::mutate(course_id = course_id)
}

#' Comment on a submission
#'
#' See https://canvas.instructure.com/doc/api/submissions.html#method.submissions_api.update
#' This plays nicely with purrr::map2, so it's easy to assign comments en masse
#' to students.
#'
#' @param course_id A valid canvas course id
#' @param assignment_id A valid assignment id
#' @param user_id A valid user id
#' @param comm What comment to give
#' @param visible Whether the comment should be visible to student or not
#' @param to_group Whether the comment should be sent to all members of the group
#'
#' @return Invisibly performs the PUT call
#' @export
#'
#' @examples
#' comment_submission(1350207, 5681164, 4928217, "test")
comment_submission <- function(course_id, assignment_id, user_id, comm,
                             to_group = TRUE, visible = TRUE) {
  url <- paste0(canvas_url(),
                paste("courses", course_id, "assignments", assignment_id,
                      "submissions", user_id, sep = "/"))
  args <- list(access_token = check_token(),
               `comment[text_comment]` = comm,
               `comment[group_comment]` = to_group,
               `include[visibility]` = visible,
               per_page = 100)

  invisible(canvas_query(url, args, "PUT"))
}

#' Grade a submission
#'
#' This plays nicely with purrr::map2, so it's easy to assign grades en masse
#' to students.
#'
#' @param course_id A valid canvas course id
#' @param assignment_id A valid assignment id
#' @param user_id User's id of submission to grade
#' @param grade grade to give
#'
#' @return Invisibly performs the PUT call
#' @export
#'
#' @examples
#' grade_submission(1350207, 5681164, 4928217, 80)
grade_submission <- function(course_id, assignment_id, user_id, grade) {
  url <- paste0(canvas_url(),
                paste("courses", course_id, "assignments", assignment_id,
                      "submissions", user_id, sep = "/"))
  args <- list(access_token = check_token(),
               `submission[posted_grade]` = grade,
               per_page = 100)

  invisible(canvas_query(url, args, "PUT"))
}
