#' Submission functions
#' @param course_id A valid canvas course id
#' @param type Either "quizzes" or "assignments"
#' @param type_id A valid quiz or assignment id
#' @param include A character vector. Options are submission_history, submission_comments, rubric_assessment, assignment, visibility, course, user, group, or read_status.
#' @name submissions
NULL

#' * `get_submissions`: For given course and assignment/quiz, get submissions
#'
#' @importFrom magrittr %>%
#' @rdname submissions
#' @return Student submissions
#' @export
#' @md
#'
#' @examples
#' \dontrun{get_submissions(27, "quizzes", 2915)}
#' \dontrun{get_submissions(27, "assignments", 254)}
get_submissions <- function(course_id, type, type_id, include=NULL) {
  if (!type %in% c("quizzes", "assignments"))
    stop("type must be 'quizzes' or 'assignments'")
  url <- make_canvas_url('courses', course_id, type, type_id,
                         'submissions')
  if(!is.null(include)){
		iopt<-c("submission_history", "submission_comments", "rubric_assessment", "assignment", "visibility", "course", "user", "group", "read_status")
		if(!all(include%in%iopt)) stop(sprintf("include must be any of: %s",paste0(iopt,collapse=", ")))
		inc<-paste0("include[]=",include)
		inc<-paste0(inc,collapse="&")
		url<-paste(url,inc,sep="?")
	}
  args <- list(access_token = check_token(),
               per_page = 100)
  process_response(url, args) %>%
    dplyr::mutate(course_id = course_id)
}

#' * `get_submission_single`: Get a single submission, based on user id.
#'
#' @rdname submissions
#' @param user_id id of user whose submission should be retrieved
#'
#' @export
#' @md
#' @examples
#' \dontrun{get_submission_single(1350207, "assignments", 5681164, 4928217)}
get_submission_single <- function(course_id, type, type_id, user_id, include=NULL) {
  if (!type %in% c("quizzes", "assignments"))
    stop("type must be 'quizzes' or 'assignments'")
  url <- make_canvas_url('courses', course_id, type, type_id,
                         'submissions', user_id)
  if(!is.null(include)){
		iopt<-c("submission_history", "submission_comments", "rubric_assessment", "assignment", "visibility", "course", "user", "group", "read_status")
		if(!all(include%in%iopt)) stop(sprintf("include must be any of: %s",paste0(iopt,collapse=", ")))
		inc<-paste0("include[]=",include)
		inc<-paste0(inc,collapse="&")
		url<-paste(url,inc,sep="?")
	}
  args <- list(access_token = check_token(),
               per_page = 100)
  resp <- canvas_query(url, args, "GET")
  df <- paginate(resp) %>%
    purrr::map(httr::content, "text") %>%
    purrr::map(jsonlite::fromJSON, flatten = TRUE)
  df[[1]]
}

#' Respond to submission
#'
#' See [Canvas API Documentation](https://canvas.instructure.com/doc/api/submissions.html#method.submissions_api.update).
#'
#' This plays nicely with `purrr::map2`, so it's easy to assign comments or grades
#' en masse to students.
#' @md
#' @name submission_response
#' @param course_id A valid canvas course id
#' @param assignment_id A valid assignment id
#' @param user_id A valid user id
#' @return Invisibly performs the PUT call
NULL

#' * `comment_submission`: Comment on a submission
#'
#' @rdname submission_response
#' @param comm What comment to give
#' @param visible Whether the comment should be visible to student or not
#' @param to_group Whether the comment should be sent to all members of the group
#'
#' @export
#' @md
#' @examples
#' \dontrun{comment_submission(1350207, 5681164, 4928217, "test")}
comment_submission <- function(course_id, assignment_id, user_id, comm,
                             to_group = TRUE, visible = TRUE) {
  url <- make_canvas_url("courses", course_id, "assignments",
                         assignment_id, "submissions", user_id)
  args <- list(access_token = check_token(),
               `comment[text_comment]` = comm,
               `comment[group_comment]` = to_group,
               `include[visibility]` = visible,
               per_page = 100)

  invisible(canvas_query(url, args, "PUT"))
}

#' * `grade_submission`: Grade a submission
#'
#' @rdname submission_response
#' @param grade grade to give
#'
#' @export
#' @md
#'
#' @examples
#' \dontrun{grade_submission(1350207, 5681164, 4928217, 80)}
grade_submission <- function(course_id, assignment_id, user_id, grade) {
  url <- make_canvas_url("courses", course_id, "assignments",
                         assignment_id, "submissions", user_id)
  args <- list(access_token = check_token(),
               `submission[posted_grade]` = grade,
               per_page = 100)

  invisible(canvas_query(url, args, "PUT"))
}
