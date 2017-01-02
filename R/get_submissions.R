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
#' #'get_submissions(27, "quizzes", 2915)
#' #'get_submissions(27, "assignments, 254)
get_submissions <- function(course_id, type, type_id) {
  if (!type %in% c("quizzes", "assignments")) stop("type must be 'quizzes' or 'assignments'")
  url <- sprintf("%scourses/%s/%s/%s/submissions", canvas_url(), course_id, type, type_id)
  args <- list(access_token = check_token(),
               per_page = 100)
  process_response(url, args)
}
