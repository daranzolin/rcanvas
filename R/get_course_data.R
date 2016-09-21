#' Gets course level data for assignments, activity, and student summaries
#'
#' @param course_id Canvas course id, could be obtained from get_course_list()
#' @param type one of "assignments", "activity", or "student_summaries"
#'
#' @return
#' @export
#'
#' @examples
get_course_data <- function(course_id, type = "assignments") {
  url <- paste0(canvas_courses(), course_id, "/analytics/", type)
  canvas_query(url)
}
