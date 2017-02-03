#' Get announcements for a course
#'
#' We transform the course id from a numeric (e.g. 123) to a context id
#' (e.g. course_123).
#'
#' @param course_id numeric course id
#'
#' @return announcements for a given course
#' @export
#'
#' @examples
get_announcements <- function(course_id) {
  if(!grepl(pattern = "course", x = course_id)) {
    course_id <- paste0("course_", course_id)
  }
  url <- paste0(canvas_url(), "announcements")
  args <- list(access_token = check_token(),
               per_page = 100)
  include <- iter_args_list(course_id, "context_codes[]")
  args <- c(args, include)
  dat <- process_response(url, args)
  dat
}
