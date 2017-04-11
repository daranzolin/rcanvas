#' Get announcements for a course
#'
#' We transform the course id from a numeric (e.g. 123) to a context id
#' (e.g. course_123).
#'
#' @param course_id numeric course id
#' @param start_date Only return announcements posted since the start_date (inclusive). Defaults to 14 days ago. The value should be formatted as: yyyy-mm-dd or ISO 8601 YYYY-MM-DDTHH:MM:SSZ.
#' @param end_date Only return announcements posted before the end_date (inclusive). Defaults to 28 days from start_date. The value should be formatted as: yyyy-mm-dd or ISO 8601 YYYY-MM-DDTHH:MM:SSZ. Announcements scheduled for future posting will only be returned to course administrators.
#' @param active_only	Only return active announcements that have been published. Applies only to requesting users that have permission to view unpublished items. Defaults to false for users with access to view unpublished items, otherwise true and unmodifiable.
#'
#' @return announcements for a given course
#' @export
#'
#' @examples
get_announcements <- function(course_id, start_date = NULL, end_date = NULL,
                              active_only = FALSE) {
  if (!grepl(pattern = "course", x = course_id)) {
    course_id <- paste0("course_", course_id)
  }
  url <- paste0(canvas_url(), "announcements")
  args <- list(access_token = check_token(),
               per_page = 100)
  include <- iter_args_list(course_id, "context_codes[]")
  include2 <- iter_args_list(start_date, "start_date")
  include3 <- iter_args_list(end_date, "end_date")
  args <- c(args, include, include2, include3)
  dat <- process_response(url, args)
  dat
}
