#' Function to list all assignments.
#'
#' @param course_id Course ID
#'
#' @return data frame
#' @export
#'
#' @examples
#' #' get_assignment_list()
get_assignment_list <- function(course_id = NULL) {
  stopifnot(!is.null(course_id))

  url <- make_canvas_url("courses", course_id, "assignments")

  args <- list(per_page = 100)

  process_response(url, args)

}
