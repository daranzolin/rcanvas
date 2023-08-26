#' Get List of modules in a course
#'
#' @param course_id a valid course id
#'
#' @return dataframe of modules
#' @export
#'
#' @examples
#' get_module_list(course_id = 27)
get_module_list <- function(course_id = NULL) {
  stopifnot(!is.null(course_id))

  url <- make_canvas_url("courses", course_id, "modules")

  args <- list(per_page = 100)

  process_response(url, args)
}
