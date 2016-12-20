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
  resp <- canvas_query(url, args)
  json <- httr::content(resp, "text")
  if (json == "[]") stop("No submissions available.")
  dat <- jsonlite::fromJSON(json, flatten = TRUE)
  other_pages <- get_pages(resp)[-1]
  if (length(other_pages) != 0) {
    dat_list <- other_pages %>%
      purrr::map(canvas_query, args) %>%
      purrr::map(httr::content, "text") %>%
      purrr::map(jsonlite::fromJSON, flatten = TRUE)
    dat <- dplyr::bind_rows(dat, dat_list)
  }
  dat
}
