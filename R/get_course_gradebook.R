#' Get a course gradebook
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %$%
#'
#' @param course_id A valid course id
#'
#' @return A gradebook
#' @export
#'
#' @examples
#' get_course_gradebook(20)
get_course_gradebook <- function(course_id) {
  course_assignment_ids <- get_course_items(course_id, "assignments") %$%
    id %>% as.list()
  course_ids <- rep(course_id, length(course_assignment_ids)) %>% as.list()

  get_assignment_submissions <- function(course_id, assignment_id) {
    url <- sprintf("%s/courses/%s/assignments/%s/submissions",
                   canvas_url(), course_id, assignment_id)
    canvas_query(url, args = list(per_page = 100)) %>%
      httr::content("text") %>%
      jsonlite::fromJSON(flatten = TRUE)
  }

  assignments <- purrr::map2_df(course_ids, course_assignment_ids,
                                get_assignment_submissions)
  students <- get_course_items(course_id, "enrollments") %>%
    dplyr::select(user.name, user_id, grades.final_score, course_id)
  dplyr::left_join(assignments, students, by = "user_id")
}
