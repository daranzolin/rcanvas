#' Get a course gradebook
#'
#' @importFrom magrittr %>%
#'
#' @param course_id A valid course id
#'
#' @return A gradebook (in long format)
#'
#' @examples
#' get_course_gradebook(20)
#'
#' @export
get_course_gradebook <- function(course_id) {
  course_assignments <- get_course_items(course_id, "assignments")
  students <- get_course_items(course_id, "enrollments") %>%
    dplyr::select(user.name, user_id, grades.final_score, course_id)
  submissions <- purrr::map2_df(course_id, course_assignments$id,
                                get_assignment_submissions)

  gradebook <- dplyr::left_join(submissions, students, by = "user_id") %>%
    dplyr::left_join(course_assignments %>%
                       dplyr::select(id, assignment_name = name),
                     by = c("assignment_id" = "id"))
  return(gradebook)
}

get_assignment_submissions <- function(course_id, assignment_id) {
  url <- sprintf("%s/courses/%s/assignments/%s/submissions",
                 canvas_url(), course_id, assignment_id)
  canvas_query(url, args = list(per_page = 100)) %>%
    httr::content("text") %>%
    jsonlite::fromJSON(flatten = TRUE)
}
