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
  n_pages <- ceiling(nrow(students)/100)

  gradebook <- purrr::map_df(seq_len(n_pages), function(page) {
    submissions <- purrr::pmap_dfr(list(course_id, course_assignments$id, page),
                                   get_assignment_submissions)
    gradebook_page <- dplyr::left_join(submissions, students, by = "user_id") %>%
      dplyr::left_join(course_assignments %>%
                         dplyr::select(id, assignment_name = name),
                       by = c("assignment_id" = "id"))
    return(gradebook_page)
  })

  return(gradebook)
}

get_assignment_submissions <- function(course_id, assignment_id, page) {
  url <- sprintf("%s/courses/%s/assignments/%s/submissions",
                 canvas_url(), course_id, assignment_id)
  submissions <- canvas_query(url, args = list(per_page = 100, page = page)) %>%
    httr::content("text") %>%
    jsonlite::fromJSON(flatten = TRUE)
  return(submissions)
}
