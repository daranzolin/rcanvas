#' Get a course gradebook
#'
#' @importFrom magrittr %>%
#'
#' @param course_id A valid course id
#' @param progress logical; print page numbers as we go? (Defaults to FALSE)
#'
#' @return A gradebook (in long format)
#'
#' @examples
#' get_course_gradebook(20)
#'
#' @export
get_course_gradebook <- function(course_id, progress = FALSE) {
  # Retrieve assignments and enrolled students
  course_assignments <- get_course_items(course_id, "assignments")
  students <- get_course_items(course_id, "enrollments") %>%
    dplyr::filter(role == "StudentEnrollment", user.name != "Test Student") %>%
    dplyr::select(user.name, user_id, grades.final_score, course_id) %>%
    unique()
  
  # Initialize list to collect submissions
  all_submissions <- list()
  
  # Loop over each assignment
  for (assignment_id in course_assignments$id) {
    page <- 1
    repeat {
      if (progress) cat(sprintf("Assignment %s, page %d\n", assignment_id, page))
      
      # Retrieve submissions with error handling and ensure tibble output
      subs <- tryCatch({
        res <- get_assignment_submissions(course_id, assignment_id, page)
        if (!inherits(res, "data.frame")) res <- tibble::as_tibble(res)
        res
      }, error = function(e) {
        warning(sprintf("Error retrieving assignment %s, page %d: %s", assignment_id, page, e$message))
        tibble::tibble()
      })
      
      # Exit the loop if no submissions are returned
      if (nrow(subs) == 0) break
      
      # Ensure required join columns exist; warn and add dummy values if missing
      if (!"user_id" %in% names(subs)) {
        warning(sprintf("Column 'user_id' missing for assignment %s, page %d. Adding dummy column.", assignment_id, page))
        subs$user_id <- NA_integer_
      }
      if (!"assignment_id" %in% names(subs)) {
        warning(sprintf("Column 'assignment_id' missing for assignment %s, page %d. Inserting assignment_id.", assignment_id, page))
        subs$assignment_id <- assignment_id
      }
      
      # Append the current page's submissions to the list
      all_submissions[[length(all_submissions) + 1]] <- subs
      page <- page + 1
    }
  }
  
  # Combine all submissions and join with student and assignment data
  submissions_all <- dplyr::bind_rows(all_submissions)
  gradebook <- submissions_all %>%
    dplyr::left_join(students, by = "user_id") %>%
    dplyr::left_join(course_assignments %>% dplyr::select(id, assignment_name = name),
                     by = c("assignment_id" = "id"))
  
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
