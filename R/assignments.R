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

#' Upload a file to an assignment and submit it
#'
#' @param course_id Course ID
#' @param assignment_id Assignment ID
#' @param user_id User ID
#' @param file_name Name of file to be submitted
#'
#' @return data frame
#' @export
submit_file_upload_assignment <-
  function(course_id, assignment_id, user_id,
          file_name,
          parent_folder_id = NULL,
          parent_folder_path = "/",
          on_duplicate = "overwrite") {

  res <- upload_assignment_file(course_id, assignment_id, user_id, file_name)
  id <- httr::content(res)$id
  url <- make_canvas_url("courses", course_id,
                         "assignments", assignment_id,
                         "submissions")
  args <- list("submission[submission_type]" = "online_upload",
               "submission[file_ids][]" = id)

  res <- canvas_query(url, args, "POST")
  if(res$status_code >= 200 & res$status_code < 300)
    message(sprintf("File %s successfully submitted as assignment.", file_name))
  else
    message(sprintf("Failed to submit %s.", file_name))
  invisible(res)

}
