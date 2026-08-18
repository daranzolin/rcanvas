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

#' Delete an assignment from a course
#'
#' Deletes a single assignment. Canvas moves the assignment to a deleted state
#' rather than removing it outright, so it can usually be restored from the
#' course's "Undelete" page (\code{/courses/:id/undelete}) shortly afterwards.
#' Any submissions and grades attached to the assignment go with it.
#'
#' @param course_id A valid course id
#' @param assignment_id A valid assignment id
#'
#' @return The httr response, invisibly
#' @export
#'
#' @examples
#' \dontrun{
#' delete_assignment(course_id = 20, assignment_id = 12345)
#' }
delete_assignment <- function(course_id, assignment_id) {
  # DELETE /api/v1/courses/:course_id/assignments/:id
  stopifnot(!is.null(course_id), !is.null(assignment_id))
  stopifnot(length(assignment_id) == 1)

  url <- make_canvas_url("courses", course_id, "assignments", assignment_id)
  resp <- canvas_query(url, type = "DELETE")

  httr::stop_for_status(resp)
  message(sprintf("Assignment %s deleted", assignment_id))
  invisible(resp)
}

#' Delete several assignments from a course
#'
#' A thin wrapper around \code{\link{delete_assignment}} for clearing out
#' unused assignments in bulk, which is common after copying a course shell that
#' carries publisher content you do not assign.
#'
#' Deletion is not reversible through this function. Call it with
#' \code{dry_run = TRUE} first to print what would be deleted.
#'
#' @param course_id A valid course id
#' @param assignment_ids A vector of assignment ids
#' @param dry_run If TRUE (the default), print the assignments that would be
#'   deleted and delete nothing
#'
#' @return A data frame of assignment ids and the resulting status codes,
#'   invisibly. Returns NULL for a dry run.
#' @export
#'
#' @examples
#' \dontrun{
#' unused <- c(12345, 12346)
#' delete_assignments(20, unused)                  # dry run, deletes nothing
#' delete_assignments(20, unused, dry_run = FALSE) # actually deletes
#' }
delete_assignments <- function(course_id, assignment_ids, dry_run = TRUE) {
  stopifnot(!is.null(course_id), length(assignment_ids) > 0)

  if (dry_run) {
    existing <- get_assignment_list(course_id)
    hits <- existing[existing$id %in% assignment_ids, c("id", "name")]
    message(sprintf("Dry run: %d assignment(s) would be deleted from course %s.",
                    nrow(hits), course_id))
    print(hits)
    missing <- setdiff(assignment_ids, existing$id)
    if (length(missing) > 0) {
      warning(sprintf("Not found in course %s: %s",
                      course_id, paste(missing, collapse = ", ")))
    }
    message("Re-run with dry_run = FALSE to delete.")
    return(invisible(NULL))
  }

  status <- vapply(assignment_ids, function(id) {
    resp <- try(delete_assignment(course_id, id), silent = TRUE)
    if (inherits(resp, "try-error")) NA_integer_ else resp$status_code
  }, integer(1))

  invisible(data.frame(assignment_id = assignment_ids, status = status))
}
