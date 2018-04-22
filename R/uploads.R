#' Upload a file to a course
#'
#' @param course_id valid course id
#' @param file_name file name in your current directory.
#' Any UTF-8 name is allowed. Path components such as `/` and `\` will be treated as part of the filename,
#' not a path to a sub-folder.
#' @param parent_folder_id The id of the folder to store the file in.
#' If this and parent_folder_path are sent an error will be returned. If neither is given,
#' a default folder will be used.
#' @param parent_folder_path The path of the folder to store the file in. The path separator is the forward slash `/`, never a back slash.
#' The folder will be created if it does not already exist. This parameter only applies to file uploads
#' in a context that has folders, such as a user, a course, or a group. If this and parent_folder_id are
#' sent an error will be returned. If neither is given, a default folder will be used.
#' @param on_duplicate How to handle duplicate filenames. If "overwrite", then this file upload will
#' overwrite any other file in the folder with the same name. If "rename", then this file will be
#' renamed if another file in the folder exists with the given name. If no parameter is given,
#' This doesn't apply to file uploads in a context that doesn't have folders.
#'
#' @return invisible
#' @export
#'
#' @examples
#' upload_course_file(course_id = 13212, file_name = "activity.pdf")
upload_course_file <- function(course_id, file_name, parent_folder_id = NULL, parent_folder_path = "/", on_duplicate = "overwrite") {
  if (!is.null(parent_folder_id) && !is.null(parent_folder_path)) stop("Do not specify both parent folder id and parent folder path.")
  file_size <- file.info(file_name)$size
  url <- paste0(canvas_url(),
                paste("courses", course_id, "files", sep = "/"))
  args <- sc(list(name = file_name,
                  size = file_size,
                  parent_folder_id = parent_folder_id,
                  parent_folder_path = parent_folder_path,
                  on_duplicate = on_duplicate))
  upload_resp <- canvas_query(url, args, "POST")
  upload_content <- httr::content(upload_resp)
  upload_url <- upload_content$upload_url
  upload_params <- upload_content$upload_params
  upload_params[[length(upload_params) + 1]] <- httr::upload_file(file_name)
  names(upload_params)[[length(upload_params)]] <- "file"
  invisible(httr::POST(url = upload_url,
             body = upload_params))
  message(sprintf("File %s uploaded", file_name))
}

#' Create a course folder
#'
#' @param course_id a valid course id
#' @param name name of the folder (required)
#' @param parent_folder_id The id of the folder to store the file in. If this and parent_folder_path
#' are sent an error will be returned. If neither is given, a default folder will be used.
#'
#' @return invisible
#' @export
#'
#' @examples
#' create_course_folder(34232, name = "activities")
create_course_folder <- function(course_id, name, parent_folder_id = NULL) {
  url <- paste0(canvas_url(),
                paste("courses", course_id, "folders", sep = "/"))
  args <- sc(list(name = name,
                  parent_folder_id = parent_folder_id))
  invisible(canvas_query(url, args, "POST"))
  message(sprintf("Folder %s created", name))
}
