#' Create a course content migration
#'
#' @param course_id a valid course id (the course to copy content into)
#' @param migration_type Migration type. Default allowed values: canvas_cartridge_importer, common_cartridge_importer,
#' course_copy_importer, zip_file_importer, qti_converter, moodle_converter
#' @param name Required if uploading a file. This is the first step in uploading a file to the content migration.
#' @param file_url A URL to download the file from. Must not require authentication.
#' @param source_course_id a valid course id (the course to copy content from)
#' @param folder_id The folder to unzip the .zip file into for a zip_file_import.
#' @param overwrite_quizzes boolean, Whether to overwrite quizzes with the same identifiers between content packages.
#' @param question_bank_id The existing question bank ID to import questions into if not specified in the content package.
#' @param question_bank_name The question bank to import questions into if not specified in the content package, if both bank id and name are set, id will take precedence.
#' @param shift_dates boolean, Whether to shift dates in the copied course
#' @param old_start_date The original start date of the source content/course
#' @param old_end_date The original end date of the source content/course
#' @param new_start_date The new start date for the content/course
#' @param new_end_date The new end date for the content/course
#' @param day_substitutions Move anything scheduled for day 'X' to the specified day. (0-Sunday, 1-Monday, 2-Tuesday, 3-Wednesday, 4-Thursday, 5-Friday, 6-Saturday)
#' @param remove_dates boolean, Whether to remove dates in the copied course. Cannot be used in conjunction with shift_dates.
#'
#' @return invisible
#' @export
#'
#' @examples
#' \dontrun{create_course_content_migration(course_id = 1532751, source_course_id = 1532594)}
create_course_content_migration <- function(course_id,
                                            migration_type = "course_copy_importer",
                                            name = NULL,
                                            file_url = NULL,
                                            source_course_id = NULL,
                                            folder_id = NULL,
                                            overwrite_quizzes = NULL,
                                            question_bank_id = NULL,
                                            question_bank_name = NULL,
                                            shift_dates = NULL,
                                            old_start_date = NULL,
                                            old_end_date = NULL,
                                            new_start_date = NULL,
                                            new_end_date = NULL,
                                            day_substitutions = NULL,
                                            remove_dates = NULL) {
  message(sprintf("Copying course content from %s to %s...", source_course_id, course_id))
  url <- paste0(canvas_url(), "courses/", course_id, "/content_migrations")

  pre_attachment_args <- sc(list(name = name))
  names(pre_attachment_args) <- sprintf("pre_attachment[%s]", names(pre_attachment_args))

  settings_args <- sc(list(
    file_url = file_url,
    source_course_id = source_course_id,
    folder_id = folder_id,
    overwrite_quizzes = overwrite_quizzes,
    question_bank_id = question_bank_id,
    question_bank_name = question_bank_name
  ))
  names(settings_args) <- sprintf("settings[%s]", names(settings_args))

  date_shift_options_args <- sc(list(
    shift_dates = shift_dates,
    old_start_date = old_start_date,
    old_end_date = old_end_date,
    new_start_date = new_start_date,
    new_end_date = new_end_date,
    day_substitutions = day_substitutions,
    remove_dates = remove_dates
  ))
  names(date_shift_options_args)  <- sprintf("date_shift_options[%s]", names(date_shift_options_args))

  args <- list(pre_attachment_args, settings_args, date_shift_options_args) %>%
    purrr::discard(is_empty) %>%
    purrr::flatten() %>%
    append(list(migration_type = migration_type))
  invisible(canvas_query(url, args, "POST"))
  message("Success!")
}
