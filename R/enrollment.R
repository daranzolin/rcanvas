#' Enroll a user into the course
#'
#' @param course_id ID of the course (or section, see below)
#' @param user_id ID of the user
#' @param type enrollment type
#' @param state enrollment state
#' @param section if TRUE, course_id should be a section_id instead of a course_id and students will be enrolled into this section
#' @param ... any other arguments passed to the API
add_enrollment <- function(course_id, user_id, type, state, section=F, ...) {
  url <- make_canvas_url(ifelse(section, "sections", "courses"), course_id, "enrollments")
  args <- list("enrollment[user_id]" = user_id, "enrollment[type]"=type, "enrollment[enrollment_state]"=state, ...)
  message(url)
  canvas_query(url, args, "POST")
}

#' Enroll user(s) into a course (or multiple courses)
#'
#' Enrolls the given user(s) into the course. If multiple course_ids are given, it should be of the same length as user_id,
#' and each user will be enrolled into the corresponding course.
#'
#' @param course_id ID of the course (or section, see below)
#' @param user_id ID of the user
#' @param type enrollment type
#' @param state enrollment state
#' @param section if TRUE, course_id should be a section_id instead of a course_id and students will be enrolled into this section
#' @param ... any other arguments passed to the API
#' @rdname enrollment
#' @export
add_enrollments <- function(course_id, user_ids, type=c("StudentEnrollment", "TeacherEnrollment", "TaEnrollment", "ObserverEnrollment", "DesignerEnrollment"),
                            state=c("invited", "active", "inactive"), section=F, ...) {
  type <- match.arg(type)
  state <- match.arg(state)
  invisible(purrr::map2(course_id, user_ids, add_enrollment, type=type, state=state, section=section, ...))
}


