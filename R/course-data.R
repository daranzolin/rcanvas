#' List all courses
#'
#' @param user_id Optional argument to specify courses for specific user id
#'
#' @return data.frame
#' @export
#'
#' @examples
#' #' get_course_list()
#' #' get_course_list(user_id = 366)
get_course_list <- function(user_id = NULL) {
  if (!is.null(user_id)) {
    url <- paste0(canvas_url(), paste("users", user_id, "courses", sep = "/"))
  } else {
    url <- paste0(canvas_url(), "courses")
  }
  canvas_query(url)
}

#' Get course analytics data by type
#'
#' @param course_id a valid Canvas course id
#' @param type one of "assignments", "users", "activity", or "student_summaries"
#' @param user_id optional argument to specify type analytics for individual user id
#'
#' @return data.frame or list if user_id is specified
#' @export
#'
#' @examples
#' #' get_course_analytics_data(course_id = 20)
#' #' get_course_analytics_data(course_id = 17, type = "activity")
#' #' get_course_analytics_data(course_id = 17, type = "student_summaries", user_id = 366)
get_course_analytics_data <- function(course_id, type = "assignments", user_id = NULL) {
  if (!is.null(user_id)) {
    url <- paste0(canvas_url(), paste("courses", course_id, "analytics/users", user_id, type, sep = "/"))
  } else {
    url <- paste0(canvas_url(), paste("courses", course_id, "analytics", type, sep = "/"))
  }
  if (type == "communication" & is.null(user_id)) {
    stop("user_id must be specified for communication data")
  }
  canvas_query(url)
}

#' Get course users
#'
#' @param course_id valid Canvas course id
#' @param type one of "students" or "users"
#'
#' @return data.frame
#' @export
#'
#' @examples
#' #' get_course_members(course_id = 20)
#' #' get_course_members(course_id = 20, type = "users")
get_course_members <- function(course_id, type = "students") {
  url <- paste0(canvas_url(), paste("courses", course_id, type, sep = "/"))
  canvas_query(url)
}

#' Get course items
#'
#' @param course_id valid Canvas course id
#' @param item one of "settings", "discussion_topics", "todo", "enrollments", "features", "files", "modules", "front_page", "pages", "quizzes"
#'
#' @return data.frame
#' @export
#'
#' @examples
#' #' get_course_items(course_id = 20, item = "settings")
#' #' get_course_items(course_id = 20, item = "enrollments")
get_course_items <- function(course_id, item = NULL) {
  if (!is.null(item)) {
    url <- paste0(canvas_url(), paste("courses", course_id, item, sep = "/"))
    canvas_query(url)
  } else {
    url <- paste0(canvas_url(), paste("courses", course_id, sep = "/"))
  }
  canvas_query(url)
}
