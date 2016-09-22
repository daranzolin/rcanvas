#' Function to list all courses.
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

#' Function to return course analytics data. Note: if an individual's user_id is specified,
#' the function will return a list.
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

#' Function to return course members.
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

#' Function to return various course items. See "item" argument below. Omitting the "item argument
#' returns a course object.
#'
#' @param course_id valid Canvas course id
#' @param item Optional -- one of "settings", "discussion_topics", "todo", "enrollments", "features", "files", "modules", "front_page", "pages", "quizzes"
#'
#' @return data.frame
#' @export
#'
#' @examples
#' #' get_course_items(course_id = 20, item = "settings")
#' #' get_course_items(course_id = 20, item = "enrollments")
get_course_items <- function(course_id, item) {
  valid_items <- c("settings", "discussion_topics", "todo", "enrollments",
                   "features", "files", "modules", "front_page", "pages", "quizzes")
  if (!missing(item) && !item %in% valid_items) {
    stop("item argument must be one of 'settings', 'discussion_topics', 'todo', 'enrollments', 'features', 'files', 'modules', 'front_page', 'pages', 'quizzes'")
  }
  if (!missing(item)) {
    url <- paste0(canvas_url(), paste("courses", course_id, item, sep = "/"))
    canvas_query(url)
  } else {
    url <- paste0(canvas_url(), paste("courses", course_id, sep = "/"))
  }
  dat <- canvas_query(url)
  if (class(dat) == "list") {
    dat <- data.frame(unlist(dat))
    dat$course_id <- course_id
    names(dat)[1] <- "item"
  }
  dat
}
