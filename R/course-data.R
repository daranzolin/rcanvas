#' @importFrom magrittr %>%
#'
#' @title Function to list all courses.
#'
#' @param user_id Optional argument to specify courses for specific user id
#' @param include Optional argument to specify additional information such as "teachers", "total_students", etc.
#'
#' @return data frame
#' @export
#'
#' @examples
#' #' get_course_list()
#' #' get_course_list(user_id = 366)
#' #' get_course_list(include = c("teachers", "total_students"))
get_course_list <- function(user_id = NULL, include = NULL) {
  if (!is.null(user_id)) {
    url <- make_canvas_url("users", user_id, "courses")
  } else {
    url <- make_canvas_url("courses")
  }
  args <- list(
               per_page = 100,
               user_id = user_id
               )
  include <- iter_args_list(include, "include[]")
  args <- c(args, include)
  dat <- process_response(url, args)
  return(unique(dat))
}

get_account_course_list <- function(acc_id = NULL, include = NULL) {
  if (!is.null(acc_id)) {
    url <- make_canvas_url("accounts", acc_id, "courses")
  } else {
    url <- make_canvas_url("courses")
  }
  args <- list(
    per_page = 100,
    acc_id = acc_id
  )
  include <- iter_args_list(include, "include[]")
  args <- c(args, include)
  dat <- process_response(url, args)
  return(unique(dat))
}

get_term_course_list <- function(term_id = NULL, acc_id = NULL, include = NULL) {
  if (!is.null(term_id)) {
    url <- paste0(canvas_url(), paste("accounts/", acc_id, "/courses?enrollment_term_id=", term_id, sep = "" ))
  } else {
    url <- paste0(canvas_url(), paste("accounts", acc_id, "courses", sep = "/"))
  }
  args <- list(
    per_page = 100,
    term_id = term_id
  )
  include <- iter_args_list(include, "include[]")
  args <- c(args, include)
  dat <- process_response(url, args)
  return(unique(dat))
}

#' @title Function to return course analytics data.
#'
#' @description Returns a data.frame of course analytics data. Note: if an individual's user_id is specified,
#' the function will return a list.
#'
#' @param course_id A valid Canvas course id
#' @param type One of "assignments", "activity", or "student_summaries"
#' @param user_id Optional argument to specify type analytics for individual user id
#'
#' @return data frame or list if user_id is specified
#' @export
#'
#' @examples
#' #' get_course_analytics_data(course_id = 20)
#' #' get_course_analytics_data(course_id = 17, type = "activity")
#' #' get_course_analytics_data(course_id = 17, type = "student_summaries", user_id = 366)
get_course_analytics_data <- function(course_id, type = "assignments", user_id = NULL) {
  if (!is.null(user_id)) {
    url <- make_canvas_url("courses", course_id, "analytics/users", user_id, type)
  } else {
    url <- make_canvas_url("courses", course_id, "analytics", type)
  }
  if (type == "communication" & is.null(user_id)) {
    stop("user_id must be specified for communication data")
  }
  args <- list(
    per_page = 100,
    user_id = user_id
  )
  resp <- canvas_query(url, args)
  json <- httr::content(resp, "text")
  if (json == "[]") stop("Nothing available for this course.")
  jsonlite::fromJSON(json, flatten = TRUE)
}

#' @title Function to return various course items.
#'
#' @description Returns a data.frame of various course items. See "item" argument below. Omitting the "item argument
#' returns a course object.
#'
#' @param course_id A valid Canvas course id
#' @param item Optional -- one of "settings", "discussion_topics", "todo", "enrollments", "features", "files", "modules", "front_page", "pages", "quizzes", "folders".
#' @param include Optional additions to the query string
#' @return data frame
#' @export
#'
#' @examples
#' #' get_course_items(course_id = 20, item = "settings")
#' #' get_course_items(course_id = 20, item = "enrollments")
#' #' get_course_items(20, item = "users", include = "email")
get_course_items <- function(course_id, item, include = NULL) {
  valid_items <- c("settings", "discussion_topics", "todo", "enrollments", "users", "students",
                   "features", "assignments", "files", "modules", "front_page", "pages", "quizzes",
                   "folders", "assignment_groups")
  if (!missing(item) && !item %in% valid_items) {
    stop(paste("item argument must be one of:", paste(valid_items, collapse = ", ")))
  }
  if (!missing(item)) {
    url <- make_canvas_url("courses", course_id, item)
  } else {
    #Omitting the item argument will return general information about the course
    url <- make_canvas_url("courses", course_id)
  }
  args <- list(per_page = 100)
  include <- iter_args_list(include, "include[]")
  args <- c(args, include)
  process_response(url, args) %>%
    dplyr::mutate(course_id = course_id)
}

#' @importFrom magrittr %>%
#' @title Search all courses
#'
#' @description Returns a data.frame of all public courses (optoinally matching the search term)
#'
#' @param search optional search keyword
#' @return data frame
#' @export
#'
#' @examples
#'   \dontrun{search_courses()}
#'   \dontrun{search_courses(search="big data")}
search_courses <- function(search=NULL) {
  url <- paste0(canvas_url(), paste("search", "all_courses", sep = "/"))
  args = list(per_page=100)
  if (!is.null(search)) args["search"] = search
  resp <- canvas_query(url, args, "GET")

  resp = canvas_query(url, args, "GET") %>%
    paginate() %>%
    purrr::map(httr::content, "text") %>%
    purrr::map(jsonlite::fromJSON, flatten = TRUE) %>%
    (function(x) x[[1]])

  return(if (is.null(nrow(resp))) data.frame() else resp)
}

#' @title Function to return course outcome results.
#'
#' @description Returns a data.frame containing course outcome results.
#'
#' @param course_id A valid Canvas course id
#' @return data frame
#' @export
#'
#' @examples
#' #' get_outcome_results(course_id = 20)
get_outcome_results <- function(course_id) {
  url <- paste0(canvas_url(), paste("courses", course_id, "outcome_results", sep = "/"))
  args <- list(per_page = 100)
  dat <- process_response(url, args)

  return(unique(dat))
}
