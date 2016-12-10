#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @importFrom purrr map
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows
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
    url <- paste0(canvas_url(), paste("users", user_id, "courses", sep = "/"))
  } else {
    url <- paste0(canvas_url(), "courses")
  }
  args <- list(access_token = check_token(),
               per_page = 100,
               user_id = user_id)
  include <- iter_args_list(include, "include[]")
  args <- sc(
    c(args, include)
  )
  resp <- canvas_query(url, args)
  json <- httr::content(resp, "text")
  if (json == "[]") stop("Nothing available for this course.")
  dat <- jsonlite::fromJSON(json, flatten = TRUE)
  other_pages <- get_pages(resp)[-1]
  if (length(other_pages) != 0) {
    dat_list <- other_pages %>%
      purrr::map(canvas_query, args) %>%
      purrr::map(httr::content, "text") %>%
      purrr::map(jsonlite::fromJSON, flatten = TRUE)
    dat <- dplyr::bind_rows(dat, dat_list)
  }
  if (class(dat) == "list") {
    dat <- data.frame(unlist(dat))
    dat$course_id <- course_id
    names(dat)[1] <- "item"
  }
  dat
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
    url <- paste0(canvas_url(), paste("courses", course_id, "analytics/users", user_id, type, sep = "/"))
  } else {
    url <- paste0(canvas_url(), paste("courses", course_id, "analytics", type, sep = "/"))
  }
  if (type == "communication" & is.null(user_id)) {
    stop("user_id must be specified for communication data")
  }
  args <- list(access_token = check_token(),
               per_page = 500,
               user_id = user_id)
  args <- sc(args)
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
#' @param item Optional -- one of "settings", "discussion_topics", "todo", "enrollments", "features", "files", "modules", "front_page", "pages", "quizzes", etc.
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
                   "features", "assignments", "files", "modules", "front_page", "pages", "quizzes")
  if (!missing(item) && !item %in% valid_items) {
    stop(paste("item argument must be one of", valid_items))
  }
  if (!missing(item)) {
    url <- paste0(canvas_url(), paste("courses", course_id, item, sep = "/"))
  } else {
    #Omitting the item argument will return general information about the course
    url <- paste0(canvas_url(), paste("courses", course_id, sep = "/"))
  }
  args <- list(access_token = check_token(),
               per_page = 100)
  include <- iter_args_list(include, "include[]")
  args <- sc(
    c(args, include)
  )
  resp <- canvas_query(url, args)
  json <- httr::content(resp, "text")
  if (json == "[]") stop("Nothing available for this course.")
  dat <- jsonlite::fromJSON(json, flatten = TRUE)
  other_pages <- get_pages(resp)[-1]
  if (length(other_pages) != 0) {
    dat_list <- other_pages %>%
      purrr::map(canvas_query, args) %>%
      purrr::map(httr::content, "text") %>%
      purrr::map(jsonlite::fromJSON, flatten = TRUE)
    dat <- dplyr::bind_rows(dat, dat_list)
  }
  if (class(dat) == "list") {
    dat <- data.frame(unlist(dat))
    dat$course_id <- course_id
    names(dat)[1] <- "item"
  }
  dat
}

#' @title Function to set Canvas API token
#'
#' @description Given a Canvas token string, this function adds it to R's
#' environment variables so it can be found by rcanvas.
#'
#' @param token your API token
#'
#' @return nothing
#' @export
#'
#' @examples
#' set_canvas_token("abc123")
set_canvas_token <- function(token) {
  Sys.setenv(CANVAS_API_TOKEN = token)
}

#' @title Function to set Canvas domain url
#'
#' @description Given a Canvas domain url, this function adds it to R's
#' environment variables so it can be found by rcanvas.
#'
#' @param domain Canvas domain
#'
#' @return nothing
#' @export
#'
#' @examples
#' set_canvas_domain("https://canvas.upenn.edu")
set_canvas_domain <- function(domain) {
  Sys.setenv(CANVAS_DOMAIN = domain)
}

