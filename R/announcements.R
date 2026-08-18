#' Get announcements for one or more courses
#'
#' Course ids are transformed from numeric ids (for example, 123) to Canvas
#' context codes (for example, \code{course_123}).
#'
#' @param course_id One or more numeric course ids or Canvas course context codes.
#' @param start_date Only return announcements posted since this date (inclusive).
#' Use yyyy-mm-dd or an ISO 8601 timestamp.
#' @param end_date Only return announcements posted before this date (inclusive).
#' Use yyyy-mm-dd or an ISO 8601 timestamp. Future announcements are returned
#' only to users who can administer the course.
#' @param active_only Only return active announcements that have been published.
#' Applies only to users with permission to view unpublished announcements.
#'
#' @return A data frame of announcements.
#' @export
#'
#' @examples
#' \dontrun{get_announcements(course_id = 27)}
#' \dontrun{get_announcements(course_id = 27, start_date = "2017-02-01")}
get_announcements <- function(course_id, start_date = NULL, end_date = NULL,
                              active_only = FALSE) {
  stopifnot(length(course_id) > 0)
  course_id <- as.character(course_id)
  course_id <- dplyr::if_else(
    stringr::str_detect(course_id, "^course_"),
    course_id,
    stringr::str_c("course_", course_id)
  )

  url <- make_canvas_url("announcements")
  args <- list(per_page = 100, active_only = active_only)
  include <- iter_args_list(course_id, "context_codes[]")
  include2 <- iter_args_list(start_date, "start_date")
  include3 <- iter_args_list(end_date, "end_date")
  args <- c(args, include, include2, include3)
  process_response(url, args)
}

#' Create an announcement
#'
#' Canvas announcements are discussion topics with
#' \code{is_announcement = TRUE}. By default this function creates an
#' unpublished draft so that it cannot notify a course accidentally. Set
#' \code{published = TRUE} to post immediately, or combine it with
#' \code{delayed_post_at} to schedule publication.
#'
#' @param course_id A valid course id.
#' @param title Announcement title.
#' @param message Announcement body as text or HTML.
#' @param published Whether the announcement is published. Defaults to
#' \code{FALSE}.
#' @param delayed_post_at Optional ISO 8601 timestamp at which Canvas should
#' publish the announcement.
#' @param lock_comment Whether participant comments are disabled.
#' @param specific_sections Optional section id or vector of section ids. Use
#' \code{"all"} or \code{NULL} for the whole course.
#'
#' @return The httr response, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' create_announcement(20, "Welcome", "<p>Welcome to the course.</p>")
#' create_announcement(
#'   20, "Exam reminder", "<p>Reserve your seat.</p>",
#'   published = TRUE, delayed_post_at = "2026-09-25T14:00:00Z",
#'   lock_comment = TRUE
#' )
#' }
create_announcement <- function(course_id, title, message, published = FALSE,
                                delayed_post_at = NULL,
                                lock_comment = FALSE,
                                specific_sections = NULL) {
  stopifnot(length(course_id) == 1, length(title) == 1, length(message) == 1)

  url <- make_canvas_url("courses", course_id, "discussion_topics")
  args <- list(
    title = title,
    message = message,
    is_announcement = TRUE,
    published = published,
    delayed_post_at = delayed_post_at,
    lock_comment = lock_comment,
    specific_sections = announcement_sections(specific_sections)
  )
  resp <- canvas_query(url, args, "POST")

  message(stringr::str_c(
    "Announcement '", title, "' created in course ", course_id
  ))
  invisible(resp)
}

#' Update an announcement
#'
#' Only non-NULL fields are sent to Canvas. To clear a scheduled publication
#' time, pass an empty string as \code{delayed_post_at}.
#'
#' @param announcement_id A valid announcement (discussion topic) id.
#' @inheritParams create_announcement
#' @param published Whether to publish the announcement.
#' \code{NULL} leaves the current value unchanged.
#' @param delayed_post_at Optional ISO 8601 publication timestamp.
#' \code{NULL} leaves it unchanged; an empty string clears it.
#' @param lock_comment Whether participant comments are disabled.
#' \code{NULL} leaves the current value unchanged.
#' @param specific_sections Optional section id or vector of section ids.
#' \code{NULL} leaves the current targeting unchanged; use \code{"all"} for the
#' whole course.
#'
#' @return The httr response, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' update_announcement(20, 12345, published = TRUE)
#' update_announcement(20, 12345, lock_comment = TRUE)
#' }
update_announcement <- function(course_id, announcement_id, title = NULL,
                                message = NULL, published = NULL,
                                delayed_post_at = NULL,
                                lock_comment = NULL,
                                specific_sections = NULL) {
  stopifnot(length(course_id) == 1, length(announcement_id) == 1)

  args <- list(
    title = title,
    message = message,
    published = published,
    delayed_post_at = delayed_post_at,
    lock_comment = lock_comment,
    specific_sections = announcement_sections(specific_sections)
  ) %>%
    purrr::discard(is.null)
  if (length(args) == 0) {
    stop("Provide at least one announcement field to update.", call. = FALSE)
  }
  args$is_announcement <- TRUE

  url <- make_canvas_url("courses", course_id, "discussion_topics",
                         announcement_id)
  resp <- canvas_query(url, args, "PUT")

  message(stringr::str_c(
    "Announcement ", announcement_id, " updated in course ", course_id
  ))
  invisible(resp)
}

#' Delete an announcement
#'
#' Canvas deletes announcements through the Discussion Topics API. Deleted
#' announcements may no longer be visible to course participants.
#'
#' @param course_id A valid course id.
#' @param announcement_id A valid announcement (discussion topic) id.
#'
#' @return The httr response, invisibly.
#' @export
#'
#' @examples
#' \dontrun{delete_announcement(20, 12345)}
delete_announcement <- function(course_id, announcement_id) {
  stopifnot(length(course_id) == 1, length(announcement_id) == 1)

  url <- make_canvas_url("courses", course_id, "discussion_topics",
                         announcement_id)
  resp <- canvas_query(url, type = "DELETE")

  message(stringr::str_c(
    "Announcement ", announcement_id, " deleted from course ", course_id
  ))
  invisible(resp)
}

announcement_sections <- function(specific_sections) {
  if (is.null(specific_sections) || length(specific_sections) == 0) return(NULL)
  stringr::str_c(specific_sections, collapse = ",")
}
