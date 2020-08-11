#' Discussion functions
#'
#' A group of functions which deal with [discussion topics](https://canvas.instructure.com/doc/api/discussion_topics.html).
#'
#' * `get_discussions_context`: get all discussions belonging to a course or group. Note that theoretically this should include announcements, as they are technically discussions, but does not. Use `get_announcements` instead.
#'
#' @param object_id course or group id
#' @param object_type "courses" or "groups"
#' @param include If "all_dates" is passed, all dates associated with graded discussions' assignments will be included.
#'
#' @return discussions belonging to requested context
#' @export
#' @md
#'
#' @examples
#' get_discussions_context(4371405)
get_discussions_context <- function(object_id, object_type = "courses",
                                    include = NULL) {
  stopifnot(object_type %in% c("courses", "groups"))
  url <- make_canvas_url(object_type, object_id, "discussion_topics")
  args <- list(per_page = 100)
  include <- iter_args_list(include, "include[]")
  args <- c(args, include)
  dat <- process_response(url, args)
  dat
}

#'
#' * `get_discussion_id`: Get single discussion by id
#'
#' @param discussion_id  specific id of discussion to get/update
#' @rdname get_discussions_context
#' @return single discussion
#' @export
#' @md
#'
#' @examples
#' get_discussion_id(4371405, 1350207)
get_discussion_id <- function(discussion_id, object_id, object_type = "courses") {
  stopifnot(object_type %in% c("courses", "groups"))
  url <- make_canvas_url(object_type, object_id, "discussion_topics", discussion_id)
  args <- list(per_page = 100)
  include <- iter_args_list(NULL, "include[]")
  args <- c(args, include)
  dat <- process_response(url, args)
  dat
}

#' * `update_discussion_id`: Update discussion by id
#'
#' @param message new body of discussion id
#'
#' @rdname get_discussions_context
#' @return silently sends put request and updates
#' @export
#' @md
#'
#' @examples
#' update_discussion_id(4371405, 1350207, newtext)
update_discussion_id <- function(discussion_id, object_id, message,
                                 object_type = "courses") {
  stopifnot(object_type %in% c("courses", "groups"))
  url <- make_canvas_url(object_type, object_id, "discussion_topics", discussion_id)
  args <- list(access_token = check_token(),
               message = message,
               per_page = 100)
  canvas_query(url, args, "PUT")
}
