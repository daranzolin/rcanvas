#' Canvas Inbox conversations
#'
#' Helpers for listing, reading, creating, and replying to Canvas Inbox
#' conversations.
#'
#' @param scope Conversation mailbox to return. One of `"inbox"`,
#' `"unread"`, `"starred"`, `"archived"`, or `"sent"`.
#' `"inbox"` is the default Canvas view of read and unread,
#' non-archived conversations.
#' @param course_id Optional course id or vector of course ids used to filter
#' conversations.
#' @param filter Optional Canvas context filters such as `"group_42"` or
#' `"user_17"`. Course filters supplied here or through `course_id`
#' are combined.
#' @param filter_mode How multiple filters are combined. One of `"and"`,
#' `"or"`, or `"default or"`.
#' @param include Optional Canvas response expansions, such as `"uuid"`.
#'
#' @return `get_conversations()` returns a data frame.
#' @md
#' @export
#'
#' @examples
#' \dontrun{
#' get_conversations(course_id = 20)
#' get_conversations(scope = "unread", course_id = 20)
#' }
get_conversations <- function(scope = "inbox", course_id = NULL,
                              filter = NULL, filter_mode = "and",
                              include = NULL) {
  valid_scopes <- c("inbox", "unread", "starred", "archived", "sent")
  if (length(scope) != 1 || !scope %in% valid_scopes) {
    stop("`scope` must be one of: ",
         stringr::str_c(valid_scopes, collapse = ", "), call. = FALSE)
  }
  valid_filter_modes <- c("and", "or", "default or")
  if (length(filter_mode) != 1 || !filter_mode %in% valid_filter_modes) {
    stop("`filter_mode` must be one of: ",
         stringr::str_c(valid_filter_modes, collapse = ", "), call. = FALSE)
  }

  course_filters <- canvas_context_code(course_id, "course")
  filters <- c(course_filters, as.character(filter))
  filters <- filters[!is.na(filters) & nzchar(filters)]

  args <- list(
    per_page = 100,
    scope = if (identical(scope, "inbox")) NULL else scope,
    filter_mode = if (length(filters) > 0) filter_mode else NULL
  )
  args <- c(
    args,
    iter_args_list(filters, "filter[]"),
    iter_args_list(include, "include[]")
  )

  process_response(make_canvas_url("conversations"), args)
}

#' Get one Canvas Inbox conversation
#'
#' Unlike the Canvas API default, this function does not mark an unread
#' conversation as read unless explicitly requested.
#'
#' @param conversation_id A Canvas conversation id.
#' @param auto_mark_as_read Whether reading the conversation should mark it as
#' read. Defaults to `FALSE`.
#' @param scope Optional mailbox scope used to calculate the response's
#' `visible` field.
#' @param filter Optional Canvas context filters.
#' @param filter_mode How multiple filters are combined.
#'
#' @return A named list containing the conversation, its participants, and all
#' messages.
#' @md
#' @export
#'
#' @examples
#' \dontrun{get_conversation(12345)}
get_conversation <- function(conversation_id, auto_mark_as_read = FALSE,
                             scope = NULL, filter = NULL,
                             filter_mode = "and") {
  stopifnot(length(conversation_id) == 1)
  valid_filter_modes <- c("and", "or", "default or")
  if (length(filter_mode) != 1 || !filter_mode %in% valid_filter_modes) {
    stop("`filter_mode` must be one of: ",
         stringr::str_c(valid_filter_modes, collapse = ", "), call. = FALSE)
  }

  filters <- as.character(filter)
  filters <- filters[!is.na(filters) & nzchar(filters)]
  args <- list(
    auto_mark_as_read = auto_mark_as_read,
    scope = scope,
    filter_mode = if (length(filters) > 0) filter_mode else NULL
  )
  args <- c(args, iter_args_list(filters, "filter[]"))

  response <- canvas_query(
    make_canvas_url("conversations", conversation_id),
    args,
    "GET"
  )
  parse_canvas_json(response)
}

#' Update a Canvas Inbox conversation
#'
#' Change the read/archive state, subscription, or starred status of a
#' conversation. Only non-`NULL` fields are sent.
#'
#' @param conversation_id A Canvas conversation id.
#' @param workflow_state Optional state: `"read"`, `"unread"`, or
#' `"archived"`.
#' @param subscribed Optional logical subscription status.
#' @param starred Optional logical starred status.
#'
#' @return The Canvas API response, invisibly.
#' @md
#' @export
#'
#' @examples
#' \dontrun{update_conversation(12345, workflow_state = "unread")}
update_conversation <- function(conversation_id, workflow_state = NULL,
                                subscribed = NULL, starred = NULL) {
  stopifnot(length(conversation_id) == 1)
  if (!is.null(workflow_state) &&
      (length(workflow_state) != 1 ||
       !workflow_state %in% c("read", "unread", "archived"))) {
    stop("`workflow_state` must be one of: read, unread, archived.",
         call. = FALSE)
  }

  values <- list(
    workflow_state = workflow_state,
    subscribed = subscribed,
    starred = starred
  ) %>%
    purrr::discard(is.null)
  if (length(values) == 0) {
    stop("Provide at least one conversation field to update.", call. = FALSE)
  }
  names(values) <- stringr::str_c("conversation[", names(values), "]")

  response <- canvas_query(
    make_canvas_url("conversations", conversation_id),
    values,
    "PUT"
  )
  invisible(response)
}

#' Create a Canvas Inbox conversation
#'
#' @param recipient_ids One or more Canvas user ids, UUIDs prefixed with
#' `"uuid:"`, or course/group context codes.
#' @param subject Optional subject line. Canvas limits subjects to 255
#' characters.
#' @param body Message body.
#' @param course_id Optional course id used as the conversation context.
#' @param group_conversation Whether recipients share one group conversation.
#' The safer default, `FALSE`, creates separate private conversations when
#' there is more than one recipient.
#' @param force_new Whether to create a new private conversation even when a
#' conversation with the same recipients already exists.
#' @param mode Whether Canvas sends a bulk private message synchronously or
#' asynchronously.
#'
#' @return The Canvas API response, invisibly.
#' @md
#' @export
#'
#' @examples
#' \dontrun{
#' create_conversation(17, "Welcome", "Welcome to the course", course_id = 20)
#' create_conversation(
#'   c(17, 18), "Welcome", "Welcome to the course", course_id = 20,
#'   group_conversation = TRUE
#' )
#' }
create_conversation <- function(recipient_ids, subject = NULL, body,
                                course_id = NULL,
                                group_conversation = FALSE,
                                force_new = FALSE,
                                mode = "sync") {
  if (length(recipient_ids) == 0) {
    stop("Provide at least one recipient id.", call. = FALSE)
  }
  stopifnot(length(body) == 1, length(course_id) <= 1,
            length(group_conversation) == 1, length(force_new) == 1)
  if (!is.null(subject) && length(subject) != 1) {
    stop("`subject` must be NULL or a single value.", call. = FALSE)
  }
  if (!mode %in% c("sync", "async")) {
    stop("`mode` must be either 'sync' or 'async'.", call. = FALSE)
  }

  args <- c(
    iter_args_list(as.character(recipient_ids), "recipients[]"),
    list(
      subject = subject,
      body = body,
      force_new = force_new,
      group_conversation = group_conversation,
      mode = mode,
      context_code = canvas_context_code(course_id, "course")
    )
  )
  response <- canvas_query(make_canvas_url("conversations"), args, "POST")
  invisible(response)
}

#' Reply to a Canvas Inbox conversation
#'
#' By default Canvas sends the reply to all current conversation recipients.
#'
#' @param conversation_id A Canvas conversation id.
#' @param body Message body.
#' @param recipient_ids Optional recipient ids. Leave `NULL` to reply to
#' all current recipients.
#' @param included_message_ids Optional message ids from the conversation to
#' include for newly added recipients.
#'
#' @return The Canvas API response, invisibly.
#' @md
#' @export
#'
#' @examples
#' \dontrun{reply_conversation(12345, "Thank you for the update.")}
reply_conversation <- function(conversation_id, body, recipient_ids = NULL,
                               included_message_ids = NULL) {
  stopifnot(length(conversation_id) == 1, length(body) == 1)

  args <- c(
    list(body = body),
    iter_args_list(recipient_ids, "recipients[]"),
    iter_args_list(included_message_ids, "included_messages[]")
  )
  response <- canvas_query(
    make_canvas_url("conversations", conversation_id, "add_message"),
    args,
    "POST"
  )
  invisible(response)
}

canvas_context_code <- function(id, type) {
  if (is.null(id) || length(id) == 0) return(NULL)
  id <- as.character(id)
  dplyr::if_else(
    stringr::str_detect(id, stringr::str_c("^", type, "_")),
    id,
    stringr::str_c(type, "_", id)
  )
}

parse_canvas_json <- function(response) {
  response %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)
}
