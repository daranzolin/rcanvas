test_that("get_conversations filters the Canvas Inbox by course", {
  request <- new.env(parent = emptyenv())
  local_mocked_bindings(
    make_canvas_url = function(...) {
      request$url_parts <- list(...)
      "https://canvas.example.edu/api/v1/conversations"
    },
    process_response = function(url, args) {
      request$url <- url
      request$args <- args
      data.frame(id = 1)
    },
    .package = "rcanvas"
  )

  result <- get_conversations(course_id = c(20, "course_21"))

  expect_s3_class(result, "data.frame")
  expect_equal(request$url_parts, list("conversations"))
  expect_equal(request$url, "https://canvas.example.edu/api/v1/conversations")
  expect_null(request$args$scope)
  filters <- request$args[names(request$args) == "filter[]"] %>%
    unlist(use.names = FALSE)
  expect_equal(filters, c("course_20", "course_21"))
  expect_identical(request$args$filter_mode, "and")
})

test_that("get_conversations passes unread scope and include values", {
  request <- new.env(parent = emptyenv())
  local_mocked_bindings(
    make_canvas_url = function(...) "https://canvas.example.edu/api/v1/conversations",
    process_response = function(url, args) {
      request$args <- args
      data.frame(id = 1)
    },
    .package = "rcanvas"
  )

  get_conversations(scope = "unread", include = "uuid")

  expect_identical(request$args$scope, "unread")
  expect_null(request$args$filter_mode)
  expect_identical(request$args[["include[]"]], "uuid")
})

test_that("get_conversation does not mark messages read by default", {
  request <- new.env(parent = emptyenv())
  response <- structure(list(status_code = 200), class = "response")
  parsed <- list(id = 123, subject = "Question")
  local_mocked_bindings(
    make_canvas_url = function(...) {
      request$url_parts <- list(...)
      "https://canvas.example.edu/api/v1/conversations/123"
    },
    canvas_query = function(url, args = NULL, type = "GET") {
      request$url <- url
      request$args <- args
      request$type <- type
      response
    },
    parse_canvas_json = function(x) parsed,
    .package = "rcanvas"
  )

  result <- get_conversation(123)

  expect_identical(result, parsed)
  expect_equal(request$url_parts, list("conversations", 123))
  expect_identical(request$type, "GET")
  expect_identical(request$args$auto_mark_as_read, FALSE)
})

test_that("update_conversation sends only requested fields", {
  request <- new.env(parent = emptyenv())
  response <- structure(list(status_code = 200), class = "response")
  local_mocked_bindings(
    make_canvas_url = function(...) {
      request$url_parts <- list(...)
      "https://canvas.example.edu/api/v1/conversations/123"
    },
    canvas_query = function(url, args = NULL, type = "GET") {
      request$url <- url
      request$args <- args
      request$type <- type
      response
    },
    .package = "rcanvas"
  )

  result <- update_conversation(123, workflow_state = "unread", starred = TRUE)

  expect_identical(result, response)
  expect_equal(request$url_parts, list("conversations", 123))
  expect_identical(request$type, "PUT")
  expect_named(request$args,
               c("conversation[workflow_state]", "conversation[starred]"))
  expect_identical(request$args[["conversation[workflow_state]"]], "unread")
  expect_identical(request$args[["conversation[starred]"]], TRUE)
})

test_that("update_conversation validates changes", {
  expect_error(update_conversation(123), "at least one")
  expect_error(update_conversation(123, workflow_state = "inbox"),
               "workflow_state")
})

test_that("create_conversation repeats recipient parameters", {
  request <- new.env(parent = emptyenv())
  response <- structure(list(status_code = 201), class = "response")
  local_mocked_bindings(
    make_canvas_url = function(...) {
      request$url_parts <- list(...)
      "https://canvas.example.edu/api/v1/conversations"
    },
    canvas_query = function(url, args = NULL, type = "GET") {
      request$url <- url
      request$args <- args
      request$type <- type
      response
    },
    parse_canvas_json = function(x) list(list(id = 5, audience = list(17, 18))),
    .package = "rcanvas"
  )

  result <- create_conversation(
    c(17, 18), "Welcome", "Hello", course_id = 20,
    group_conversation = TRUE
  )

  expect_identical(result, list(list(id = 5, audience = list(17, 18))))
  expect_equal(request$url_parts, list("conversations"))
  expect_identical(request$type, "POST")
  recipients <- request$args[names(request$args) == "recipients[]"] %>%
    unlist(use.names = FALSE)
  expect_equal(recipients, c("17", "18"))
  expect_identical(request$args$subject, "Welcome")
  expect_identical(request$args$body, "Hello")
  expect_identical(request$args$group_conversation, TRUE)
  expect_identical(request$args$context_code, "course_20")
})

test_that("reply_conversation replies to all recipients by default", {
  request <- new.env(parent = emptyenv())
  response <- structure(list(status_code = 200), class = "response")
  local_mocked_bindings(
    make_canvas_url = function(...) {
      request$url_parts <- list(...)
      "https://canvas.example.edu/api/v1/conversations/123/add_message"
    },
    canvas_query = function(url, args = NULL, type = "GET") {
      request$url <- url
      request$args <- args
      request$type <- type
      response
    },
    .package = "rcanvas"
  )

  result <- reply_conversation(123, "Thanks")

  expect_identical(result, response)
  expect_equal(request$url_parts, list("conversations", 123, "add_message"))
  expect_identical(request$type, "POST")
  expect_named(request$args, "body")
  expect_identical(request$args$body, "Thanks")
})

test_that("conversation helpers validate arguments", {
  expect_error(get_conversations(scope = "all"), "scope")
  expect_error(create_conversation(character(), body = "Hello"), "recipient")
  expect_error(create_conversation(1, body = "Hello", mode = "later"), "mode")
})

# Canvas can answer 201 yet leave a recipient out of the conversation
# (observed 2026-09-27 with an actively enrolled, messageable student).
mock_conversation_api <- function(request, created, refreshed = NULL) {
  request$calls <- list()
  list(
    make_canvas_url = function(...) {
      stringr::str_c(c("https://canvas.example.edu/api/v1", ...),
                     collapse = "/")
    },
    canvas_query = function(url, args = NULL, type = "GET") {
      request$calls[[length(request$calls) + 1]] <-
        list(url = url, args = args, type = type)
      structure(list(url = url), class = "response")
    },
    parse_canvas_json = function(x) {
      if (grepl("add_recipients$", x$url)) return(refreshed)
      if (grepl("conversations/[0-9]+$", x$url)) return(refreshed)
      created
    }
  )
}

test_that("create_conversation makes no extra calls when all recipients arrive", {
  request <- new.env(parent = emptyenv())
  created <- list(list(id = 5, audience = list(17, 18)))
  do.call(local_mocked_bindings,
          c(mock_conversation_api(request, created), .package = "rcanvas"))

  result <- create_conversation(c(17, 18), "Hi", "Hello",
                                group_conversation = TRUE)

  expect_identical(result, created)
  expect_length(request$calls, 1)
})

test_that("create_conversation adds a dropped recipient to the same thread", {
  request <- new.env(parent = emptyenv())
  created <- list(list(id = 5, audience = list(18)))
  refreshed <- list(id = 5, audience = list(18, 17))
  do.call(local_mocked_bindings,
          c(mock_conversation_api(request, created, refreshed),
            .package = "rcanvas"))

  expect_message(
    result <- create_conversation(c(17, 18), "Hi", "Hello",
                                  group_conversation = TRUE),
    "left recipient\\(s\\) 17 out of conversation 5"
  )

  expect_identical(result, list(refreshed))
  expect_length(request$calls, 3)
  repair <- request$calls[[2]]
  expect_identical(repair$type, "POST")
  expect_match(repair$url, "conversations/5/add_recipients$")
  expect_equal(unlist(repair$args[names(repair$args) == "recipients[]"],
                      use.names = FALSE), "17")
  expect_match(request$calls[[3]]$url, "conversations/5$")
  expect_identical(request$calls[[3]]$type, "GET")
})

test_that("create_conversation warns when a repair does not take", {
  request <- new.env(parent = emptyenv())
  created <- list(list(id = 5, audience = list(18)))
  do.call(local_mocked_bindings,
          c(mock_conversation_api(request, created,
                                  list(id = 5, audience = list(18))),
            .package = "rcanvas"))

  expect_warning(
    suppressMessages(create_conversation(c(17, 18), "Hi", "Hello",
                                         group_conversation = TRUE)),
    "did not include recipient\\(s\\) 17"
  )
})

test_that("create_conversation warns instead of repairing private conversations", {
  request <- new.env(parent = emptyenv())
  created <- list(list(id = 5, audience = list(18)))
  do.call(local_mocked_bindings,
          c(mock_conversation_api(request, created), .package = "rcanvas"))

  expect_warning(create_conversation(c(17, 18), "Hi", "Hello"),
                 "did not include recipient\\(s\\) 17")
  expect_length(request$calls, 1)
})

test_that("create_conversation skips verification when asked or async", {
  request <- new.env(parent = emptyenv())
  do.call(local_mocked_bindings,
          c(mock_conversation_api(request, list()), .package = "rcanvas"))

  expect_silent(create_conversation(c(17, 18), "Hi", "Hello", mode = "async"))
  expect_silent(create_conversation(c(17, 18), "Hi", "Hello",
                                    group_conversation = TRUE, verify = FALSE))
  expect_length(request$calls, 2)
})

test_that("create_conversation only verifies numeric user ids", {
  request <- new.env(parent = emptyenv())
  created <- list(list(id = 5, audience = list(18)))
  do.call(local_mocked_bindings,
          c(mock_conversation_api(request, created), .package = "rcanvas"))

  expect_silent(create_conversation(c("course_20_students", 18), "Hi",
                                    "Hello", group_conversation = TRUE))
})

test_that("add_conversation_recipients posts repeated recipient parameters", {
  request <- new.env(parent = emptyenv())
  refreshed <- list(id = 5, audience = list(17, 18, 19))
  do.call(local_mocked_bindings,
          c(mock_conversation_api(request, NULL, refreshed),
            .package = "rcanvas"))

  result <- add_conversation_recipients(5, c(17, 19))

  expect_identical(result, refreshed)
  call <- request$calls[[1]]
  expect_identical(call$type, "POST")
  expect_match(call$url, "conversations/5/add_recipients$")
  expect_equal(unlist(call$args[names(call$args) == "recipients[]"],
                      use.names = FALSE), c("17", "19"))
  expect_error(add_conversation_recipients(5, integer()), "recipient")
})

test_that("create_conversation counts the sender, whom Canvas omits from audience", {
  request <- new.env(parent = emptyenv())
  created <- list(list(id = 5, audience = list(),
                       participants = list(list(id = 60912, name = "Me"))))
  do.call(local_mocked_bindings,
          c(mock_conversation_api(request, created), .package = "rcanvas"))

  expect_silent(create_conversation(60912, "Hi", "Hello",
                                    group_conversation = TRUE))
  expect_length(request$calls, 1)
})
