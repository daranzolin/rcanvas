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
    .package = "rcanvas"
  )

  result <- create_conversation(
    c(17, 18), "Welcome", "Hello", course_id = 20,
    group_conversation = TRUE
  )

  expect_identical(result, response)
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
