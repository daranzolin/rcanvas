test_that("get_announcements builds a valid request", {
  request <- new.env(parent = emptyenv())
  local_mocked_bindings(
    make_canvas_url = function(...) {
      request$url_parts <- list(...)
      "https://canvas.example.edu/api/v1/announcements"
    },
    process_response = function(url, args) {
      request$url <- url
      request$args <- args
      data.frame(id = 1)
    },
    .package = "rcanvas"
  )

  result <- get_announcements(
    c(20, "course_21"),
    start_date = "2026-08-01",
    end_date = "2026-08-31",
    active_only = TRUE
  )

  expect_s3_class(result, "data.frame")
  expect_equal(request$url_parts, list("announcements"))
  expect_equal(request$url, "https://canvas.example.edu/api/v1/announcements")
  contexts <- request$args[names(request$args) == "context_codes[]"] %>%
    unlist(use.names = FALSE)
  expect_equal(contexts, c("course_20", "course_21"))
  expect_identical(request$args$active_only, TRUE)
  expect_equal(request$args$start_date, "2026-08-01")
  expect_equal(request$args$end_date, "2026-08-31")
})

test_that("create_announcement defaults to an unpublished course-wide draft", {
  request <- new.env(parent = emptyenv())
  response <- structure(list(status_code = 201), class = "response")
  local_mocked_bindings(
    make_canvas_url = function(...) {
      request$url_parts <- list(...)
      "https://canvas.example.edu/api/v1/courses/20/discussion_topics"
    },
    canvas_query = function(url, args = NULL, type = "GET") {
      request$url <- url
      request$args <- args
      request$type <- type
      response
    },
    .package = "rcanvas"
  )

  result <- suppressMessages(create_announcement(20, "Welcome", "<p>Hello</p>"))

  expect_identical(result, response)
  expect_equal(request$url_parts, list("courses", 20, "discussion_topics"))
  expect_equal(request$url,
               "https://canvas.example.edu/api/v1/courses/20/discussion_topics")
  expect_equal(request$type, "POST")
  expect_identical(request$args$is_announcement, TRUE)
  expect_identical(request$args$published, FALSE)
  expect_identical(request$args$lock_comment, FALSE)
  sent_args <- purrr::discard(request$args, is.null)
  expect_false("delayed_post_at" %in% names(sent_args))
  expect_false("specific_sections" %in% names(sent_args))
})

test_that("create_announcement supports scheduling and sections", {
  request <- new.env(parent = emptyenv())
  local_mocked_bindings(
    make_canvas_url = function(...) {
      request$url_parts <- list(...)
      "https://canvas.example.edu/api/v1/courses/20/discussion_topics"
    },
    canvas_query = function(url, args = NULL, type = "GET") {
      request$args <- args
      structure(list(status_code = 201), class = "response")
    },
    .package = "rcanvas"
  )

  suppressMessages(create_announcement(
    20, "Exam", "Reserve a seat", published = TRUE,
    delayed_post_at = "2026-09-25T14:00:00Z",
    lock_comment = TRUE, specific_sections = c(101, 102)
  ))

  expect_identical(request$args$published, TRUE)
  expect_identical(request$args$lock_comment, TRUE)
  expect_equal(request$args$delayed_post_at, "2026-09-25T14:00:00Z")
  expect_equal(request$args$specific_sections, "101,102")
})

test_that("update_announcement sends only requested fields", {
  request <- new.env(parent = emptyenv())
  local_mocked_bindings(
    make_canvas_url = function(...) {
      request$url_parts <- list(...)
      "https://canvas.example.edu/api/v1/courses/20/discussion_topics/123"
    },
    canvas_query = function(url, args = NULL, type = "GET") {
      request$url <- url
      request$args <- args
      request$type <- type
      structure(list(status_code = 200), class = "response")
    },
    .package = "rcanvas"
  )

  suppressMessages(update_announcement(20, 123, published = TRUE,
                                        specific_sections = c(101, 102)))

  expect_equal(request$url,
               "https://canvas.example.edu/api/v1/courses/20/discussion_topics/123")
  expect_equal(request$url_parts,
               list("courses", 20, "discussion_topics", 123))
  expect_equal(request$type, "PUT")
  expect_named(request$args,
               c("published", "specific_sections", "is_announcement"))
  expect_identical(request$args$published, TRUE)
  expect_equal(request$args$specific_sections, "101,102")
  expect_identical(request$args$is_announcement, TRUE)
})

test_that("update_announcement rejects an empty update", {
  expect_error(update_announcement(20, 123),
               "Provide at least one announcement field")
})

test_that("delete_announcement uses the discussion topic endpoint", {
  request <- new.env(parent = emptyenv())
  response <- structure(list(status_code = 200), class = "response")
  local_mocked_bindings(
    make_canvas_url = function(...) {
      request$url_parts <- list(...)
      "https://canvas.example.edu/api/v1/courses/20/discussion_topics/123"
    },
    canvas_query = function(url, args = NULL, type = "GET") {
      request$url <- url
      request$args <- args
      request$type <- type
      response
    },
    .package = "rcanvas"
  )

  result <- suppressMessages(delete_announcement(20, 123))

  expect_identical(result, response)
  expect_equal(request$url,
               "https://canvas.example.edu/api/v1/courses/20/discussion_topics/123")
  expect_equal(request$url_parts,
               list("courses", 20, "discussion_topics", 123))
  expect_equal(request$type, "DELETE")
  expect_null(request$args)
})
