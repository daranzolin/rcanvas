test_that("reorder_quiz_items serializes ordered quiz items", {
  request <- new.env(parent = emptyenv())
  response <- structure(list(status_code = 204), class = "response")
  local_mocked_bindings(
    make_canvas_url = function(...) {
      request$url_parts <- list(...)
      "https://canvas.example.edu/api/v1/courses/20/quizzes/123/reorder"
    },
    canvas_query = function(url, args = NULL, type = "GET") {
      request$args <- args
      request$type <- type
      response
    },
    .package = "rcanvas"
  )

  result <- suppressMessages(reorder_quiz_items(
    20, 123, c(456, 99), c("question", "group")
  ))

  expect_identical(result, response)
  expect_equal(request$type, "POST")
  expect_equal(request$url_parts,
               list("courses", 20, "quizzes", 123, "reorder"))
  ids <- request$args[names(request$args) == "order[][id]"] %>%
    unlist(use.names = FALSE)
  types <- request$args[names(request$args) == "order[][type]"] %>%
    unlist(use.names = FALSE)
  expect_equal(ids, c(456, 99))
  expect_equal(types, c("question", "group"))
})

test_that("reorder_quiz_items recycles one item type", {
  request <- new.env(parent = emptyenv())
  local_mocked_bindings(
    make_canvas_url = function(...) "https://canvas.example.edu/reorder",
    canvas_query = function(url, args = NULL, type = "GET") {
      request$args <- args
      structure(list(status_code = 204), class = "response")
    },
    .package = "rcanvas"
  )

  suppressMessages(reorder_quiz_items(20, 123, c(456, 457)))
  types <- request$args[names(request$args) == "order[][type]"] %>%
    unlist(use.names = FALSE)
  expect_equal(types, c("question", "question"))
})

test_that("reorder_quiz_items validates item types", {
  expect_error(reorder_quiz_items(20, 123, c(456, 457), "page"),
               "one valid type per item id")
  expect_error(reorder_quiz_items(20, 123, c(456, 457),
                                  c("question", "group", "question")),
               "one valid type per item id")
})
