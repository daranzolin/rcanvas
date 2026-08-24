test_that("update_quiz_settings sends only requested fields", {
  request <- new.env(parent = emptyenv())
  response <- structure(list(status_code = 200), class = "response")
  local_mocked_bindings(
    make_canvas_url = function(...) {
      request$url_parts <- list(...)
      "https://canvas.example.edu/api/v1/courses/20/quizzes/123"
    },
    canvas_query = function(url, args = NULL, type = "GET") {
      request$url <- url
      request$args <- args
      request$type <- type
      response
    },
    .package = "rcanvas"
  )

  result <- suppressMessages(update_quiz_settings(
    20, 123, title = "Syllabus Quiz", shuffle_answers = TRUE,
    show_correct_answers = FALSE, published = FALSE
  ))

  expect_identical(result, response)
  expect_equal(request$url_parts, list("courses", 20, "quizzes", 123))
  expect_equal(request$url,
               "https://canvas.example.edu/api/v1/courses/20/quizzes/123")
  expect_equal(request$type, "PUT")
  expect_named(
    request$args,
    c("quiz[title]", "quiz[shuffle_answers]",
      "quiz[show_correct_answers]", "quiz[published]")
  )
  expect_identical(request$args$`quiz[title]`, "Syllabus Quiz")
  expect_identical(request$args$`quiz[shuffle_answers]`, TRUE)
  expect_identical(request$args$`quiz[show_correct_answers]`, FALSE)
  expect_identical(request$args$`quiz[published]`, FALSE)
})

test_that("update_quiz_settings rejects an empty update", {
  expect_error(update_quiz_settings(20, 123),
               "Provide at least one quiz setting")
})
