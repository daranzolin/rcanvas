test_that("get_quiz_questions builds the classic quiz endpoint", {
  request <- new.env(parent = emptyenv())
  local_mocked_bindings(
    make_canvas_url = function(...) {
      request$url_parts <- list(...)
      "https://canvas.example.edu/api/v1/courses/20/quizzes/123/questions"
    },
    process_response = function(url, args) {
      request$url <- url
      request$args <- args
      data.frame(id = 456)
    },
    .package = "rcanvas"
  )
  result <- get_quiz_questions(20, 123)
  expect_s3_class(result, "data.frame")
  expect_equal(request$url_parts,
               list("courses", 20, "quizzes", 123, "questions"))
  expect_identical(request$args$per_page, 100)
})

test_that("create_quiz_question serializes Canvas answer fields", {
  request <- new.env(parent = emptyenv())
  response <- structure(list(status_code = 201), class = "response")
  local_mocked_bindings(
    make_canvas_url = function(...) {
      "https://canvas.example.edu/api/v1/courses/20/quizzes/123/questions"
    },
    canvas_query = function(url, args = NULL, type = "GET") {
      request$args <- args
      request$type <- type
      response
    },
    .package = "rcanvas"
  )
  result <- suppressMessages(create_quiz_question(
    20, 123, "Where?",
    data.frame(answer_text = c("CASA", "Home"),
               answer_weight = c(100, 0))
  ))
  expect_identical(result, response)
  expect_equal(request$type, "POST")
  expect_identical(request$args$`question[question_text]`, "Where?")
  answer_text <- request$args[
    names(request$args) == "question[answers][][answer_text]"
  ] %>% unlist(use.names = FALSE)
  answer_weight <- request$args[
    names(request$args) == "question[answers][][answer_weight]"
  ] %>% unlist(use.names = FALSE)
  expect_equal(answer_text, c("CASA", "Home"))
  expect_equal(answer_weight, c(100, 0))
})

test_that("update_quiz_question sends only requested fields", {
  request <- new.env(parent = emptyenv())
  response <- structure(list(status_code = 200), class = "response")
  local_mocked_bindings(
    make_canvas_url = function(...) {
      request$url_parts <- list(...)
      "https://canvas.example.edu/api/v1/courses/20/quizzes/123/questions/456"
    },
    canvas_query = function(url, args = NULL, type = "GET") {
      request$args <- args
      request$type <- type
      response
    },
    .package = "rcanvas"
  )
  result <- suppressMessages(update_quiz_question(
    20, 123, 456, question_text = "Updated", points_possible = 2
  ))
  expect_identical(result, response)
  expect_equal(request$type, "PUT")
  expect_named(request$args,
               c("question[question_text]", "question[points_possible]"))
})

test_that("update_quiz_question rejects an empty update", {
  expect_error(update_quiz_question(20, 123, 456),
               "Provide at least one quiz question field")
})

test_that("quiz question answers require text and weight", {
  expect_error(
    create_quiz_question(20, 123, "Where?",
                         data.frame(answer_text = "CASA")),
    "answer_text and answer_weight"
  )
})

test_that("delete_quiz_question uses the classic quiz endpoint", {
  request <- new.env(parent = emptyenv())
  response <- structure(list(status_code = 204), class = "response")
  local_mocked_bindings(
    make_canvas_url = function(...) {
      request$url_parts <- list(...)
      "https://canvas.example.edu/api/v1/courses/20/quizzes/123/questions/456"
    },
    canvas_query = function(url, args = NULL, type = "GET") {
      request$args <- args
      request$type <- type
      response
    },
    .package = "rcanvas"
  )
  result <- suppressMessages(delete_quiz_question(20, 123, 456))
  expect_identical(result, response)
  expect_equal(request$type, "DELETE")
  expect_null(request$args)
  expect_equal(request$url_parts,
               list("courses", 20, "quizzes", 123, "questions", 456))
})
