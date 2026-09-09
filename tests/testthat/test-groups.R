test_that("update_group renames a group", {
  request <- new.env(parent = emptyenv())
  response <- structure(list(status_code = 200), class = "response")
  local_mocked_bindings(
    make_canvas_url = function(...) {
      request$url_parts <- list(...)
      "https://canvas.example.edu/api/v1/groups/23"
    },
    canvas_query = function(url, args = NULL, type = "GET") {
      request$args <- args
      request$type <- type
      response
    },
    .package = "rcanvas"
  )
  result <- suppressMessages(update_group(23, name = "Project team 1"))
  expect_identical(result, response)
  expect_equal(request$type, "PUT")
  expect_equal(request$url_parts, list("groups", 23))
  expect_named(request$args, "name")
  expect_identical(request$args$name, "Project team 1")
})

test_that("update_group sends only requested fields and repeats members", {
  request <- new.env(parent = emptyenv())
  response <- structure(list(status_code = 200), class = "response")
  local_mocked_bindings(
    make_canvas_url = function(...) "https://canvas.example.edu/api/v1/groups/23",
    canvas_query = function(url, args = NULL, type = "GET") {
      request$args <- args
      response
    },
    .package = "rcanvas"
  )
  suppressMessages(update_group(23, description = "Final project",
                                members = c(327, 328)))
  expect_equal(names(request$args),
               c("description", "members[]", "members[]"))
  members <- request$args[names(request$args) == "members[]"] %>%
    unlist(use.names = FALSE)
  expect_equal(members, c(327, 328))
})

test_that("update_group rejects an empty update", {
  expect_error(update_group(23), "Provide at least one group field")
})

test_that("update_group validates join_level", {
  expect_error(update_group(23, join_level = "whenever"), "should be one of")
})

test_that("update_group_category builds the group category endpoint", {
  request <- new.env(parent = emptyenv())
  response <- structure(list(status_code = 200), class = "response")
  local_mocked_bindings(
    make_canvas_url = function(...) {
      request$url_parts <- list(...)
      "https://canvas.example.edu/api/v1/group_categories/52872"
    },
    canvas_query = function(url, args = NULL, type = "GET") {
      request$args <- args
      request$type <- type
      response
    },
    .package = "rcanvas"
  )
  result <- suppressMessages(update_group_category(
    52872, name = "Final project groups", group_limit = 4
  ))
  expect_identical(result, response)
  expect_equal(request$type, "PUT")
  expect_equal(request$url_parts, list("group_categories", 52872))
  expect_named(request$args, c("name", "group_limit"))
})

test_that("update_group_category allows clearing self_signup", {
  request <- new.env(parent = emptyenv())
  local_mocked_bindings(
    make_canvas_url = function(...) {
      "https://canvas.example.edu/api/v1/group_categories/52872"
    },
    canvas_query = function(url, args = NULL, type = "GET") {
      request$args <- args
      structure(list(status_code = 200), class = "response")
    },
    .package = "rcanvas"
  )
  suppressMessages(update_group_category(52872, self_signup = ""))
  expect_identical(request$args$self_signup, "")
})

test_that("update_group_category rejects an empty update", {
  expect_error(update_group_category(52872),
               "Provide at least one group category field")
})

test_that("update_group_category validates auto_leader", {
  expect_error(update_group_category(52872, auto_leader = "oldest"),
               "should be one of")
})
