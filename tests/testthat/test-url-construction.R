test_that("group context uses the supplied object id", {
  request <- new.env(parent = emptyenv())
  local_mocked_bindings(
    make_canvas_url = function(...) {
      request$url_parts <- list(...)
      "https://canvas.example.edu/api/v1/courses/27/groups"
    },
    process_response = function(url, args) data.frame(id = 1),
    .package = "rcanvas"
  )

  result <- get_groups_context(27)

  expect_s3_class(result, "data.frame")
  expect_equal(request$url_parts, list("courses", 27, "groups"))
})

test_that("enrollment URLs support courses and sections without output", {
  request <- new.env(parent = emptyenv())
  request$url_parts <- list()
  local_mocked_bindings(
    make_canvas_url = function(...) {
      request$url_parts[[length(request$url_parts) + 1]] <- list(...)
      "https://canvas.example.edu/api/v1/enrollments"
    },
    canvas_query = function(url, args = NULL, type = "GET") {
      request$type <- type
      structure(list(status_code = 200), class = "response")
    },
    .package = "rcanvas"
  )

  expect_silent(add_enrollment(20, 1001, "StudentEnrollment", "active"))
  expect_silent(add_enrollment(30, 1002, "StudentEnrollment", "active",
                               section = TRUE))

  expect_equal(request$url_parts[[1]],
               list("courses", 20, "enrollments"))
  expect_equal(request$url_parts[[2]],
               list("sections", 30, "enrollments"))
  expect_equal(request$type, "POST")
})

test_that("course sections are accepted as course items", {
  request <- new.env(parent = emptyenv())
  local_mocked_bindings(
    make_canvas_url = function(...) {
      request$url_parts <- list(...)
      "https://canvas.example.edu/api/v1/courses/20/sections"
    },
    process_response = function(url, args) data.frame(id = 1),
    .package = "rcanvas"
  )

  result <- get_course_items(20, "sections")

  expect_equal(request$url_parts, list("courses", 20, "sections"))
  expect_equal(result$course_id, 20)
})

test_that("group helpers build slash-safe Canvas URLs", {
  request <- new.env(parent = emptyenv())
  request$url_parts <- list()
  record_url <- function(...) {
    request$url_parts[[length(request$url_parts) + 1]] <- list(...)
    "https://canvas.example.edu/api/v1/groups"
  }
  local_mocked_bindings(
    make_canvas_url = record_url,
    process_response = function(url, args) data.frame(id = 1),
    canvas_query = function(url, args = NULL, type = "GET") {
      structure(list(status_code = 200), class = "response")
    },
    .package = "rcanvas"
  )

  get_groups_self()
  get_group_users(23)
  get_group_category(52)
  get_group_categories(20)
  add_group_user(23, 1001)
  add_group(52, "Group 1", "Test group", "invitation_only")

  expect_equal(request$url_parts, list(
    list("users", "self", "groups"),
    list("groups", 23, "users"),
    list("group_categories", 52),
    list("courses", 20, "group_categories"),
    list("groups", 23, "memberships"),
    list("group_categories", 52, "groups")
  ))
})
