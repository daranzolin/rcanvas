# Keep a reference to the real exported function while gradebook tests mock it.
course_submissions_impl <- get_course_submissions

submission_fixture <- function(user_id = 1, score = 5, assignment_id = 10) {
  data.frame(id = 100 + user_id, user_id = user_id, assignment_id = assignment_id,
             score = score, grade = as.character(score), workflow_state = "graded")
}

gradebook_metadata_fixture <- function(course_id, item, ...) {
  if (item == "assignments") return(data.frame(id = c(10, 20), name = c("Exam 1", "Quiz"), published = c(TRUE, TRUE)))
  data.frame(role = rep("StudentEnrollment", 3),
             user.name = c("Student A", "Student B", "Test Student"),
             user_id = c(1, 2, 3), grades.final_score = c(50, 60, NA),
             course_id = course_id)
}

cached_gradebook_fixture <- function() {
  x <- submission_fixture(user_id = c(1, 2), score = c(5, 6))
  x$user.name <- c("Student A", "Student B")
  x$grades.final_score <- c(50, 60)
  x$course_id <- 99
  x$assignment_name <- "Exam 1"
  attr(x, "canvas_course_id") <- 99
  attr(x, "canvas_domain") <- "https://canvas.example.edu/api/v1"
  attr(x, "canvas_synced_at") <- as.POSIXct("2026-10-02 12:00:00", tz = "UTC")
  attr(x, "canvas_full_synced_at") <- as.POSIXct("2026-10-01 12:00:00", tz = "UTC")
  attr(x, "canvas_assignments") <- gradebook_metadata_fixture(99, "assignments")
  attr(x, "canvas_students") <- gradebook_metadata_fixture(99, "enrollments") %>%
    dplyr::filter(user_id != 3) %>% dplyr::select(-role)
  x
}

test_that("full gradebook uses one course-wide call and stores a watermark", {
  calls <- list()
  local_mocked_bindings(
    canvas_url = function() "https://canvas.example.edu/api/v1",
    .gradebook_course_items = gradebook_metadata_fixture,
    get_course_submissions = function(...) {calls[[length(calls) + 1]] <<- list(...); submission_fixture()},
    .package = "rcanvas"
  )
  before <- Sys.time()
  x <- get_course_gradebook(99)
  expect_length(calls, 1)
  expect_equal(x$assignment_name, "Exam 1")
  expect_equal(x$user.name, "Student A")
  expect_true(attr(x, "canvas_synced_at") >= before)
  expect_true(attr(x, "canvas_synced_at") <= Sys.time())
  expect_equal(attr(x, "canvas_course_id"), 99)
  expect_identical(attr(x, "canvas_full_synced_at"), attr(x, "canvas_synced_at"))
})

test_that("incremental update unions submissions and regrades without metadata calls", {
  calls <- list()
  local_mocked_bindings(
    canvas_url = function() "https://canvas.example.edu/api/v1",
    .gradebook_course_items = function(course_id, item, ...) {
      if (item != "assignments") stop("Unexpected roster metadata request")
      gradebook_metadata_fixture(course_id, item)
    },
    get_course_submissions = function(course_id, submitted_since = NULL, graded_since = NULL, ...) {
      calls[[length(calls) + 1]] <<- list(submitted_since = submitted_since, graded_since = graded_since)
      if (!is.null(submitted_since)) return(submission_fixture(user_id = c(1, 4), score = c(7, 8)))
      submission_fixture(user_id = 1, score = 9)
    }, .package = "rcanvas"
  )
  x <- get_course_gradebook(99, previous = cached_gradebook_fixture())
  expect_length(calls, 2)
  expect_null(calls[[1]]$graded_since)
  expect_null(calls[[2]]$submitted_since)
  expect_equal(format(calls[[1]]$submitted_since, "%H:%M:%S", tz = "UTC"), "11:59:00")
  expect_equal(nrow(x), 3)
  expect_equal(x$score[x$user_id == 1], 9)
  expect_equal(x$score[x$user_id == 2], 6)
  expect_equal(x$score[x$user_id == 4], 8)
  expect_equal(x$user.name[x$user_id == 1], "Student A")
  expect_identical(attr(x, "canvas_full_synced_at"), attr(cached_gradebook_fixture(), "canvas_full_synced_at"))
  expect_false(anyDuplicated(x[c("assignment_id", "user_id")]) > 0)
})

test_that("empty deltas preserve rows and saved caches retain attributes", {
  local_mocked_bindings(
    canvas_url = function() "https://canvas.example.edu/api/v1",
    .gradebook_course_items = gradebook_metadata_fixture,
    get_course_submissions = function(...) data.frame(id = numeric(), assignment_id = numeric(), user_id = numeric()),
    .package = "rcanvas"
  )
  x <- get_course_gradebook(99, previous = cached_gradebook_fixture())
  expect_equal(sort(x$score), c(5, 6))
  path <- tempfile(); on.exit(unlink(path))
  saveRDS(x, path)
  expect_identical(readRDS(path), x)
})

test_that("metadata refresh updates labels and legacy snapshots require since", {
  local_mocked_bindings(
    canvas_url = function() "https://canvas.example.edu/api/v1",
    .gradebook_course_items = function(course_id, item, ...) {
      x <- gradebook_metadata_fixture(course_id, item)
      if (item == "assignments") x$name[1] <- "Renamed exam"
      x
    },
    get_course_submissions = function(...) data.frame(id = numeric(), assignment_id = numeric(), user_id = numeric()),
    .package = "rcanvas"
  )
  x <- get_course_gradebook(99, previous = cached_gradebook_fixture(), refresh_metadata = TRUE)
  expect_equal(unique(x$assignment_name), "Renamed exam")
  old <- submission_fixture()
  expect_error(get_course_gradebook(99, previous = old), "legacy snapshot")
  expect_equal(nrow(get_course_gradebook(99, previous = old, since = "2026-10-02T12:00:00Z")), 1)
})

test_that("invalid caches fail before any API request", {
  local_mocked_bindings(canvas_url = function() "https://canvas.example.edu/api/v1", .package = "rcanvas")
  x <- cached_gradebook_fixture()
  expect_error(get_course_gradebook(98, previous = x), "different course")
  attr(x, "canvas_domain") <- "https://other.example.edu/api/v1"
  expect_error(get_course_gradebook(99, previous = x), "different Canvas instance")
  x <- cached_gradebook_fixture(); x <- dplyr::bind_rows(x, x)
  expect_error(get_course_gradebook(99, previous = x), "duplicate")
  x <- cached_gradebook_fixture(); x$user_id[1] <- NA
  expect_error(get_course_gradebook(99, previous = x), "missing")
  expect_error(get_course_gradebook(99, since = "2026-10-02T12:00:00Z"), "requires a previous")
  expect_error(get_course_gradebook(99, overlap = -1), "non-negative")
  expect_error(get_course_gradebook(99, previous = cached_gradebook_fixture(), since = "2999-10-02T12:00:00Z"), "future")
})

test_that("an incremental API failure never returns a partially updated cache", {
  x <- cached_gradebook_fixture(); original <- x
  local_mocked_bindings(
    canvas_url = function() "https://canvas.example.edu/api/v1",
    .gradebook_course_items = gradebook_metadata_fixture,
    get_course_submissions = function(course_id, graded_since = NULL, ...) {
      if (!is.null(graded_since)) stop("Simulated API failure")
      submission_fixture(score = 9)
    }, .package = "rcanvas"
  )
  expect_error(get_course_gradebook(99, previous = x), "Simulated API failure")
  expect_identical(x, original)
})

submission_response_fixture <- function(body, link = NULL, status = 200) {
  structure(list(status_code = status, content = charToRaw(body),
                 headers = c(list(`content-type` = "application/json"), if (!is.null(link)) list(link = link)),
                 url = "https://canvas.example.edu/api/v1/courses/99/students/submissions"),
            class = "response")
}

test_that("course submissions builds filter arrays and follows bookmarks with GET only", {
  calls <- list()
  next_url <- "https://canvas.example.edu/api/v1/courses/99/students/submissions?page=bookmark%3Aabc&student_ids%5B%5D=all"
  local_mocked_bindings(
    canvas_url = function() "https://canvas.example.edu/api/v1",
    make_canvas_url = function(...) "https://canvas.example.edu/api/v1/courses/99/students/submissions",
    canvas_query = function(urlx, args, type) {
      calls[[length(calls) + 1]] <<- list(url = urlx, args = args, type = type)
      if (length(calls) == 1) return(submission_response_fixture('[{"id":101,"assignment_id":10,"user_id":1}]', paste0('<', next_url, '>; rel="next"')))
      submission_response_fixture('[{"id":102,"assignment_id":20,"user_id":2}]')
    }, .package = "rcanvas"
  )
  x <- course_submissions_impl(99, assignment_ids = c(10, 20), graded_since = "2026-10-02T12:00:00Z")
  expect_equal(nrow(x), 2)
  expect_equal(unlist(calls[[1]]$args[names(calls[[1]]$args) == "assignment_ids[]"]), c(`assignment_ids[]` = 10, `assignment_ids[]` = 20))
  expect_equal(calls[[1]]$args[['student_ids[]']], "all")
  expect_equal(calls[[1]]$args$graded_since, "2026-10-02T12:00:00Z")
  expect_equal(calls[[2]]$url, next_url)
  expect_null(calls[[2]]$args)
  expect_true(all(vapply(calls, function(x) x$type == "GET", logical(1))))
})

test_that("empty results, invalid inputs, HTTP errors and malformed bodies are safe", {
  local_mocked_bindings(
    canvas_url = function() "https://canvas.example.edu/api/v1",
    make_canvas_url = function(...) "https://canvas.example.edu/api/v1/courses/99/students/submissions",
    canvas_query = function(...) submission_response_fixture('[]'), .package = "rcanvas"
  )
  expect_named(course_submissions_impl(99), c("id", "assignment_id", "user_id"))
  expect_error(course_submissions_impl(99, student_ids = character()), "student_ids")
  expect_error(course_submissions_impl(99, assignment_ids = NA), "assignment_ids")
  expect_error(course_submissions_impl(99, enrollment_state = "inactive"), "enrollment_state")
  expect_error(course_submissions_impl(99, graded_since = "yesterday"), "ISO 8601")
  expect_error(course_submissions_impl(99, graded_since = "2026-02-31T12:00:00Z"), "Invalid timestamp")
  local_mocked_bindings(canvas_query = function(...) submission_response_fixture('{}'), .package = "rcanvas")
  # Empty object is not an array and must not masquerade as an empty successful response.
  expect_error(course_submissions_impl(99), "unexpected")
  local_mocked_bindings(canvas_query = function(...) submission_response_fixture('{"error":"bad"}'), .package = "rcanvas")
  expect_error(course_submissions_impl(99), "unexpected")
  local_mocked_bindings(canvas_query = function(...) submission_response_fixture('[]', status = 500), .package = "rcanvas")
  expect_error(course_submissions_impl(99), "500")
})

test_that("repeated links and cross-origin links fail rather than leaking credentials", {
  local_mocked_bindings(
    canvas_url = function() "https://canvas.example.edu/api/v1",
    make_canvas_url = function(...) "https://canvas.example.edu/api/v1/courses/99/students/submissions",
    canvas_query = function(...) submission_response_fixture('[]', '<https://evil.example/api>; rel="next"'),
    .package = "rcanvas"
  )
  expect_error(course_submissions_impl(99), "different origin")
  local_mocked_bindings(canvas_query = function(...) submission_response_fixture('[]', '<https://canvas.example.edu/api/v1/courses/99/students/submissions>; rel="next"'), .package = "rcanvas")
  expect_error(course_submissions_impl(99), "repeated")
})

test_that("grouped student pages flatten without losing student ids or empty groups", {
  local_mocked_bindings(
    canvas_url = function() "https://canvas.example.edu/api/v1",
    make_canvas_url = function(...) "https://canvas.example.edu/api/v1/courses/99/students/submissions",
    canvas_query = function(urlx, args, type) {
      expect_true(args$grouped)
      submission_response_fixture('[{"user_id":1,"submissions":[{"id":101,"assignment_id":10,"score":8},{"id":102,"assignment_id":20,"score":9}]},{"user_id":2,"submissions":[]}]')
    }, .package = "rcanvas"
  )
  x <- course_submissions_impl(99)
  expect_equal(nrow(x), 2)
  expect_equal(x$user_id, c(1, 1))
  expect_equal(x$assignment_id, c(10, 20))
})

test_that("unpublished assignments are excluded from full and legacy cached gradebooks", {
  local_mocked_bindings(
    canvas_url = function() "https://canvas.example.edu/api/v1",
    .gradebook_course_items = function(course_id, item, ...) {
      x <- gradebook_metadata_fixture(course_id, item)
      if (item == "assignments") x$published[2] <- FALSE
      x
    },
    get_course_submissions = function(...) submission_fixture(),
    .package = "rcanvas"
  )
  x <- get_course_gradebook(99)
  expect_equal(x$assignment_id, 10)
  previous <- cached_gradebook_fixture()
  previous$assignment_id[2] <- 20
  x <- get_course_gradebook(99, previous = previous)
  expect_equal(x$assignment_id, 10)
})

test_that("newly published assignments get a complete baseline", {
  previous <- cached_gradebook_fixture()
  attr(previous, "canvas_assignments") <- gradebook_metadata_fixture(99, "assignments")[1, ]
  calls <- 0L
  local_mocked_bindings(
    canvas_url = function() "https://canvas.example.edu/api/v1",
    .gradebook_course_items = gradebook_metadata_fixture,
    get_course_submissions = function(course_id, assignment_ids = NULL, ...) {
      if (is.null(assignment_ids)) return(data.frame(id = numeric(), assignment_id = numeric(), user_id = numeric()))
      expect_equal(assignment_ids, 20)
      calls <<- calls + 1L
      submission_fixture(user_id = 1, assignment_id = 20, score = 8)
    }, .package = "rcanvas"
  )
  x <- get_course_gradebook(99, previous = previous)
  expect_equal(calls, 1L)
  expect_equal(x$score[x$assignment_id == 20], 8)
})

test_that("unpublished assignment inclusion is explicit on full and incremental imports", {
  count <- 0L
  local_mocked_bindings(
    canvas_url = function() "https://canvas.example.edu/api/v1",
    .gradebook_course_items = function(course_id, item, ...) {
      x <- gradebook_metadata_fixture(course_id, item)
      if (item == "assignments") x$published[2] <- FALSE
      x
    },
    get_course_submissions = function(...) submission_fixture(),
    make_canvas_url = function(...) paste(list(...), collapse = "/"),
    .course_submission_pages = function(url, args, progress) {
      expect_match(url, "assignments/20/submissions")
      count <<- count + 1L
      submission_fixture(assignment_id = 20, score = 7)
    }, .package = "rcanvas"
  )
  x <- get_course_gradebook(99, include_unpublished = TRUE)
  expect_equal(sort(x$assignment_id), c(10, 20))
  expect_equal(count, 1L)
  x <- get_course_gradebook(99, previous = x, include_unpublished = TRUE)
  expect_equal(count, 2L)
  expect_equal(x$score[x$assignment_id == 20], 7)
  x <- get_course_gradebook(99, previous = x)
  expect_equal(x$assignment_id, 10)
  expect_error(get_course_gradebook(99, include_unpublished = NA), "TRUE or FALSE")
})
