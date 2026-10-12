fake_visibility <- function(top_error = NULL, payload_error = NULL, missing_job = FALSE,
                            skip = FALSE, change_other = FALSE, change_policy = FALSE,
                            state = "completed", published = TRUE, env = parent.frame()) {
  canvas <- new.env(parent = emptyenv())
  canvas$posts <- 0L
  canvas$reads <- 0L
  canvas$times <- c(NA_character_, NA_character_, "old")
  local_mocked_bindings(
    canvas_url = function() "https://canvas.example.edu/api/v1",
    make_canvas_url = function(...) paste(c("https://canvas.example.edu/api/v1", ...), collapse = "/"),
    .rcanvas_json = function(x) x,
    .rcanvas_next_link = function(x) NULL,
    .rcanvas_sleep = function(x) NULL,
    canvas_query = function(urlx, args = NULL, type = "GET", retry_429 = 0, encode = NULL) {
      if (type == "POST") {
        canvas$posts <- canvas$posts + 1L
        canvas$url <- urlx
        canvas$variables <- args$variables
        canvas$encode <- encode
        canvas$query <- args$query
        ids <- unlist(canvas$variables$input$onlyStudentIds)
        post <- grepl("postAssignmentGrades", args$query)
        mutation <- if (post) "postAssignmentGrades" else "hideAssignmentGrades"
        if (!skip) canvas$times[match(ids, c("101", "102", "103"))] <- if (post) "new" else NA_character_
        if (change_other) canvas$times[3] <- "changed"
        return(list(errors = top_error, data = stats::setNames(list(list(
          errors = payload_error,
          progress = if (missing_job) NULL else list(`_id` = "7", state = state))), mutation)))
      }
      if (grepl("/submissions$", urlx)) {
        canvas$reads <- canvas$reads + 1L
        return(lapply(seq_len(3), function(i) list(user_id = 100 + i,
                    posted_at = if (is.na(canvas$times[i])) NULL else canvas$times[i])))
      }
      list(published = published, post_manually = !(change_policy && canvas$posts > 0))
    }, .package = "rcanvas", .env = env)
  canvas
}

test_that("posting targets only explicit ids, uses variables and verifies", {
  canvas <- fake_visibility()
  result <- post_grades(10, 20, c(101, 102))
  expect_equal(canvas$posts, 1L)
  expect_equal(canvas$url, "https://canvas.example.edu/api/graphql")
  expect_equal(canvas$encode, "json")
  expect_equal(canvas$variables$input$assignmentId, "20")
  expect_equal(canvas$variables$input$onlyStudentIds, list("101", "102"))
  expect_true(all(result$verified))
  expect_equal(result$posted_at_after, c("new", "new"))
  expect_equal(canvas$times[3], "old")
})

test_that("hide verifies targets are hidden and preserves non-targets", {
  canvas <- fake_visibility()
  canvas$times[1:2] <- "before"
  result <- hide_grades(10, 20, 101:102)
  expect_true(all(result$verified))
  expect_true(all(is.na(result$posted_at_after)))
  expect_equal(result$posted_at_before, c("before", "before"))
  expect_match(canvas$query, "hideAssignmentGrades")
})

test_that("invalid or unknown ids never mutate Canvas", {
  canvas <- fake_visibility()
  for (ids in list(integer(), NA, c(101, 101), "bad", 1.5, 0))
    expect_error(post_grades(10, 20, ids), "user_id")
  expect_error(post_grades(10, 20, 999), "no submission")
  expect_error(post_grades("bad", 20, 101), "course_id")
  expect_error(post_grades(10, c(20, 21), 101), "scalar")
  expect_equal(canvas$posts, 0L)
})

test_that("unpublished assignments cannot be posted", {
  canvas <- fake_visibility(published = FALSE)
  expect_error(post_grades(10, 20, 101), "unpublished")
  expect_equal(canvas$posts, 0L)
})

test_that("both GraphQL error levels and missing Progress fail", {
  fake_visibility(top_error = list(list(message = "denied")))
  expect_error(post_grades(10, 20, 101), "denied")
  fake_visibility(payload_error = list(list(message = list("invalid"))))
  expect_error(post_grades(10, 20, 101), "invalid")
  fake_visibility(missing_job = TRUE)
  expect_error(post_grades(10, 20, 101), "Progress")
  fake_visibility(state = "failed")
  expect_error(post_grades(10, 20, 101), "failed")
})

test_that("verification failures carry results and collateral-change details", {
  canvas <- fake_visibility(skip = TRUE)
  err <- tryCatch(post_grades(10, 20, 101), error = identity)
  expect_s3_class(err, "rcanvas_grade_visibility_error")
  expect_false(err$results$verified)
  expect_equal(canvas$reads, 4L)
  fake_visibility(change_other = TRUE)
  err <- tryCatch(post_grades(10, 20, 101), error = identity)
  expect_equal(err$non_target_changed, "103")
  fake_visibility(change_policy = TRUE)
  err <- tryCatch(post_grades(10, 20, 101), error = identity)
  expect_true(err$policy_changed)
})

test_that("async and unverified modes return without verification", {
  canvas <- fake_visibility(state = "queued")
  result <- post_grades(10, 20, 101, wait = FALSE)
  expect_equal(result$id, "7")
  expect_equal(result$workflow_state, "queued")
  expect_equal(canvas$reads, 1L)
  canvas <- fake_visibility()
  result <- post_grades(10, 20, 101, verify = FALSE)
  expect_true(is.na(result$verified))
  expect_equal(canvas$reads, 1L)
})

test_that("single student remains an array in the JSON body", {
  canvas <- fake_visibility()
  post_grades(10, 20, 101)
  body <- jsonlite::toJSON(list(variables = canvas$variables), auto_unbox = TRUE)
  expect_match(body, '"onlyStudentIds":\\["101"\\]')
})

test_that("missing posting policy stops before mutation", {
  canvas <- fake_visibility()
  original <- get("canvas_query", asNamespace("rcanvas"))
  local_mocked_bindings(canvas_query = function(urlx, args = NULL, type = "GET", ...) {
    if (grepl("/assignments/20$", urlx)) return(list(published = TRUE))
    original(urlx, args, type, ...)
  }, .package = "rcanvas")
  expect_error(post_grades(10, 20, 101), "post_manually")
  expect_equal(canvas$posts, 0L)
})
