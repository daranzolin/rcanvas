# A fake Canvas that behaves like the real one where it matters: it normalizes
# posted grades (percentages become points of points_possible), applies a late
# deduction to `score` while keeping `entered_score`, answers Progress polls
# from a script, and advances a fake clock when the code sleeps.
fake_canvas <- function(states = c("queued", "completed"), points_possible = 100, published = TRUE,
                        skip = character(), late = list(), fail_message = NULL,
                        post_error_on = integer(), submissions = NULL, env = parent.frame()) {
  canvas <- new.env(parent = emptyenv())
  canvas$posts <- list()
  canvas$polls <- 0
  canvas$entered <- list()
  canvas$sleeps <- numeric()
  canvas$clock <- as.POSIXct("2026-10-05 12:00:00", tz = "UTC")
  apply_grade <- function(user, value) {
    if (grepl("%$", value)) {
      points <- as.numeric(sub("%$", "", value)) / 100 * points_possible
      return(list(entered_score = points, entered_grade = format(points)))
    }
    points <- suppressWarnings(as.numeric(value))
    if (is.na(points)) return(list(entered_score = NULL, entered_grade = value))
    list(entered_score = points, entered_grade = format(points))
  }
  local_mocked_bindings(
    make_canvas_url = function(...) paste(c("https://canvas.example.edu/api/v1", ...), collapse = "/"),
    canvas_query = function(urlx, args = NULL, type = "GET", retry_429 = 0) {
      if (type == "POST") {
        canvas$posts[[length(canvas$posts) + 1]] <- list(url = urlx, args = args, retry_429 = retry_429)
        if (length(canvas$posts) %in% post_error_on) stop("HTTP 500 Internal Server Error")
        for (key in names(args)) {
          user <- sub("^grade_data\\[([^]]+)\\].*$", "\\1", key)
          if (!user %in% skip) canvas$entered[[user]] <- apply_grade(user, args[[key]])
        }
        return(list(id = 7, workflow_state = states[1]))
      }
      if (grepl("/progress/", urlx)) {
        canvas$polls <- canvas$polls + 1
        return(list(id = 7, workflow_state = states[min(length(states), canvas$polls + 1)],
                    message = fail_message))
      }
      if (grepl("/submissions$", urlx)) {
        canvas$verify_url <- urlx
        if (!is.null(submissions)) return(submissions)
        return(lapply(names(canvas$entered), function(user) {
          e <- canvas$entered[[user]]
          deduct <- if (is.null(late[[user]])) 0 else late[[user]]
          list(user_id = as.integer(user), entered_score = e$entered_score,
               score = if (is.null(e$entered_score)) NULL else e$entered_score - deduct,
               entered_grade = e$entered_grade)
        }))
      }
      list(id = 20, published = published, points_possible = points_possible, grading_type = "points")
    },
    .rcanvas_json = function(response) response,
    .rcanvas_next_link = function(response) NULL,
    .rcanvas_sleep = function(seconds) {
      canvas$sleeps <- c(canvas$sleeps, seconds)
      canvas$clock <- canvas$clock + seconds
    },
    .rcanvas_now = function() canvas$clock,
    .package = "rcanvas",
    .env = env
  )
  canvas
}

test_that("update_grades posts grade_data to the bulk endpoint and verifies", {
  canvas <- fake_canvas()
  result <- update_grades(10, 20, user_id = c(101, 102), grade = c(88.5, 92))

  expect_length(canvas$posts, 1)
  expect_equal(canvas$posts[[1]]$url,
               "https://canvas.example.edu/api/v1/courses/10/assignments/20/submissions/update_grades")
  expect_equal(canvas$posts[[1]]$args,
               list(`grade_data[101][posted_grade]` = "88.5", `grade_data[102][posted_grade]` = "92"))
  expect_equal(canvas$verify_url, "https://canvas.example.edu/api/v1/courses/10/assignments/20/submissions")
  expect_equal(result$user_id, c("101", "102"))
  expect_equal(result$entered_score, c(88.5, 92))
  expect_true(all(result$verified))
})

test_that("update_grades refuses an unpublished assignment before posting", {
  canvas <- fake_canvas(published = FALSE)
  expect_error(update_grades(10, 20, user_id = 101, grade = 80), "unpublished")
  expect_length(canvas$posts, 0)
})

test_that("update_grades splits large uploads into chunks", {
  canvas <- fake_canvas()
  result <- update_grades(10, 20, user_id = 1:250, grade = seq(0, 100, length.out = 250), chunk_size = 100)

  expect_equal(vapply(canvas$posts, function(p) length(p$args), integer(1)), c(100L, 100L, 50L))
  expect_equal(nrow(result), 250)
  expect_true(all(result$verified))
})

test_that("verification uses entered_score, so a late deduction still verifies", {
  fake_canvas(late = list(`101` = 5))
  result <- update_grades(10, 20, user_id = 101, grade = 80)
  expect_equal(result$entered_score, 80)
  expect_equal(result$score, 75)
  expect_true(result$verified)
})

test_that("percentages are verified as points of points_possible", {
  fake_canvas(points_possible = 50)
  result <- update_grades(10, 20, user_id = c(101, 102), grade = c("40%", "40.00%"))
  expect_equal(result$expected_points, c(20, 20))
  expect_equal(result$entered_score, c(20, 20))
  expect_true(all(result$verified))
})

test_that("letter grades are verified by entered_grade", {
  fake_canvas()
  result <- update_grades(10, 20, user_id = c(101, 102), grade = c("A-", "B+"))
  expect_true(all(is.na(result$expected_points)))
  expect_true(all(result$verified))
})

test_that("the verification tolerance is inclusive at 0.005", {
  subs <- list(list(user_id = 101, entered_score = 80.005, score = 80.005, entered_grade = "80.005"),
               list(user_id = 102, entered_score = 80.0051, score = 80.0051, entered_grade = "80.0051"))
  fake_canvas(submissions = subs)
  expect_warning(result <- update_grades(10, 20, user_id = c(101, 102), grade = c(80, 80)),
                 "1 of 2 grades did not verify")
  expect_equal(result$verified, c(TRUE, FALSE))
})

test_that("numbers are sent with a '.' decimal mark at full precision", {
  canvas <- fake_canvas()
  old <- options(OutDec = ",", digits = 3)
  on.exit(options(old), add = TRUE)
  update_grades(10, 20, user_id = c(101, 102, 103), grade = c(88.5, 92.125, 1e6))
  expect_equal(unname(unlist(canvas$posts[[1]]$args)), c("88.5", "92.125", "1000000"))
})

test_that("update_grades polls Progress until completed", {
  canvas <- fake_canvas(states = c("queued", "running", "running", "completed"))
  update_grades(10, 20, user_id = 101, grade = 75, poll_interval = 2)

  expect_equal(canvas$polls, 3)
  expect_equal(canvas$sleeps, c(2, 2, 2))
})

test_that("update_grades stops when the Progress job fails", {
  fake_canvas(states = c("queued", "failed"), fail_message = "grading period closed")
  expect_error(update_grades(10, 20, user_id = 101, grade = 75), "failed: grading period closed")
})

test_that("update_grades stops at an elapsed-time deadline", {
  canvas <- fake_canvas(states = c("queued", "running"))
  expect_error(update_grades(10, 20, user_id = 101, grade = 75, poll_interval = 2, timeout = 5),
               "still running after 5 seconds")
  expect_equal(sum(canvas$sleeps), 6)
})

test_that("a later chunk failing reports what was already applied", {
  fake_canvas(post_error_on = 2L)
  err <- tryCatch(update_grades(10, 20, user_id = 1:5, grade = c(70, 71, 72, 73, 74), chunk_size = 2),
                  rcanvas_update_grades_error = function(e) e)

  expect_s3_class(err, "rcanvas_update_grades_error")
  expect_match(conditionMessage(err), "stopped at chunk 2 of 3")
  expect_equal(err$completed_user_ids, c("1", "2"))
  expect_equal(err$uncertain_user_ids, c("3", "4"))
  expect_equal(err$unsent_user_ids, "5")
  expect_equal(err$chunks$status, c("completed", "failed", "unsent"))
  expect_true(all(err$results$verified))
})

test_that("update_grades flags grades a completed job silently skipped", {
  fake_canvas(skip = "102")
  expect_warning(result <- update_grades(10, 20, user_id = c(101, 102, 103), grade = c(80, 85, 90)),
                 "1 of 3 grades did not verify")
  expect_equal(result$verified, c(TRUE, FALSE, TRUE))
  expect_true(is.na(result$entered_score[2]))
})

test_that("an empty submissions response leaves every grade unverified", {
  fake_canvas(submissions = list())
  expect_warning(result <- update_grades(10, 20, user_id = c(101, 102), grade = c(80, 85)),
                 "2 of 2 grades did not verify")
  expect_equal(result$verified, c(FALSE, FALSE))
})

test_that("update_grades returns Progress objects without waiting when asked", {
  canvas <- fake_canvas(states = c("queued", "completed"))
  progress <- update_grades(10, 20, user_id = 101, grade = 75, wait = FALSE)

  expect_equal(progress[[1]]$workflow_state, "queued")
  expect_equal(canvas$polls, 0)
  expect_null(canvas$verify_url)
})

test_that("update_grades rejects malformed input before contacting Canvas", {
  canvas <- fake_canvas()
  expect_error(update_grades(10, 20, user_id = c(1, 2), grade = 90), "same length")
  expect_error(update_grades(10, 20, user_id = c(1, 1), grade = c(90, 91)), "unique")
  expect_error(update_grades(10, 20, user_id = 1, grade = NA), "missing")
  expect_error(update_grades(10, 20, user_id = 1, grade = Inf), "finite")
  expect_error(update_grades(10, 20, user_id = integer(), grade = numeric()), "No grades")
  expect_error(update_grades(10, 20, user_id = 1, grade = 90, chunk_size = 1.5), "chunk_size")
  expect_error(update_grades(10, 20, user_id = 1, grade = 90, poll_interval = 0), "poll_interval")
  expect_error(update_grades(10, 20, user_id = 1, grade = 90, timeout = Inf), "timeout")
  expect_error(update_grades(10, 20, user_id = 1, grade = 90, wait = NA), "wait")
  expect_error(update_grades(10, 20, user_id = 1, grade = 90, verify = c(TRUE, FALSE)), "verify")
  expect_length(canvas$posts, 0)
})

test_that("canvas_query retries a 429 after Retry-After, then succeeds", {
  calls <- 0
  sleeps <- numeric()
  response <- function(status, headers = list()) structure(
    list(status_code = status, headers = structure(headers, class = c("insensitive", "list")),
         url = "https://canvas.example.edu/api/v1/x", content = raw()),
    class = "response")
  local_mocked_bindings(
    GET = function(...) {
      calls <<- calls + 1
      if (calls == 1) response(429, list(`retry-after` = "3")) else response(200)
    },
    check_token = function() "token",
    .rcanvas_sleep = function(seconds) sleeps <<- c(sleeps, seconds),
    .package = "rcanvas"
  )
  resp <- canvas_query("https://canvas.example.edu/api/v1/x", list(), "GET", retry_429 = 2)
  expect_equal(httr::status_code(resp), 200)
  expect_equal(calls, 2)
  expect_equal(sleeps, 3)
})

test_that("canvas_query does not retry 429 by default", {
  calls <- 0
  local_mocked_bindings(
    GET = function(...) {
      calls <<- calls + 1
      structure(list(status_code = 429, headers = list(), url = "https://canvas.example.edu/api/v1/x",
                     content = raw()), class = "response")
    },
    check_token = function() "token",
    .package = "rcanvas"
  )
  expect_error(canvas_query("https://canvas.example.edu/api/v1/x", list(), "GET"))
  expect_equal(calls, 1)
})
