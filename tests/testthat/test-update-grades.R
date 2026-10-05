# A fake Canvas: records POST bodies, answers Progress polls, and serves the
# submissions that the posts produced (optionally dropping some students).
fake_canvas <- function(states = c("queued", "completed"), skip = character(), fail_message = NULL) {
  canvas <- new.env(parent = emptyenv())
  canvas$posts <- list()
  canvas$polls <- 0
  canvas$scores <- list()
  canvas$sleeps <- 0
  json_response <- function(x) structure(list(body = jsonlite::toJSON(x, auto_unbox = TRUE, null = "null")),
                                         class = "fake_response")
  local_mocked_bindings(
    make_canvas_url = function(...) paste(c("https://canvas.example.edu/api/v1", ...), collapse = "/"),
    canvas_query = function(urlx, args = NULL, type = "GET") {
      if (type == "POST") {
        canvas$posts[[length(canvas$posts) + 1]] <- list(url = urlx, args = args)
        for (key in names(args)) {
          user <- sub("^grade_data\\[([^]]+)\\].*$", "\\1", key)
          if (!user %in% skip) canvas$scores[[user]] <- args[[key]]
        }
        return(json_response(list(id = 7, workflow_state = states[1])))
      }
      canvas$polls <- canvas$polls + 1
      state <- states[min(length(states), canvas$polls + 1)]
      json_response(list(id = 7, workflow_state = state, message = fail_message))
    },
    process_response = function(url, args) {
      canvas$verify_url <- url
      data.frame(user_id = as.integer(names(canvas$scores)),
                 score = suppressWarnings(as.numeric(unlist(canvas$scores))),
                 entered_grade = unlist(canvas$scores, use.names = FALSE))
    },
    .rcanvas_json = function(response) jsonlite::fromJSON(response$body, simplifyVector = FALSE),
    .rcanvas_sleep = function(seconds) canvas$sleeps <- canvas$sleeps + 1,
    .package = "rcanvas",
    .env = parent.frame()
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
  expect_equal(result$score, c(88.5, 92))
  expect_true(all(result$verified))
})

test_that("update_grades splits large uploads into chunks", {
  canvas <- fake_canvas()
  result <- update_grades(10, 20, user_id = 1:250, grade = seq(0, 100, length.out = 250), chunk_size = 100)

  expect_equal(vapply(canvas$posts, function(p) length(p$args), integer(1)), c(100L, 100L, 50L))
  expect_equal(nrow(result), 250)
  expect_true(all(result$verified))
})

test_that("update_grades polls Progress until completed", {
  canvas <- fake_canvas(states = c("queued", "running", "running", "completed"))
  update_grades(10, 20, user_id = 101, grade = 75)

  expect_equal(canvas$polls, 3)
  expect_equal(canvas$sleeps, 3)
})

test_that("update_grades stops when the Progress job fails", {
  fake_canvas(states = c("queued", "failed"), fail_message = "grading period closed")
  expect_error(update_grades(10, 20, user_id = 101, grade = 75), "failed: grading period closed")
})

test_that("update_grades stops at the timeout", {
  fake_canvas(states = c("queued", "running"))
  expect_error(update_grades(10, 20, user_id = 101, grade = 75, poll_interval = 2, timeout = 4),
               "still running after 4 seconds")
})

test_that("update_grades flags grades a completed job silently skipped", {
  fake_canvas(skip = "102")
  expect_warning(result <- update_grades(10, 20, user_id = c(101, 102, 103), grade = c(80, 85, 90)),
                 "1 of 3 grades did not verify")
  expect_equal(result$verified, c(TRUE, FALSE, TRUE))
  expect_true(is.na(result$score[2]))
})

test_that("update_grades verifies letter grades by entered_grade", {
  fake_canvas()
  result <- update_grades(10, 20, user_id = c(101, 102), grade = c("A-", "B+"))
  expect_true(all(result$verified))
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
  expect_error(update_grades(10, 20, user_id = integer(), grade = numeric()), "No grades")
  expect_length(canvas$posts, 0)
})
