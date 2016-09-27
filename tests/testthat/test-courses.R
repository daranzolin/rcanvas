context("course-data")

test_that("returns the correct", {

  # Note: the user and course ids tested below are unique to an institution's instance of Canvas

  a <- get_course_list()
  b <- get_course_list(user_id = 366)
  c <- get_course_items(course_id = 20)
  d <- get_course_items(course_id = 20, item = "settings")
  e <- get_course_analytics_data(course_id = 20)
  f <- get_course_analytics_data(course_id = 20, type = "activity")
  g <- get_course_analytics_data(course_id = 20, type = "activity", user_id = 366)
  h <- get_course_list(include = c("teachers", "total_students"))

  # classes
  expect_is(a, "data.frame")
  expect_is(b, "data.frame")
  expect_is(c, "data.frame")
  expect_is(d, "data.frame")
  expect_is(e, "data.frame")
  expect_is(f, "data.frame")
  expect_is(g, "list")
  expect_is(e, "data.frame")

})

test_that("vectorization works:", {

  expect_true("teachers" %in% names(h))
  expect_true("total_students" %in% names(h))

})
