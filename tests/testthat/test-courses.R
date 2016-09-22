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
  h <- get_course_members(course_id = 20)

  # classes
  expect_is(a, "data.frame")
  expect_is(b, "data.frame")
  expect_is(c, "data.frame")
  expect_is(d, "data.frame")
  expect_is(e, "data.frame")
  expect_is(f, "data.frame")
  expect_is(g, "list")
  expect_is(h, "data.frame")
  expect_is(e, "data.frame")

  # values
  expect_equal(a$name[1], "Welcome to Canvas")
  expect_equal(unique(d$course_id), 20)

})
