context("user-data")

test_that("returns the correct", {

  a <- get_user_items(366, "details")

  expect_is(a, "data.frame")

})
