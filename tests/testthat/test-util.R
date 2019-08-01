context("util")

test_that("or", {
  expect_null(NULL %||% NULL)
  expect_true(NULL %||% TRUE)
  expect_false(FALSE %||% TRUE)
})
