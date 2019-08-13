context("util (assert)")

test_that("assert_scalar", {
  expect_error(assert_scalar(NULL), "must be a scalar")
  expect_error(assert_scalar(numeric(0)), "must be a scalar")
  expect_error(assert_scalar(1:2), "must be a scalar")
})


test_that("assert_character", {
  expect_error(assert_character(1), "must be character")
  expect_error(assert_character(TRUE), "must be character")
})


test_that("assert_logical", {
  expect_error(assert_logical("one"), "must be logical")
  expect_error(assert_logical(1), "must be logical")
})


test_that("assert_integer", {
  object <- NULL
  expect_error(assert_integer(object), "'object' must be integer")

  expect_error(assert_integer(1.12), "must be integer")
  expect_error(assert_integer(pi), "must be integer")

  expect_silent(assert_integer(1))
  expect_silent(assert_integer(1L))

  expect_error(assert_integer(1, strict = TRUE), "must be integer")

  large <- .Machine$integer.max * 2
  expect_silent(assert_integer(large))
  expect_error(assert_integer(large, strict = TRUE),
               "'large' must be integer")
})
