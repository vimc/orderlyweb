context("util")

test_that("or", {
  expect_null(NULL %||% NULL)
  expect_true(NULL %||% TRUE)
  expect_false(FALSE %||% TRUE)
})


test_that("trim string", {
  expect_equal(trim_string("this is a long string", 80),
               "this is a long string")
  expect_equal(trim_string("this is a long string", 15),
               "this is a l ...")
  expect_equal(trim_string("this is a long string", 15, "..."),
               "this is a lo...")
})
