context("Integration tests")

## Tests that require a running orderlyweb server

test_that("Can use API client", {
  cl <- test_orderlyweb_api_client()
  expect_false(cl$is_authorised())
  res <- cl$GET("/")
  expect_true(cl$is_authorised())
  expect_equal(res$name, "OrderlyWeb")
  expect_is(res$endpoints, "character")
})
