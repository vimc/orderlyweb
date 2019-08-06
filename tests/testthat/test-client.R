context("API client")

test_that("Authentication logic", {
  cl <- test_orderlyweb_api_client(FALSE)
  expect_false(cl$is_authorised())
  res <- cl$GET("/")
  expect_true(cl$is_authorised())
  expect_equal(res$name, "OrderlyWeb")
  expect_is(res$endpoints, "character")

  expect_silent(res1 <- cl$GET("/reports/"))
  cl$api_token <- httr::add_headers("Authorization" =
                                      paste("Bearer", "aninvalidtoken"))
  expect_message(
    res2 <- cl$GET("/reports/"),
    "Authorising with server http://localhost:8888")
  expect_equal(res1, res2)
})
