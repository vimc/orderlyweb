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


test_that("API client must use absolute paths", {
  cl <- test_orderlyweb_api_client()
  expect_error(cl$GET("reports")
               "Expected an absolute path")
})


test_that("API client URL can use prefix, stripping slashes", {
  expect_equal(
    orderlyweb_api_client_url("host", 443, TRUE, "prefix", 1),
    list(www = "https://host:443/prefix",
         api = "https://host:443/prefix/api/v1"))
  expect_equal(
    orderlyweb_api_client_url("host", 443, TRUE, "/prefix/", 1),
    list(www = "https://host:443/prefix",
         api = "https://host:443/prefix/api/v1"))
})


test_that("API client URL ignores empty prefix", {
  cmp <- list(www = "https://host:443",
              api = "https://host:443/api/v1")
  expect_equal(
    orderlyweb_api_client_url("host", 443, TRUE, "", 1), cmp)
  expect_equal(
    orderlyweb_api_client_url("host", 443, TRUE, "/", 1), cmp)
  expect_equal(
    orderlyweb_api_client_url("host", 443, TRUE, NULL, 1), cmp)
})
