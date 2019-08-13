context("orderly")


test_that("create", {
  skip_if_no_orderlyweb_server()
  token <- Sys.getenv("ORDERLYWEB_TEST_TOKEN")
  remote <- orderlyweb_remote(host = "localhost", port = 8888,
                              token = token, https = FALSE)
  expect_is(remote, "orderlyweb_remote")
  expect_true(orderly:::implements_remote(remote))
})


test_that("list", {
  skip_if_no_orderlyweb_server()
  token <- Sys.getenv("ORDERLYWEB_TEST_TOKEN")
  remote <- orderlyweb_remote(host = "localhost", port = 8888,
                              token = token, https = FALSE)
  res <- remote$list_reports()
  expect_is(res, "character")
  expect_true("minimal" %in% res)
})


test_that("list_versions", {
  skip_if_no_orderlyweb_server()
  token <- Sys.getenv("ORDERLYWEB_TEST_TOKEN")
  remote <- orderlyweb_remote(host = "localhost", port = 8888,
                              token = token, https = FALSE)
  res <- remote$list_versions("minimal")
  expect_is(res, "character")
})


test_that("list_versions", {
  skip_if_no_orderlyweb_server()
  token <- Sys.getenv("ORDERLYWEB_TEST_TOKEN")
  remote <- orderlyweb_remote(host = "localhost", port = 8888,
                              token = token, https = FALSE)
  res <- remote$list_versions("minimal")
  expect_is(res, "character")
  expect_match(res, "^([0-9]{8}-[0-9]{6})-([[:xdigit:]]{8})$")
})


test_that("list_versions", {
  skip_if_no_orderlyweb_server()
  token <- Sys.getenv("ORDERLYWEB_TEST_TOKEN")
  remote <- orderlyweb_remote(host = "localhost", port = 8888,
                              token = token, https = FALSE)
  res <- remote$list_versions("minimal")
  expect_is(res, "character")
  expect_match(res, "^([0-9]{8}-[0-9]{6})-([[:xdigit:]]{8})$")
})


test_that("pull", {
  skip_if_no_orderlyweb_server()
  token <- Sys.getenv("ORDERLYWEB_TEST_TOKEN")
  remote <- orderlyweb_remote(host = "localhost", port = 8888,
                              token = token, https = FALSE)
  dest <- orderly::orderly_example("demo")
  v <- max(remote$list_versions("minimal"))
  remote$pull("minimal", v, dest, FALSE)
  res <- orderly::orderly_list_archive(root = dest)
  expect_equal(res, data_frame(name = "minimal", id = v))
})


test_that("run", {
  skip_if_no_orderlyweb_server()
  token <- Sys.getenv("ORDERLYWEB_TEST_TOKEN")
  remote <- orderlyweb_remote(host = "localhost", port = 8888,
                              token = token, https = FALSE)
  res <- remote$run("minimal", open = FALSE, progress = FALSE)
  expect_equal(max(remote$list_versions("minimal")), res$id)
})
