context("orderly")


test_that("create", {
  skip_if_no_orderlyweb_server()
  token <- Sys.getenv("ORDERLYWEB_TEST_TOKEN")
  remote <- orderlyweb_remote(host = "localhost", port = 8888,
                              token = token, https = FALSE, name = "remote")
  expect_is(remote, "orderlyweb_remote")
  expect_true(orderly:::implements_remote(remote))
  expect_equal(remote$name, "remote")
})


test_that("list", {
  skip_if_no_orderlyweb_server()
  token <- Sys.getenv("ORDERLYWEB_TEST_TOKEN")
  remote <- orderlyweb_remote(host = "localhost", port = 8888,
                              token = token, https = FALSE)
  res <- remote$list_reports()
  expect_is(res, "character")
  expect_true("minimal" %in% res)
  expect_match(remote$name, "localhost")
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

  ## Directly pull and see the report:
  path <- remote$pull("minimal", v, FALSE)
  expect_true("orderly_run.rds" %in% dir(path))
  expect_equal(nrow(orderly::orderly_list_archive(root = dest)), 0)

  ## Pull into the archive
  orderly::orderly_pull_archive("minimal", v, dest, remote = remote)
  res <- orderly::orderly_list_archive(root = dest)
  expect_equal(res, data_frame(name = "minimal", id = v))
})


test_that("metadata", {
  skip_if_no_orderlyweb_server()
  token <- Sys.getenv("ORDERLYWEB_TEST_TOKEN")
  remote <- orderlyweb_remote(host = "localhost", port = 8888,
                              token = token, https = FALSE)
  dest <- orderly::orderly_example("demo")
  v <- max(remote$list_versions("minimal"))

  path <- remote$pull("minimal", v, FALSE)
  meta <- remote$metadata("minimal", v)
  ## There is a chance this will failing during a migration
  expect_identical(
    readRDS(meta),
    readRDS(file.path(path, "orderly_run.rds")))
})


test_that("run", {
  skip_if_no_orderlyweb_server()
  token <- Sys.getenv("ORDERLYWEB_TEST_TOKEN")
  remote <- orderlyweb_remote(host = "localhost", port = 8888,
                              token = token, https = FALSE)
  res <- remote$run("minimal", open = FALSE, progress = FALSE)
  expect_equal(max(remote$list_versions("minimal")), res$id)
})


test_that("url_report returns expected url", {
  cl <- orderlyweb_remote("host", 8888, "token")
  expect_equal(
    cl$url_report("myreport", "20191007-160636-c822cacd"),
    "https://host:8888/report/myreport/20191007-160636-c822cacd/")
})


test_that("url_report includes prefix", {
  cl <- orderlyweb_remote("host", 8888, "token", prefix = "prefix")
  expect_equal(
    cl$url_report("name", "id"),
    "https://host:8888/prefix/report/name/id/")
})
