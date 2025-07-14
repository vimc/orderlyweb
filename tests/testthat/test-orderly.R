context("orderly")


test_that("create", {
  skip_if_no_orderlyweb_server()
  token <- Sys.getenv("ORDERLYWEB_TEST_TOKEN")
  remote <- orderlyweb_remote(host = "localhost", port = 8888,
                              token = token, https = FALSE, name = "remote")
  expect_is(remote, "orderlyweb_remote")
  expect_true(orderly1:::implements_remote(remote))
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
  dest <- orderly1::orderly_example("demo")
  v <- max(remote$list_versions("minimal"))

  ## Directly pull and see the report:
  path <- remote$pull("minimal", v, FALSE)
  expect_true("orderly_run.rds" %in% dir(path))
  expect_equal(nrow(orderly1::orderly_list_archive(root = dest)), 0)

  ## Pull into the archive
  orderly1::orderly_pull_archive("minimal", v, dest, remote = remote)
  res <- orderly1::orderly_list_archive(root = dest)
  expect_equal(res, data_frame(name = "minimal", id = v))
})


test_that("metadata", {
  skip_if_no_orderlyweb_server()
  token <- Sys.getenv("ORDERLYWEB_TEST_TOKEN")
  remote <- orderlyweb_remote(host = "localhost", port = 8888,
                              token = token, https = FALSE)
  dest <- orderly1::orderly_example("demo")
  v <- max(remote$list_versions("minimal"))

  path <- remote$pull("minimal", v, FALSE)
  meta <- remote$metadata("minimal", v)
  ## There is a chance this will fail during a migration
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

test_that("run with instance", {
  skip_if_no_orderlyweb_server()
  token <- Sys.getenv("ORDERLYWEB_TEST_TOKEN")
  remote <- orderlyweb_remote(host = "localhost", port = 8888,
                              token = token, https = FALSE)
  expect_error(remote$run("minimal", open = FALSE, progress = FALSE,
                          instance = "other"),
               "Report has failed: see above for details")
  expect_error(remote$run("minimal", open = FALSE, progress = FALSE,
                          instance = "missing"),
               "Report has failed: see above for details")
  res <- remote$run("minimal", open = FALSE, progress = FALSE,
                          instance = "default")
  expect_equal(res$status, "success")
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

test_that("run with instance", {
  skip_if_no_orderlyweb_server()
  token <- Sys.getenv("ORDERLYWEB_TEST_TOKEN")
  remote <- orderlyweb_remote(host = "localhost", port = 8888,
                              token = token, https = FALSE)
  res <- remote$run("minimal", open = FALSE, progress = FALSE)
  expect_equal(max(remote$list_versions("minimal")), res$id)
})


test_that("bundle interface", {
  skip_if_no_orderlyweb_server()
  token <- Sys.getenv("ORDERLYWEB_TEST_TOKEN")
  remote <- orderlyweb_remote(host = "localhost", port = 8888,
                              token = token, https = FALSE)

  res <- remote$bundle_pack("minimal", progress = FALSE)
  ans <- orderly1::orderly_bundle_run(res, echo = FALSE)
  expect_true(remote$bundle_import(ans$path, progress = FALSE))
  expect_true(ans$id %in% remote$list_versions("minimal"))
})


test_that("bundle high level interface", {
  skip_if_no_orderlyweb_server()
  token <- Sys.getenv("ORDERLYWEB_TEST_TOKEN")
  remote <- orderlyweb_remote(host = "localhost", port = 8888,
                              token = token, https = FALSE)

  capture.output(
    res <- orderly1::orderly_bundle_pack_remote("minimal", remote = remote))
  ans <- orderly1::orderly_bundle_run(res, echo = FALSE)
  capture.output(
    res <- orderly1::orderly_bundle_import_remote(ans$path, remote = remote))

  expect_true(ans$id %in% remote$list_versions("minimal"))
})


test_that("queue status", {
  skip_if_no_orderlyweb_server()
  token <- Sys.getenv("ORDERLYWEB_TEST_TOKEN")
  remote <- orderlyweb_remote(host = "localhost", port = 8888,
                              token = token, https = FALSE)
  out <- remote$run("slow3", open = FALSE, progress = FALSE, wait = FALSE)
  Sys.sleep(2) ## Ensure report gets started
  res <- remote$queue_status()
  expect_length(res$tasks, 1)
  expect_equal(res$tasks[[1]]$inputs$name, "slow3")
  expect_true(!is.null(res$tasks[[1]]$version))
  expect_equal(res$tasks[[1]]$key, out$key)
  expect_equal(res$tasks[[1]]$status, "running")
})

test_that("can kill report run", {
  skip_if_no_orderlyweb_server()
  token <- Sys.getenv("ORDERLYWEB_TEST_TOKEN")
  remote <- orderlyweb_remote(host = "localhost", port = 8888,
                              token = token, https = FALSE)
  out <- remote$run("slow3", open = FALSE, progress = FALSE, wait = FALSE)
  Sys.sleep(2) ## Ensure report gets started

  res <- remote$kill(out)
  expect_true(res$killed)
  expect_null(res$message)
})
