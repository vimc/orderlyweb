context("Integration tests")

## Tests that require a running orderlyweb server
test_that("can list reports", {
  cl <- test_orderlyweb()
  d <- cl$report_list()
  expect_is(d, "data.frame")
  expect_equal(names(d), c("name", "display_name", "latest_version"))
  expect_true("minimal" %in% d$name)
})


test_that("can list report versions", {
  cl <- test_orderlyweb()
  d <- cl$report_list()
  v <- cl$report_versions("minimal")
  expect_is(v, "character")
  expect_true(length(v) >= 3)
  expect_true(d$latest_version[d$name == "minimal"] %in% v)
})


test_that("missing report version behaviour", {
  cl <- test_orderlyweb()
  expect_error(
    cl$report_versions("report-that-does-not-exist"),
    "Unknown report : 'report-that-does-not-exist'",
    class = "orderlyweb_api_error")
  expect_equal(
    cl$report_versions("report-that-does-not-exist", FALSE),
    character(0))
})


test_that("can fetch changelog", {
  cl <- test_orderlyweb()
  v <- cl$report_versions("changelog")

  d1 <- cl$report_changelog("changelog", v[[1]])
  expect_equal(names(d1), c("report_version", "label", "value", "from_file"))

  d2 <- cl$report_changelog("changelog", v[[2]])
  d2_prev <- tail(d2, nrow(d1))
  rownames(d2_prev) <- NULL
  expect_equal(d2_prev, d1)

  d3 <- cl$report_changelog("changelog", NULL)
  expect_equal(d2, d3)
})


test_that("can fetch metadata", {
  cl <- test_orderlyweb()
  d <- cl$report_list()

  name <- "minimal"
  version <- d$latest_version[d$name == name]

  dat <- cl$report_metadata(name, version)

  ## This one is not well defined
  expect_equal(dat$name, name)
  expect_equal(dat$id, version)
  expect_true("description" %in% names(dat))
})


test_that("can fetch run metadata", {
  cl <- test_orderlyweb()
  d <- cl$report_list()

  name <- "minimal"
  version <- d$latest_version[d$name == name]

  res <- cl$report_metadata_orderly(name, version)
  expect_true(file.exists(res))

  dat <- readRDS(res)
  expect_equal(dat$meta$name, name)
  expect_equal(dat$meta$id, version)
  expect_true("description" %in% names(dat$meta))
})


test_that("download", {
  cl <- test_orderlyweb()
  d <- cl$report_list()

  name <- "global"
  version <- d$latest_version[d$name == name]

  zip <- cl$report_download(name, version, progress = FALSE)
  expect_match(zip, "\\.zip$")
  path <- tempfile()
  unzip(zip, exdir = path)
  expect_equal(dir(path), version)
  ## NOTE: this may fail when the demo repo is out of date because
  ## there will be a migration
  found <- dir(file.path(path, version))
  is_migration_file <- grepl("^orderly_run_([0-9]+\\.){3}rds$", found)
  expect_setequal(
    found[!is_migration_file],
    c("data.csv", "orderly.yml", "orderly_run.rds", "out.rds", "script.R"))
})


test_that("download progress", {
  cl <- test_orderlyweb()
  d <- cl$report_list()

  name <- "minimal"
  version <- d$latest_version[d$name == name]

  out <- capture.output(
    zip <- cl$report_download(name, version, progress = TRUE))
  expect_is(out, "character")
})


test_that("artefact list", {
  cl <- test_orderlyweb()
  d <- cl$report_list()

  name <- "minimal"
  version <- d$latest_version[d$name == name]

  res <- cl$report_artefact_list(name, version)
  expect_is(res, "character")
  expect_equal(names(res), "mygraph.png")
  expect_match(res, "^[[:xdigit:]]{32}$")
})


## TODO: test on subdirectory artefact
test_that("artefact download", {
  cl <- test_orderlyweb()
  d <- cl$report_list()

  name <- "minimal"
  version <- d$latest_version[d$name == name]
  info <- cl$report_artefact_list(name, version)

  path <- cl$report_artefact_download(name, version, "mygraph.png",
                                      progress = FALSE)
  expect_true(file.exists(path))
  expect_equal(unname(tools::md5sum(path)), unname(info))
})


test_that("list data", {
  cl <- test_orderlyweb()
  d <- cl$report_list()

  name <- "minimal"
  version <- d$latest_version[d$name == name]
  info <- cl$report_data_list(name, version)

  expect_equal(names(info), "dat")
  expect_match(info, "^[[:xdigit:]]{32}$")
})


test_that("download report data", {
  cl <- test_orderlyweb()
  d <- cl$report_list()

  name <- "minimal"
  version <- d$latest_version[d$name == name]

  path_rds <- cl$report_data_download(name, version, "dat", progress = FALSE)
  path_csv <- cl$report_data_download(name, version, "dat", csv = TRUE,
                                      progress = FALSE)

  d1 <- readRDS(path_rds)
  d2 <- read.csv(path_csv, stringsAsFactors = FALSE)
  expect_equal(d1, d2)
  expect_equal(names(d1), c("name", "number"))
})


test_that("resource list", {
  cl <- test_orderlyweb()
  d <- cl$report_list()
  name <- "use_resource"
  version <- d$latest_version[d$name == name]

  info <- cl$report_resource_list(name, version)
  expect_equal(names(info), "meta/data.csv")
  expect_match(info, "^[[:xdigit:]]{32}$")
})


test_that("resource download", {
  cl <- test_orderlyweb()
  d <- cl$report_list()
  name <- "use_resource"
  version <- d$latest_version[d$name == name]

  path <- cl$report_resource_download(name, version, "meta/data.csv",
                                      progress = FALSE)
  expect_true(file.exists(path))
  d <- read.csv(path, stringsAsFactors = FALSE)
  expect_equal(d, data_frame(a = c(1, 4), b = c(2, 5), c = c(3, 6)))
})


test_that("data download", {
  cl <- test_orderlyweb()
  d <- cl$report_list()

  name <- "minimal"
  version <- d$latest_version[d$name == name]
  info <- cl$report_data_list(name, version)

  path_rds <- cl$data_download(info, progress = FALSE)
  path_csv <- cl$data_download(info, csv = TRUE, progress = FALSE)

  d1 <- readRDS(path_rds)
  d2 <- read.csv(path_csv, stringsAsFactors = FALSE)
  expect_equal(d1, d2)
  expect_equal(names(d1), c("name", "number"))

  cmp <- cl$report_data_download(name, version, "dat", progress = FALSE)
  expect_equal(unname(tools::md5sum(cmp)),
               unname(tools::md5sum(path_rds)))
})


test_that("summary", {
  cl <- test_orderlyweb()
  d <- cl$versions()
  expect_is(d, "data.frame")
  expect_equal(names(d),
               c("name", "id", "latest_version", "published", "date",
                 "display_name"))
})


test_that("publish", {
  cl <- test_orderlyweb()
  name <- "other"
  version <- min(cl$report_versions(name))
  expect_false(cl$report_metadata(name, version)$published)

  expect_true(cl$report_publish(name, version, TRUE))
  expect_true(cl$report_metadata(name, version)$published)
  expect_true(cl$report_publish(name, version, TRUE))
  expect_true(cl$report_metadata(name, version)$published)
  expect_false(cl$report_publish(name, version, FALSE))
  expect_false(cl$report_metadata(name, version)$published)
  expect_false(cl$report_publish(name, version, FALSE))
  expect_false(cl$report_metadata(name, version)$published)
})


test_that("git", {
  cl <- test_orderlyweb()
  st <- cl$git_status()
  expect_equal(st$branch, "master")
  expect_error(cl$git_pull(), NA)
  expect_error(cl$git_fetch(), NA)
})


test_that("run: simple", {
  cl <- test_orderlyweb()
  res <- cl$report_run("minimal", poll = 0.1, progress = FALSE)

  expect_equal(names(res), c("name", "id", "status", "output", "url"))
  expect_equal(res$name, "minimal")
  expect_equal(res$status, "success")
  expect_equal(res$url,
               paste0("http://localhost:8888/report/minimal/", res$id))
  expect_match(res$output, "[ name       ]  minimal",
               fixed = TRUE, all = FALSE)
})


test_that("run: simple", {
  cl <- test_orderlyweb()
  res <- cl$report_run("minimal", poll = 0.1, progress = FALSE)

  expect_equal(names(res), c("name", "id", "status", "output", "url"))
  expect_equal(res$name, "minimal")
  expect_equal(res$status, "success")
  expect_equal(res$url,
               paste0("http://localhost:8888/report/minimal/", res$id))
  expect_match(res$output, "[ name       ]  minimal",
               fixed = TRUE, all = FALSE)
})


test_that("run: get handle", {
  cl <- test_orderlyweb()
  ans <- cl$report_run("minimal", wait = FALSE)
  expect_is(ans, "orderlyweb_run")
  testthat::try_again(10, {
    Sys.sleep(2)
    expect_equal(cl$report_run_status(ans)$status, "success")
  })
  expect_equal(cl$report_run_status(ans$key)$status, "success")
  res <- cl$report_run_wait(ans, progress = FALSE)
  expect_equal(res$name, "minimal")

  msg <- capture_messages(cl$report_run_wait(ans, progress = TRUE))
  expect_equal(msg[[1]],
               sprintf("running report 'minimal' as '%s'\n", ans$key))
})


test_that("run: pass parameters", {
  cl <- test_orderlyweb()
  ans <- cl$report_run("other", parameters = list(nmin = 0.5), wait = FALSE)
  expect_is(ans, "orderlyweb_run")

  testthat::try_again(10, {
    Sys.sleep(2)
    expect_equal(cl$report_run_status(ans)$status, "success")
  })
  expect_equal(cl$report_run_status(ans$key)$status, "success")
  res <- cl$report_run_wait(ans, progress = FALSE)
  expect_equal(res$name, "other")

  expect_match(res$output, "nmin: 0.5", all = FALSE, fixed = TRUE)

  msg <- capture_messages(cl$report_run_wait(ans, progress = TRUE))
  expect_equal(msg[[1]],
               sprintf("running report 'other' as '%s'\n", ans$key))
})

test_that("run: instance", {
  cl <- test_orderlyweb()
  res <- cl$report_run("minimal", poll = 0.1, instance = "other",
                       progress = FALSE)
  expect_equal(res$status, "error")
  expect_true("Error: no such table: thing" %in% res$output)

  res <- cl$report_run("minimal", poll = 0.1, instance = "missing",
                       progress = FALSE)
  expect_equal(res$status, "error")
  expect_true("Error: Invalid instance 'missing' for database 'source'" %in%
                res$output)

  res <- cl$report_run("minimal", poll = 0.1, instance = "default",
                       progress = FALSE)
  expect_equal(names(res), c("name", "id", "status", "output", "url"))
  expect_equal(res$status, "success")
  expect_match(res$output, "[ name       ]  minimal",
               fixed = TRUE, all = FALSE)
})


test_that("wait validation", {
  cl <- test_orderlyweb()
  expect_error(cl$report_run_wait(TRUE),
               "Expected an 'orderlyweb_run' object")
})


test_that("timeout", {
  cl <- test_orderlyweb()
  ans <- cl$report_run("slow1", wait = FALSE)
  expect_error(
    cl$report_run_wait(ans, timeout = 0, poll = 0, progress = FALSE),
    "timeout reached")
  expect_error(
    cl$report_run_wait(ans, timeout = 10, poll = 1, progress = FALSE),
    NA)
})


test_that("timeout without error", {
  cl <- test_orderlyweb()
  ans <- cl$report_run("slow10", wait = FALSE)
  res <- cl$report_run_wait(ans, timeout = 2, poll = 0, progress = FALSE,
                            stop_on_timeout = FALSE)
  expect_equal(res$status, "running")
  res <- cl$report_run_wait(ans, timeout = 15, poll = 0, progress = FALSE,
                            stop_on_timeout = FALSE)
  expect_equal(res$status, "success")
})


test_that("can pack a bundle", {
  cl <- test_orderlyweb()
  res <- cl$bundle_pack("minimal", progress = FALSE)
  expect_true(file.exists(res))
  expect_equal(dirname(res), tempdir())
  expect_match(basename(res), "^[0-9]{8}-[0-9]{6}-[[:xdigit:]]{8}\\.zip$")

  ans <- orderly::orderly_bundle_run(res, echo = FALSE)
  expect_equal(ans$id, sub("\\.zip$", "", basename(res)))
  expect_false(ans$id %in% cl$report_versions("minimal"))

  cl$bundle_import(ans$path, progress = FALSE)

  expect_true(ans$id %in% cl$report_versions("minimal"))
})


test_that("can pass parameters to bundle pack", {
  cl <- test_orderlyweb()
  res <- cl$bundle_pack("other", list(nmin = 0.5), progress = FALSE)

  tmp <- tempfile()
  ans <- zip::unzip(res, exdir = tmp)
  expect_equal(
    readRDS(file.path(tmp, dir(tmp), "meta", "info.rds"))$parameters,
    list(nmin = 0.5))
})


test_that("can pass instance to bundle pack", {
  cl <- test_orderlyweb()
  res <- cl$bundle_pack("minimal", instance = "default", progress = FALSE)

  tmp <- tempfile()
  ans <- zip::unzip(res, exdir = tmp)
  expect_equal(
    readRDS(file.path(tmp, dir(tmp), "meta", "info.rds"))$instance,
    "default")
})


test_that("queue status", {
  cl <- test_orderlyweb()
  run <- cl$report_run("slow3", open = FALSE, progress = FALSE, wait = FALSE)
  Sys.sleep(2) ## Ensure report gets started
  res <- cl$queue_status()
  expect_length(res$tasks, 1)
  expect_equal(res$tasks[[1]]$name, "slow3")
  expect_true(!is.null(res$tasks[[1]]$version))
  expect_equal(res$tasks[[1]]$key, run$key)
  expect_equal(res$tasks[[1]]$status, "running")
})

test_that("report can be killed", {
  cl <- test_orderlyweb()
  ans <- cl$report_run("slow10", wait = FALSE)
  Sys.sleep(2) ## Ensure report gets started
  res <- cl$report_kill(ans)
  expect_true(res$killed)
  expect_null(res$message)
})

test_that("failed to kill report returns message", {
  cl <- test_orderlyweb()
  out <- cl$report_run("minimal", wait = FALSE)
  output <- cl$report_run_wait(out, progress = FALSE)

  res <- cl$report_kill(out)
  expect_false(res$killed)
  expect_match(res$message, "Failed to kill")
})
