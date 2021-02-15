context("report run")

test_that("progress - no output", {
  p <- report_wait_progress("key", TRUE, force = TRUE)
  id <- "20190805-153610-eebad7d5"
  expect_is(p, "function")
  msg <- capture_messages(
    p(list(status = "running", version = id, output = NULL)))
  expect_match(msg, "running: 20190805-153610-eebad7d5", all = FALSE)

  msg <- capture_messages(
    p(list(status = "success", version = id, output = NULL), TRUE))
  expect_match(msg, "^\r\\s*$")
})


test_that("progress - with output", {
  p <- report_wait_progress("key", TRUE, force = TRUE)
  id <- "20190805-153610-eebad7d5"
  expect_is(p, "function")
  msg <- capture_messages(
    p(list(status = "running", version = id, output = list("a"),
           queue = list())))
  expect_equal(msg[1:3], c("\r", "a\n", "\r"))
  expect_match(msg[[4]], "running: 20190805-153610-eebad7d5")

  msg <- capture_messages(
    p(list(status = "running", version = id, output = list("a"))))
  expect_equal(length(msg), 2)
  expect_match(msg[[2]], "running: 20190805-153610-eebad7d5")

  msg <- capture_messages(
    p(list(status = "running", version = id,
           output = list("a", "b", "c"))))
  expect_equal(msg[2:4], c("\r", "b\nc\n", "\r"))
  expect_match(msg[[5]], "running: 20190805-153610-eebad7d5")
})


test_that("progress - queued", {
  p <- report_wait_progress("key", TRUE, force = TRUE)
  id <- "20190805-153610-eebad7d5"
  expect_is(p, "function")
  msg <- capture_messages(
    p(list(status = "queued", version = id, output = list(),
           queue = list(
             list(
               key = "key1",
               status = "running",
               name = "name1"
             ),
             list(
               key = "key2",
               status = "queued",
               name = "name2"
             )))))
  expect_equal(msg[[2]], "[-] (key)  0s queued (2): name1 < name2")
  msg <- capture_messages(
    p(list(status = "queued", version = id, output = list(),
           queue = list(list(
             key = "key2",
             status = "running",
             name = "name2"
           )))))
  expect_equal(msg[[3]], "[\\] (key)  0s queued (1): name2")
})


test_that("query", {
  expect_null(
    report_run_query(NULL))
  expect_equal(
    report_run_query(1),
    list(timeout = "1"))
})


test_that("parameters", {
  expect_null(report_run_parameters(NULL))
  expect_null(report_run_parameters(list()))
  expect_equal(report_run_parameters(list(a = 1, b = 2)),
               list(a = 1, b = 2))

  expect_error(report_run_parameters(list(1, 2)),
               "'parameters' must be named")
  expect_error(report_run_parameters(list(a = 1, 2)),
               "'parameters' names must not be empty (check 2)",
               fixed = TRUE)
  expect_error(report_run_parameters(list(a = 1, a = 2)),
               "'parameters' names must be unique (check 'a')",
               fixed = TRUE)
  expect_error(report_run_parameters(c(a = 1, b = 2)),
               "'parameters' must be a list",
               fixed = TRUE)
  expect_error(report_run_parameters(list(a = 1:2, b = 2)),
               "All parameters must be scalar (check 'a')",
               fixed = TRUE)
})

test_that("body", {
  expect_null(report_run_body(NULL, NULL, NULL))
  expect_equal(report_run_body(list(a = 1, b = 2), NULL, NULL),
               list(
                  params = report_run_parameters(list(a = 1, b = 2))))
  expect_equal(report_run_body(NULL, "12345", NULL),
               list(gitCommit = "12345"))
  expect_equal(report_run_body(NULL, NULL, "science"),
               list(instances = list(source = "science")))
  expect_equal(report_run_body(NULL, NULL, list(source = "science",
                                                annex = "annex")),
               list(instances = list(source = "science",
                                     annex = "annex")))
})


test_that("cleanup", {
  client <- NULL
  ans <- list(status = "error",
              version = "id",
              output = list("output"))
  out <- capture_output(
    expect_error(
      report_wait_cleanup("name", ans, FALSE, TRUE, FALSE, client),
      "Report has failed: see above for details"))
  expect_equal(out, trimws(format_output(ans$output)))

  ans$status <- "interrupted"
  out <- capture_output(
    expect_error(
      report_wait_cleanup("name", ans, FALSE, TRUE, FALSE, client),
      "job killed by remote server"))
  expect_equal(out, trimws(format_output(ans$output)))

  expect_equal(
    report_wait_cleanup("name", ans, FALSE, FALSE, FALSE, client),
    list(name = "name", id = "id", status = "interrupted", output = ans$output,
         url = NULL))
})


test_that("cleanup - open url", {
  skip_on_cran()
  skip_if_not_installed("mockery")

  client <- list(api_client =
                   list(url = list(www = "https://example.com/reports")))
  ans <- list(status = "success",
              version = "id",
              output = list("output"))

  m <- mockery::mock()
  mockery::stub(report_wait_cleanup, "utils::browseURL", m)
  res1 <- report_wait_cleanup("name", ans, FALSE, TRUE, FALSE, client)
  expect_equal(res1$url, "https://example.com/reports/report/name/id")
  mockery::expect_called(m, 0)

  res2 <- report_wait_cleanup("name", ans, FALSE, TRUE, TRUE, client)
  mockery::expect_args(m, 1, res2$url)
  expect_equal(res1, res2)
})
