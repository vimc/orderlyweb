context("report run")

test_that("progress - no output", {
  p <- report_wait_progress("key", TRUE, force = TRUE)
  id <- "20190805-153610-eebad7d5"
  expect_is(p, "function")
  msg <- capture_messages(
    p(list(status = "running", version = id, output = NULL)))
  expect_match(msg, "running: 20190805-153610-eebad7d5", all = FALSE)
})


test_that("progress - with output", {
  p <- report_wait_progress("key", TRUE, force = TRUE)
  id <- "20190805-153610-eebad7d5"
  expect_is(p, "function")
  msg <- capture_messages(
    p(list(status = "running", version = id, output = list(stdout = "a"))))
  expect_equal(msg[1:3], c("\r", "a\n", "\r"))
  expect_match(msg[[4]], "running: 20190805-153610-eebad7d5")

  msg <- capture_messages(
    p(list(status = "running", version = id, output = list(stdout = "a"))))
  expect_equal(length(msg), 2)
  expect_match(msg[[2]], "running: 20190805-153610-eebad7d5")

  msg <- capture_messages(
    p(list(status = "running", version = id,
           output = list(stdout = c("a", "b", "c")))))
  expect_equal(msg[2:4], c("\r", "b\nc\n", "\r"))
  expect_match(msg[[5]], "running: 20190805-153610-eebad7d5")
})


test_that("progress - queued", {
  p <- report_wait_progress("key", TRUE, force = TRUE)
  id <- "20190805-153610-eebad7d5"
  expect_is(p, "function")
  msg <- capture_messages(
    p(list(status = "queued", version = id,
           output = list(stdout =
                           c("running:key1:name1", "queued:key2:name2")))))
  expect_equal(msg[[2]], "[-] (key)  0s queued (2): name1 < name2")
  msg <- capture_messages(
    p(list(status = "queued", version = id,
           output = list(stdout = c("running:key2:name2")))))
  expect_equal(msg[[3]], "[\\] (key)  0s queued (1): name2")
})