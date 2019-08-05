has_orderlyweb_server <- function() {
  if (!nzchar(Sys.getenv("ORDERLYWEB_TEST_TOKEN", ""))) {
    return(FALS)
  }
  TRUE
}


skip_if_no_orderlyweb_server <- function() {
  testthat::skip_on_cran()

  if (!has_orderlyweb_server()) {
    testtthat::skip("No orderlyweb server")
  }
}


test_orderlyweb_api_client <- function() {
  skip_if_no_orderlyweb_server()
  host <- "localhost"
  port <- 8888
  token <- Sys.getenv("ORDERLYWEB_TEST_TOKEN")
  https <- FALSE
  orderlyweb_api_client(host, port, token, https = FALSE)
}
