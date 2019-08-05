has_orderlyweb_server <- function() {
  if (!nzchar(Sys.getenv("ORDERLYWEB_TEST_TOKEN", ""))) {
    return(FALSE)
  }
  TRUE
}


skip_if_no_orderlyweb_server <- function() {
  testthat::skip_on_cran()

  if (!has_orderlyweb_server()) {
    testtthat::skip("No orderlyweb server")
  }
}


env <- new.env(parent = emptyenv())

test_orderlyweb_api_client <- function(use_cache = TRUE) {
  skip_if_no_orderlyweb_server()
  host <- "localhost"
  port <- 8888
  token <- Sys.getenv("ORDERLYWEB_TEST_TOKEN")
  https <- FALSE
  cl <- orderlyweb_api_client(host, port, token, https = FALSE)
  if (use_cache) {
    if (is.null(env$api_token)) {
      cl$GET("/")
      env$api_token <- cl$api_token
    } else {
      cl$api_token <- env$api_token
    }
  }
  cl
}


test_orderlyweb <- function(use_cache = TRUE) {
  orderlyweb(api_client = test_orderlyweb_api_client(use_cache))
}
