##' Implements an orderly "remote" using OrderlyWeb as a backend.  Use
##' this within an \code{orderly_config.yml} configuration.
##'
##' @title Create orderly remote
##' @inheritParams orderlyweb_api_client
##' @export
##' @examples
##' remote <- orderlyweb::orderlyweb_remote("example.com", 443, "mytoken")
##' remote
orderlyweb_remote <- function(hostname, port, token, https = TRUE,
                              prefix = NULL, name = NULL) {
  R6_orderlyweb_remote$new(hostname, port, token, https, prefix, name)
}


R6_orderlyweb_remote <- R6::R6Class(
  "orderlyweb_remote",
  cloneable = FALSE,

  private = list(
    client = NULL
  ),

  public = list(
    initialize = function(hostname, port, token, https, prefix, name) {
      private$client <- orderlyweb(hostname = hostname, port = port,
                                   token = token, https = https,
                                   prefix = prefix, name = name)
    },

    list_reports = function() {
      private$client$report_list()$name
    },

    list_versions = function(name) {
      private$client$report_versions(name)
    },

    pull = function(name, id, root, progress = TRUE) {
      dest <- private$client$report_download(name, id, progress = progress)
      orderly:::unzip_archive(dest, root, name, id)
    },

    run = function(name, parameters = NULL, ref = NULL, timeout = NULL,
                   wait = 1000, poll = 1, progress = TRUE,
                   stop_on_error = TRUE, open = FALSE) {
      private$client$report_run(name = name, parameters = parameters,
                                ref = ref, timeout = timeout, wait = wait,
                                poll = poll, open = open,
                                stop_on_error = stop_on_error,
                                progress = progress)
    }
  ))
