##' Implements an orderly "remote" using OrderlyWeb as a backend.  Use
##' this within an \code{orderly_config.yml} configuration.
##'
##' @title Create orderly remote
##' @inheritParams orderlyweb_api_client
##' @export
##' @examples
##' remote <- orderlyweb::orderlyweb_remote("example.com", 443, "mytoken")
##' remote
orderlyweb_remote <- function(host, port, token, https = TRUE,
                              prefix = NULL, name = NULL) {
  R6_orderlyweb_remote$new(host, port, token, https, prefix, name)
}


R6_orderlyweb_remote <- R6::R6Class(
  "orderlyweb_remote",
  cloneable = FALSE,

  private = list(
    client = NULL
  ),

  public = list(
    name = NULL,

    initialize = function(host, port, token, https, prefix, name) {
      private$client <- orderlyweb(host = host, port = port,
                                   token = token, https = https,
                                   prefix = prefix, name = name)
      self$name <- private$client$api_client$name
    },

    list_reports = function() {
      private$client$report_list()$name
    },

    list_versions = function(name) {
      private$client$report_versions(name)
    },

    pull = function(name, id, progress = TRUE) {
      zip <- private$client$report_download(name, id, progress = progress)
      on.exit(unlink(zip))
      unzip_archive(zip, name, id)
    },

    url_report = function(name, id) {
      sprintf("%s/report/%s/%s/", private$client$api_client$url$www, name, id)
    },

    metadata = function(name, id) {
      private$client$report_metadata_orderly(name, id)
    },

    run = function(name, parameters = NULL, ref = NULL, timeout = NULL,
                   wait = 1000, poll = 1, progress = TRUE,
                   stop_on_error = TRUE, stop_on_timeout = TRUE, open = FALSE,
                   instance = NULL) {
      private$client$report_run(name = name, parameters = parameters,
                                ref = ref, timeout = timeout, wait = wait,
                                poll = poll, open = open,
                                stop_on_error = stop_on_error,
                                stop_on_timeout = stop_on_timeout,
                                progress = progress, instance = instance)
    },

    bundle_pack = function(name, parameters = NULL, instance = NULL,
                           progress = TRUE) {
      private$client$bundle_pack(name, parameters, instance, progress)
    },

    bundle_import = function(path, progress = TRUE) {
      private$client$bundle_import(path, progress)
    }
  ))
