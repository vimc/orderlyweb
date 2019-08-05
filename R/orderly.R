orderlyweb_remote <- function(hostname, port, token, https = TRUE,
                              prefix = NULL) {
  R6_orderlyweb_remote$new(hostname, port, token, https, prefix)
}


R6_orderlyweb_remote <- R6::R6Class(
  "orderlyweb_remote",
  cloneable = FALSE,

  private = list(
    client = NULL
  ),

  public = list(
    initialize = function(hostname, port, token, https, prefix) {
      private$client <- orderlyweb(hostname = hostname , port = port,
                                   token = token, https = https,
                                   prefix = prefix)
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
                   stop_on_error = TRUE, stop_on_timeout = TRUE,
                   open = FALSE) {
      private$client$report_run(name = name, parameters = parameters,
                                ref = ref, timeout = timeout, wait = wait,
                                poll = poll, open = open,
                                stop_on_error = stop_on_error,
                                stop_on_timeout = stop_on_timeout,
                                progress = progress)
    }
  ))