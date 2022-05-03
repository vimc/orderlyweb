##' Create an orderlyweb object, for interaction with the OrderlyWeb
##' API at a high level.  For low level access, see
##' \code{\link{orderlyweb_api_client}} or the \code{$api_client}
##' field of the created object.
##'
##' @title Create an orderlyweb object
##'
##' @param ... Arguments passed through to
##'   \code{\link{orderlyweb_api_client}}
##'
##' @param api_client A pre-constructed
##'   \code{\link{orderlyweb_api_client}}, in which case \code{...} is
##'   ignored
##'
##' @export
##' @examples
##' cl <- orderlyweb::orderlyweb(host = "example.com", port = 443,
##'                              token = "mytoken")
##' cl$api_client$is_authorised()
orderlyweb <- function(..., api_client = NULL) {
  if (is.null(api_client)) {
    api_client <- orderlyweb_api_client(...)
  }
  R6_orderlyweb$new(api_client = api_client)
}


R6_orderlyweb <- R6::R6Class(
  "orderlyweb",
  cloneable = FALSE,

  public = list(
    api_client = NULL,

    initialize = function(api_client) {
      self$api_client <- api_client
      lockBinding("api_client", self)
    },

    report_list = function() {
      res <- self$api_client$GET("/reports/")
      name <- vcapply(res, "[[", "name")
      display_name <- vcapply(
        res, function(x) x$display_name %||% NA_character_)
      latest_version <- vcapply(res, "[[", "latest_version")
      data_frame(name = name,
                 display_name = display_name,
                 latest_version = latest_version)
    },

    report_versions = function(name, error_if_missing = TRUE) {
      assert_scalar_logical(error_if_missing)
      res <- tryCatch(
        self$api_client$GET(sprintf("/reports/%s/", name)),
        error = identity)
      report_versions_return(res, error_if_missing)
    },

    report_changelog = function(name, version = NULL) {
      if (is.null(version)) {
        path <- sprintf("/reports/%s/latest/changelog/", name)
      } else {
        path <- sprintf("/reports/%s/versions/%s/changelog/", name, version)
      }
      dat <- self$api_client$GET(path)
      data_frame(report_version = vcapply(dat, "[[", "report_version"),
                 label = vcapply(dat, "[[", "label"),
                 value = vcapply(dat, "[[", "value"),
                 from_file = vlapply(dat, "[[", "from_file"))
    },

    report_metadata = function(name, version) {
      self$api_client$GET(sprintf("/reports/%s/versions/%s/", name, version))
    },

    report_metadata_orderly = function(name, version, dest = NULL) {
      download <- orderlyweb_download(dest, FALSE, "rds")
      path <- sprintf("/reports/%s/versions/%s/run-meta", name, version)
      self$api_client$GET(path, download = download)
    },

    report_download = function(name, version, dest = NULL, progress = TRUE) {
      download <- orderlyweb_download(dest, progress, "zip")
      ret <- self$api_client$GET(
        sprintf("/reports/%s/versions/%s/all/", name, version),
        download = download)
      fix_progress_print(progress)
      ret
    },

    report_artefact_list = function(name, version) {
      res <- self$api_client$GET(
        sprintf("/reports/%s/versions/%s/artefacts/", name, version))
      list_to_character(res)
    },

    report_artefact_download = function(name, version, filename,
                                        dest = NULL, progress = TRUE) {
      filename_enc <- encode_path(filename)
      path <- sprintf("/reports/%s/versions/%s/artefacts/%s/",
                      name, version, filename_enc)
      download <- orderlyweb_download(dest, progress, "binary")
      self$api_client$GET(path, download = download)
    },

    report_resource_list = function(name, version) {
      res <- self$api_client$GET(
        sprintf("/reports/%s/versions/%s/resources/", name, version))
      list_to_character(res)
    },

    report_resource_download = function(name, version, filename,
                                        dest = NULL, progress = TRUE) {
      filename_enc <- encode_path(filename)
      path <- sprintf("/reports/%s/versions/%s/resources/%s/",
                      name, version, filename_enc)
      download <- orderlyweb_download(dest, progress, "binary")
      self$api_client$GET(path, download = download)
    },

    report_data_list = function(name, version) {
      res <- self$api_client$GET(
        sprintf("/reports/%s/versions/%s/data/", name, version))
      list_to_character(res)
    },

    report_data_download = function(name, version, hash, csv = FALSE,
                                    dest = NULL, progress = TRUE) {
      path <- sprintf("/reports/%s/versions/%s/data/%s", name, version, hash)
      type <- if (csv) "csv" else "rds"
      download <- orderlyweb_download(dest, progress, type)
      self$api_client$GET(path, query = list(type = type), download = download)
    },

    report_run = function(name, parameters = NULL, ref = NULL,
                          timeout = NULL, wait = Inf,
                          poll = 0.5, open = FALSE,
                          stop_on_error = FALSE, stop_on_timeout = TRUE,
                          progress = TRUE, instance = NULL) {
      query <- report_run_query(timeout)
      body <- report_run_body(parameters, ref, instance)
      res <- self$api_client$POST(sprintf("/reports/%s/run/", name),
                                  query = query, body = body,
                                  encode = "json")
      class(res) <- "orderlyweb_run"

      if (wait > 0) {
        self$report_run_wait(res, timeout = wait, poll = poll, open = open,
                             stop_on_error = stop_on_error,
                             stop_on_timeout = stop_on_timeout,
                             progress = progress)
      } else {
        res
      }
    },

    report_run_status = function(key, output = FALSE) {
      if (inherits(key, "orderlyweb_run")) {
        key <- key$key
      }
      path <- sprintf("/reports/%s/status/", key)
      query <- if (output) list(output = TRUE) else NULL
      self$api_client$GET(path, query = query)
    },

    report_run_wait = function(x, timeout = Inf, poll = 0.5,
                               open = FALSE, stop_on_error = FALSE,
                               stop_on_timeout = TRUE,
                               progress = TRUE, output = TRUE) {
      if (!inherits(x, "orderlyweb_run")) {
        stop("Expected an 'orderlyweb_run' object")
      }
      report_run_wait(x$path, x$name, x$key, self,
                      timeout = timeout, poll = poll, open = open,
                      stop_on_error = stop_on_error,
                      stop_on_timeout = stop_on_timeout,
                      progress = progress)
    },

    report_kill = function(key) {
      if (inherits(key, "orderlyweb_run")) {
        key <- key$key
      }
      path <- sprintf("/reports/%s/kill/", key)
      self$api_client$DELETE(path)
    },

    data_download = function(hash, csv = FALSE, dest = NULL,
                             progress = TRUE) {
      path <- sprintf("/data/%s/%s", if (csv) "csv" else "rds", hash)
      accept <- if (csv) "csv" else "binary"
      download <- orderlyweb_download(dest, progress, accept)
      self$api_client$GET(path, download = download)
    },

    report_publish = function(name, version, value = TRUE) {
      ## A bit more complicated than it used to be: mrc-1473
      assert_scalar_logical(value)
      status <- self$report_metadata(name, version)$published
      if (status != value) {
        self$api_client$POST(
          sprintf("/reports/%s/versions/%s/publish/", name, version),
          query = list(value = tolower(value)))
      }
      value
    },

    bundle_pack = function(name, parameters = NULL, instance = NULL,
                           progress = TRUE) {
      parameters <- report_run_parameters(parameters)
      if (!is.null(instance)) {
        query <- list(instance = instance)
      } else {
        query <- NULL
      }
      download <- orderlyweb_download(tempfile(), progress, "zip")
      res <- self$api_client$POST(sprintf("/bundle/pack/%s/", name),
                                  body = parameters, download = download,
                                  encode = "json", query = query)
      fix_progress_print(progress)

      filename <- paste0(sub("/.*", "", zip::zip_list(res)$filename[[1]]),
                         ".zip")

      dest <- file.path(tempdir(), filename)
      fs::file_move(res, dest)

      dest
    },

    bundle_import = function(path, progress = TRUE) {
      self$api_client$POST("/bundle/import/",
                           body = httr::upload_file(path),
                           if (progress) httr::progress("up"))
    },

    versions = function() {
      dat <- self$api_client$GET("/versions/")
      data_frame(
        name = vcapply(dat, "[[", "name"),
        id = vcapply(dat, "[[", "id"),
        latest_version = vcapply(dat, "[[", "latest_version"),
        published = vlapply(dat, "[[", "published"),
        date = vcapply(dat, "[[", "date"),
        display_name = vcapply(dat, function(x)
          x$display_name %||% NA_character_))
    },

    git_status = function(client = NULL) {
      self$api_client$GET("/reports/git/status/")
    },

    git_pull = function(client = NULL) {
      self$api_client$POST("/reports/git/pull/")
    },

    git_fetch = function(client = NULL) {
      self$api_client$POST("/reports/git/fetch/")
    },

    queue_status = function() {
      self$api_client$GET("/queue/status/")
    }
  ))


report_versions_return <- function(res, error_if_missing) {
  allow_missing <- !error_if_missing
  if (inherits(res, "character")) {
    return(res)
  } else if (allow_missing &&
             inherits(res, "orderlyweb_api_error") &&
             length(res$errors) == 1L &&
             res$errors[[1]]$code == "unknown-report") {
    return(character(0))
  } else {
    stop(res)
  }
}
