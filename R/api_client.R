##' Create a low-level HTTP API client for use with OrderlyWeb.
##'
##' @title Create a low-level OrderlyWeb client
##'
##' @param host Fully qualified hostname for the OrderlyWeb instance
##'
##' @param port Port to use
##'
##' @param token Your application token for authentication.  The
##'   appropriate value here will depend on the authentication support
##'   that is built into the OrderlyWeb server that you are
##'   communicating with.  Provide the token directly (as a string) or
##'   provide a callback function that takes no arguments and returns
##'   token.
##'
##' @param name A friendly name for the server (e.g, "production" or
##'   "testing") which may be printed when using the remote, or when
##'   authenticating.  If not provided then a name will be constructed
##'   from \code{host}, \code{port} and (if provided)
##'   \code{prefix}.
##'
##' @param https Optional logical, indicating if this is an https
##'   connection - this should be \code{TRUE} in all production
##'   settings or credentials will be sent in the clear!
##'
##' @param prefix A prefix, if your OrderlyWeb server is mounted at a
##'   path within some larger website.
##'
##' @param api_version The API version to request - this should be
##'   left as 1.
##'
##' @param insecure Avoid SSL certificate testing - this is completely
##'   insecure (as bad as http) and exists only for testing.
##'
##' @param verbose Be verbose in all http exchanges.  This will be
##'   very noisy.
##'
##' @export
##' @examples
##' cl <- orderlyweb::orderlyweb_api_client(host = "example.com", port = 443,
##'                                         token = "mytoken")
##' cl$is_authorised()
orderlyweb_api_client <- function(host, port, token, name = NULL,
                                  https = TRUE, prefix = NULL, api_version = 2,
                                  insecure = FALSE, verbose = FALSE) {
  R6_orderlyweb_api_client$new(host, port, token, name = name,
                               https = https, prefix = prefix,
                               api_version = api_version, insecure = insecure,
                               verbose = verbose)
}


##' @importFrom R6 R6Class
R6_orderlyweb_api_client <- R6::R6Class(
  "orderlyweb_api_client",
  cloneable = FALSE,
  public = list(
    name = NULL,
    url = NULL,
    options = NULL,
    token = NULL,
    api_token = NULL,

    initialize = function(host, port, token, name, https, prefix,
                          api_version, insecure, verbose) {
      self$url <- orderlyweb_api_client_url(host, port, https, prefix,
                                            api_version)
      self$name <- orderlyweb_api_client_name(name, host, port, prefix)
      if (!is.function(token)) {
        token <- orderlyweb_token_constant(token)
      }
      self$token <- token
      self$options <- orderlyweb_api_client_options(insecure, verbose)
    },

    is_authorised = function() {
      !is.null(self$api_token)
    },

    authorise = function(refresh = FALSE) {
      if (refresh || is.null(self$api_token)) {
        message(sprintf("Authorising with server '%s'", self$name))
        self$api_token <-
          orderlyweb_api_client_login(self$url$api, self$token(), self$options)
      }
    },

    GET = function(...) {
      self$request(httr::GET, ...)
    },

    POST = function(...) {
      self$request(httr::POST, ...)
    },

    DELETE = function(...) {
      self$request(httr::DELETE, ...)
    },

    request = function(verb, path, ..., download = NULL) {
      self$authorise()
      if (!grepl("^/", path)) {
        stop("Expected an absolute path")
      }
      url <- paste0(self$url$api, path)

      do_request <- function() {
        verb(url, self$api_token, self$options, download$request, ...)
      }

      r <- do_request()
      if (httr::status_code(r) == 401L) {
        errors <- vcapply(response_to_json(r)$errors, "[[", "error")
        if ("bearer-token-invalid" %in% errors) {
          self$authorise(TRUE)
          r <- do_request()
        }
      }

      orderlyweb_api_client_response(r, download)
    }

  ))


orderlyweb_api_client_login <- function(url, token, options) {
  headers <- httr::add_headers(c("Authorization" = paste("token", token)))
  r <- httr::POST(paste0(url, "/login/"), headers, options,
                  encode = "form")
  httr::stop_for_status(r)
  data <- from_json(httr::content(r, "text", encoding = "UTF-8"))
  httr::add_headers("Authorization" = paste("Bearer", data$access_token))
}


orderlyweb_api_client_name <- function(name, host, port, prefix) {
  if (!is.null(name)) {
    assert_scalar_character(name)
    return(name)
  }
  if (is.null(prefix)) {
    sprintf("%s:%d", host, port)
  } else {
    sprintf("%s:%d/%s", host, port, prefix)
  }
}


## For our current systems we have:
##
## We have apis at:
##   https://ebola2018.dide.ic.ac.uk/api/v2/
##   https://support.montagu.dide.ic.ac.uk:10443/reports/api/v2/
##
## <protocol>://<host>:<port><prefix>/api/v2
orderlyweb_api_client_url <- function(host, port, https, prefix,
                                      api_version) {
  assert_scalar_character(host)
  assert_scalar_integer(port)
  assert_scalar_logical(https)
  assert_scalar_integer(api_version)

  protocol <- if (https) "https" else "http"

  if (is.null(prefix)) {
    prefix <- ""
  } else {
    assert_scalar_character(prefix)
    prefix <- gsub("(^/|/$)", "", prefix, perl = TRUE)
    if (nzchar(prefix)) {
      prefix <- paste0("/", prefix)
    }
  }

  url_www <- sprintf("%s://%s:%d%s", protocol, host, port, prefix)
  list(www = url_www,
       api = sprintf("%s/api/v%d", url_www, api_version))
}


orderlyweb_api_client_options <- function(verbose, insecure) {
  c(if (verbose) httr::verbose(),
    if (insecure) httr::config(ssl_verifypeer = 0, ssl_verifyhost = 0))
}


orderlyweb_api_client_response <- function(r, download) {
  code <- httr::status_code(r)
  if (code >= 300) {
    if (is_json_response(r)) {
      res <- response_to_json(r)
      stop(orderlyweb_api_error(res$errors[[1]]$detail, code, res$errors))
    }
    ## This should never really be used - it's just for when things go
    ## really pear shaped.
    if (code == 404) {
      stop("endpoint or resource not found")
    } else if (code == 403) {
      stop("endpoint or resource not found, or you do not have permission")
    } else {
      stop("server returned error code ", code)
    }
  }

  if (is.null(download)) {
    response_to_json(r)$data
  } else {
    download$dest
  }
}


orderlyweb_api_error <- function(msg, code, errors) {
  err <- list(message = msg, errors = errors, code = code)
  class(err) <- c("orderlyweb_api_error", "error", "condition")
  err
}


orderlyweb_download <- function(dest, progress, accept) {
  if (is.null(dest)) {
    ext <- switch(accept,
                  zip = ".zip",
                  csv = ".csv",
                  rds = ".rds",
                  "")
    dest <- tempfile(fileext = ext)
  }
  assert_scalar_character(dest)
  assert_scalar_logical(progress)
  list(dest = dest, progress = progress, accept = accept,
       request = c(httr::write_disk(dest),
                   orderlyweb_accept(accept),
                   if (progress) httr::progress()))
}


orderlyweb_accept <- function(accept) {
  switch(accept,
         binary = httr::accept("application/octet-stream"),
         rds = httr::accept("application/octet-stream"),
         zip = httr::accept("application/zip"),
         csv = httr::accept("text/csv"),
         stop("unknown type ", accept))
}


orderlyweb_token_constant <- function(token) {
  assert_scalar_character(token)
  function() {
    token
  }
}
