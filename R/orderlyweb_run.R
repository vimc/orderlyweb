report_run_wait <- function(path, name, key, client, timeout, poll, open,
                            progress, stop_on_error) {
  if (progress) {
    message(sprintf("running report '%s' as '%s'", name, key))
  }
  t_start <- Sys.time()
  t_stop <- t_start + timeout
  tick <- report_wait_progress(key, progress)
  ans <- client$report_run_status(key, output = progress)

  repeat {
    ans <- client$report_run_status(key, output = progress)

    if (!(ans$status %in% c("queued", "running"))) {
      tick(ans, finish = TRUE)
      break
    } else {
      tick(ans)
    }

    if (Sys.time() > t_stop) {
      stop("timeout reached")
    }

    Sys.sleep(if (ans$status == "queued") max(poll, 1) else poll)
  }

  report_wait_cleanup(name, ans, progress, stop_on_error, open, client)
}


report_run_query <- function(ref, update, timeout) {
  query <- list()
  if (!is.null(ref)) {
    query$ref <- ref
  }
  if (!update) {
    query$update <- "false"
  }
  if (!is.null(timeout)) {
    assert_scalar_integer(timeout)
    query$timeout <- as.character(timeout)
  }
  if (length(query) == 0L) {
    query <- NULL
  }
  query
}


report_wait_progress <- function(key, progress, force = FALSE) {
  if (!progress) {
    return(function(...) {})
  }

  fmt <- sprintf("[:spin] (%s) :elapsed :status", key)
  p <- progress::progress_bar$new(fmt, NA, show_after = 0, force = force)
  prev_output <- list(stderr = NULL, stdout = NULL)
  w <- getOption("width", 80L)

  new <- function(now, prev) {
    if (length(prev) == 0) {
      now
    } else {
      now[-seq_along(prev)]
    }
  }

  tick <- function(response, finish = FALSE) {
    state <- response$status
    output <- response$output
    version <- response$version %||% "???"

    if (state == "queued") {
      queue <- matrix(
        unlist(strsplit(output$stdout, ":", fixed = TRUE)),
        length(output$stdout), byrow = TRUE)
      status <- trim_string(sprintf(
        "queued (%d): %s", nrow(queue), paste(queue[, 3], collapse = " < ")),
        w - 12L)
    } else {
      status <- sprintf("%s: %s", state, version)
    }

    if (state %in% c("running", "error", "success") && !is.null(output)) {
      new_output <- Map(new, output, prev_output)
      if (any(lengths(new_output)) > 0L) {
        clear_progress_bar(p)
        message(format_output(new_output), appendLF = FALSE)
      }
      prev_output <<- output
    }

    if (finish) {
      p$terminate()
    } else {
      p$tick(tokens = list(status = status))
    }
    invisible()
  }

  tick
}


report_wait_cleanup <- function(name, ans, progress, stop_on_error,
                                open, client) {
  if (is.null(ans$output)) {
    ans <- client$report_run_status(ans$key, TRUE)
  }

  if (stop_on_error && ans$status %in% c("error", "killed")) {
    ## TODO: It would be super nice to get the full stack trace back
    ## here from orderly on error.  That should not be hard to do.
    if (!progress) {
      cat(format_output(ans$output))
    }
    if (ans$status == "killed") {
      stop("job killed by remote server")
    } else {
      stop("Report has failed: see above for details")
    }
  }

  if (ans$status == "success") {
    url <- sprintf("%s/reports/%s/%s", client$api_client$url$www,
                   name, ans$version)
    if (open) {
      message("Opening report in browser (you may need to log in)")
      utils::browseURL(url)
    }
  } else {
    url <- NULL
  }
  list(name = name,
       id = ans$version,
       status = ans$status,
       output = ans$output,
       url = url)
}
