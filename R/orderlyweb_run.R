report_run_wait <- function(path, name, key, client, timeout, poll, open,
                            progress, stop_on_error, stop_on_timeout) {
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
      if (stop_on_timeout) {
        stop("timeout reached")
      } else {
        break
      }
    }

    Sys.sleep(if (ans$status == "queued") max(poll, 1) else poll)
  }

  report_wait_cleanup(name, ans, progress, stop_on_error, open, client)
}


report_run_query <- function(timeout) {
  if (!is.null(timeout)) {
    assert_scalar_integer(timeout)
    query <- list(
      timeout = as.character(timeout)
    )
  } else {
    query <- NULL
  }
  query
}

report_run_body <- function(parameters, ref, instance) {
  body <- list()
  if (!is.null(ref)) {
    assert_scalar_character(ref)
    body[["gitCommit"]] <- ref
  }
  if (!is.null(instance)) {
    ## Instance can be single string
    ## Or named of instances e.g. list(source = x, annex = f)
    if (is.character(instance)) {
      ## TODO: name "source" should not be hard coded and this
      ## default should depend on the particular orderly configuration
      ## see VIMC-4587
      body[["instances"]] <- list(source = instance)
    } else if (is.list(instance)) {
      body[["instances"]] <- instance
    }
  }
  body[["params"]] <- report_run_parameters(parameters)
  if (length(body) == 0) {
    body <- NULL
  }
  body
}


report_run_parameters <- function(parameters) {
  if (is.null(parameters) || length(parameters) == 0) {
    return(NULL)
  }
  nms <- names(parameters)
  if (is.null(nms)) {
    stop("'parameters' must be named")
  }
  if (!all(nzchar(nms))) {
    stop(sprintf("'parameters' names must not be empty (check %s)",
                 paste(which(!nchar(nms)), collapse = ", ")))
  }
  if (any(duplicated(nms))) {
    stop(sprintf("'parameters' names must be unique (check %s)",
                 paste(squote(unique(nms[duplicated(nms)])), collapse = ", ")))
  }
  if (!is.list(parameters)) {
    stop("'parameters' must be a list")
  }
  err <- lengths(parameters) != 1
  if (any(err)) {
    stop(sprintf("All parameters must be scalar (check %s)",
                 paste(squote(nms[err]), collapse = ", ")))
  }
  parameters
}


report_wait_progress <- function(key, progress, force = FALSE) {
  if (!progress) {
    return(function(...) {})
  }

  fmt <- sprintf("[:spin] (%s) :elapsed :status", key)
  p <- progress::progress_bar$new(fmt, NA, show_after = 0, force = force)
  prev_output <- NULL
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
    queue <- response$queue
    output <- response$output
    version <- response$version %||% "???"

    if (state == "queued") {
      queued <- vcapply(queue, function(item) {
        item$name
      })
      status <- sprintf("queued (%d): %s", length(queue),
                        paste(queued, collapse = " < "))
    } else {
      status <- sprintf("%s: %s", state, version)
    }

    if (state != "queued" && !is.null(output)) {
      new_output <- new(output, prev_output)
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

  if (stop_on_error && ans$status %in%
      c("error", "orphan", "interrupted", "redirect", "missing")) {
    ## TODO: It would be super nice to get the full stack trace back
    ## here from orderly on error.  That should not be hard to do.
    if (!progress) {
      cat(format_output(ans$output))
    }
    if (ans$status == "interrupted") {
      stop("job killed by remote server", call. = FALSE)
    } else {
      stop("Report has failed: see above for details", call. = FALSE)
    }
  }

  if (ans$status == "success") {
    url <- sprintf("%s/report/%s/%s", client$api_client$url$www,
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
