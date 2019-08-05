assert_scalar <- function(x, name = deparse(substitute(x))) {
  if (length(x) != 1) {
    stop(sprintf("'%s' must be a scalar", name), call. = FALSE)
  }
}


assert_character <- function(x, name = deparse(substitute(x))) {
  if (!is.character(x)) {
    stop(sprintf("'%s' must be character", name), call. = FALSE)
  }
}


assert_numeric <- function(x, name = deparse(substitute(x))) {
  if (!is.numeric(x)) {
    stop(sprintf("'%s' must be numeric", name), call. = FALSE)
  }
}


assert_integer <- function(x, name = deparse(substitute(x))) {
  if (!is.numeric(x)) {
    stop(sprintf("'%s' must be numeric", name), call. = FALSE)
  }
}


assert_logical <- function(x, name = deparse(substitute(x))) {
  if (!is.logical(x)) {
    stop(sprintf("'%s' must be logical", name), call. = FALSE)
  }
}


assert_hash <- function(x, name = deparse(substitute(x))) {
  if (!all(grepl("^[[:xdigit:]]{32}$", x))) {
    stop(sprintf("'%s' must be a hash", name), call. = FALSE)
  }
}


assert_scalar_logical <- function(x, name = deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_logical(x, name)
}


assert_scalar_numeric <- function(x, name = deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_numeric(x, name)
}


assert_scalar_character <- function(x, name = deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_character(x, name)
}


assert_scalar_integer <- function(x, strict = FALSE,
                                  name = deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_integer(x, strict, name)
}


assert_named <- function(x, unique = FALSE, name = deparse(substitute(x))) {
  if (is.null(names(x))) {
    stop(sprintf("'%s' must be named", name), call. = FALSE)
  }
  if (unique && any(duplicated(names(x)))) {
    stop(sprintf("'%s' must have unique names", name), call. = FALSE)
  }
}


assert_is <- function(x, what, name = deparse(substitute(x))) {
  if (!inherits(x, what)) {
    stop(sprintf("'%s' must be a %s", name,
                 paste(what, collapse = " / ")), call. = FALSE)
  }
}


match_value <- function(arg, choices, name = deparse(substitute(arg))) {
  assert_scalar_character(arg)
  if (!(arg %in% choices)) {
    stop(sprintf("%s must be one of %s",
                 name, paste(squote(choices), collapse = ", ")))
  }
  arg
}


assert_type <- function(x, type, name = deparse(substitute(x))) {
  switch(type,
         logical = assert_scalar_logical(x, name),
         numeric = assert_scalar_numeric(x, name),
         character = assert_scalar_character(x, name))
}


assert_nonmissing <- function(x, name = deparse(substitute(x)),
                              what = "non-NA") {
  if (any(is.na(x))) {
    stop(sprintf("'%s' must be %s", name, what), call. = FALSE)
  }
  invisible(x)
}


assert_integer <- function(x, strict = FALSE, name = deparse(substitute(x)),
                           what = "integer") {
  if (!(is.integer(x))) {
    usable_as_integer <-
      !strict && is.numeric(x) && (max(abs(round(x) - x)) < 1e-8)
    if (!usable_as_integer) {
      stop(sprintf("'%s' must be %s", name, what), call. = FALSE)
    }
  }
  invisible(x)
}
