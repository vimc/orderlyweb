`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}


from_json <- function(x) {
  jsonlite::fromJSON(x, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
}


## The error handler here is for responding to nginx gateway timeouts
## without checking the headers (because I don't know what it
## returns!)
response_to_json <- function(r) {
  txt <- httr::content(r, "text", encoding = "UTF-8")
  withCallingHandlers(
    from_json(txt),
    error = function(e) message("Original response:\n\n", txt))
}


is_json_response <- function(r) {
  type <- r$headers[["Content-Type"]]
  httr::parse_media(type)$complete == "application/json"
}


vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}


vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, logical(1), ...)
}


data_frame <- function(...) {
  data.frame(..., stringsAsFactors = FALSE)
}


squote <- function(x) {
  sprintf("'%s'", x)
}


list_to_character <- function(x, ...) {
  vcapply(x, identity, ...)
}


encode_path <- function (x) {
  gsub("[/\\\\]", ":", x)
}


clear_progress_bar <- function(p) {
  private <- environment(p$tick)$private
  if (nchar(private$last_draw) > 0) {
    str <- paste0(c("\r", rep(" ", private$width)), collapse = "")
    message(str, appendLF = FALSE)
  }
  message("\r", appendLF = FALSE)
}


trim_string <- function (s, w, elipsis = " ...") {
  if (nchar(s) > w) {
    s <- paste0(substr(s, 1L, w - nchar(elipsis)), elipsis)
  }
  s
}


format_output <- function(output) {
  paste(sprintf("%s\n", c(output$stderr, output$stdout)), collapse = "")
}
