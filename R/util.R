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


data_frame <- function(...) {
  data.frame(..., stringsAsFactors = FALSE)
}


squote <- function(x) {
  sprintf("'%s'", x)
}
