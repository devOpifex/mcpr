#' Create a new mcp IO
#'
#' @param endpoint The endpoint to connect to
#' @param command The command to run
#' @param args Arguments to pass to the command
#' @param name The name of the client
#' @param version The version of the client
#' @param headers A named list (or named character vector) of HTTP headers
#'   to send with every request, e.g. `list(Authorization = "Bearer <token>")`.
#'
#' @return A new mcp client
#' @export
#' @name client
new_client_io <- function(
  command,
  args = character(),
  name,
  version = "1.0.0"
) {
  stopifnot(is.character(command), length(command) == 1)
  stopifnot(!missing(name), is.character(name), length(name) == 1)

  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("The processx package is required to use the io client")
  }

  p <- processx::process$new(
    command = command,
    args = args,
    stdin = "|",
    stdout = "|"
  )

  new_client(
    p,
    name = name,
    version = version,
    type = "io"
  )
}

#' @rdname client
#' @export
new_client_http <- function(
  endpoint,
  name,
  version = "1.0.0",
  headers = list()
) {
  stopifnot(is.character(endpoint), length(endpoint) == 1)
  stopifnot(!missing(name), is.character(name), length(name) == 1)
  stopifnot(is.list(headers) || is.character(headers))

  if (
    length(headers) && (is.null(names(headers)) || any(names(headers) == ""))
  ) {
    stop("`headers` must be a named list")
  }

  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("The httr2 package is required to use the http client")
  }

  # Streamable HTTP servers reject requests (406) that don't accept both
  r <- httr2::request(endpoint) |>
    httr2::req_method("POST") |>
    httr2::req_headers(Accept = "application/json, text/event-stream")

  if (length(headers)) {
    r <- do.call(
      httr2::req_headers,
      c(list(r), as.list(headers), list(.redact = "Authorization"))
    )
  }

  new_client(
    r,
    name = name,
    version = version,
    type = "http"
  )
}

new_client <- function(obj, name, version, type = c("io", "http")) {
  type <- match.arg(type)
  cls <- sprintf("client_%s", type)

  structure(
    obj,
    name = name,
    version = version,
    # mutable state set during the session, e.g.: session id, protocol version
    state = new.env(parent = emptyenv()),
    class = c(cls, "client", class(obj))
  )
}

#' Get the name of a client
#'
#' @param x A client object
#'
#' @return The name of the client
#' @export
get_name <- function(x) UseMethod("get_name")

#' @export
get_name.client <- function(x) {
  attr(x, "name")
}

#' @export
get_name.server <- function(x) {
  attr(x, "name")
}
