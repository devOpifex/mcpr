#' Create a new mcp IO
#'
#' @param endpoint The endpoint to connect to
#' @param command The command to run
#' @param args Arguments to pass to the command
#' @param name The name of the client
#' @param version The version of the client
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
new_client_http <- function(
  endpoint,
  name,
  version = "1.0.0"
) {
  stopifnot(is.character(endpoint), length(endpoint) == 1)
  stopifnot(!missing(name), is.character(name), length(name) == 1)

  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("The httr2 package is required to use the http client")
  }

  r <- httr2::request(endpoint) |>
    httr2::req_method("POST")

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
