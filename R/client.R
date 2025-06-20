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
    version = "1.0.0") {
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

#' Create a new MCP HTTP client
#'
#' Creates an MCP client that connects to an HTTP endpoint using httr2.
#' This client can be used to communicate with MCP servers exposed via HTTP.
#'
#' @param endpoint The HTTP endpoint URL of the MCP server to connect to
#' @param name The name of the client, used for identification
#' @param version The version of the client (defaults to "1.0.0")
#'
#' @return A new MCP HTTP client object that can communicate
#'          with the MCP server at the specified endpoint
#' @export
#' @rdname client
new_client_http <- function(
    endpoint,
    name,
    version = "1.0.0") {
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

#' Create a new MCP client
#'
#' @param obj The underlying object for communication (process or request)
#' @param name The name of the client
#' @param version The version of the client
#' @param type The type of client to create ("io" or "http")
#'
#' @return A new mcp client object
#' @rdname client
#' @export
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
