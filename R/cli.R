#' Run the MCP Server CLI
#'
#' @param mcp An MCP server object
#' @param port Port number to listen on (optional)
#'
#' @return Nothing, runs indefinitely
#' @export
run <- function(mcp, port = NULL) {
  # Validate MCP object
  if (missing(mcp)) {
    stop("An MCP server object is required")
  }

  if (!inherits(mcp, "mcp_server")) {
    stop("mcp must be an MCP server object")
  }

  # Handle port parameter
  if (!is.null(port)) {
    # TODO: Implement server socket in future versions
    stop("Port-based server not yet implemented")
  }

  # Main request-response loop
  while (TRUE) {
    # Read a line from stdin
    line <- readLines(con = stdin(), n = 1, warn = FALSE)

    # Exit on EOF (Ctrl+D)
    if (length(line) == 0) break

    # Process the request through JSON-RPC
    response <- parse_request(line, mcp)

    # Only print responses for non-notifications
    if (!is.null(response)) {
      cat(response, "\n")
    }
  }
}
