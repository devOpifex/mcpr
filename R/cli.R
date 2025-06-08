#' Serve an MCP server using stdin/stdout
#'
#' @param mcp An MCP server object
#'
#' @return Nothing, runs indefinitely
#' @export
serve_std_io <- function(mcp) {
  # Validate MCP object
  if (missing(mcp)) {
    stop("An MCP server object is required")
  }

  if (!inherits(mcp, "mcp_server")) {
    stop("mcp must be an MCP server object")
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
