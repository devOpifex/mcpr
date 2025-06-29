#' Serve an MCP server using stdin/stdout
#'
#' @param mcp An MCP server object created with new_server()
#' @type mcp object
#'
#' @return Nothing, runs indefinitely in normal mode, or the response in test mode
#' @mcp start_mcp_server_io Start an MCP server using stdin/stdout transport
#' @export
serve_io <- function(
  mcp
) {
  # Validate MCP object
  if (missing(mcp)) {
    stop("An MCP server object is required")
  }

  if (!inherits(mcp, "server")) {
    stop("mcp must be an MCP server object")
  }

  # Main request-response loop
  while (TRUE) {
    # Read a line from input connection
    line <- readLines("stdin", n = 1, warn = FALSE)

    # Exit on EOF (Ctrl+D or end of file)
    if (length(line) == 0) break

    # Process the request through JSON-RPC
    response <- parse_request(line, mcp)

    if (!length(response)) {
      next
    }

    send(response, stdout())
  }
}
