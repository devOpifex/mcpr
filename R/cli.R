#' Serve an MCP server using stdin/stdout
#'
#' @param mcp An MCP server object
#' @param input_con Connection to read from (defaults to stdin())
#' @param output_con Connection to write to (defaults to stdout())
#'
#' @return Nothing, runs indefinitely in normal mode, or the response in test mode
#' @export
serve_std_io <- function(
  mcp,
  input_con = stdin(),
  output_con = stdout()
) {
  # Validate MCP object
  if (missing(mcp)) {
    stop("An MCP server object is required")
  }

  if (!inherits(mcp, "mcp_server")) {
    stop("mcp must be an MCP server object")
  }

  # Main request-response loop
  while (TRUE) {
    # Read a line from input connection
    line <- readLines(con = input_con, n = 1, warn = FALSE)

    # Exit on EOF (Ctrl+D or end of file)
    if (length(line) == 0) break

    # Process the request through JSON-RPC
    response <- parse_request(line, mcp)

    if (is.null(response)) {
      # If it's a notification, just continue
      next
    }

    send(response, output_con)
  }
}
