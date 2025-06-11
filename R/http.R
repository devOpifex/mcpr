#' Serve an MCP server over HTTP using ambiorix
#'
#' @param mcp An MCP server object
#' @param port Port to listen on, defaults to 3000
#' @param path Path to serve the MCP endpoint, defaults to "/mcp"
#'
#' @return Invisible, runs indefinitely
#' @export
serve_http <- function(
  mcp,
  port = Sys.getenv("SHINY_PORT", 3000),
  path = "/mcp"
) {
  # Validate MCP object
  if (missing(mcp)) {
    stop("An MCP server object is required")
  }

  if (!inherits(mcp, "server")) {
    stop("mcp must be an MCP server object")
  }

  # Check if ambiorix is installed
  if (!requireNamespace("ambiorix", quietly = TRUE)) {
    stop(
      "The 'ambiorix' package is required for HTTP transport. Please install it with install.packages('ambiorix')"
    )
  }

  port <- as.integer(port)

  # Create a new ambiorix app
  app <- ambiorix::Ambiorix$new()

  # Add CORS headers middleware
  app$use(function(req, res) {
    res$set_header("Access-Control-Allow-Origin", "*")
    res$set_header("Access-Control-Allow-Methods", "POST, OPTIONS")
    res$set_header("Access-Control-Allow-Headers", "Content-Type")
  })

  # Define MCP endpoint
  app$post(path, function(req, res) {
    # Get request body
    body <- ambiorix::parse_json(req) |>
      yyjsonr::write_json_str(opts = list(auto_unbox = TRUE))

    # Process the request through JSON-RPC
    response <- tryCatch(
      parse_request(body, mcp),
      error = function(e) {
        create_error(
          JSONRPC_PARSE_ERROR,
          paste("Parse error:", e$message)
        )
      }
    )

    if (is.null(response)) {
      # For notifications, return an empty success response
      res$set_status(204) # No Content
      res$send("")
      return()
    }

    # Set content type and send response
    res$set_header("Content-Type", "application/json")

    # Handle different response types
    if (
      inherits(response, "jsonrpc_response") ||
        inherits(response, "jsonrpc_error")
    ) {
      res$send(to_json(response))
    } else {
      # It's already JSON from a batch request
      res$send(response)
    }
  })

  # Start the server
  app$start(port = port)

  invisible()
}
