#' Serve an MCP server over HTTP using ambiorix
#'
#' @param mcp An MCP server object
#' @param host Host to bind to, defaults to "127.0.0.1"
#' @param port Port to listen on, defaults to 3000
#' @param path Path to serve the MCP endpoint, defaults to "/mcp"
#'
#' @return Invisible, runs indefinitely
#' @export
serve_http <- function(
  mcp,
  host = "127.0.0.1",
  port = 3000,
  path = "/"
) {
  # Validate MCP object
  if (missing(mcp)) {
    stop("An MCP server object is required")
  }

  if (!inherits(mcp, "mcp_server")) {
    stop("mcp must be an MCP server object")
  }

  # Check if ambiorix is installed
  if (!requireNamespace("ambiorix", quietly = TRUE)) {
    stop(
      "The 'ambiorix' package is required for HTTP transport. Please install it with install.packages('ambiorix')"
    )
  }

  # Create a new ambiorix app
  app <- ambiorix::Ambiorix$new()

  # Add CORS headers middleware
  app$use(function(req, res) {
    res$set_header("Access-Control-Allow-Origin", "*")
    res$set_header("Access-Control-Allow-Methods", "POST, OPTIONS")
    res$set_header("Access-Control-Allow-Headers", "Content-Type")

    # Handle preflight OPTIONS requests
    if (req$method == "OPTIONS") {
      res$send("")
      return(FALSE) # Stop processing
    }

    TRUE # Continue processing
  })

  # Define MCP endpoint
  app$post(path, function(req, res) {
    # Get request body
    body <- req$body

    if (!length(body) || body == "") {
      error_response <- create_error(
        JSONRPC_INVALID_REQUEST,
        "Empty request body"
      )
      res$set_header("Content-Type", "application/json")
      res$send(to_json(error_response))
      return()
    }

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
  message("Starting MCP HTTP server at http://", host, ":", port, path)
  app$start(host = host, port = port)

  invisible()
}

