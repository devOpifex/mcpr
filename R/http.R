#' Serve an MCP server over HTTP using ambiorix
#'
#' @param mcp An MCP server object
#' @param port Port to listen on, defaults to 3000
#' @param path Path to serve the MCP endpoint, defaults to "/mcp"
#' @param oauth_config Optional OAuth server configuration for authentication
#' @param require_auth Whether to require OAuth authentication (default: FALSE for backward compatibility)
#' @param protected_scopes Required OAuth scopes for MCP operations
#'
#' @return Invisible, runs indefinitely
#' @export
serve_http <- function(
  mcp,
  port = Sys.getenv("SHINY_PORT", 3000),
  path = "/mcp",
  oauth_config = NULL,
  require_auth = FALSE,
  protected_scopes = c("mcp:tools", "mcp:resources", "mcp:prompts")
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

  # Setup OAuth endpoints if OAuth is configured
  if (!is.null(oauth_config)) {
    if (!inherits(oauth_config, "oauth_server")) {
      stop("oauth_config must be an oauth_server object")
    }
    
    setup_oauth_endpoints(app, oauth_config)
    cat("OAuth endpoints enabled at", paste0("http://localhost:", port), "\n")
    cat("Authorization Server Metadata:", paste0("http://localhost:", port, "/.well-known/oauth-authorization-server"), "\n")
  }

  # Add CORS headers middleware (OAuth-aware)
  if (!is.null(oauth_config)) {
    app$use(oauth_cors_middleware())
  } else {
    # Traditional CORS for non-OAuth mode
    app$use(function(req, res) {
      res$set_header("Access-Control-Allow-Origin", "*")
      res$set_header("Access-Control-Allow-Methods", "POST, OPTIONS")
      res$set_header("Access-Control-Allow-Headers", "Content-Type")
    })
  }

  # Setup OAuth middleware for MCP endpoint if authentication is required
  if (!is.null(oauth_config)) {
    if (require_auth) {
      app$use(path, oauth_middleware(oauth_config))
      
      if (length(protected_scopes) > 0) {
        app$use(path, oauth_require_scopes(protected_scopes))
      }
      
      cat("OAuth authentication required for MCP endpoint\n")
      cat("Required scopes:", paste(protected_scopes, collapse = ", "), "\n")
    } else {
      # Optional authentication
      app$use(path, oauth_optional_middleware(oauth_config))
      cat("OAuth authentication optional for MCP endpoint\n")
    }
  }

  # Define MCP endpoint
  app$post(path, function(req, res) {
    # Log authentication context if available
    if (!is.null(oauth_config) && oauth_is_authenticated(req)) {
      user_id <- oauth_get_user_id(req)
      scopes <- oauth_get_user_scopes(req)
      cat("Authenticated request from user:", user_id, "with scopes:", paste(scopes, collapse = ", "), "\n")
    }
    
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

  # Health check endpoint
  app$get("/health", function(req, res) {
    health_info <- list(
      status = "healthy",
      timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
      mcp_server = attr(mcp, "name"),
      oauth_enabled = !is.null(oauth_config),
      authentication_required = require_auth
    )
    
    if (!is.null(oauth_config)) {
      health_info$oauth_issuer <- oauth_config$issuer
      health_info$client_count <- oauth_config$clients$size()
      health_info$token_count <- oauth_config$tokens$size()
    }
    
    res$set_header("Content-Type", "application/json")
    res$send(health_info)
  })

  # Start the server
  cat("Starting MCP server on port", port, "\n")
  cat("MCP endpoint:", paste0("http://localhost:", port, path), "\n")
  
  if (!is.null(oauth_config)) {
    cat("OAuth 2.1 enabled with issuer:", oauth_config$issuer, "\n")
    cat("Well-known endpoints:\n")
    cat("  - Authorization Server Metadata:", paste0("http://localhost:", port, "/.well-known/oauth-authorization-server"), "\n")
    cat("  - JWKS:", paste0("http://localhost:", port, "/.well-known/jwks.json"), "\n")
    if (oauth_config$config$allow_dynamic_registration) {
      cat("  - Client Registration:", paste0("http://localhost:", port, "/oauth/register"), "\n")
    }
  }
  
  cat("Health check:", paste0("http://localhost:", port, "/health"), "\n")
  cat("Server ready!\n")
  
  app$start(port = port)

  invisible()
}
