#' OAuth 2.1 Implementation for MCP
#'
#' Main OAuth 2.1 implementation providing complete authorization server
#' functionality for Model Context Protocol compliance.
#'

#' Setup OAuth endpoints in ambiorix application
#'
#' @param app Ambiorix application object
#' @param oauth_server OAuth server configuration
#' @export
setup_oauth_endpoints <- function(app, oauth_server) {
  if (!inherits(oauth_server, "oauth_server")) {
    stop("oauth_server must be an oauth_server object")
  }
  
  # Authorization Server Metadata (RFC 8414)
  app$get("/.well-known/oauth-authorization-server", oauth_metadata_endpoint(oauth_server))
  
  # Protected Resource Metadata (draft-ietf-oauth-resource-metadata)
  app$get("/.well-known/oauth-protected-resource", oauth_resource_metadata_endpoint(oauth_server))
  
  # JSON Web Key Set (JWKS)
  app$get("/.well-known/jwks.json", oauth_jwks_endpoint(oauth_server))
  
  # Dynamic Client Registration (RFC 7591) - if enabled
  if (oauth_server$config$allow_dynamic_registration) {
    app$post("/oauth/register", oauth_register_client_endpoint(oauth_server))
  }
  
  # Authorization Endpoint
  app$get("/oauth/authorize", oauth_authorization_endpoint(oauth_server))
  
  # Token Endpoint
  app$post("/oauth/token", oauth_token_endpoint(oauth_server))
  
  # Token Introspection Endpoint (RFC 7662) - optional
  app$post("/oauth/introspect", oauth_introspection_endpoint(oauth_server))
  
  # Token Revocation Endpoint (RFC 7009) - optional
  app$post("/oauth/revoke", oauth_revocation_endpoint(oauth_server))
  
  invisible(app)
}

#' OAuth token introspection endpoint (RFC 7662)
#'
#' @param oauth_server OAuth server object
#' @return Function for handling introspection requests
oauth_introspection_endpoint <- function(oauth_server) {
  function(req, res) {
    tryCatch({
      # Parse form data
      body <- parse_form_data(req$body)
      
      if (is.null(body$token)) {
        res$set_status(400)
        res$send(oauth_error_response("invalid_request", "Missing token parameter"))
        return()
      }
      
      # Validate the token
      validation_result <- oauth_validate_bearer_token(oauth_server, body$token, req)
      
      if (!validation_result$valid) {
        # Return inactive token response
        response <- list(active = FALSE)
      } else {
        # Return active token info
        response <- list(
          active = TRUE,
          scope = paste(validation_result$scopes, collapse = " "),
          client_id = validation_result$client_id,
          username = validation_result$subject,
          sub = validation_result$subject,
          aud = validation_result$audience,
          iss = oauth_server$issuer,
          exp = as.numeric(validation_result$expires_at),
          iat = as.numeric(Sys.time())
        )
      }
      
      res$set_header("Content-Type", "application/json")
      res$send(response)
      
    }, error = function(e) {
      res$set_status(500)
      res$send(oauth_error_response("server_error", "Introspection failed"))
    })
  }
}

#' OAuth token revocation endpoint (RFC 7009)
#'
#' @param oauth_server OAuth server object  
#' @return Function for handling revocation requests
oauth_revocation_endpoint <- function(oauth_server) {
  function(req, res) {
    tryCatch({
      # Parse form data
      body <- parse_form_data(req$body)
      
      if (is.null(body$token)) {
        res$set_status(400)
        res$send(oauth_error_response("invalid_request", "Missing token parameter"))
        return()
      }
      
      # Try to decode token to get token ID
      tryCatch({
        claims <- jose::jwt_decode_sig(body$token, oauth_server$keys$public)
        token_id <- claims$jti
        
        if (!is.null(token_id)) {
          oauth_revoke_access_token(oauth_server$tokens, token_id)
        }
      }, error = function(e) {
        # Token might be invalid, but that's OK for revocation
      })
      
      # Always return success (RFC 7009 section 2.2)
      res$set_status(200)
      res$send("")
      
    }, error = function(e) {
      res$set_status(500)
      res$send(oauth_error_response("server_error", "Revocation failed"))
    })
  }
}

#' Create a complete OAuth-enabled MCP server
#'
#' @param mcp MCP server object
#' @param oauth_config OAuth server configuration
#' @param require_auth Whether to require authentication for all MCP endpoints
#' @param protected_scopes Required scopes for MCP operations
#' @return Enhanced MCP server with OAuth
#' @export
create_oauth_mcp_server <- function(
  mcp,
  oauth_config,
  require_auth = TRUE,
  protected_scopes = c("mcp:tools", "mcp:resources", "mcp:prompts")
) {
  if (!inherits(mcp, "server")) {
    stop("mcp must be a server object")
  }
  
  if (!inherits(oauth_config, "oauth_server")) {
    stop("oauth_config must be an oauth_server object")
  }
  
  structure(list(
    mcp = mcp,
    oauth = oauth_config,
    require_auth = require_auth,
    protected_scopes = protected_scopes
  ), class = c("oauth_mcp_server", "list"))
}

#' Start OAuth-enabled MCP server via HTTP
#'
#' @param oauth_mcp_server OAuth MCP server object
#' @param port HTTP port (default: 8080)
#' @param host Host to bind to (default: "localhost")
#' @param mcp_path Path for MCP endpoint (default: "/mcp")
#' @return Invisible, starts server
#' @export
serve_oauth_mcp_http <- function(
  oauth_mcp_server,
  port = 8080,
  host = "localhost", 
  mcp_path = "/mcp"
) {
  if (!inherits(oauth_mcp_server, "oauth_mcp_server")) {
    stop("oauth_mcp_server must be an oauth_mcp_server object")
  }
  
  if (!requireNamespace("ambiorix", quietly = TRUE)) {
    stop("The 'ambiorix' package is required for HTTP transport")
  }
  
  # Create ambiorix app
  app <- ambiorix::Ambiorix$new()
  
  # Setup CORS middleware
  app$use(oauth_cors_middleware())
  
  # Setup OAuth endpoints
  setup_oauth_endpoints(app, oauth_mcp_server$oauth)
  
  # Setup OAuth middleware for MCP endpoints
  if (oauth_mcp_server$require_auth) {
    app$use(mcp_path, oauth_middleware(oauth_mcp_server$oauth))
    
    if (length(oauth_mcp_server$protected_scopes) > 0) {
      app$use(mcp_path, oauth_require_scopes(oauth_mcp_server$protected_scopes))
    }
  }
  
  # MCP endpoint
  app$post(mcp_path, function(req, res) {
    # Add authentication context to MCP processing
    if (!is.null(req$oauth)) {
      # Could enhance MCP processing with user context here
    }
    
    # Process MCP request normally
    body <- yyjsonr::read_json_str(req$body) |>
      yyjsonr::write_json_str(opts = list(auto_unbox = TRUE))
    
    response <- tryCatch(
      parse_request(body, oauth_mcp_server$mcp),
      error = function(e) {
        create_error(
          JSONRPC_PARSE_ERROR,
          paste("Parse error:", e$message)
        )
      }
    )
    
    if (is.null(response)) {
      res$set_status(204)
      res$send("")
      return()
    }
    
    res$set_header("Content-Type", "application/json")
    
    if (inherits(response, "jsonrpc_response") || inherits(response, "jsonrpc_error")) {
      res$send(to_json(response))
    } else {
      res$send(response)
    }
  })
  
  # Health check endpoint
  app$get("/health", function(req, res) {
    res$send(list(
      status = "healthy",
      timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
      oauth_enabled = TRUE
    ))
  })
  
  # Start server
  cat("Starting OAuth-enabled MCP server on", paste0(host, ":", port), "\n")
  cat("MCP endpoint:", paste0("http://", host, ":", port, mcp_path), "\n")
  cat("OAuth metadata:", paste0("http://", host, ":", port, "/.well-known/oauth-authorization-server"), "\n")
  
  app$start(port = port, host = host)
  invisible()
}

#' Generate OAuth client for testing
#'
#' @param oauth_server OAuth server object
#' @param client_name Name for the test client
#' @param redirect_uri Redirect URI for the client
#' @return List with client credentials
#' @export
oauth_create_test_client <- function(
  oauth_server,
  client_name = "Test MCP Client",
  redirect_uri = "http://localhost:3000/callback"
) {
  client_id <- paste0("test_", generate_id())
  client_secret <- oauth_generate_client_secret()
  
  client_info <- list(
    client_id = client_id,
    client_secret = client_secret,
    client_name = client_name,
    redirect_uris = c(redirect_uri),
    grant_types = c("authorization_code"),
    response_types = c("code"),
    token_endpoint_auth_method = "client_secret_basic",
    scope = paste(oauth_server$config$supported_scopes, collapse = " ")
  )
  
  oauth_store_client(oauth_server$clients, client_id, client_info)
  
  list(
    client_id = client_id,
    client_secret = client_secret,
    client_name = client_name,
    authorization_url = paste0(oauth_server$issuer, "/oauth/authorize"),
    token_url = paste0(oauth_server$issuer, "/oauth/token"),
    redirect_uri = redirect_uri,
    scope = client_info$scope
  )
}

#' Generate OAuth authorization URL with PKCE
#'
#' @param oauth_server OAuth server object
#' @param client_id OAuth client ID
#' @param redirect_uri Redirect URI
#' @param scope Requested scopes
#' @param state Optional state parameter
#' @return List with authorization URL and PKCE parameters
#' @export
oauth_generate_auth_url <- function(
  oauth_server,
  client_id,
  redirect_uri,
  scope = NULL,
  state = NULL
) {
  # Generate PKCE parameters
  pkce <- oauth_generate_pkce()
  
  # Build authorization URL
  params <- list(
    response_type = "code",
    client_id = client_id,
    redirect_uri = redirect_uri,
    code_challenge = pkce$code_challenge,
    code_challenge_method = pkce$code_challenge_method
  )
  
  if (!is.null(scope)) {
    params$scope <- scope
  }
  
  if (!is.null(state)) {
    params$state <- state
  }
  
  query_string <- paste(
    mapply(function(k, v) paste0(k, "=", utils::URLencode(as.character(v))), 
           names(params), params),
    collapse = "&"
  )
  
  auth_url <- paste0(oauth_server$metadata$authorization_endpoint, "?", query_string)
  
  list(
    authorization_url = auth_url,
    code_verifier = pkce$code_verifier,
    code_challenge = pkce$code_challenge,
    state = state
  )
}

#' Exchange authorization code for access token
#'
#' @param oauth_server OAuth server object
#' @param client_id OAuth client ID  
#' @param client_secret OAuth client secret
#' @param code Authorization code
#' @param redirect_uri Redirect URI
#' @param code_verifier PKCE code verifier
#' @return List with token response
#' @export
oauth_exchange_code <- function(
  oauth_server,
  client_id,
  client_secret = NULL,
  code,
  redirect_uri,
  code_verifier
) {
  # This would typically be done via HTTP request to token endpoint
  # For testing/internal use, we can call the endpoint handler directly
  
  body_params <- list(
    grant_type = "authorization_code",
    code = code,
    redirect_uri = redirect_uri,
    client_id = client_id,
    code_verifier = code_verifier
  )
  
  if (!is.null(client_secret)) {
    body_params$client_secret <- client_secret
  }
  
  # Create mock request object
  mock_req <- list(
    body = paste(
      mapply(function(k, v) paste0(k, "=", utils::URLencode(as.character(v))), 
             names(body_params), body_params),
      collapse = "&"
    ),
    headers = list(
      host = gsub("https?://", "", oauth_server$issuer)
    )
  )
  
  # Create mock response object
  mock_res <- list(
    status = 200,
    headers = list(),
    body = NULL,
    set_status = function(status) mock_res$status <<- status,
    set_header = function(name, value) mock_res$headers[[name]] <<- value,
    send = function(data) mock_res$body <<- data
  )
  
  # Call token endpoint
  token_handler <- oauth_token_endpoint(oauth_server)
  token_handler(mock_req, mock_res)
  
  if (mock_res$status == 200) {
    mock_res$body
  } else {
    stop("Token exchange failed: ", mock_res$body$error_description %||% mock_res$body$error)
  }
}

#' Print method for OAuth MCP server
#'
#' @param x OAuth MCP server object
#' @param ... Additional arguments
#' @export
print.oauth_mcp_server <- function(x, ...) {
  cat("OAuth-enabled MCP Server\n")
  cat("MCP Server:", attr(x$mcp, "name"), "\n")
  cat("OAuth Issuer:", x$oauth$issuer, "\n")
  cat("Authentication required:", x$require_auth, "\n")
  cat("Protected scopes:", paste(x$protected_scopes, collapse = ", "), "\n")
  invisible(x)
}