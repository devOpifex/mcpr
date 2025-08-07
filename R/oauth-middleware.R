#' OAuth Middleware for HTTP Authentication
#'
#' Middleware functions for validating OAuth Bearer tokens in HTTP requests
#' according to MCP specification requirements.
#'

#' Create OAuth authentication middleware
#'
#' @param oauth_server OAuth server configuration object
#' @param protected_paths Vector of paths that require authentication (default: all non-OAuth paths)
#' @param optional_auth Whether authentication is optional (default: FALSE)
#' @return Middleware function for ambiorix
#' @export
oauth_middleware <- function(oauth_server, protected_paths = NULL, optional_auth = FALSE) {
  if (!inherits(oauth_server, "oauth_server")) {
    stop("oauth_server must be an oauth_server object")
  }
  
  function(req, res) {
    # Skip authentication for OAuth endpoints and OPTIONS requests
    if (is_oauth_endpoint(req$path) || req$method == "OPTIONS") {
      return()
    }
    
    # Check if path requires authentication
    if (!is.null(protected_paths) && !req$path %in% protected_paths) {
      return()
    }
    
    # Extract Authorization header
    auth_header <- req$headers$authorization
    
    if (is.null(auth_header)) {
      if (optional_auth) {
        return()
      }
      
      oauth_send_auth_error(res, "invalid_request", "Missing Authorization header")
      return()
    }
    
    # Validate Bearer token format
    if (!startsWith(auth_header, "Bearer ")) {
      oauth_send_auth_error(res, "invalid_request", "Invalid Authorization header format")
      return()
    }
    
    # Extract token
    token <- trimws(substring(auth_header, 8))
    
    if (nchar(token) == 0) {
      oauth_send_auth_error(res, "invalid_token", "Empty bearer token")
      return()
    }
    
    # Validate token
    validation_result <- oauth_validate_bearer_token(oauth_server, token, req)
    
    if (!validation_result$valid) {
      oauth_send_auth_error(res, validation_result$error, validation_result$error_description)
      return()
    }
    
    # Add authentication context to request
    req$oauth <- list(
      authenticated = TRUE,
      user_id = validation_result$subject,
      client_id = validation_result$client_id,
      scopes = validation_result$scopes,
      token_id = validation_result$token_id
    )
  }
}

#' Validate OAuth bearer token
#'
#' @param oauth_server OAuth server object
#' @param token Bearer token string
#' @param req HTTP request object
#' @return List with validation results
oauth_validate_bearer_token <- function(oauth_server, token, req) {
  tryCatch({
    # Get expected audience (the MCP server URL)
    expected_audience <- get_request_audience(req)
    
    # Validate JWT token
    token_result <- oauth_validate_access_token(
      token,
      oauth_server$keys$public,
      expected_audience = expected_audience,
      expected_issuer = oauth_server$issuer
    )
    
    if (!token_result$valid) {
      return(token_result)
    }
    
    claims <- token_result$claims
    
    # Check token in storage (for revocation)
    stored_token <- oauth_get_access_token(oauth_server$tokens, claims$jti)
    
    if (is.null(stored_token)) {
      return(list(
        valid = FALSE,
        error = "invalid_token",
        error_description = "Token not found in storage"
      ))
    }
    
    if (stored_token$revoked) {
      return(list(
        valid = FALSE,
        error = "invalid_token", 
        error_description = "Token has been revoked"
      ))
    }
    
    # Check if token has expired (additional check beyond JWT exp)
    if (!is.null(stored_token$expires_at) && Sys.time() > stored_token$expires_at) {
      return(list(
        valid = FALSE,
        error = "invalid_token",
        error_description = "Token has expired"
      ))
    }
    
    # Return successful validation with additional metadata
    list(
      valid = TRUE,
      subject = claims$sub,
      client_id = stored_token$client_id,
      scopes = token_result$scopes,
      token_id = claims$jti,
      audience = claims$aud,
      expires_at = stored_token$expires_at
    )
    
  }, error = function(e) {
    list(
      valid = FALSE,
      error = "invalid_token",
      error_description = paste("Token validation error:", e$message)
    )
  })
}

#' Send OAuth authentication error response
#'
#' @param res HTTP response object
#' @param error OAuth error code
#' @param error_description Optional error description
#' @param error_uri Optional error documentation URI
oauth_send_auth_error <- function(res, error, error_description = NULL, error_uri = NULL) {
  # Set proper HTTP status code based on error type
  status_code <- switch(error,
    "invalid_request" = 400,
    "invalid_token" = 401,
    "insufficient_scope" = 403,
    401 # default to unauthorized
  )
  
  # Build WWW-Authenticate header
  auth_header <- paste0('Bearer realm="MCP Server"')
  
  if (!is.null(error)) {
    auth_header <- paste0(auth_header, ', error="', error, '"')
  }
  
  if (!is.null(error_description)) {
    auth_header <- paste0(auth_header, ', error_description="', gsub('"', '\\"', error_description), '"')
  }
  
  if (!is.null(error_uri)) {
    auth_header <- paste0(auth_header, ', error_uri="', error_uri, '"')
  }
  
  res$set_status(status_code)
  res$set_header("WWW-Authenticate", auth_header)
  res$set_header("Content-Type", "application/json")
  
  # Send JSON error response
  error_response <- oauth_error_response(error, error_description, error_uri)
  res$send(error_response)
}

#' Get request audience for token validation
#'
#' @param req HTTP request object  
#' @return Audience string
get_request_audience <- function(req) {
  # Try to determine the full URL that was requested
  scheme <- req$headers$`x-forwarded-proto` %||% "https"
  host <- req$headers$host %||% req$headers$`x-forwarded-host` %||% "localhost"
  
  paste0(scheme, "://", host)
}

#' Require specific OAuth scopes
#'
#' @param required_scopes Vector of required scope strings
#' @return Middleware function that checks for required scopes
#' @export
oauth_require_scopes <- function(required_scopes) {
  function(req, res) {
    # Check if request is authenticated
    if (is.null(req$oauth) || !req$oauth$authenticated) {
      oauth_send_auth_error(res, "invalid_token", "Authentication required")
      return()
    }
    
    # Check if user has required scopes
    user_scopes <- req$oauth$scopes %||% character(0)
    
    missing_scopes <- setdiff(required_scopes, user_scopes)
    
    if (length(missing_scopes) > 0) {
      error_description <- paste(
        "Insufficient scope. Required:", 
        paste(required_scopes, collapse = " "),
        "Missing:",
        paste(missing_scopes, collapse = " ")
      )
      
      oauth_send_auth_error(res, "insufficient_scope", error_description)
      return()
    }
  }
}

#' Create middleware for optional authentication
#'
#' @param oauth_server OAuth server object
#' @return Middleware function that sets authentication context if present
#' @export
oauth_optional_middleware <- function(oauth_server) {
  oauth_middleware(oauth_server, optional_auth = TRUE)
}

#' CORS middleware with OAuth considerations
#'
#' @param allowed_origins Vector of allowed origins (default: "*")
#' @param allow_credentials Whether to allow credentials (default: TRUE for OAuth)
#' @return CORS middleware function
#' @export
oauth_cors_middleware <- function(allowed_origins = "*", allow_credentials = TRUE) {
  function(req, res) {
    # Handle preflight requests
    if (req$method == "OPTIONS") {
      res$set_header("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
      res$set_header("Access-Control-Allow-Headers", "Content-Type, Authorization")
      res$set_header("Access-Control-Max-Age", "86400") # 24 hours
    }
    
    # Set CORS headers
    origin <- req$headers$origin
    
    if (length(allowed_origins) == 1 && allowed_origins == "*") {
      res$set_header("Access-Control-Allow-Origin", "*")
    } else if (!is.null(origin) && origin %in% allowed_origins) {
      res$set_header("Access-Control-Allow-Origin", origin)
    }
    
    if (allow_credentials) {
      res$set_header("Access-Control-Allow-Credentials", "true")
    }
  }
}

#' Create authentication context from request
#'
#' @param req HTTP request object
#' @return Authentication context list or NULL
#' @export
oauth_get_auth_context <- function(req) {
  req$oauth
}

#' Check if request is authenticated
#'
#' @param req HTTP request object
#' @return Logical indicating if request has valid authentication
#' @export
oauth_is_authenticated <- function(req) {
  !is.null(req$oauth) && req$oauth$authenticated
}

#' Get authenticated user ID from request
#'
#' @param req HTTP request object
#' @return User ID string or NULL if not authenticated
#' @export
oauth_get_user_id <- function(req) {
  if (oauth_is_authenticated(req)) {
    req$oauth$user_id
  } else {
    NULL
  }
}

#' Get user scopes from authenticated request
#'
#' @param req HTTP request object
#' @return Character vector of scopes or NULL
#' @export
oauth_get_user_scopes <- function(req) {
  if (oauth_is_authenticated(req)) {
    req$oauth$scopes
  } else {
    NULL
  }
}

#' Check if user has specific scope
#'
#' @param req HTTP request object
#' @param scope Scope to check for
#' @return Logical indicating if user has the scope
#' @export
oauth_has_scope <- function(req, scope) {
  user_scopes <- oauth_get_user_scopes(req)
  !is.null(user_scopes) && scope %in% user_scopes
}

# Null-coalescing operator helper
`%||%` <- function(x, y) if (is.null(x)) y else x