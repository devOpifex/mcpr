#' OAuth 2.1 Server Configuration
#'
#' OAuth server configuration and metadata endpoints for MCP compliance.
#' Implements OAuth 2.1 authorization server with MCP-specific requirements.
#'

#' Create OAuth 2.1 server configuration
#'
#' @param issuer_url Base URL of the OAuth server
#' @param client_storage Storage for OAuth clients (default: memory)
#' @param token_storage Storage for tokens and codes (default: memory)
#' @param key_pair OAuth server key pair for JWT signing
#' @param config Additional server configuration
#' @return OAuth server object
#' @export
new_oauth_server <- function(
  issuer_url,
  client_storage = oauth_memory_storage(),
  token_storage = oauth_memory_storage(),
  key_pair = oauth_generate_keypair(),
  config = list()
) {
  # Validate required parameters
  if (missing(issuer_url) || !is.character(issuer_url)) {
    stop("issuer_url must be provided and be a character string")
  }

  # Normalize issuer URL (remove trailing slash)
  issuer_url <- gsub("/$", "", issuer_url)

  # Default configuration
  default_config <- list(
    authorization_code_expires_in = 600, # 10 minutes
    access_token_expires_in = 3600, # 1 hour
    require_pkce = TRUE,
    allow_dynamic_registration = TRUE,
    supported_scopes = c("mcp:tools", "mcp:resources", "mcp:prompts"),
    rate_limit = list(
      enabled = TRUE,
      requests_per_minute = 100
    )
  )

  # Merge with user config
  config <- utils::modifyList(default_config, config)

  # Create server metadata according to RFC 8414
  metadata <- list(
    issuer = issuer_url,
    authorization_endpoint = paste0(issuer_url, "/oauth/authorize"),
    token_endpoint = paste0(issuer_url, "/oauth/token"),
    registration_endpoint = if (config$allow_dynamic_registration) {
      paste0(issuer_url, "/oauth/register")
    } else {
      NULL
    },
    jwks_uri = paste0(issuer_url, "/.well-known/jwks.json"),

    # Supported features
    response_types_supported = c("code"),
    grant_types_supported = c("authorization_code"),
    code_challenge_methods_supported = c("S256"),
    token_endpoint_auth_methods_supported = c("none", "client_secret_basic"),

    # Scopes and claims
    scopes_supported = config$supported_scopes,

    # Additional OAuth 2.1 features
    require_request_uri_registration = FALSE,
    require_signed_request_object = FALSE
  )

  # Protected resource metadata (draft-ietf-oauth-resource-metadata)
  resource_metadata <- list(
    resource = issuer_url,
    authorization_servers = list(issuer_url),
    jwks_uri = paste0(issuer_url, "/.well-known/jwks.json"),
    bearer_methods_supported = c("header"),
    resource_documentation = "https://modelcontextprotocol.io"
  )

  structure(
    list(
      issuer = issuer_url,
      clients = client_storage,
      tokens = token_storage,
      keys = key_pair,
      config = config,
      metadata = metadata,
      resource_metadata = resource_metadata,

      # Rate limiting state
      rate_limits = new.env(parent = emptyenv())
    ),
    class = "oauth_server"
  )
}

#' Get OAuth server metadata endpoint handler
#'
#' @param oauth_server OAuth server object
#' @return Function for handling metadata requests
#' @export
oauth_metadata_endpoint <- function(oauth_server) {
  function(req, res) {
    res$set_header("Content-Type", "application/json")
    res$send(oauth_server$metadata)
  }
}

#' Get OAuth protected resource metadata endpoint handler
#'
#' @param oauth_server OAuth server object
#' @return Function for handling resource metadata requests
#' @export
oauth_resource_metadata_endpoint <- function(oauth_server) {
  function(req, res) {
    res$set_header("Content-Type", "application/json")
    res$send(oauth_server$resource_metadata)
  }
}

#' Get JWKS endpoint handler
#'
#' @param oauth_server OAuth server object
#' @return Function for handling JWKS requests
#' @export
oauth_jwks_endpoint <- function(oauth_server) {
  function(req, res) {
    res$set_header("Content-Type", "application/json")
    res$send(oauth_server$keys$public_jwks)
  }
}

#' Dynamic client registration endpoint handler (RFC 7591)
#'
#' @param oauth_server OAuth server object
#' @return Function for handling client registration
#' @export
oauth_register_client_endpoint <- function(oauth_server) {
  function(req, res) {
    if (!oauth_server$config$allow_dynamic_registration) {
      res$set_status(403)
      res$send(oauth_error_response(
        "access_denied",
        "Dynamic registration is disabled"
      ))
      return()
    }

    # Rate limiting
    if (!check_rate_limit(oauth_server, "register", req)) {
      res$set_status(429)
      res$send(oauth_error_response(
        "rate_limit_exceeded",
        "Too many registration requests"
      ))
      return()
    }

    tryCatch(
      {
        # Parse registration request
        if (is.null(req$body) || length(req$body) == 0) {
          res$set_status(400)
          res$send(oauth_error_response(
            "invalid_request",
            "Missing request body"
          ))
          return()
        }

        body <- yyjsonr::read_json_str(req$body)

        # Validate required fields for MCP
        if (is.null(body$client_name)) {
          res$set_status(400)
          res$send(oauth_error_response(
            "invalid_request",
            "client_name is required"
          ))
          return()
        }

        # Generate client credentials
        client_id <- paste0("mcp_", generate_id())
        client_secret <- if (body$token_endpoint_auth_method == "none") {
          NULL
        } else {
          oauth_generate_client_secret()
        }

        # Create client record
        client_info <- list(
          client_id = client_id,
          client_secret = client_secret,
          client_name = body$client_name,
          redirect_uris = body$redirect_uris,
          grant_types = body$grant_types %||% c("authorization_code"),
          response_types = body$response_types %||% c("code"),
          token_endpoint_auth_method = body$token_endpoint_auth_method %||%
            "client_secret_basic",
          scope = body$scope %||%
            paste(oauth_server$config$supported_scopes, collapse = " ")
        )

        # Store client
        oauth_store_client(oauth_server$clients, client_id, client_info)

        # Return client credentials
        response <- list(
          client_id = client_id,
          client_name = client_info$client_name,
          grant_types = client_info$grant_types,
          response_types = client_info$response_types,
          token_endpoint_auth_method = client_info$token_endpoint_auth_method
        )

        if (!is.null(client_secret)) {
          response$client_secret <- client_secret
        }

        res$set_status(201)
        res$set_header("Content-Type", "application/json")
        res$send(response)
      },
      error = function(e) {
        res$set_status(500)
        res$send(oauth_error_response("server_error", "Registration failed"))
      }
    )
  }
}

#' Authorization endpoint handler
#'
#' @param oauth_server OAuth server object
#' @return Function for handling authorization requests
#' @export
oauth_authorization_endpoint <- function(oauth_server) {
  function(req, res) {
    # Rate limiting
    if (!check_rate_limit(oauth_server, "authorize", req)) {
      res$set_status(429)
      res$send("Too many requests")
      return()
    }

    # Parse query parameters
    query <- req$query

    # Validate required parameters
    if (is.null(query$response_type) || query$response_type != "code") {
      redirect_error(
        res,
        query$redirect_uri,
        "unsupported_response_type",
        query$state
      )
      return()
    }

    if (is.null(query$client_id)) {
      redirect_error(
        res,
        query$redirect_uri,
        "invalid_request",
        query$state,
        "Missing client_id"
      )
      return()
    }

    # Validate client
    client <- oauth_get_client(oauth_server$clients, query$client_id)
    if (is.null(client)) {
      redirect_error(res, query$redirect_uri, "invalid_client", query$state)
      return()
    }

    # Validate redirect URI
    if (
      is.null(query$redirect_uri) ||
        !query$redirect_uri %in% client$redirect_uris
    ) {
      res$set_status(400)
      res$send("Invalid redirect URI")
      return()
    }

    # Validate PKCE (required in OAuth 2.1)
    if (
      oauth_server$config$require_pkce &&
        (is.null(query$code_challenge) || is.null(query$code_challenge_method))
    ) {
      redirect_error(
        res,
        query$redirect_uri,
        "invalid_request",
        query$state,
        "PKCE is required"
      )
      return()
    }

    # For simplicity, auto-approve for MCP (in production, show consent screen)
    # Generate authorization code
    auth_code <- oauth_generate_authorization_code()

    # Store authorization code
    code_info <- list(
      client_id = query$client_id,
      redirect_uri = query$redirect_uri,
      scope = query$scope %||% client$scope,
      code_challenge = query$code_challenge,
      code_challenge_method = query$code_challenge_method,
      user_id = "mcp_user" # In production, get from authenticated session
    )

    oauth_store_authorization_code(
      oauth_server$tokens,
      auth_code,
      code_info,
      oauth_server$config$authorization_code_expires_in
    )

    # Redirect back with authorization code
    redirect_url <- build_redirect_url(
      query$redirect_uri,
      list(
        code = auth_code,
        state = query$state
      )
    )

    res$redirect(redirect_url)
  }
}

#' Token endpoint handler
#'
#' @param oauth_server OAuth server object
#' @return Function for handling token requests
#' @export
oauth_token_endpoint <- function(oauth_server) {
  function(req, res) {
    # Rate limiting
    if (!check_rate_limit(oauth_server, "token", req)) {
      res$set_status(429)
      res$send(oauth_error_response("rate_limit_exceeded"))
      return()
    }

    tryCatch(
      {
        # Parse request body
        body <- parse_form_data(req$body)

        if (body$grant_type != "authorization_code") {
          res$set_status(400)
          res$send(oauth_error_response("unsupported_grant_type"))
          return()
        }

        # Validate client
        client <- oauth_get_client(oauth_server$clients, body$client_id)
        if (is.null(client)) {
          res$set_status(401)
          res$send(oauth_error_response("invalid_client"))
          return()
        }

        # Consume authorization code
        code_data <- oauth_consume_authorization_code(
          oauth_server$tokens,
          body$code
        )
        if (is.null(code_data)) {
          res$set_status(400)
          res$send(oauth_error_response(
            "invalid_grant",
            "Invalid or expired authorization code"
          ))
          return()
        }

        # Validate PKCE
        if (!is.null(code_data$code_challenge)) {
          if (
            is.null(body$code_verifier) ||
              !oauth_verify_pkce(body$code_verifier, code_data$code_challenge)
          ) {
            res$set_status(400)
            res$send(oauth_error_response(
              "invalid_grant",
              "PKCE verification failed"
            ))
            return()
          }
        }

        # Generate access token
        access_token <- oauth_create_access_token(
          subject = code_data$user_id,
          audience = req$headers$host %||% oauth_server$issuer,
          issuer = oauth_server$issuer,
          scopes = strsplit(code_data$scope, " ")[[1]],
          expires_in = oauth_server$config$access_token_expires_in,
          private_key = oauth_server$keys$private,
          key_id = oauth_server$keys$key_id
        )

        # Extract token ID for storage
        token_claims <- jose::jwt_decode_sig(
          access_token,
          oauth_server$keys$public
        )

        # Store token metadata
        oauth_store_access_token(
          oauth_server$tokens,
          token_claims$jti,
          list(
            client_id = code_data$client_id,
            user_id = code_data$user_id,
            scope = code_data$scope,
            audience = token_claims$aud,
            expires_in = oauth_server$config$access_token_expires_in
          )
        )

        # Return token response
        response <- list(
          access_token = access_token,
          token_type = "Bearer",
          expires_in = oauth_server$config$access_token_expires_in,
          scope = code_data$scope
        )

        res$set_header("Content-Type", "application/json")
        res$send(response)
      },
      error = function(e) {
        res$set_status(500)
        res$send(oauth_error_response(
          "server_error",
          paste("Token generation failed:", e$message)
        ))
      }
    )
  }
}

# Helper functions

oauth_generate_client_secret <- function() {
  if (!requireNamespace("openssl", quietly = TRUE)) {
    paste0(sample(c(letters, LETTERS, 0:9), 32, replace = TRUE), collapse = "")
  } else {
    openssl::base64_encode(openssl::rand_bytes(32))
  }
}

check_rate_limit <- function(oauth_server, endpoint, req) {
  if (!oauth_server$config$rate_limit$enabled) {
    return(TRUE)
  }

  # Simple IP-based rate limiting
  ip <- req$remote_addr %||% "unknown"
  key <- paste0(endpoint, ":", ip)
  now <- Sys.time()

  # Get current window data
  window_data <- oauth_server$rate_limits[[key]]
  if (is.null(window_data)) {
    window_data <- list(count = 0, window_start = now)
  }

  # Reset if window expired (1 minute)
  if (now - window_data$window_start > 60) {
    window_data <- list(count = 1, window_start = now)
  } else {
    window_data$count <- window_data$count + 1
  }

  # Store updated data
  oauth_server$rate_limits[[key]] <- window_data

  # Check limit
  window_data$count <= oauth_server$config$rate_limit$requests_per_minute
}

redirect_error <- function(
  res,
  redirect_uri,
  error,
  state = NULL,
  error_description = NULL
) {
  if (is.null(redirect_uri)) {
    res$set_status(400)
    res$send(paste("Error:", error))
    return()
  }

  error_params <- list(error = error)
  if (!is.null(state)) {
    error_params$state <- state
  }
  if (!is.null(error_description)) {
    error_params$error_description <- error_description
  }

  redirect_url <- build_redirect_url(redirect_uri, error_params)
  res$redirect(redirect_url)
}

build_redirect_url <- function(base_uri, params) {
  if (length(params) == 0) {
    return(base_uri)
  }

  query_string <- paste(
    mapply(
      function(k, v) paste0(k, "=", utils::URLencode(as.character(v))),
      names(params),
      params
    ),
    collapse = "&"
  )

  separator <- if (grepl("\\?", base_uri)) "&" else "?"
  paste0(base_uri, separator, query_string)
}

parse_form_data <- function(body) {
  if (is.null(body) || length(body) == 0) {
    return(list())
  }

  # Parse URL-encoded form data
  pairs <- strsplit(body, "&")[[1]]
  result <- list()

  for (pair in pairs) {
    if (grepl("=", pair)) {
      parts <- strsplit(pair, "=", fixed = TRUE)[[1]]
      key <- utils::URLdecode(parts[1])
      value <- if (length(parts) > 1) utils::URLdecode(parts[2]) else ""
      result[[key]] <- value
    }
  }

  result
}

#' @export
print.oauth_server <- function(x, ...) {
  cat("OAuth 2.1 Server\n")
  cat("Issuer:", x$issuer, "\n")
  cat("Clients:", x$clients$size(), "\n")
  cat("Tokens:", x$tokens$size(), "\n")
  cat("PKCE required:", x$config$require_pkce, "\n")
  invisible(x)
}

