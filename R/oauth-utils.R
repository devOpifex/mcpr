#' OAuth 2.1 Utility Functions
#'
#' Core utilities for OAuth 2.1 implementation including PKCE support,
#' token generation, and validation functions.
#'

#' Generate PKCE code verifier and challenge
#'
#' @param length Length of the code verifier (default: 128)
#' @return List with code_verifier and code_challenge
#' @export
oauth_generate_pkce <- function(length = 128) {
  if (!requireNamespace("openssl", quietly = TRUE)) {
    stop("The 'openssl' package is required for PKCE support")
  }

  # Generate code verifier (base64url-encoded random string)
  code_verifier <- openssl::base64_encode(openssl::rand_bytes(96)) |>
    gsub("\\+", "-", x = _) |>
    gsub("/", "_", x = _) |>
    gsub("=+$", "", x = _)

  # Generate code challenge (SHA256 hash of verifier, base64url-encoded)
  code_challenge <- openssl::sha256(charToRaw(code_verifier)) |>
    openssl::base64_encode() |>
    gsub("\\+", "-", x = _) |>
    gsub("/", "_", x = _) |>
    gsub("=+$", "", x = _)

  list(
    code_verifier = code_verifier,
    code_challenge = code_challenge,
    code_challenge_method = "S256"
  )
}

#' Verify PKCE code challenge
#'
#' @param code_verifier The original code verifier
#' @param code_challenge The code challenge to verify
#' @param method Challenge method (default: "S256")
#' @return Logical indicating if verification succeeded
#' @export
oauth_verify_pkce <- function(code_verifier, code_challenge, method = "S256") {
  if (!requireNamespace("openssl", quietly = TRUE)) {
    stop("The 'openssl' package is required for PKCE verification")
  }

  if (method != "S256") {
    return(FALSE)
  }

  # Regenerate challenge from verifier
  expected_challenge <- openssl::sha256(charToRaw(code_verifier)) |>
    openssl::base64_encode() |>
    gsub("\\+", "-", x = _) |>
    gsub("/", "_", x = _) |>
    gsub("=+$", "", x = _)

  identical(code_challenge, expected_challenge)
}

#' Generate OAuth keypair for JWT signing
#'
#' @return List with private key, public key, and public JWKS
#' @export
oauth_generate_keypair <- function() {
  if (!requireNamespace("openssl", quietly = TRUE)) {
    stop("The 'openssl' package is required for key generation")
  }

  # Generate RSA key pair
  private_key <- openssl::rsa_keygen(2048)
  public_key <- as.list(private_key)$pubkey

  # Create JWKS representation
  public_jwk <- list(
    kty = "RSA",
    use = "sig",
    alg = "RS256",
    kid = generate_key_id(),
    n = openssl::base64_encode(public_key$n),
    e = openssl::base64_encode(public_key$e)
  )

  list(
    private = private_key,
    public = public_key,
    public_jwks = list(keys = list(public_jwk)),
    key_id = public_jwk$kid
  )
}

#' Generate a unique key ID
#'
#' @return Character string key ID
generate_key_id <- function() {
  if (!requireNamespace("uuid", quietly = TRUE)) {
    paste0("key-", format(Sys.time(), "%Y%m%d%H%M%S"))
  } else {
    uuid::UUIDgenerate()
  }
}

#' Create a JWT access token
#'
#' @param subject Subject (user ID)
#' @param audience Audience (MCP server URL)
#' @param issuer Token issuer
#' @param scopes Token scopes
#' @param expires_in Token lifetime in seconds (default: 3600)
#' @param private_key Private key for signing
#' @param key_id Key ID for the private key
#' @return JWT token string
#' @export
oauth_create_access_token <- function(
  subject,
  audience,
  issuer,
  scopes = NULL,
  expires_in = 3600,
  private_key,
  key_id
) {
  if (!requireNamespace("jose", quietly = TRUE)) {
    stop("The 'jose' package is required for JWT creation")
  }

  if (!requireNamespace("uuid", quietly = TRUE)) {
    stop("The 'uuid' package is required for token ID generation")
  }

  now <- as.numeric(Sys.time())

  # JWT claims
  claims <- list(
    iss = issuer,
    sub = subject,
    aud = audience,
    exp = now + expires_in,
    iat = now,
    jti = uuid::UUIDgenerate()
  )

  if (!is.null(scopes)) {
    claims$scope <- paste(scopes, collapse = " ")
  }

  # JWT header
  header <- list(
    typ = "JWT",
    alg = "RS256",
    kid = key_id
  )

  # Sign the token
  jose::jwt_encode_sig(claims, private_key, header = header)
}

#' Validate an access token
#'
#' @param token JWT token string
#' @param public_key Public key for verification
#' @param expected_audience Expected audience
#' @param expected_issuer Expected issuer
#' @return List with validation result and claims
#' @export
oauth_validate_access_token <- function(
  token,
  public_key,
  expected_audience = NULL,
  expected_issuer = NULL
) {
  if (!requireNamespace("jose", quietly = TRUE)) {
    stop("The 'jose' package is required for JWT validation")
  }

  tryCatch(
    {
      # Decode and verify the token
      claims <- jose::jwt_decode_sig(token, public_key)

      # Validate expiration
      if (claims$exp < as.numeric(Sys.time())) {
        return(list(
          valid = FALSE,
          error = "token_expired",
          error_description = "Token has expired"
        ))
      }

      # Validate audience if specified
      if (!is.null(expected_audience) && claims$aud != expected_audience) {
        return(list(
          valid = FALSE,
          error = "invalid_audience",
          error_description = "Token audience does not match"
        ))
      }

      # Validate issuer if specified
      if (!is.null(expected_issuer) && claims$iss != expected_issuer) {
        return(list(
          valid = FALSE,
          error = "invalid_issuer",
          error_description = "Token issuer does not match"
        ))
      }

      list(
        valid = TRUE,
        claims = claims,
        subject = claims$sub,
        scopes = if (!is.null(claims$scope)) {
          strsplit(claims$scope, " ")[[1]]
        } else {
          NULL
        }
      )
    },
    error = function(e) {
      list(
        valid = FALSE,
        error = "invalid_token",
        error_description = paste("Token validation failed:", e$message)
      )
    }
  )
}

#' Generate authorization code
#'
#' @param length Code length (default: 32)
#' @return Authorization code string
#' @export
oauth_generate_authorization_code <- function(length = 32) {
  if (!requireNamespace("openssl", quietly = TRUE)) {
    paste0(
      sample(c(letters, LETTERS, 0:9), length, replace = TRUE),
      collapse = ""
    )
  } else {
    openssl::base64_encode(openssl::rand_bytes(length)) |>
      gsub("[^A-Za-z0-9]", "", x = _) |>
      substr(1, length)
  }
}

#' Parse OAuth error parameters
#'
#' @param error Error code
#' @param error_description Error description
#' @param error_uri Optional error URI
#' @return List with OAuth error response
#' @export
oauth_error_response <- function(
  error,
  error_description = NULL,
  error_uri = NULL
) {
  response <- list(error = error)

  if (!is.null(error_description)) {
    response$error_description <- error_description
  }

  if (!is.null(error_uri)) {
    response$error_uri <- error_uri
  }

  response
}

#' Check if a path is an OAuth endpoint
#'
#' @param path Request path
#' @return Logical indicating if path is OAuth-related
is_oauth_endpoint <- function(path) {
  oauth_paths <- c(
    "/.well-known/oauth-authorization-server",
    "/.well-known/oauth-protected-resource",
    "/.well-known/jwks.json",
    "/oauth/authorize",
    "/oauth/token",
    "/oauth/register"
  )

  path %in% oauth_paths
}

