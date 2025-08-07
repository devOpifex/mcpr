# OAuth 2.1 Client Test Example
#
# This script demonstrates how to authenticate with the OAuth-protected MCP server
# and make authenticated requests using Bearer tokens.

library(httr2)
library(mcpr)

# Configuration (match the server configuration)
SERVER_URL <- "http://localhost:8080"
CLIENT_ID <- "test_mcp_client"  # Replace with actual client ID from server
CLIENT_SECRET <- "test_secret"  # Replace with actual client secret
REDIRECT_URI <- "http://localhost:3000/callback"
MCP_ENDPOINT <- paste0(SERVER_URL, "/mcp")

cat("=== OAuth 2.1 MCP Client Test ===\n\n")

# Step 1: Get OAuth server metadata
cat("1. Fetching OAuth server metadata...\n")
metadata_response <- request(paste0(SERVER_URL, "/.well-known/oauth-authorization-server")) |>
  req_perform()

if (resp_status(metadata_response) == 200) {
  metadata <- resp_body_json(metadata_response)
  cat("   ✓ OAuth server metadata retrieved\n")
  cat("   Issuer:", metadata$issuer, "\n")
  cat("   Authorization Endpoint:", metadata$authorization_endpoint, "\n")
  cat("   Token Endpoint:", metadata$token_endpoint, "\n\n")
} else {
  stop("Failed to fetch OAuth metadata")
}

# Step 2: Generate authorization URL with PKCE
cat("2. Generating authorization URL with PKCE...\n")

# Create OAuth server object for utility functions
oauth_server <- new_oauth_server(
  issuer_url = SERVER_URL,
  config = list(supported_scopes = c("mcp:tools", "mcp:resources"))
)

# Generate authorization URL
auth_data <- oauth_generate_auth_url(
  oauth_server,
  client_id = CLIENT_ID,
  redirect_uri = REDIRECT_URI,
  scope = "mcp:tools mcp:resources",
  state = "test_state_123"
)

cat("   ✓ Authorization URL generated with PKCE\n")
cat("   URL:", auth_data$authorization_url, "\n")
cat("   Code Challenge:", auth_data$code_challenge, "\n")
cat("   State:", auth_data$state, "\n\n")

# Step 3: Manual authorization (in real app, user would visit URL in browser)
cat("3. Simulating authorization flow...\n")
cat("   In a real application, the user would:\n")
cat("   1. Visit the authorization URL in their browser\n")
cat("   2. Grant permission to the application\n")
cat("   3. Be redirected back with an authorization code\n\n")

# For testing, we'll call the authorization endpoint directly
auth_response <- request(metadata$authorization_endpoint) |>
  req_url_query(
    response_type = "code",
    client_id = CLIENT_ID,
    redirect_uri = REDIRECT_URI,
    code_challenge = auth_data$code_challenge,
    code_challenge_method = "S256",
    scope = "mcp:tools mcp:resources",
    state = auth_data$state
  ) |>
  req_perform()

# Extract authorization code from redirect (this is simplified for testing)
if (resp_status(auth_response) == 302) {
  location <- resp_header(auth_response, "location")
  auth_code <- gsub(".*code=([^&]+).*", "\\1", location)
  cat("   ✓ Authorization code received:", auth_code, "\n\n")
} else {
  stop("Authorization failed")
}

# Step 4: Exchange authorization code for access token
cat("4. Exchanging authorization code for access token...\n")

token_response <- request(metadata$token_endpoint) |>
  req_method("POST") |>
  req_headers("Content-Type" = "application/x-www-form-urlencoded") |>
  req_body_raw(paste(
    "grant_type=authorization_code",
    paste0("code=", auth_code),
    paste0("redirect_uri=", utils::URLencode(REDIRECT_URI)),
    paste0("client_id=", CLIENT_ID),
    paste0("client_secret=", CLIENT_SECRET),
    paste0("code_verifier=", auth_data$code_verifier),
    sep = "&"
  )) |>
  req_perform()

if (resp_status(token_response) == 200) {
  token_data <- resp_body_json(token_response)
  access_token <- token_data$access_token
  
  cat("   ✓ Access token received\n")
  cat("   Token Type:", token_data$token_type, "\n")
  cat("   Expires In:", token_data$expires_in, "seconds\n")
  cat("   Scope:", token_data$scope, "\n\n")
} else {
  stop("Token exchange failed: ", resp_body_string(token_response))
}

# Step 5: Make authenticated MCP request
cat("5. Making authenticated MCP request...\n")

# Create JSON-RPC request
mcp_request <- list(
  jsonrpc = "2.0",
  id = 1,
  method = "tools/call",
  params = list(
    name = "secure_calculator",
    arguments = list(
      operation = "add",
      a = 15,
      b = 25
    )
  )
)

# Make authenticated request to MCP server
mcp_response <- request(MCP_ENDPOINT) |>
  req_method("POST") |>
  req_headers(
    "Content-Type" = "application/json",
    "Authorization" = paste("Bearer", access_token)
  ) |>
  req_body_json(mcp_request) |>
  req_perform()

if (resp_status(mcp_response) == 200) {
  result <- resp_body_json(mcp_response)
  cat("   ✓ MCP request successful\n")
  cat("   Result:", result$result$content[[1]]$text, "\n\n")
} else {
  cat("   ✗ MCP request failed:", resp_status(mcp_response), "\n")
  cat("   Error:", resp_body_string(mcp_response), "\n\n")
}

# Step 6: Test without authentication (should fail)
cat("6. Testing request without authentication...\n")

unauth_response <- request(MCP_ENDPOINT) |>
  req_method("POST") |>
  req_headers("Content-Type" = "application/json") |>
  req_body_json(mcp_request) |>
  req_perform()

if (resp_status(unauth_response) == 401) {
  cat("   ✓ Unauthenticated request correctly rejected (401)\n")
  cat("   WWW-Authenticate:", resp_header(unauth_response, "www-authenticate"), "\n")
} else {
  cat("   ✗ Expected 401 but got:", resp_status(unauth_response), "\n")
}

cat("\n=== OAuth 2.1 Client Test Complete ===\n")