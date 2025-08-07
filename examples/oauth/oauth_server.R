# OAuth 2.1 Enabled MCP Server Example
# 
# This example demonstrates how to create an MCP server with OAuth 2.1 authentication
# following the Model Context Protocol specification requirements.

library(mcpr)

# Create a simple calculator tool
calculator <- new_tool(
  name = "secure_calculator", 
  description = "Performs secure arithmetic operations with OAuth protection",
  input_schema = schema(
    properties = properties(
      operation = property_enum(
        "Operation",
        "Math operation to perform", 
        values = c("add", "subtract", "multiply", "divide"),
        required = TRUE
      ),
      a = property_number("First number", "First operand", required = TRUE),
      b = property_number("Second number", "Second operand", required = TRUE)
    )
  ),
  handler = function(params) {
    result <- switch(
      params$operation,
      "add" = params$a + params$b,
      "subtract" = params$a - params$b, 
      "multiply" = params$a * params$b,
      "divide" = if (params$b != 0) params$a / params$b else "Division by zero error"
    )
    response_text(paste("Secure calculation result:", result))
  }
)

# Create MCP server
mcp_server <- new_server(
  name = "OAuth Protected Calculator",
  description = "A calculator server with OAuth 2.1 authentication",
  version = "1.0.0"
)

mcp_server <- add_capability(mcp_server, calculator)

# Create OAuth 2.1 server configuration
oauth_config <- new_oauth_server(
  issuer_url = "http://localhost:8080",
  client_storage = oauth_memory_storage(),  # In production, use oauth_file_storage()
  token_storage = oauth_memory_storage(),   # In production, use persistent storage
  config = list(
    access_token_expires_in = 3600,  # 1 hour
    supported_scopes = c("mcp:tools", "mcp:resources", "mcp:prompts"),
    allow_dynamic_registration = TRUE
  )
)

# Create a test client for development
test_client <- oauth_create_test_client(
  oauth_config,
  client_name = "MCP Test Client",
  redirect_uri = "http://localhost:3000/callback"
)

cat("=== OAuth 2.1 MCP Server Starting ===\n")
cat("Test Client Created:\n")
cat("  Client ID:", test_client$client_id, "\n")
cat("  Client Secret:", test_client$client_secret, "\n")
cat("  Authorization URL:", test_client$authorization_url, "\n")
cat("  Token URL:", test_client$token_url, "\n")
cat("\n")

# Start the server with OAuth protection
serve_http(
  mcp_server,
  port = 8080,
  path = "/mcp",
  oauth_config = oauth_config,
  require_auth = TRUE,  # Require authentication for all MCP operations
  protected_scopes = c("mcp:tools")  # Require mcp:tools scope for calculator
)