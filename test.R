#!/usr/bin/env Rscript

devtools::load_all()

# Create a simple echo tool
echo_tool <- new_tool(
  name = "echo",
  description = "Echoes back the message sent to it",
  input_schema = schema(
    properties = list(
      message = prop_string("Message", "The message to echo", required = TRUE)
    )
  ),
  handler = function(params) {
    list(
      echo = params$message,
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
  }
)

# Create an MCP server
mcp <- new_mcp(
  name = "Echo Server",
  description = "A simple echo server to test the JSON-RPC implementation",
  version = "1.0.0"
)

mcp <- add_capability(mcp, echo_tool)

# Check if we want to run in test mode or interactive mode
is_test <- "--test" %in% commandArgs(trailingOnly = TRUE)

if (is_test) {
  # Test mode - create a test request
  test_request <- jsonlite::toJSON(
    list(
      jsonrpc = "2.0",
      method = "tool.echo",
      params = list(message = "Hello, world!"),
      id = 1
    ),
    auto_unbox = TRUE
  )

  # Create a temporary file with test input
  tmp_file <- tempfile()
  on.exit(unlink(tmp_file))
  write(test_request, tmp_file)

  # Run with test input
  con <- file(tmp_file, "r")
  on.exit(close(con), add = TRUE)

  serve_std_io(mcp, input_con = con)
} else {
  # Interactive mode - run the server normally
  cat("Starting MCP server in interactive mode.\n")
  cat("Enter a JSON-RPC request, or press Ctrl+D to exit.\n")
  cat(
    "Example request: {\"jsonrpc\":\"2.0\",\"method\":\"tool.echo\",\"params\":{\"message\":\"Hello\"},\"id\":1}\n\n"
  )

  # Use serve_std_io without any while loop after it
  serve_std_io(mcp)
}
