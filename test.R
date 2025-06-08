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

run(mcp)
