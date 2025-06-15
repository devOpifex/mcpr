<div align="center">
<img src="man/figures/logo.png" />
</div>

mcpr is an R implementation of the [Model Context Protocol (MCP)](https://modelcontextprotocol.io),
enabling R applications to expose capabilities (tools, resources, and prompts)
to AI models through a standard JSON-RPC 2.0 interface. It also provides client
functionality to connect to and interact with MCP servers.

See the official [MCP documentation](https://modelcontextprotocol.io) for more information,
particularly on schemas and capabilities.
Visit [package docs](https://mcpr.opifex.org/) for more information on the R implementation.

## Installation

You can install mcpr from GitHub using the [pak](https://pak.r-lib.org/) package:

```r
pak::pkg_install("devOpifex/mcpr")
```

## Basic Usage

### Server

Here's a simple example that creates an MCP server with a calculator tool:

```r
library(mcpr)

calculator <- new_tool(
  name = "calculator",
  description = "Performs basic arithmetic operations",
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
    result <- switch(params$operation,
      "add" = params$a + params$b,
      "subtract" = params$a - params$b,
      "multiply" = params$a * params$b,
      "divide" = params$a / params$b
    )

    response_text(result)
  }
)

mcp <- new_mcp(
  name = "R Calculator Server",
  description = "A simple calculator server implemented in R",
  version = "1.0.0"
)

mcp <- add_capability(mcp, calculator)

serve_io(mcp)
```

You can return multiple responses by returning a list of `response` objects:

```r
response(
  response_text("Hello, world!"),
  response_image(system.file("extdata/logo.png", package = "mcpr")),
  response_audio(system.file("extdata/sound.mp3", package = "mcpr")),
  response_video(system.file("extdata/video.mp4", package = "mcpr")),
  response_file(system.file("extdata/file.txt", package = "mcpr")),
  response_resource(system.file("extdata/resource.json", package = "mcpr"))
)
```

You can also serve via HTTP transport with `serve_http`:

```r
# Serve via HTTP on port 3000
serve_http(mcp, port = 3000)
```

See the [Get Started](https://mcpr.opifex.org/articles/get-started) guide for more information.

### Client

Here's a simple example of using the client to interact with an MCP server:

```r
library(mcpr)

# Create a client that connects to an MCP server
# For HTTP transport
client <- new_client_http(
  "http://localhost:8080",
  name = "calculator",
  version = "1.0.0"
)

# Or for standard IO transport
# client <- new_client_io(
#   "Rscript",
#   "/path/to/server.R",
#   name = "calculator",
#   version = "1.0.0"
# )

# List available tools
tools <- tools_list(client)
print(tools)

# Call a tool
result <- tools_call(
  client,
  params = list(
    name = "calculator",
    arguments = list(
      operation = "add",
      a = 5,
      b = 3
    )
  ),
  id = 1L
)
print(result)

# List available prompts
prompts <- prompts_list(client)
print(prompts)

# List available resources
resources <- resources_list(client)
print(resources)

# Read a resource
resource_content <- resources_read(
  client,
  params = list(
    name = "example-resource"
  )
)
print(resource_content)
```

## ellmer integrations

Use the `register_mcpr_tools` function to convert MCPR tools to ellmer tools and 
register them with an ellmer chat session.

```r
# Create an MCPR client connected to the calculator server
client <- new_client_io(
  "Rscript",
  "path/to/server.R",
  name = "calculator",
  version = "1.0.0"
)

# Create a Claude chat session with ellmer
chat <- ellmer::chat_anthropic()

## Convert MCPR tools to ellmer tools and register them with the chat
chat <- register_mcpr_tools(chat, client)

## Try using the tools in a chat
chat$chat(
  "Subtract 2 from 44"
)
```

## Using mcpr

### Claude Code Integration

To use your MCP server with Claude Code, see the [documentation](https://docs.anthropic.com/en/docs/claude-code/tutorials#set-up-model-context-protocol-mcp)

```bash
claude mcp add r-calculator -- Rscript /path/to/calculator_server.R
```

### Cursor Integration

To integrate with Cursor see the [documentation](https://docs.cursor.com/context/model-context-protocol)

```json
{
  "customCommands": {
    "r-calculator": {
      "command": "Rscript 'path/to/calculator_server.R'"
    }
  }
}
```
### VS Code Agent Mode Integration
To integrate with VS Code Agent mode see the [documentation](https://code.visualstudio.com/docs/copilot/chat/mcp-servers#_add-an-mcp-server-to-your-user-settings)
```json
"mcp": {
        "servers": {
            "my-mcp-server-calculator": {
                "type": "stdio",
                "command": "Rscript",
                "args": [
                    "path/to/calculator_server.R"
                ]
            }
}
```
More integrations in the [docs](https://mcpr.opifex.org/articles/client-integration)
