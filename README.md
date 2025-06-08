# mcpr

mcpr is an R implementation of the [Model Context Protocol (MCP)](https://modelcontextprotocol.io),
enabling R applications to expose capabilities (tools, resources, and prompts)
to AI models through a standard JSON-RPC 2.0 interface.

## Installation

You can install MCPR from GitHub using the [pak](https://pak.r-lib.org/) package:

```r
# Install MCPR from GitHub
pak::pkg_install("devOpifex/mcpr")
```

## Basic Usage

Here's a simple example that creates an MCP server with a calculator tool:

```r
library(mcpr)

# Create a calculator tool
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

# Create an MCP server and add the calculator tool
mcp <- new_mcp(
  name = "R Calculator Server",
  description = "A simple calculator server implemented in R",
  version = "1.0.0"
)

mcp <- add_capability(mcp, calculator)

# Start the server (listening on stdin/stdout)
serve_std_io(mcp)
```

## Using MCPR with AI Coding Assistants

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
      "command": "Rscript -e 'path/to/calculator_server.R'"
    }
  }
}
```
