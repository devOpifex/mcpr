# Create a new MCP object

Create a new MCP object

## Usage

``` r
new_server(
  name,
  description,
  version,
  tools = list(),
  resources = list(),
  prompts = list()
)

new_mcp(...)
```

## Arguments

- name:

  Name of the MCP server

- description:

  Description of the MCP server

- version:

  Version of the MCP server

- tools:

  List of tools (optional)

- resources:

  List of resources (optional)

- prompts:

  List of prompts (optional)

- ...:

  Forwarded to `new_server()`

## Value

A new MCP object

## Examples

``` r
mcp <- new_server(
  name = "My MCP",
  description = "This is a description",
  version = "1.0.0"
)
```
