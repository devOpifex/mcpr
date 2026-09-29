# Convert multiple MCPR clients to ellmer tools

This function converts tools from multiple MCPR clients to a format
compatible with the ellmer package. It's useful when you want to combine
tools from multiple MCP servers into a single ellmer session.

## Usage

``` r
mcpr_clients_to_ellmer_tools(...)
```

## Arguments

- ...:

  One or more mcpr client objects

## Value

A list of ellmer-compatible tool functions from all clients

## Examples

``` r
if (FALSE) { # \dontrun{
# Create multiple MCPR clients
client1 <- new_client_io("path/to/server1")
client2 <- new_client_io("path/to/server2")

# Convert all tools
ellmer_tools <- mcpr_clients_to_ellmer_tools(client1, client2)

# Use with ellmer
chat <- ellmer::chat_claude()
chat$set_tools(ellmer_tools)
} # }
```
