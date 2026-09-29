# Convert MCPR tools to ellmer tools

This function converts tools from an MCPR client to a format compatible
with the ellmer package. It retrieves all available tools from the
client and creates wrapper functions that call these tools through the
MCPR protocol.

## Usage

``` r
mcpr_to_ellmer_tools(client)
```

## Arguments

- client:

  An mcpr client object

## Value

A list of ellmer-compatible tool functions

## Examples

``` r
if (FALSE) { # \dontrun{
# Create an MCPR client
client <- new_client_io("path/to/server")

# Convert its tools to ellmer format
ellmer_tools <- mcpr_to_ellmer_tools(client)

# Use with ellmer
chat <- ellmer::chat_claude()
chat$set_tools(ellmer_tools)
} # }
```
