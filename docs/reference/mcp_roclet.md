# MCP Roclet for Generating MCP Servers

This roclet automatically generates MCP (Model Context Protocol) servers
from R functions annotated with @mcp tags.

## Usage

``` r
mcp_roclet()
```

## Examples

``` r
if (FALSE) { # \dontrun{
# Use the roclet in roxygenise
roxygen2::roxygenise(roclets = c("rd", "mcpr::mcp_roclet"))
} # }
```
