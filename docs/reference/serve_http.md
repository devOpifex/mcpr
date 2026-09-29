# Serve an MCP server over HTTP using ambiorix

Serve an MCP server over HTTP using ambiorix

## Usage

``` r
serve_http(mcp, port = Sys.getenv("SHINY_PORT", 3000), path = "/mcp")
```

## Arguments

- mcp:

  An MCP server object

- port:

  Port to listen on, defaults to 3000

- path:

  Path to serve the MCP endpoint, defaults to "/mcp"

## Value

Invisible, runs indefinitely
