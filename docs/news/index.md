# Changelog

## mcpr 0.0.2

- Added support for
  [`ellmer::tool`](https://ellmer.tidyverse.org/reference/tool.html)
- Added MCP roclet for automatic server generation:
  - New `@mcp` tag for marking functions as MCP tools
  - New `@type` tag for specifying parameter types (string, number,
    boolean, array, object, enum)
  - [`mcp_roclet()`](https://mcpr.opifex.org/reference/mcp_roclet.md)
    function integrates with roxygen2 to generate complete MCP servers
  - Automatically creates `inst/mcp_server.R` with tool definitions and
    handlers
  - Supports all standard JSON Schema types and enum values
- Added support for headers for clients
- HTTP client works with Streamable HTTP servers: sends the required
  `Accept` header, reads `text/event-stream` responses, and tracks
  `Mcp-Session-Id` and `MCP-Protocol-Version`
- [`initialize()`](https://mcpr.opifex.org/reference/initialize.md) on a
  client now sends `notifications/initialized` and requests protocol
  `2025-11-25`
- HTTP client `timeout` is now honoured in milliseconds, as documented
  (it was passed as seconds); pass a larger `timeout` for slow tools
- [`tools_call()`](https://mcpr.opifex.org/reference/tools_call.md),
  [`resources_read()`](https://mcpr.opifex.org/reference/resources_read.md),
  and
  [`prompts_get()`](https://mcpr.opifex.org/reference/prompts_get.md) on
  a client generate a request id when none is given, previously they
  were sent as notifications and never got a response
- [`serve_http()`](https://mcpr.opifex.org/reference/serve_http.md)
  works with ambiorix 3.0.0, which removed `res$set_header()`

## mcpr 0.0.1

### Major Changes

- Added comprehensive client support for interacting with MCP servers:
  - `new_client()` for creating client connections to both local process
    and HTTP servers
  - [`initialize()`](https://mcpr.opifex.org/reference/initialize.md)
    for initializing client connections
  - [`tools_list()`](https://mcpr.opifex.org/reference/tools_list.md)
    and
    [`tools_call()`](https://mcpr.opifex.org/reference/tools_call.md)
    for discovering and using server tools
  - [`prompts_list()`](https://mcpr.opifex.org/reference/prompts_list.md)
    and
    [`prompts_get()`](https://mcpr.opifex.org/reference/prompts_get.md)
    for working with server prompts
  - [`resources_list()`](https://mcpr.opifex.org/reference/resources_list.md)
    and
    [`resources_read()`](https://mcpr.opifex.org/reference/resources_read.md)
    for accessing server resources
- Renamed [`new_mcp()`](https://mcpr.opifex.org/reference/new_server.md)
  (deprecated) to
  [`new_server()`](https://mcpr.opifex.org/reference/new_server.md) for
  clarity
- Added example implementations for both server and client
- Added client usage vignette
- Fix major issue with
  [`serve_http()`](https://mcpr.opifex.org/reference/serve_http.md)

## mcpr 0.0.0

- Initial CRAN submission.
