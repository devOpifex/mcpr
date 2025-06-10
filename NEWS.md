# mcpr 0.0.1

## Major Changes

- Added comprehensive client support for interacting with MCP servers:
  - `new_client()` for creating client connections to both local process and HTTP servers
  - `initialize()` for initializing client connections
  - `tools_list()` and `tools_call()` for discovering and using server tools
  - `prompts_list()` and `prompts_get()` for working with server prompts
  - `resources_list()` and `resources_read()` for accessing server resources
- Renamed `new_mcp()` (deprecated) to `new_server()` for clarity
- Added example implementations for both server and client
- Added client usage vignette

# mcpr 0.0.0

* Initial CRAN submission.
