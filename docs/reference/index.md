# Package index

## MCP Core Functions

Core functions for creating and managing MCP server objects

- [`new_server()`](https://mcpr.opifex.org/reference/new_server.md)
  [`new_mcp()`](https://mcpr.opifex.org/reference/new_server.md) :
  Create a new MCP object
- [`add_capability()`](https://mcpr.opifex.org/reference/add_capability.md)
  : Add a capability to an MCP object
- [`register_mcpr_tools()`](https://mcpr.opifex.org/reference/register_mcpr_tools.md)
  : Register MCPR tools with an ellmer chat

## Capability Creation

Functions for creating different types of capabilities

- [`new_tool()`](https://mcpr.opifex.org/reference/new_tool.md) : Create
  a new tool
- [`new_resource()`](https://mcpr.opifex.org/reference/new_resource.md)
  : Create a new resource
- [`new_prompt()`](https://mcpr.opifex.org/reference/new_prompt.md) :
  Create a new prompt

## Schema and Properties

Functions for defining input schemas and properties

- [`schema()`](https://mcpr.opifex.org/reference/schema.md) : Create a
  new input schema
- [`properties()`](https://mcpr.opifex.org/reference/properties.md) :
  Create a new properties list
- [`property_string()`](https://mcpr.opifex.org/reference/property_string.md)
  : Create a string property definition
- [`property_number()`](https://mcpr.opifex.org/reference/property_number.md)
  : Create a number property definition
- [`property_boolean()`](https://mcpr.opifex.org/reference/property_boolean.md)
  : Create a boolean property definition
- [`property_array()`](https://mcpr.opifex.org/reference/property_array.md)
  : Create an array property definition
- [`property_object()`](https://mcpr.opifex.org/reference/property_object.md)
  : Create an object property definition
- [`property_enum()`](https://mcpr.opifex.org/reference/property_enum.md)
  : Create an enum property with predefined values
- [`new_property()`](https://mcpr.opifex.org/reference/new_property.md)
  : Create a new property

## Server Functions

Functions for serving MCP implementations

- [`serve_io()`](https://mcpr.opifex.org/reference/serve_io.md) : Serve
  an MCP server using stdin/stdout
- [`serve_http()`](https://mcpr.opifex.org/reference/serve_http.md) :
  Serve an MCP server over HTTP using ambiorix
- [`get_name()`](https://mcpr.opifex.org/reference/get_name.md) : Get
  the name of a client

## Client Functions

Functions for connecting to and interacting with MCP servers

- [`new_client_io()`](https://mcpr.opifex.org/reference/client.md)
  [`new_client_http()`](https://mcpr.opifex.org/reference/client.md) :
  Create a new mcp IO
- [`initialize()`](https://mcpr.opifex.org/reference/initialize.md) :
  Initialize the server with protocol information
- [`tools_list()`](https://mcpr.opifex.org/reference/tools_list.md) :
  List all available tools
- [`tools_call()`](https://mcpr.opifex.org/reference/tools_call.md) :
  Call a tool with the given parameters
- [`prompts_list()`](https://mcpr.opifex.org/reference/prompts_list.md)
  : List all available prompts
- [`prompts_get()`](https://mcpr.opifex.org/reference/prompts_get.md) :
  Get a prompt with the given parameters
- [`resources_list()`](https://mcpr.opifex.org/reference/resources_list.md)
  : List all available resources
- [`resources_read()`](https://mcpr.opifex.org/reference/resources_read.md)
  : Read a resource with the given parameters
- [`read()`](https://mcpr.opifex.org/reference/read.md) : Read a
  JSON-RPC response from a client provider
- [`write()`](https://mcpr.opifex.org/reference/write.md) : Write a
  JSON-RPC request to a client provider

## Response Functions

Functions for creating various response types

- [`response_text()`](https://mcpr.opifex.org/reference/response.md)
  [`response_image()`](https://mcpr.opifex.org/reference/response.md)
  [`response_audio()`](https://mcpr.opifex.org/reference/response.md)
  [`response_video()`](https://mcpr.opifex.org/reference/response.md)
  [`response_file()`](https://mcpr.opifex.org/reference/response.md)
  [`response_resource()`](https://mcpr.opifex.org/reference/response.md)
  [`response_error()`](https://mcpr.opifex.org/reference/response.md)
  [`response_item()`](https://mcpr.opifex.org/reference/response.md)
  [`response()`](https://mcpr.opifex.org/reference/response.md) : Create
  a response object

## Roxygen2 Extension

Functions for extending roxygen2 with MCP server generation

- [`mcp_roclet()`](https://mcpr.opifex.org/reference/mcp_roclet.md) :
  MCP Roclet for Generating MCP Servers
- [`roxy_tag_parse(`*`<roxy_tag_mcp>`*`)`](https://mcpr.opifex.org/reference/roxy_tag_parse.roxy_tag_mcp.md)
  : Parse @mcp tag
- [`roxy_tag_parse(`*`<roxy_tag_type>`*`)`](https://mcpr.opifex.org/reference/roxy_tag_parse.roxy_tag_type.md)
  : Parse @type tag
- [`roclet_process(`*`<roclet_mcp>`*`)`](https://mcpr.opifex.org/reference/roclet_process.roclet_mcp.md)
  : Process blocks for MCP roclet
- [`roxy_tag_rd(`*`<roxy_tag_mcp>`*`)`](https://mcpr.opifex.org/reference/roxy_tag_rd.roxy_tag_mcp.md)
  : Roxygen2 tag for @mcp This function is called by Roxygen2 to
  generate documentation for the @mcp tag
- [`roxy_tag_rd(`*`<roxy_tag_type>`*`)`](https://mcpr.opifex.org/reference/roxy_tag_rd.roxy_tag_type.md)
  : Roxygen2 tag handler for @type This function is called by Roxygen2
  to generate documentation for the @type
- [`roclet_output(`*`<roclet_mcp>`*`)`](https://mcpr.opifex.org/reference/roclet_output.roclet_mcp.md)
  : Generate MCP server output

## Ellmer Integration

Functions for integrating with the Ellmer package

- [`register_mcpr_tools()`](https://mcpr.opifex.org/reference/register_mcpr_tools.md)
  : Register MCPR tools with an ellmer chat
- [`ellmer_to_mcpr_tool()`](https://mcpr.opifex.org/reference/ellmer_to_mcpr_tool.md)
  : Convert ellmer tools to mcpr tools
