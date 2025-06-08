tools_list <- function(mcp) {
  # List all available tools
  tools <- list()
  for (tool_name in names(mcp$tools)) {
    tool <- mcp$tools[[tool_name]]
    tools <- c(tools, list(tool))
  }
  tools
}

resources_list <- function(mcp) {
  # List all available resources
  resources <- list()
  for (resource_name in names(mcp$resources)) {
    resource <- mcp$resources[[resource_name]]
    resources <- c(resources, list(resource))
  }
  resources
}

prompts_list <- function(mcp) {
  # List all available prompts
  prompts <- list()
  for (prompt_name in names(mcp$prompts)) {
    prompt <- mcp$prompts[[prompt_name]]
    prompts <- c(prompts, list(prompt))
  }
  prompts
}

initialize_server <- function(mcp) {
  # Return information about the server and its capabilities in the expected format
  list(
    protocolVersion = "2024-11-05",
    capabilities = list(
      tools = list(
        listChanged = TRUE
      ),
      resources = list(
        subscribe = TRUE,
        listChanged = TRUE
      ),
      prompts = list(
        listChanged = TRUE
      ),
      logging = list()
    ),
    serverInfo = list(
      name = attr(mcp, "name"),
      version = mcp$version
    )
  )
}

tools_call <- function(mcp, params, id = NULL) {
  # Check required parameters
  if (is.null(params$name) || !is.character(params$name)) {
    return(create_error(
      JSONRPC_INVALID_PARAMS,
      "Missing or invalid tool name",
      id = id
    ))
  }
  
  tool_name <- params$name
  arguments <- params$arguments
  
  # Check if tool exists
  if (is.null(mcp$tools) || is.null(mcp$tools[[tool_name]])) {
    return(create_error(
      JSONRPC_METHOD_NOT_FOUND,
      paste0("Tool not found: ", tool_name),
      id = id
    ))
  }
  
  tool <- mcp$tools[[tool_name]]
  handler <- attr(tool, "handler")
  
  if (is.null(handler) || !is.function(handler)) {
    return(create_error(
      JSONRPC_INTERNAL_ERROR,
      paste0("Invalid handler for tool: ", tool_name),
      id = id
    ))
  }
  
  # Execute the tool handler
  tryCatch({
    result <- handler(arguments)
    return(result) # Just return the result, process_request will wrap it
  }, error = function(e) {
    return(create_error(
      JSONRPC_INTERNAL_ERROR,
      paste0("Error executing tool: ", e$message),
      id = id
    ))
  })
}
