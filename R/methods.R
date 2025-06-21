#' List all available tools
#'
#' @param mcp A server object
#'
#' @return A list containing all available tools
#' @export
tools_list <- function(mcp) {
  UseMethod("tools_list")
}

#' @export
#' @method tools_list server
tools_list.server <- function(mcp) {
  # List all available tools
  tools <- list()
  for (tool_name in names(mcp$tools)) {
    tool <- mcp$tools[[tool_name]]
    tools <- c(tools, list(tool))
  }
  list(tools = tools)
}

#' @export
#' @method tools_list client
tools_list.client <- function(mcp) {
  write(mcp, "tools/list", NULL)
}

#' List all available resources
#'
#' @param mcp A server object
#'
#' @return A list containing all available resources
#' @export
resources_list <- function(mcp) {
  UseMethod("resources_list")
}

#' @export
#' @method resources_list server
resources_list.server <- function(mcp) {
  # List all available resources
  resources <- list()
  for (resource_name in names(mcp$resources)) {
    resource <- mcp$resources[[resource_name]]
    resources <- c(resources, list(resource))
  }
  list(resources = resources)
}

#' @export
#' @method resources_list client
resources_list.client <- function(mcp) {
  write(mcp, "resources/list", NULL)
}

#' List all available prompts
#'
#' @param mcp A server object
#'
#' @return A list containing all available prompts
#' @export
prompts_list <- function(mcp) {
  UseMethod("prompts_list")
}

#' @export
#' @method prompts_list server
prompts_list.server <- function(mcp) {
  # List all available prompts
  prompts <- list()
  for (prompt_name in names(mcp$prompts)) {
    prompt <- mcp$prompts[[prompt_name]]
    prompts <- c(prompts, list(prompt))
  }
  list(prompts = prompts)
}

#' @export
#' @method prompts_list client
prompts_list.client <- function(mcp) {
  write(mcp, "prompts/list", NULL)
}

#' Initialize the server with protocol information
#'
#' @param mcp A server object
#'
#' @return A list containing protocol version, server info, and capabilities
#' @export
initialize <- function(mcp) {
  UseMethod("initialize")
}

#' @export
#' @method initialize server
initialize.server <- function(mcp) {
  # Return information about the server and its capabilities in the expected format
  list(
    protocolVersion = "2024-11-05",
    serverInfo = list(
      name = attr(mcp, "name"),
      version = mcp$version
    ),
    capabilities = mcp$capabilities
  )
}

#' @export
#' @method initialize client
initialize.client <- function(mcp) {
  write(
    mcp,
    "initialize",
    list(
      protocolVersion = "2024-11-05",
      clientInfo = list(
        name = attr(mcp, "name"),
        version = attr(mcp, "version")
      ),
      capabilities = list(
        roots = list(
          listChanged = FALSE
        ),
        sampling = list()
      )
    )
  )
}

#' Call a tool with the given parameters
#'
#' @param mcp A server object
#' @param params Parameters for the tool call
#' @param id Optional request ID for response tracking
#'
#' @return A response object with the tool call results or an error
#' @export
tools_call <- function(mcp, params, id = NULL) {
  UseMethod("tools_call")
}

#' @export
#' @method tools_call server
tools_call.server <- function(mcp, params, id = NULL) {
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
  tryCatch(
    {
      handler_result <- handler(arguments) |>
        response_force()

      # this may be dangerous, we just assume that the handler returns a response
      if (!inherits(handler_result, "response")) {
        return(create_error(
          JSONRPC_INTERNAL_ERROR,
          paste0("Tool handler did not return a response object."),
          id = id
        ))
      }

      return(create_response(handler_result, id = id))
    },
    error = function(e) {
      create_error(
        JSONRPC_INTERNAL_ERROR,
        paste0("Error executing tool: ", e$message),
        id = id
      )
    }
  )
}

#' @export
#' @method tools_call client
tools_call.client <- function(mcp, params, id = NULL) {
  write(mcp, "tools/call", params, id)
}

#' Read a resource with the given parameters
#'
#' @param mcp A server object
#' @param params Parameters for the resource read
#' @param id Optional request ID for response tracking
#'
#' @return A response object with the resource read results or an error
#' @export
resources_read <- function(mcp, params, id = NULL) {
  UseMethod("resources_read")
}

#' @export
#' @method resources_read server
resources_read.server <- function(mcp, params, id = NULL) {
  # Check required parameters
  if (is.null(params$name) || !is.character(params$name)) {
    return(create_error(
      JSONRPC_INVALID_PARAMS,
      "Missing or invalid resource name",
      id = id
    ))
  }

  resource_name <- params$name
  arguments <- params$arguments

  # Check if resource exists
  if (is.null(mcp$resources) || is.null(mcp$resources[[resource_name]])) {
    return(create_error(
      JSONRPC_METHOD_NOT_FOUND,
      paste0("Resource not found: ", resource_name),
      id = id
    ))
  }

  resource <- mcp$resources[[resource_name]]
  handler <- attr(resource, "handler")

  if (is.null(handler) || !is.function(handler)) {
    return(create_error(
      JSONRPC_INTERNAL_ERROR,
      paste0("Invalid handler for resource: ", resource_name),
      id = id
    ))
  }

  # Execute the resource handler
  tryCatch(
    {
      handler_result <- handler(arguments) |>
        response_force()

      # this may be dangerous, we just assume that the handler returns a response
      if (!inherits(handler_result, "response")) {
        return(create_error(
          JSONRPC_INTERNAL_ERROR,
          paste0("Tool handler did not return a response object."),
          id = id
        ))
      }

      return(create_response(handler_result, id = id))
    },
    error = function(e) {
      create_error(
        JSONRPC_INTERNAL_ERROR,
        paste0("Error executing tool: ", e$message),
        id = id
      )
    }
  )
}

#' @export
#' @method resources_read client
resources_read.client <- function(mcp, params, id = NULL) {
  write(mcp, "resources/read", params, id)
}

#' Get a prompt with the given parameters
#'
#' @param mcp A server object
#' @param params Parameters for the prompt request
#' @param id Optional request ID for response tracking
#'
#' @return A response object with the prompt results or an error
#' @export
prompts_get <- function(mcp, params, id = NULL) {
  UseMethod("prompts_get")
}

#' @export
#' @method prompts_get server
prompts_get.server <- function(mcp, params, id = NULL) {
  # Check required parameters
  if (is.null(params$name) || !is.character(params$name)) {
    return(create_error(
      JSONRPC_INVALID_PARAMS,
      "Missing or invalid prompt name",
      id = id
    ))
  }

  prompt_name <- params$name
  arguments <- params$arguments

  # Check if prompt exists
  if (is.null(mcp$prompts) || is.null(mcp$prompts[[prompt_name]])) {
    return(create_error(
      JSONRPC_METHOD_NOT_FOUND,
      paste0("Prompt not found: ", prompt_name),
      id = id
    ))
  }

  prompt <- mcp$prompts[[prompt_name]]
  handler <- attr(prompt, "handler")

  if (is.null(handler) || !is.function(handler)) {
    return(create_error(
      JSONRPC_INTERNAL_ERROR,
      paste0("Invalid handler for prompt: ", prompt_name),
      id = id
    ))
  }

  # Execute the prompt handler
  tryCatch(
    {
      handler_result <- handler(arguments) |>
        response_force()

      # this may be dangerous, we just assume that the handler returns a response
      if (!inherits(handler_result, "response")) {
        return(create_error(
          JSONRPC_INTERNAL_ERROR,
          paste0("Tool handler did not return a response object."),
          id = id
        ))
      }

      return(create_response(handler_result, id = id))
    },
    error = function(e) {
      create_error(
        JSONRPC_INTERNAL_ERROR,
        paste0("Error executing tool: ", e$message),
        id = id
      )
    }
  )
}

#' @export
#' @method prompts_get client
prompts_get.client <- function(mcp, params, id = NULL) {
  write(mcp, "prompts/get", params, id)
}
