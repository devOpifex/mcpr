#' Convert MCPR tools to ellmer tools
#'
#' This function converts tools from an MCPR client to a format compatible
#' with the ellmer package. It retrieves all available tools from the client
#' and creates wrapper functions that call these tools through the MCPR protocol.
#'
#' @param client An mcpr client object
#' @return A list of ellmer-compatible tool functions
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Create an MCPR client
#' client <- new_client_io("path/to/server")
#'
#' # Convert its tools to ellmer format
#' ellmer_tools <- mcpr_to_ellmer_tools(client)
#'
#' # Use with ellmer
#' chat <- ellmer::chat_claude()
#' chat$set_tools(ellmer_tools)
#' }
mcpr_to_ellmer_tools <- function(client) {
  # Get all available tools from the client
  tools_response <- tools_list(client)

  # Extract the tools from the response
  if (!is.null(tools_response$error)) {
    stop("Failed to get tools list: ", tools_response$error$message)
  }

  tools <- tools_response$result$tools
  if (length(tools) == 0) {
    return(list())
  }

  # Convert each tool to ellmer format
  ellmer_tools <- list()
  for (tool in tools) {
    # Use the tool name directly
    tool_name <- tool$name

    # Create the handler function
    handler <- create_ellmer_handler(client, tool$name, tool$inputSchema)

    # Get the property types for ellmer
    prop_types <- create_ellmer_types(tool$inputSchema)

    # Construct the tool call with dynamic arguments
    tool_call_args <- c(
      list(
        handler,
        tool$description,
        .name = tool_name,
        .annotations = ellmer::tool_annotations(
          title = tool_name
        )
      ),
      prop_types
    )

    # Create an ellmer tool
    ellmer_tool <- do.call(ellmer::tool, tool_call_args)

    # Add the tool to the result list
    ellmer_tools[[tool_name]] <- ellmer_tool
  }

  return(ellmer_tools)
}

#' Register MCPR tools with an ellmer chat
#'
#' This function registers tools from an MCPR client with an ellmer chat instance.
#'
#' @param chat An ellmer chat object
#' @param client An mcpr client object
#' @return The chat object (invisibly)
#' @export
register_mcpr_tools <- function(chat, client) {
  stopifnot(!missing(chat), !missing(client))

  # Get ellmer tools from the client
  ellmer_tools <- mcpr_to_ellmer_tools(client)

  # Register the tools with the chat
  chat$set_tools(ellmer_tools)

  invisible(chat)
}

#' Create ellmer type functions from MCP schema properties
#'
#' @param schema The MCP schema object
#' @return A list of ellmer type function calls for each property
#' @keywords internal
create_ellmer_types <- function(schema) {
  types <- list()

  # Process each property in the schema
  for (prop_name in names(schema$properties)) {
    prop <- schema$properties[[prop_name]]

    # Create the appropriate type function based on MCP type
    type_fn <- create_ellmer_type(prop)

    # Add to the list of type functions
    types[[prop_name]] <- type_fn
  }

  return(types)
}

#' Create an ellmer type function for a specific MCP property
#'
#' @param prop The MCP property
#' @return An ellmer type function call
#' @keywords internal
create_ellmer_type <- function(prop) {
  # Get the description
  description <- if (!is.null(prop$description)) {
    prop$description
  } else {
    prop$title
  }

  # Create type based on MCP type
  if (prop$type == "string") {
    return(ellmer::type_string(description))
  }

  if (prop$type == "number") {
    return(ellmer::type_number(description))
  }

  if (prop$type == "integer") {
    return(ellmer::type_integer(description))
  }

  if (prop$type == "boolean") {
    return(ellmer::type_boolean(description))
  }

  if (prop$type == "enum") {
    return(ellmer::type_enum(description, prop$enum))
  }

  if (prop$type == "array") {
    # For arrays, we need to create the item type
    item_type <- create_ellmer_type(prop$items)
    return(ellmer::type_array(description, item_type))
  }

  if (prop$type == "object") {
    # For objects, we need to create a list of property types
    property_types <- create_ellmer_types(prop)
    return(ellmer::type_object(description, property_types))
  }

  # Default to string if unknown type
  ellmer::type_string(description)
}

#' Create an ellmer handler function for an MCP tool
#'
#' @param client The mcpr client
#' @param tool_name The name of the MCP tool
#' @param input_schema The tool's input schema
#' @return A function that can be used as an ellmer tool handler
#' @keywords internal
create_ellmer_handler <- function(client, tool_name, input_schema) {
  # Get parameter names from the schema

  # Get parameter names for functions with parameters
  param_names <- names(input_schema$properties)

  # For functions with parameters, create them dynamically
  args_str <- paste(param_names, collapse = ", ")
  params_list_str <- paste(
    paste0(param_names, " = ", param_names),
    collapse = ", "
  )

  # Build the function body as a string with proper line breaks
  fn_body <- sprintf(
    "
  function(%s) {
    # Collect arguments into a list
    args <- list(%s)
    
    # Call the MCP tool with the correct parameters structure
    tools_call(client, list(
      name = tool_name,
      arguments = args
    ), id = mcpr:::generate_id())
  }
  ",
    args_str,
    params_list_str
  )

  # Evaluate the function definition
  eval(parse(text = fn_body))
}

#' Convert multiple MCPR clients to ellmer tools
#'
#' This function converts tools from multiple MCPR clients to a format compatible
#' with the ellmer package. It's useful when you want to combine tools from
#' multiple MCP servers into a single ellmer session.
#'
#' @param ... One or more mcpr client objects
#' @return A list of ellmer-compatible tool functions from all clients
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Create multiple MCPR clients
#' client1 <- new_client_io("path/to/server1")
#' client2 <- new_client_io("path/to/server2")
#'
#' # Convert all tools
#' ellmer_tools <- mcpr_clients_to_ellmer_tools(client1, client2)
#'
#' # Use with ellmer
#' chat <- ellmer::chat_claude()
#' chat$set_tools(ellmer_tools)
#' }
mcpr_clients_to_ellmer_tools <- function(...) {
  clients <- list(...)

  # Convert each client's tools
  all_tools <- list()
  for (i in seq_along(clients)) {
    client_tools <- mcpr_to_ellmer_tools(clients[[i]])
    all_tools <- c(all_tools, client_tools)
  }

  all_tools
}
