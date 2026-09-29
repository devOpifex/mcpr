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

    # Create an ellmer tool
    ellmer_tool <- ellmer::tool(
      handler,
      tool$description,
      arguments = create_ellmer_types(tool$inputSchema),
      name = tool_name,
      annotations = ellmer::tool_annotations(title = tool_name)
    )

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
#' @return A list of ellmer type objects for each property
#' @keywords internal
create_ellmer_types <- function(schema) {
  required <- unlist(schema$required)
  types <- list()

  for (prop_name in names(schema$properties)) {
    types[[prop_name]] <- create_ellmer_type(
      schema$properties[[prop_name]],
      required = prop_name %in% required
    )
  }

  types
}

#' Create an ellmer type for a specific MCP property
#'
#' @param prop The MCP property
#' @param required Whether the property is required
#' @return An ellmer type object
#' @keywords internal
create_ellmer_type <- function(prop, required = TRUE) {
  description <- prop$description
  if (is.null(description)) {
    description <- prop$title
  }

  # JSON Schema allows `type: ["string", "null"]`; null means optional
  types <- unlist(prop$type)
  if ("null" %in% types) {
    required <- FALSE
  }
  types <- setdiff(types, "null")
  # Missing type (e.g. anyOf/oneOf) falls back to string
  type <- if (length(types) > 0) types[[1]] else "string"

  # Enums are expressed as `enum` alongside a base type
  if (!is.null(prop$enum)) {
    return(ellmer::type_enum(
      as.character(unlist(prop$enum)),
      description,
      required = required
    ))
  }

  switch(
    type,
    number = ellmer::type_number(description, required = required),
    integer = ellmer::type_integer(description, required = required),
    boolean = ellmer::type_boolean(description, required = required),
    array = {
      items <- prop$items
      if (is.null(items)) {
        items <- list(type = "string")
      }
      ellmer::type_array(
        create_ellmer_type(items),
        description,
        required = required
      )
    },
    object = do.call(
      ellmer::type_object,
      c(
        list(.description = description, .required = required),
        create_ellmer_types(prop)
      )
    ),
    ellmer::type_string(description, required = required)
  )
}

#' Create an ellmer handler function for an MCP tool
#'
#' @param client The mcpr client
#' @param tool_name The name of the MCP tool
#' @param input_schema The tool's input schema
#' @return A function that can be used as an ellmer tool handler
#' @keywords internal
create_ellmer_handler <- function(client, tool_name, input_schema) {
  # Force now: the caller's loop variable changes before the handler runs
  force(client)
  force(tool_name)

  param_names <- names(input_schema$properties)

  # Optional parameters default to NULL so ellmer can omit them
  args_str <- ""
  params_list_str <- ""
  if (length(param_names) > 0) {
    args_str <- paste0(param_names, " = NULL", collapse = ", ")
    params_list_str <- paste0(param_names, " = ", param_names, collapse = ", ")
  }

  # Build the function body as a string with proper line breaks
  fn_body <- sprintf(
    "
  function(%s) {
    # Collect arguments, dropping omitted optional ones
    args <- list(%s)
    args <- args[!vapply(args, is.null, logical(1))]
    # Named so an empty list serialises to {} rather than []
    names(args) <- as.character(names(args))

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
