#' Create a new MCP object
#'
#' @param ... Named arguments to be included in the MCP object
#'
#' @return A new MCP object
#' @export
#'
#' @examples
#' mcp <- new_mcp(name = "My MCP", description = "This is a description")
new_mcp <- function(
  name,
  description,
  version,
  tools = list(),
  resources = list(),
  prompts = list()
) {
  # Validate inputs
  if (missing(name)) {
    stop("name is required")
  }
  if (missing(description)) {
    stop("description is required")
  }
  if (missing(version)) {
    stop("version is required")
  }
  if (!is.character(version) || length(version) != 1) {
    stop("version must be a single character string")
  }
  if (!is.character(name) || length(name) != 1) {
    stop("name must be a single character string")
  }
  if (!is.character(description) || length(description) != 1) {
    stop("description must be a single character string")
  }
  if (!is.list(tools)) {
    stop("tools must be a list")
  }
  if (!is.list(resources)) {
    stop("resources must be a list")
  }
  if (!is.list(prompts)) {
    stop("prompts must be a list")
  }

  # Create the structure
  server <- list(
    tools = tools,
    resources = resources,
    prompts = prompts,
    version = version,
    capabilities = list(
      tools = length(tools) > 0,
      resources = length(resources) > 0,
      prompts = length(prompts) > 0
    )
  )

  # Set attributes
  attr(server, "name") <- name
  attr(server, "description") <- description
  attr(server, "class") <- c("mcp_server", "list")

  return(server)
}

#' Create a new tool
#'
#' @param name Name of the tool
#' @param description Description of the tool
#' @param input_schema Input schema for the tool
#'
#' @return A new tool capability
#' @export
#'
#' @examples
#' tool <- new_tool(
#'   name = "My Tool",
#'   description = "This is a description",
#'   input_schema = list(
#'     type = "object",
#'     properties = list(
#'       input1 = list(
#'         type = "string",
#'         description = "Input 1"
#'       ),
#'       input2 = list(
#'         type = "number",
#'         description = "Input 2"
#'       )
#'     )
#'   )
#' )
new_tool <- function(name, description, input_schema = list()) {
  new_capability(
    name = name,
    description = description,
    inputSchema = input_schema,
    type = "tool"
  )
}

#' Create a new resource
#'
#' @param name Name of the resource
#' @param description Description of the resource
#' @param uri URI of the resource
#' @param mime_type Mime type of the resource
#'
#' @return A new resource capability
#' @export
#'
#' @examples
#' resource <- new_resource(
#'   name = "My Resource",
#'   description = "This is a description",
#'   uri = "https://example.com/resource",
#'   mime_type = "text/plain"
#' )
new_resource <- function(name, description, uri, mime_type = NULL) {
  new_capability(
    name = name,
    description = description,
    uri = uri,
    mimeType = mime_type,
    type = "resource"
  )
}

#' Create a new prompt
#'
#' @param name Name of the prompt
#' @param description Description of the prompt
#' @param arguments List of arguments for the prompt
#'
#' @return A new prompt capability
#' @export
#'
#' @examples
#' prompt <- new_prompt(
#'   name = "My Prompt",
#'   description = "This is a description",
#'   arguments = list(
#'     input1 = list(
#'       type = "string",
#'       description = "Input 1"
#'     ),
#'     input2 = list(
#'       type = "number",
#'       description = "Input 2"
#'     )
#'   )
#' )
new_prompt <- function(name, description, arguments = list()) {
  new_capability(
    name = name,
    description = description,
    arguments = arguments,
    type = "prompt"
  )
}

# Print method for mcp_server
new_capability <- function(
  name,
  description,
  ...,
  type = c("tool", "resource", "prompt")
) {
  stopifnot(!missing(name), !missing(description))
  type <- match.arg(type)

  structure(
    list(
      ...
    ),
    name = name,
    description = description,
    class = c("capability", type, "list")
  )
}

#' Add a capability to an MCP object
#'
#' @param capability A capability object
#' @param mcp An MCP object
#'
#' @return The MCP object with the capability added
add_capability <- function(mcp, capability) UseMethod("add_tool", capability)

#' @export
#' @method add_capability tool
add_capability.tool <- function(mcp, capability) {
  mcp$tools[[capability$name]] <- capability
  invisible(mcp)
}

#' @export
#' @method add_capability resource
add_capability.resource <- function(mcp, capability) {
  mcp$resources[[capability$name]] <- capability
  invisible(mcp)
}

#' @export
#' @method add_capability prompt
add_capability.prompt <- function(mcp, capability) {
  mcp$prompts[[capability$name]] <- capability
  invisible(mcp)
}
