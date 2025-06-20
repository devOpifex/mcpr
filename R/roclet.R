#' MCP Roclet for Generating MCP Servers
#'
#' This roclet automatically generates MCP (Model Context Protocol) servers
#' from R functions annotated with @mcp tags.
#'
#' @export
#' @examples
#' \dontrun{
#' # Use the roclet in roxygenise
#' roxygen2::roxygenise(roclets = c("rd", "mcpr::mcp_roclet"))
#' }
mcp_roclet <- function() {
  roclet("mcp")
}

#' Parse @mcp tag
#'
#' Parses @mcp tags to extract tool name and description.
#' Format: @mcp [tool_name] [tool_description]
#'
#' @param x A roxy_tag object
#' @return The modified roxy_tag object with parsed val
#' @export
roxy_tag_parse.roxy_tag_mcp <- function(x) {
  # Parse the raw content
  raw <- trimws(x$raw)

  if (raw == "") {
    # If no content, use defaults from function
    x$val <- list(
      name = NULL, # Will be filled from function name
      description = NULL # Will be filled from title
    )
  } else {
    # Split by first whitespace to separate name and description
    # Use regexpr to find first space
    first_space <- regexpr("\\s", raw)

    if (first_space == -1) {
      # Only name provided (no spaces)
      x$val <- list(
        name = raw,
        description = NULL
      )
    } else {
      # Both name and description provided
      name_part <- substr(raw, 1, first_space - 1)
      desc_part <- trimws(substr(raw, first_space + 1, nchar(raw)))

      x$val <- list(
        name = name_part,
        description = desc_part
      )
    }
  }

  x
}


#' Process blocks for MCP roclet
#'
#' @param x MCP roclet object
#' @param blocks List of roxy_block objects
#' @param env Environment
#' @param base_path Base path
#' @return List of processed MCP tools
#' @export
roclet_process.roclet_mcp <- function(x, blocks, env, base_path) {
  tools <- list()

  for (block in blocks) {
    # Check if block has @mcp tag
    mcp_tags <- block_get_tags(block, "mcp")

    if (length(mcp_tags) > 0) {
      # Process this block as an MCP tool
      tool_info <- process_mcp_block(block)
      if (!is.null(tool_info)) {
        tools[[length(tools) + 1]] <- tool_info
      }
    }
  }

  tools
}

#' Generate MCP server output
#'
#' @param x MCP roclet object
#' @param results List of processed tools
#' @param base_path Base path
#' @param ... Additional arguments
#' @return NULL (invisible)
#' @export
roclet_output.roclet_mcp <- function(x, results, base_path, ...) {
  if (length(results) == 0) {
    message("No @mcp tags found. No MCP server generated.")
    return(invisible(NULL))
  }

  # Generate MCP server file
  server_content <- generate_mcp_server(results, base_path)

  # Write to file
  output_file <- file.path(base_path, "inst", "mcp_server.R")
  writeLines(server_content, output_file)

  message("Generated MCP server: ", output_file)
  message("Found ", length(results), " MCP tools:")
  for (tool in results) {
    message("  - ", tool$name, ": ", tool$description)
  }

  invisible(NULL)
}

#' Process a single block with @mcp tag
#'
#' @param block A roxy_block object
#' @return A list with tool information or NULL
process_mcp_block <- function(block) {
  # Get @mcp tag
  mcp_tags <- block_get_tags(block, "mcp")
  if (length(mcp_tags) == 0) {
    return(NULL)
  }

  mcp_tag <- mcp_tags[[1]] # Use first @mcp tag

  # Get function name from block object
  func_name <- if (!is.null(block$object) && !is.null(block$object$alias)) {
    block$object$alias
  } else {
    "unknown_function"
  }

  # Get tool name and description
  tool_name <- mcp_tag$val$name %||% paste0(func_name, "_tool")
  tool_description <- mcp_tag$val$description %||%
    block_get_tag_value(block, "title") %||%
    paste("Tool for", func_name)

  # Get parameters
  param_tags <- block_get_tags(block, "param")

  # Build parameter information
  params <- build_param_info(param_tags)

  list(
    name = tool_name,
    description = tool_description,
    function_name = func_name,
    params = params,
    file = block$file,
    line = block$line
  )
}

#' Build parameter information from @param tags with embedded type syntax
#'
#' @param param_tags List of @param tags
#' @return List of parameter information
build_param_info <- function(param_tags) {
  params <- list()

  # Process each @param tag
  for (param_tag in param_tags) {
    # Parse parameter name(s) and description
    raw <- trimws(param_tag$raw)
    if (raw == "") {
      next
    }

    # Split by first whitespace to separate names and description
    first_space <- regexpr("\\s", raw)
    if (first_space == -1) {
      next
    } # No space found

    param_names <- substr(raw, 1, first_space - 1)
    param_desc <- trimws(substr(raw, first_space + 1, nchar(raw)))

    # Handle multiple parameter names separated by comma
    names <- trimws(strsplit(param_names, ",")[[1]])

    for (name in names) {
      # Parse type information from description
      # Format: {type} description or {enum:val1,val2,val3} description
      type_info <- parse_param_type_from_description(param_desc)

      params[[name]] <- list(
        name = name,
        description = type_info$description,
        type = type_info$type,
        enum_values = type_info$enum_values,
        required = TRUE # Default to required, could be enhanced later
      )
    }
  }

  params
}

#' Parse type information from parameter description
#'
#' @param desc Parameter description that may contain {type} syntax
#' @return List with type, enum_values, and cleaned description
parse_param_type_from_description <- function(desc) {
  # Look for {type} or {enum:val1,val2} at the beginning
  type_match <- regexpr("^\\{([^}]+)\\}\\s*(.*)", desc)

  if (type_match == -1) {
    # No type specified, default to string
    return(list(
      type = "string",
      enum_values = NULL,
      description = desc
    ))
  }

  # Extract type and clean description
  matches <- regmatches(desc, type_match, invert = FALSE)
  type_part <- gsub("^\\{([^}]+)\\}\\s*.*", "\\1", matches)
  clean_desc <- gsub("^\\{([^}]+)\\}\\s*(.*)", "\\2", matches)

  # Parse enum values if present
  if (grepl("^enum:", type_part)) {
    enum_string <- gsub("^enum:", "", type_part)
    enum_values <- trimws(strsplit(enum_string, ",")[[1]])
    return(list(
      type = "enum",
      enum_values = enum_values,
      description = clean_desc
    ))
  }

  # Validate type
  valid_types <- c("string", "number", "integer", "boolean", "array", "object")
  param_type <- if (type_part %in% valid_types) type_part else "string"

  list(
    type = param_type,
    enum_values = NULL,
    description = clean_desc
  )
}

#' Generate MCP server R code
#'
#' @param tools List of tool information
#' @param base_path Base path for the package
#' @return Character vector of R code lines
generate_mcp_server <- function(tools, base_path) {
  lines <- c(
    "# Auto-generated MCP Server",
    "# Generated by mcpr::mcp_roclet",
    "",
    "library(mcpr)",
    ""
  )

  # Generate tool definitions
  for (i in seq_along(tools)) {
    tool <- tools[[i]]
    lines <- c(lines, generate_tool_code(tool, i))
  }

  # Generate server creation
  lines <- c(
    lines,
    "",
    "# Create MCP server",
    "mcp_server <- new_server(",
    "  name = \"Auto-generated MCP Server\",",
    "  description = \"MCP server generated from R functions with @mcp tags\",",
    "  version = \"1.0.0\"",
    ")",
    ""
  )

  # Add tools to server
  for (i in seq_along(tools)) {
    tool <- tools[[i]]
    lines <- c(
      lines,
      paste0("mcp_server <- add_capability(mcp_server, ", tool$name, "_tool)")
    )
  }

  # Add server deployment
  lines <- c(
    lines,
    "",
    "# Start the server",
    "serve_io(mcp_server)"
  )

  lines
}

#' Generate R code for a single tool
#'
#' @param tool Tool information list
#' @param index Tool index for naming
#' @return Character vector of R code lines
generate_tool_code <- function(tool, index) {
  lines <- c(
    paste0("# Tool: ", tool$name),
    paste0(tool$name, "_tool <- new_tool("),
    paste0("  name = \"", tool$name, "\","),
    paste0("  description = \"", tool$description, "\","),
    "  input_schema = schema("
  )

  # Generate properties
  if (length(tool$params) > 0) {
    lines <- c(lines, "    properties = properties(")

    prop_lines <- c()
    for (param_name in names(tool$params)) {
      param <- tool$params[[param_name]]
      prop_line <- generate_property_code(param)
      prop_lines <- c(prop_lines, paste0("      ", prop_line))
    }

    # Join properties with commas
    if (length(prop_lines) > 0) {
      prop_lines[length(prop_lines)] <- prop_lines[length(prop_lines)] # No comma on last
      for (i in seq_len(length(prop_lines) - 1)) {
        prop_lines[i] <- paste0(prop_lines[i], ",")
      }
    }

    lines <- c(lines, prop_lines, "    )")
  } else {
    lines <- c(lines, "    properties = properties()")
  }

  lines <- c(
    lines,
    "  ),",
    "  handler = function(params) {",
    paste0("    # Call the original function: ", tool$function_name),
    generate_handler_code(tool),
    "  }",
    ")",
    ""
  )

  lines
}

#' Generate property code for a parameter
#'
#' @param param Parameter information
#' @return Character string with property code
generate_property_code <- function(param) {
  type <- param$type
  name <- param$name
  desc <- param$description
  required <- param$required

  if (type == "enum" && !is.null(param$enum_values)) {
    values_str <- paste0(
      "c(",
      paste0("\"", param$enum_values, "\"", collapse = ", "),
      ")"
    )
    return(paste0(
      name,
      " = property_enum(\"",
      name,
      "\", \"",
      desc,
      "\", values = ",
      values_str,
      ", required = ",
      required,
      ")"
    ))
  }

  func_name <- switch(
    type,
    "string" = "property_string",
    "number" = "property_number",
    "integer" = "property_number", # Use property_number for integers
    "boolean" = "property_boolean",
    "array" = "property_array",
    "object" = "property_object",
    "property_string" # default
  )

  paste0(
    name,
    " = ",
    func_name,
    "(\"",
    name,
    "\", \"",
    desc,
    "\", required = ",
    required,
    ")"
  )
}

#' Generate handler code that calls the original function
#'
#' @param tool Tool information
#' @return Character vector of handler code lines
generate_handler_code <- function(tool) {
  if (length(tool$params) == 0) {
    return(c(
      paste0("    result <- ", tool$function_name, "()"),
      "    response_text(result)"
    ))
  }

  # Generate parameter extraction and function call
  param_names <- names(tool$params)
  args_str <- paste0("params$", param_names, collapse = ", ")

  c(
    paste0("    result <- ", tool$function_name, "(", args_str, ")"),
    "    response_text(result)"
  )
}

# Helper function for null coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x

