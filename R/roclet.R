#' MCP Roclet for Generating MCP Servers
#'
#' This roclet automatically generates MCP (Model Context Protocol) servers
#' from R functions annotated with @mcp tags.
#'
#' @importFrom roxygen2 roclet roclet_process roclet_output roxy_tag_parse roxy_tag_rd block_get_tags roxy_tag_warning block_get_tag_value
#' @export
#' @examples
#' \dontrun{
#' # Use the roclet in roxygenise
#' roxygen2::roxygenise(roclets = c("rd", "mcpr::mcp_roclet"))
#' }
mcp_roclet <- function() {
  roxygen2::roclet("mcp")
}

# Custom tags are automatically recognized by having roxy_tag_parse methods

#' Parse @mcp tag
#'
#' Parses @mcp tags to extract tool name and description.
#'
#' @param x A roxy_tag object
#' @method roxy_tag_parse roxy_tag_mcp
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

#' Parse @type tag
#'
#' Parses @type tags to extract parameter type information.
#' Format: @type param_name type enum_values
#'
#' @param x A roxy_tag object
#' @method roxy_tag_parse roxy_tag_type
#' @export
roxy_tag_parse.roxy_tag_type <- function(x) {
  # Parse the raw content
  raw <- trimws(x$raw)

  if (raw == "") {
    roxygen2::roxy_tag_warning(x, "Empty @type tag")
    return(x)
  }

  # Split by whitespace
  parts <- strsplit(raw, "\\s+")[[1]]

  if (length(parts) < 2) {
    roxygen2::roxy_tag_warning(
      x,
      "Invalid @type format. Expected: param_name type [enum_values]"
    )
    return(x)
  }

  param_name <- parts[1]
  param_type <- parts[2]

  # Validate type
  valid_types <- c(
    "string",
    "number",
    "integer",
    "boolean",
    "array",
    "object",
    "enum"
  )

  # Handle enum type specially
  if (grepl("^enum:", param_type)) {
    param_type_base <- "enum"
    enum_string <- gsub("^enum:", "", param_type)
    enum_values <- trimws(strsplit(enum_string, ",")[[1]])
  } else {
    param_type_base <- param_type
    enum_values <- NULL
  }

  if (!param_type_base %in% valid_types) {
    roxygen2::roxy_tag_warning(
      x,
      paste(
        "Invalid parameter type:",
        param_type_base,
        ". Valid types:",
        paste(valid_types, collapse = ", ")
      )
    )
    return(x)
  }

  x$val <- list(
    param_name = param_name,
    type = param_type_base,
    enum_values = enum_values
  )

  x
}

#' Roxygen2 tag for @mcp
#' This function is called by Roxygen2 to generate documentation for the @mcp tag
#' @param x Roxygen2 tag object
#' @param base_path Base path for the package
#' @param env Environment
#' @return NULL (invisible)
#' @method roxy_tag_rd roxy_tag_mcp
#' @export
roxy_tag_rd.roxy_tag_mcp <- function(x, base_path, env) {
  # @mcp tags are not for .Rd generation, only for MCP roclet
  NULL
}

#' Roxygen2 tag handler for @type
#' This function is called by Roxygen2 to generate documentation for the @type
#' @param x Roxygen2 tag object
#' @param base_path Base path for the package
#' @param env Environment
#' @return NULL (invisible)
#' @method roxy_tag_rd roxy_tag_type
#' @export
roxy_tag_rd.roxy_tag_type <- function(x, base_path, env) {
  # @type tags are not for .Rd generation, only for MCP roclet
  NULL
}

#' Process blocks for MCP roclet
#'
#' @param x MCP roclet object
#' @param blocks List of roxy_block objects
#' @param env Environment
#' @param base_path Base path
#' @return List of processed MCP tools
#' @method roclet_process roclet_mcp
#' @export
roclet_process.roclet_mcp <- function(x, blocks, env, base_path) {
  tools <- list()

  for (block in blocks) {
    # Check if block has @mcp tag
    mcp_tags <- roxygen2::block_get_tags(block, "mcp")

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
#' @method roclet_output roclet_mcp
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
#' @keywords internal
process_mcp_block <- function(block) {
  # Get @mcp tag
  mcp_tags <- roxygen2::block_get_tags(block, "mcp")
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
    roxygen2::block_get_tag_value(block, "title") %||%
    paste("Tool for", func_name)

  # Get parameters and types
  param_tags <- roxygen2::block_get_tags(block, "param")
  type_tags <- roxygen2::block_get_tags(block, "type")

  # Build parameter information
  params <- build_param_info(param_tags, type_tags)

  list(
    name = tool_name,
    description = tool_description,
    function_name = func_name,
    params = params,
    file = block$file,
    line = block$line
  )
}

#' Build parameter information from @param and @type tags
#'
#' @param param_tags List of @param tags
#' @param type_tags List of @type tags
#' @return List of parameter information
#' @keywords internal
build_param_info <- function(param_tags, type_tags = list()) {
  params <- list()

  # Create lookup table for parameter types
  type_lookup <- list()
  for (type_tag in type_tags) {
    if (!is.null(type_tag$val$param_name)) {
      type_lookup[[type_tag$val$param_name]] <- type_tag$val
    }
  }

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
      # Get type information from @type tags
      type_info <- type_lookup[[name]]
      param_type <- if (!is.null(type_info)) type_info$type else "string"
      enum_values <- if (!is.null(type_info)) type_info$enum_values else NULL

      params[[name]] <- list(
        name = name,
        description = param_desc,
        type = param_type,
        enum_values = enum_values,
        required = TRUE # Default to required, could be enhanced later
      )
    }
  }

  params
}


#' Generate MCP server R code
#'
#' @param tools List of tool information
#' @param base_path Base path for the package
#' @return Character vector of R code lines
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
    gsub("\"", "'", desc),
    "\", required = ",
    required,
    ")"
  )
}

#' Generate handler code that calls the original function
#'
#' @param tool Tool information
#' @return Character vector of handler code lines
#' @keywords internal
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
