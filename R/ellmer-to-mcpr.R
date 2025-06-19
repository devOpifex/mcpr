#' Convert ellmer tools to mcpr tools
#'
#' This function converts tools from the ellmer package to a format compatible
#' with the mcpr package. It takes an ellmer ToolDef object and creates an
#' mcpr tool object with the appropriate input schema and handler function.
#'
#' @param ellmer_tool An ellmer ToolDef object created with `ellmer::tool()`
#' @return An mcpr tool object compatible with `new_tool()`
#' @export
#'
#' @examples
#' \dontrun{
#' # Create an ellmer tool
#' ellmer_rnorm <- ellmer::tool(
#'   rnorm,
#'   "Generate random normal numbers",
#'   n = ellmer::type_integer("Number of observations"),
#'   mean = ellmer::type_number("Mean value", required = FALSE),
#'   sd = ellmer::type_number("Standard deviation", required = FALSE)
#' )
#'
#' # Convert to mcpr format
#' mcpr_tool <- ellmer_to_mcpr_tool(ellmer_rnorm)
#'
#' # Add to an mcpr server
#' server <- new_server("MyServer", "Test server", "1.0.0")
#' add_capability(server, mcpr_tool)
#' }
#'
#' @seealso [new_tool()], [ellmer::tool()], [add_capability()]
#' @export
ellmer_to_mcpr_tool <- function(ellmer_tool) {
  # Validate input
  if (!inherits(ellmer_tool, "ellmer::ToolDef")) {
    stop("ellmer_tool must be a ToolDef object created with ellmer::tool()")
  }

  # Extract basic information
  name <- ellmer_tool@name
  description <- ellmer_tool@description
  fun <- ellmer_tool@fun
  convert <- ellmer_tool@convert

  # Convert ellmer type schema to mcpr input schema
  input_schema <- convert_ellmer_types_to_mcpr(ellmer_tool@arguments)

  # Create handler function that wraps the original ellmer function
  handler <- function(arguments) {
    # Convert arguments if needed (similar to ellmer's behavior)
    if (convert) {
      # Apply any argument conversion logic here if needed
      processed_args <- arguments
    } else {
      processed_args <- arguments
    }

    # Call the original function
    result <- tryCatch(
      {
        do.call(fun, processed_args)
      },
      error = function(e) {
        stop("Tool execution error: ", e$message)
      }
    )

    response(
      response_text(
        yyjsonr::write_json_str(result, list(auto_unbox = TRUE))
      )
    )
  }

  # Create the mcpr tool
  new_tool(
    name = name,
    description = description,
    input_schema = input_schema,
    handler = handler
  )
}

#' Convert ellmer TypeObject to mcpr schema
#'
#' @param type_object An ellmer TypeObject containing the function arguments
#' @return An mcpr schema object
#' @keywords internal
convert_ellmer_types_to_mcpr <- function(type_object) {
  if (!inherits(type_object, "ellmer::TypeObject")) {
    stop("Expected TypeObject from ellmer")
  }

  # Convert each property
  mcpr_properties <- list()
  for (prop_name in names(type_object@properties)) {
    ellmer_type <- type_object@properties[[prop_name]]
    mcpr_prop <- convert_ellmer_type_to_mcpr_property(ellmer_type, prop_name)
    mcpr_properties[[prop_name]] <- mcpr_prop
  }

  # Create the schema
  schema(
    properties = do.call(properties, mcpr_properties),
    additional_properties = type_object@additional_properties
  )
}

#' Convert a single ellmer type to an mcpr property
#'
#' @param ellmer_type An ellmer Type object (TypeBasic, TypeArray, etc.)
#' @param prop_name The name of the property (for better error messages)
#' @return An mcpr property object
#' @keywords internal
convert_ellmer_type_to_mcpr_property <- function(ellmer_type, prop_name) {
  description <- ellmer_type@description %||% prop_name
  required <- ellmer_type@required

  if (inherits(ellmer_type, "ellmer::TypeBasic")) {
    type <- ellmer_type@type

    switch(
      type,
      "boolean" = property_boolean(
        title = prop_name,
        description = description,
        required = required
      ),
      "integer" = property_number(
        title = prop_name,
        description = description,
        required = required,
        integer = TRUE
      ),
      "number" = property_number(
        title = prop_name,
        description = description,
        required = required,
        integer = FALSE
      ),
      "string" = property_string(
        title = prop_name,
        description = description,
        required = required
      ),
      # Default to string for unknown basic types
      property_string(
        title = prop_name,
        description = description,
        required = required
      )
    )
  } else if (inherits(ellmer_type, "ellmer::TypeEnum")) {
    property_enum(
      title = prop_name,
      description = description,
      values = ellmer_type@values,
      required = required
    )
  } else if (inherits(ellmer_type, "ellmer::TypeArray")) {
    # Convert the items type
    items_prop <- convert_ellmer_type_to_mcpr_property(
      ellmer_type@items,
      paste0(prop_name, "_item")
    )

    property_array(
      title = prop_name,
      description = description,
      items = items_prop,
      required = required
    )
  } else if (inherits(ellmer_type, "ellmer::TypeObject")) {
    # Convert nested object properties
    nested_properties <- list()
    for (nested_name in names(ellmer_type@properties)) {
      nested_type <- ellmer_type@properties[[nested_name]]
      nested_prop <- convert_ellmer_type_to_mcpr_property(
        nested_type,
        nested_name
      )
      nested_properties[[nested_name]] <- nested_prop
    }

    property_object(
      title = prop_name,
      description = description,
      properties = nested_properties,
      required = required,
      additional_properties = ellmer_type@additional_properties
    )
  } else if (inherits(ellmer_type, "ellmer::TypeJsonSchema")) {
    # For custom JSON schemas, we'll need to interpret them
    # This is a complex case that would need custom handling
    warning(
      "TypeJsonSchema conversion not fully implemented, converting to string"
    )
    property_string(
      title = prop_name,
      description = description,
      required = required
    )
  } else {
    # Unknown type, default to string
    warning(paste(
      "Unknown ellmer type:",
      class(ellmer_type)[1],
      "- converting to string"
    ))
    property_string(
      title = prop_name,
      description = description,
      required = required
    )
  }
}
