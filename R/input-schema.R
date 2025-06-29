#' Create a new input schema
#'
#' @param properties List of property definitions created with properties()
#' @type properties object
#' @param type Type of the schema (default: "object")
#' @type type string
#' @param additional_properties Whether additional properties are allowed
#' @type additional_properties boolean
#'
#' @return A list representing a JSON Schema object
#' @mcp create_json_schema Create a JSON Schema object for MCP tool inputs with property validation
#' @export
#'
#' @examples
#' schema <- schema(
#'   properties = properties(
#'     name = property_string("User name", "The name of the user", required = TRUE),
#'     age = property_number("User age", "The age of the user in years", minimum = 0)
#'   )
#' )
schema <- function(properties, type = "object", additional_properties = FALSE) {
  stopifnot(!missing(properties))

  # Extract required properties
  required <- character(0)
  for (name in names(properties)) {
    property <- properties[[name]]
    if (attr(property, "required")) {
      required <- c(required, name)
    }
  }

  # Build schema
  schema <- list(
    type = type,
    properties = properties,
    additionalProperties = additional_properties
  )

  # Add required list if any properties are required
  if (length(required) > 0) {
    schema$required <- as.list(required)
  }

  # Set class
  class(schema) <- c("schema", "list")

  return(schema)
}

#' Create a new properties list
#'
#' @param ... Property objects
#'
#' @return A list of property objects
#' @export
#'
#' @examples
#' properties <- properties(
#'   property_string("Name", "The name of the user", required = TRUE),
#'   property_number("Age", "The age of the user in years", minimum = 0)
#' )
properties <- function(...) {
  structure(
    list(...),
    class = c("properties", "list")
  )
}

#' Create a new property
#'
#' @param type Type of the property
#' @param title Short title for the property
#' @param description Longer description of the property
#' @param required Whether the property is required
#' @param ... Additional attributes for the property
#'
#' @return A property object with the specified attributes
#' @keywords internal
new_property <- function(type, title, description, required = FALSE, ...) {
  cls <- sprintf("property_%s", type)

  property <- list(
    type = type,
    title = title,
    description = description,
    ...
  )

  structure(
    property,
    required = required,
    class = c("property", cls, class(property))
  )
}

#' Create a string property definition
#'
#' @param title Short title for the property
#' @type title string
#' @param description Longer description of the property
#' @type description string
#' @param required Whether the property is required
#' @type required boolean
#' @param enum Optional character vector of allowed values
#' @type enum array
#' @param pattern Optional regex pattern the string must match
#' @type pattern string
#' @param min_length Optional minimum length
#' @type min_length number
#' @param max_length Optional maximum length
#' @type max_length number
#' @param format Optional format constraint
#' @type format enum:date-time,email,uri,hostname,ipv4,ipv6
#'
#' @return A string property object
#' @mcp create_string_property Create a string property with validation constraints for MCP schemas
#' @export
#'
#' @examples
#' name_prop <- property_string(
#'   "User name",
#'   "The name of the user",
#'   required = TRUE,
#'   min_length = 2,
#'   max_length = 50
#' )
property_string <- function(
  title,
  description,
  required = FALSE,
  enum = NULL,
  pattern = NULL,
  min_length = NULL,
  max_length = NULL,
  format = NULL
) {
  args <- list()

  if (!is.null(enum)) args$enum <- enum
  if (!is.null(pattern)) args$pattern <- pattern
  if (!is.null(min_length)) args$minLength <- min_length
  if (!is.null(max_length)) args$maxLength <- max_length
  if (!is.null(format)) args$format <- format

  do.call(
    new_property,
    c(
      list(
        type = "string",
        title = title,
        description = description,
        required = required
      ),
      args
    )
  )
}

#' Create a number property definition
#'
#' @param title Short title for the property
#' @type title string
#' @param description Longer description of the property
#' @type description string
#' @param required Whether the property is required
#' @type required boolean
#' @param minimum Optional minimum value
#' @type minimum number
#' @param maximum Optional maximum value
#' @type maximum number
#' @param exclusive_minimum Whether minimum is exclusive
#' @type exclusive_minimum boolean
#' @param exclusive_maximum Whether maximum is exclusive
#' @type exclusive_maximum boolean
#' @param multiple_of Optional value the number must be a multiple of
#' @type multiple_of number
#' @param integer Whether the number should be an integer
#' @type integer boolean
#'
#' @return A number property object
#' @mcp create_number_property Create a number property with range and type constraints for MCP schemas
#' @export
#'
#' @examples
#' age_prop <- property_number(
#'   "User age",
#'   "The age of the user in years",
#'   required = TRUE,
#'   minimum = 0,
#'   integer = TRUE
#' )
property_number <- function(
  title,
  description,
  required = FALSE,
  minimum = NULL,
  maximum = NULL,
  exclusive_minimum = NULL,
  exclusive_maximum = NULL,
  multiple_of = NULL,
  integer = FALSE
) {
  type <- if (integer) "integer" else "number"

  args <- list()

  if (!is.null(minimum)) args$minimum <- minimum
  if (!is.null(maximum)) args$maximum <- maximum
  if (!is.null(exclusive_minimum)) args$exclusiveMinimum <- exclusive_minimum
  if (!is.null(exclusive_maximum)) args$exclusiveMaximum <- exclusive_maximum
  if (!is.null(multiple_of)) args$multipleOf <- multiple_of

  do.call(
    new_property,
    c(
      list(
        type = type,
        title = title,
        description = description,
        required = required
      ),
      args
    )
  )
}

#' Create a boolean property definition
#'
#' @param title Short title for the property
#' @type title string
#' @param description Longer description of the property
#' @type description string
#' @param required Whether the property is required
#' @type required boolean
#'
#' @return A boolean property object
#' @mcp create_boolean_property Create a boolean property for MCP schemas
#' @export
#'
#' @examples
#' active_prop <- property_boolean(
#'   "Active status",
#'   "Whether the user account is active",
#'   required = TRUE
#' )
property_boolean <- function(title, description, required = FALSE) {
  new_property(
    type = "boolean",
    title = title,
    description = description,
    required = required
  )
}

#' Create an array property definition
#'
#' @param title Short title for the property
#' @param description Longer description of the property
#' @param items Schema for the items in the array
#' @param required Whether the property is required
#' @param min_items Optional minimum number of items
#' @param max_items Optional maximum number of items
#' @param unique_items Logical indicating if items must be unique
#'
#' @return An array property object
#' @export
#'
#' @examples
#' tags_prop <- property_array(
#'   "Tags",
#'   "List of tags for the user",
#'   items = property_string("Tag", "A user tag"),
#'   unique_items = TRUE
#' )
property_array <- function(
  title,
  description,
  items,
  required = FALSE,
  min_items = NULL,
  max_items = NULL,
  unique_items = FALSE
) {
  args <- list(items = items)

  if (!is.null(min_items)) args$minItems <- min_items
  if (!is.null(max_items)) args$maxItems <- max_items
  if (unique_items) args$uniqueItems <- TRUE

  do.call(
    new_property,
    c(
      list(
        type = "array",
        title = title,
        description = description,
        required = required
      ),
      args
    )
  )
}

#' Create an object property definition
#'
#' @param title Short title for the property
#' @param description Longer description of the property
#' @param properties List of property definitions for this object
#' @param required Whether the property is required
#' @param additional_properties Logical indicating if additional properties are allowed
#'
#' @return An object property object
#' @export
#'
#' @examples
#' address_prop <- property_object(
#'   "Address",
#'   "User's address information",
#'   properties = list(
#'     street = property_string("Street", "Street address", required = TRUE),
#'     city = property_string("City", "City name", required = TRUE),
#'     country = property_string("Country", "Country name")
#'   )
#' )
property_object <- function(
  title,
  description,
  properties,
  required = FALSE,
  additional_properties = FALSE
) {
  new_property(
    type = "object",
    title = title,
    description = description,
    required = required,
    properties = properties,
    additionalProperties = additional_properties
  )
}

#' Create an enum property with predefined values
#'
#' @param title Short title for the property
#' @type title string
#' @param description Longer description of the property
#' @type description string
#' @param values Character vector of allowed values
#' @type values array
#' @param required Whether the property is required
#' @type required boolean
#'
#' @return An enum property object
#' @mcp create_enum_property Create an enum property with predefined values for MCP schemas
#' @export
#'
#' @examples
#' status_prop <- property_enum(
#'   "Status",
#'   "User account status",
#'   values = c("active", "pending", "suspended"),
#'   required = TRUE
#' )
property_enum <- function(title, description, values, required = FALSE) {
  if (length(values) == 0) {
    stop("Enum property must have at least one value")
  }

  new_property(
    type = "string",
    title = title,
    description = description,
    required = required,
    enum = values
  )
}
