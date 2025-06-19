devtools::load_all()

# Create a calculator tool
calculator <- new_tool(
  name = "math_calculator",
  description = "Performs basic arithmetic operations",
  input_schema = schema(
    properties = properties(
      operation = property_enum(
        "Operation",
        "Math operation to perform",
        values = c("add", "subtract", "multiply", "divide"),
        required = TRUE
      ),
      a = property_number("First number", "First operand", required = TRUE),
      b = property_number("Second number", "Second operand", required = TRUE)
    )
  ),
  handler = function(params) {
    result <- switch(
      params$operation,
      "add" = params$a + params$b,
      "subtract" = params$a - params$b,
      "multiply" = params$a * params$b,
      "divide" = params$a / params$b
    )
    response_text(
      sprintf("The result from the R calculator is %s", result)
    )
  }
)

current_time <- ellmer::tool(
  \(tz = "UTC") {
    format(Sys.time(), tz = tz, usetz = TRUE)
  },
  "Gets the current time in the given time zone.",
  tz = ellmer::type_string(
    "The time zone to get the current time in. Defaults to `\"UTC\"`.",
    required = FALSE
  )
)

# Create an MCP server and add the calculator tool
mcp <- new_server(
  name = "R Calculator Server",
  description = "A simple calculator server implemented in R",
  version = "1.0.0"
)

mcp <- add_capability(mcp, calculator)
mcp <- add_capability(mcp, current_time)

# Start the server (listening on stdin/stdout)
serve_io(mcp)
#serve_http(mcp, path = "/mcp")
