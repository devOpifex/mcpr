# Convert ellmer tools to mcpr tools

This function converts tools from the ellmer package to a format
compatible with the mcpr package. It takes an ellmer ToolDef object and
creates an mcpr tool object with the appropriate input schema and
handler function.

## Usage

``` r
ellmer_to_mcpr_tool(ellmer_tool)
```

## Arguments

- ellmer_tool:

  An ellmer ToolDef object created with
  [`ellmer::tool()`](https://ellmer.tidyverse.org/reference/tool.html)

## Value

An mcpr tool object compatible with
[`new_tool()`](https://mcpr.opifex.org/reference/new_tool.md)

## See also

[`new_tool()`](https://mcpr.opifex.org/reference/new_tool.md),
[`ellmer::tool()`](https://ellmer.tidyverse.org/reference/tool.html),
[`add_capability()`](https://mcpr.opifex.org/reference/add_capability.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Create an ellmer tool
ellmer_rnorm <- ellmer::tool(
  rnorm,
  "Generate random normal numbers",
  n = ellmer::type_integer("Number of observations"),
  mean = ellmer::type_number("Mean value", required = FALSE),
  sd = ellmer::type_number("Standard deviation", required = FALSE)
)

# Convert to mcpr format
mcpr_tool <- ellmer_to_mcpr_tool(ellmer_rnorm)

# Add to an mcpr server
server <- new_server("MyServer", "Test server", "1.0.0")
add_capability(server, mcpr_tool)
} # }
```
