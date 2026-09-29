# Create a new tool

Create a new tool

## Usage

``` r
new_tool(name, description, input_schema, handler)
```

## Arguments

- name:

  Name of the tool

- description:

  Description of the tool

- input_schema:

  Input schema for the tool (must be a schema object)

- handler:

  Function to handle the tool execution

## Value

A new tool capability

## Examples

``` r
tool <- new_tool(
  name = "My Tool",
  description = "This is a description",
  input_schema = schema(
    properties = list(
      input1 = property_string("Input 1", "Description of input 1"),
      input2 = property_number("Input 2", "Description of input 2")
    )
  ),
  handler = function(input) {
    # Process the input here
    return(input)
  }
)
```
