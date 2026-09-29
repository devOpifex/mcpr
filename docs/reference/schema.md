# Create a new input schema

Create a new input schema

## Usage

``` r
schema(properties, type = "object", additional_properties = FALSE)
```

## Arguments

- properties:

  List of property definitions created with properties()

- type:

  Type of the schema (default: "object")

- additional_properties:

  Whether additional properties are allowed

## Value

A list representing a JSON Schema object

## Examples

``` r
schema <- schema(
  properties = properties(
    name = property_string("User name", "The name of the user", required = TRUE),
    age = property_number("User age", "The age of the user in years", minimum = 0)
  )
)
```
