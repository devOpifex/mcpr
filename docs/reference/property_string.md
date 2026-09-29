# Create a string property definition

Create a string property definition

## Usage

``` r
property_string(
  title,
  description,
  required = FALSE,
  enum = NULL,
  pattern = NULL,
  min_length = NULL,
  max_length = NULL,
  format = NULL
)
```

## Arguments

- title:

  Short title for the property

- description:

  Longer description of the property

- required:

  Whether the property is required

- enum:

  Optional character vector of allowed values

- pattern:

  Optional regex pattern the string must match

- min_length:

  Optional minimum length

- max_length:

  Optional maximum length

- format:

  Optional format constraint

## Value

A string property object

## Examples

``` r
name_prop <- property_string(
  "User name",
  "The name of the user",
  required = TRUE,
  min_length = 2,
  max_length = 50
)
```
