# Create a boolean property definition

Create a boolean property definition

## Usage

``` r
property_boolean(title, description, required = FALSE)
```

## Arguments

- title:

  Short title for the property

- description:

  Longer description of the property

- required:

  Whether the property is required

## Value

A boolean property object

## Examples

``` r
active_prop <- property_boolean(
  "Active status",
  "Whether the user account is active",
  required = TRUE
)
```
