# Create an enum property with predefined values

Create an enum property with predefined values

## Usage

``` r
property_enum(title, description, values, required = FALSE)
```

## Arguments

- title:

  Short title for the property

- description:

  Longer description of the property

- values:

  Character vector of allowed values

- required:

  Whether the property is required

## Value

An enum property object

## Examples

``` r
status_prop <- property_enum(
  "Status",
  "User account status",
  values = c("active", "pending", "suspended"),
  required = TRUE
)
```
