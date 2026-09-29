# Create a number property definition

Create a number property definition

## Usage

``` r
property_number(
  title,
  description,
  required = FALSE,
  minimum = NULL,
  maximum = NULL,
  exclusive_minimum = NULL,
  exclusive_maximum = NULL,
  multiple_of = NULL,
  integer = FALSE
)
```

## Arguments

- title:

  Short title for the property

- description:

  Longer description of the property

- required:

  Whether the property is required

- minimum:

  Optional minimum value

- maximum:

  Optional maximum value

- exclusive_minimum:

  Whether minimum is exclusive

- exclusive_maximum:

  Whether maximum is exclusive

- multiple_of:

  Optional value the number must be a multiple of

- integer:

  Whether the number should be an integer

## Value

A number property object

## Examples

``` r
age_prop <- property_number(
  "User age",
  "The age of the user in years",
  required = TRUE,
  minimum = 0,
  integer = TRUE
)
```
