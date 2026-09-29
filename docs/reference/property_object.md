# Create an object property definition

Create an object property definition

## Usage

``` r
property_object(
  title,
  description,
  properties,
  required = FALSE,
  additional_properties = FALSE
)
```

## Arguments

- title:

  Short title for the property

- description:

  Longer description of the property

- properties:

  List of property definitions for this object

- required:

  Whether the property is required

- additional_properties:

  Logical indicating if additional properties are allowed

## Value

An object property object

## Examples

``` r
address_prop <- property_object(
  "Address",
  "User's address information",
  properties = list(
    street = property_string("Street", "Street address", required = TRUE),
    city = property_string("City", "City name", required = TRUE),
    country = property_string("Country", "Country name")
  )
)
```
