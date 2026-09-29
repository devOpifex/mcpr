# Create an array property definition

Create an array property definition

## Usage

``` r
property_array(
  title,
  description,
  items,
  required = FALSE,
  min_items = NULL,
  max_items = NULL,
  unique_items = FALSE
)
```

## Arguments

- title:

  Short title for the property

- description:

  Longer description of the property

- items:

  Schema for the items in the array

- required:

  Whether the property is required

- min_items:

  Optional minimum number of items

- max_items:

  Optional maximum number of items

- unique_items:

  Logical indicating if items must be unique

## Value

An array property object

## Examples

``` r
tags_prop <- property_array(
  "Tags",
  "List of tags for the user",
  items = property_string("Tag", "A user tag"),
  unique_items = TRUE
)
```
