# Create a new properties list

Create a new properties list

## Usage

``` r
properties(...)
```

## Arguments

- ...:

  Property objects

## Value

A list of property objects

## Examples

``` r
properties <- properties(
  property_string("Name", "The name of the user", required = TRUE),
  property_number("Age", "The age of the user in years", minimum = 0)
)
```
