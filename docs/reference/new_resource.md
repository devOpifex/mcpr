# Create a new resource

Create a new resource

## Usage

``` r
new_resource(name, description, uri, mime_type = NULL, handler)
```

## Arguments

- name:

  Name of the resource

- description:

  Description of the resource

- uri:

  URI of the resource

- mime_type:

  MIME type of the resource (optional)

- handler:

  Function to handle the resource request

## Value

A new resource capability

## Examples

``` r
resource <- new_resource(
  name = "My Resource",
  description = "This is a description",
  uri = "https://example.com/resource",
  mime_type = "text/plain",
  handler = function(params) {
    # Process the resource request
    return(list(content = "Resource content"))
  }
)
```
