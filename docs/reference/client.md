# Create a new mcp IO

Create a new mcp IO

## Usage

``` r
new_client_io(command, args = character(), name, version = "1.0.0")

new_client_http(endpoint, name, version = "1.0.0", headers = list())
```

## Arguments

- command:

  The command to run

- args:

  Arguments to pass to the command

- name:

  The name of the client

- version:

  The version of the client

- endpoint:

  The endpoint to connect to

- headers:

  A named list (or named character vector) of HTTP headers to send with
  every request, e.g. `list(Authorization = "Bearer <token>")`.

## Value

A new mcp client
