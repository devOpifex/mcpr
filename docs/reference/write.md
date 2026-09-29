# Write a JSON-RPC request to a client provider

Write a JSON-RPC request to a client provider

## Usage

``` r
write(x, method, params = NULL, id = generate_id(), timeout = 5000)
```

## Arguments

- x:

  A client provider

- method:

  The method to call

- params:

  The parameters to pass to the method

- id:

  The id of the request

- timeout:

  Timeout in milliseconds for reading the response

## Value

The client provider
