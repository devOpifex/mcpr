#' Write a JSON-RPC request to a client provider
#'
#' @param x A client provider
#' @param method The method to call
#' @param params The parameters to pass to the method
#' @param id The id of the request
#' @param timeout Timeout in milliseconds for reading the response
#'
#' @return The client provider
#' @export
write <- function(x, method, params = NULL, id = generate_id(), timeout = 5000)
  UseMethod("write")

#' @method write client_io
#' @export
write.client_io <- function(
  x,
  method,
  params = NULL,
  id = generate_id(),
  timeout = 5000
) {
  # Check if process is alive before writing
  if (!x$is_alive()) {
    stop("client process is not alive")
  }

  # Create and write the request with proper newline
  r <- rpc_request(method, params, id)
  x$write_input(sprintf("%s\n", r))

  # Read the response with the specified timeout
  read(x, timeout)
}

#' @method write client_http
#' @export
write.client_http <- function(
  x,
  method,
  params = NULL,
  id = generate_id(),
  timeout = 5000
) {
  r <- rpc_request(method, params, id, convert = FALSE)

  x |>
    httr2::req_timeout(timeout) |>
    httr2::req_body_json(r) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}

#' Read a JSON-RPC response from a client provider
#'
#' @param x A client provider
#' @param timeout Timeout in milliseconds for reading the response
#'
#' @return The response
#' @export
read <- function(x, timeout = 60 * 1000) UseMethod("read")

#' @method read client_io
#' @export
read.client_io <- function(x, timeout = 60 * 1000) {
  # Check if process is alive before reading
  if (!x$is_alive()) {
    stop("client process is not alive")
  }

  # Use poll_io with timeout to wait for response
  poll_result <- x$poll_io(timeout)

  # Check if we have data to read
  if (poll_result["output"] == "ready") {
    # Read the output when it's ready

    res <- tryCatch(
      {
        res <- x$read_output()
        from_json(res)
      },
      error = function(e) {
        warning("Invalid JSON response: ", e$message)
        NULL
      }
    )

    return(res)
  }

  if (poll_result["output"] == "timeout") {
    warning("Timeout waiting for client response")
    return(NULL)
  }
  # Process closed the connection or other issue
  warning("client connection closed or error")
  return(NULL)
}

rpc_request <- function(
  method,
  params = NULL,
  id = generate_id(),
  convert = TRUE
) {
  r <- list(
    jsonrpc = "2.0",
    method = method,
    params = params,
    id = id
  )

  r <- Filter(Negate(is.null), r)

  if (!convert) return(r)

  to_json(r) |>
    as.character()
}
