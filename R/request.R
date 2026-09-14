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

  # Notifications (no id) do not get a response
  if (is.null(id)) {
    return(invisible(NULL))
  }

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
  state <- attr(x, "state")

  # timeout is in milliseconds, httr2 takes seconds
  req <- x |>
    httr2::req_timeout(timeout / 1000) |>
    httr2::req_body_json(r)

  if (!is.null(state$protocol_version)) {
    req <- httr2::req_headers(
      req,
      `MCP-Protocol-Version` = state$protocol_version
    )
  }

  if (!is.null(state$session_id)) {
    req <- httr2::req_headers(req, `Mcp-Session-Id` = state$session_id)
  }

  resp <- httr2::req_perform(req)

  session_id <- httr2::resp_header(resp, "Mcp-Session-Id")
  if (!is.null(session_id)) {
    state$session_id <- session_id
  }

  # Notifications (no id) get a 202 with no body
  if (is.null(id) || !httr2::resp_has_body(resp)) {
    return(invisible(NULL))
  }

  if (identical(httr2::resp_content_type(resp), "text/event-stream")) {
    return(read_sse(httr2::resp_body_string(resp), id))
  }

  httr2::resp_body_json(resp)
}

# Extract the JSON-RPC response matching `id` from a server-sent events body,
# skipping any notifications the server sent on the stream before it
read_sse <- function(body, id) {
  events <- strsplit(gsub("\r\n", "\n", body), "\n\n", fixed = TRUE)[[1]]

  for (event in events) {
    lines <- strsplit(event, "\n", fixed = TRUE)[[1]]
    data <- sub("^data: ?", "", lines[startsWith(lines, "data:")])
    data <- paste(data, collapse = "\n")

    # servers may send a priming event with an empty data field
    if (!nzchar(trimws(data))) {
      next
    }

    msg <- jsonlite::parse_json(data)

    if (!is.null(msg$id) && same_id(msg$id, id)) {
      return(msg)
    }
  }

  warning("No response found in event stream")
  NULL
}

same_id <- function(a, b) {
  if (is.numeric(a) && is.numeric(b)) {
    return(isTRUE(all.equal(a, b, tolerance = 0)))
  }

  identical(as.character(a), as.character(b))
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

  to_json(r)
}
