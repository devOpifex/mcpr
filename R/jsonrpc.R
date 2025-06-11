#' JSON-RPC 2.0 Standard Error Codes
#' @keywords internal
JSONRPC_PARSE_ERROR <- -32700
JSONRPC_INVALID_REQUEST <- -32600
JSONRPC_METHOD_NOT_FOUND <- -32601
JSONRPC_INVALID_PARAMS <- -32602
JSONRPC_INTERNAL_ERROR <- -32603

#' Convert an R object to JSON
#'
#' @param x R object to convert to JSON
#' @param ... Additional arguments passed to yyjsonr::write_json_str
#'
#' @return JSON string
#' @keywords internal
to_json <- function(x, ...) {
  yyjsonr::write_json_str(x, list(auto_unbox = TRUE), ...)
}

#' Parse JSON to an R object
#'
#' @param json JSON string to parse
#' @param ... Additional arguments passed to yyjsonr::read_json_str
#'
#' @return R object
#' @keywords internal
from_json <- function(json, ...) {
  yyjsonr::read_json_str(
    json,
    opts = list(
      arr_of_objs_to_df = FALSE,
      obj_of_arrs_to_df = FALSE
    ),
    ...
  )
}

#' Create a JSON-RPC 2.0 error response
#'
#' @param id Request identifier
#' @param code Error code
#' @param message Error message
#' @param data Optional error data
#'
#' @return A structured JSON-RPC 2.0 error response
#' @keywords internal
create_error <- function(code, message, data = NULL, id = NULL) {
  error <- list(code = code, message = message)
  if (!is.null(data)) error$data <- data
  if (is.null(id)) id <- generate_id()

  structure(
    list(
      jsonrpc = "2.0",
      error = error,
      id = id
    ),
    class = c("jsonrpc_error", "list")
  )
}

#' Create a JSON-RPC 2.0 success response
#'
#' @param id Request identifier
#' @param result Result data
#'
#' @return A structured JSON-RPC 2.0 success response
#' @keywords internal
create_response <- function(result, id = NULL) {
  if (is.null(id)) id <- generate_id()
  structure(
    list(
      jsonrpc = "2.0",
      result = result,
      id = id
    ),
    class = c("jsonrpc_response", "list")
  )
}

send <- function(response, con = stdout()) UseMethod("send")

#' @export
#' @method send jsonrpc_error
send.jsonrpc_error <- function(response, con = stdout()) {
  # Check if it's a notification
  if (is.null(response$id)) {
    return(NULL)
  }

  # Send the response
  cat(to_json(response), "\n", file = con)
  flush(con)
}

#' @export
#' @method send jsonrpc_response
send.jsonrpc_response <- function(response, con = stdout()) {
  # Check if it's a notification
  if (is.null(response$id)) {
    return(NULL)
  }

  # Send the response
  cat(to_json(response), "\n", file = con)
  flush(con)
}

#' Validate a JSON-RPC 2.0 request
#'
#' @param request Parsed request object
#'
#' @return NULL if valid, error response if invalid
#' @keywords internal
validate_request <- function(request) {
  # Check JSON-RPC version
  if (is.null(request$jsonrpc) || request$jsonrpc != "2.0") {
    return(create_error(
      id = request$id,
      JSONRPC_INVALID_REQUEST,
      "Invalid JSON-RPC version"
    ))
  }

  # Check method is present and a string
  if (is.null(request$method) || !is.character(request$method)) {
    return(create_error(
      id = request$id,
      JSONRPC_INVALID_REQUEST,
      "Method must be a string"
    ))
  }

  # If params exist, they must be object or array
  if (
    !is.null(request$params) &&
      !is.list(request$params) &&
      !is.vector(request$params)
  ) {
    return(create_error(
      id = request$id,
      JSONRPC_INVALID_REQUEST,
      "Params must be object or array"
    ))
  }

  NULL
}

#' Process a JSON-RPC 2.0 request
#'
#' @param request Parsed request object
#' @param mcp MCP server object
#'
#' @return Response object or NULL for notifications
#' @export
process_request <- function(request, mcp) {
  # First validate the request
  validation_error <- validate_request(request)
  if (!is.null(validation_error)) {
    return(validation_error)
  }

  # Extract request components
  method <- request$method
  params <- request$params
  id <- request$id

  # it's a notification (no id)
  # we shoud not return a response
  if (is.null(id)) {
    process_notification(request, mcp)
    return(NULL)
  }

  # Process the method
  result <- switch(
    method,
    "initialize" = {
      initialize(mcp)
    },
    "tools/list" = {
      tools_list(mcp)
    },
    "tools/call" = {
      tools_call(mcp, params, id)
    },
    "resources/list" = {
      resources_list(mcp)
    },
    "resources/read" = {
      resources_read(mcp, params, id)
    },
    "prompts/list" = {
      prompts_list(mcp)
    },
    "prompts/get" = {
      prompts_get(mcp, params, id)
    },
    NULL # For method not found
  )

  if (inherits(result, "jsonrpc_response")) {
    return(result)
  }

  if (inherits(result, "jsonrpc_error")) {
    return(result)
  }

  if (length(result)) {
    return(create_response(result, id = id))
  }

  create_error(
    id = id,
    JSONRPC_METHOD_NOT_FOUND,
    paste("Method not found:", method)
  )
}

process_notification <- function(request, mcp) {
  switch(
    request$method,
    "initialized" = {
      attr(mcp, "initialized") <- TRUE
    },
    NULL # For method not found
  )
}

#' Process batch JSON-RPC 2.0 requests
#'
#' @param batch_requests List of request objects
#' @param mcp MCP server object
#'
#' @return List of response objects
#' @export
process_batch <- function(batch_requests, mcp) {
  # Validate batch structure
  if (!is.list(batch_requests) || length(batch_requests) == 0) {
    return(list(create_error(
      JSONRPC_INVALID_REQUEST,
      "Invalid batch request - must be non-empty array"
    )))
  }

  # Process each request in the batch
  responses <- list()
  for (request in batch_requests) {
    # Process each request individually
    # No need to validate again, as process_request already does validation
    response <- process_request(request, mcp)

    # Only add non-notification responses to the batch response
    if (!is.null(response)) {
      responses <- c(responses, list(response))
    }
  }

  responses
}

#' Parse and process a JSON-RPC 2.0 request
#'
#' @param json_text JSON request text
#' @param mcp MCP server object
#'
#' @return JSON response text or NULL for notifications
#' @export
parse_request <- function(json_text, mcp) {
  tryCatch(
    {
      # Parse the JSON request
      request <- from_json(json_text)

      # Check if it's a batch request (array of requests)
      if (is.list(request) && !length(names(request))) {
        responses <- process_batch(request, mcp)
        if (length(responses) == 0) {
          return(NULL) # All were notifications
        }
        return(to_json(responses))
      } else {
        # Single request
        response <- process_request(request, mcp)

        # Notifications don't get responses
        if (is.null(response)) {
          return(NULL)
        }

        return(response)
      }
    },
    error = function(e) {
      create_error(
        JSONRPC_PARSE_ERROR,
        paste("Parse error:", e$message)
      )
    }
  )
}
