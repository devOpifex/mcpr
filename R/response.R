#' Create a tool_response object
#'
#' @param text Text content
#' @param image Image content
#' @param audio Audio content
#' @param video Video content
#' @param file File content
#' @param resource Resource content
#' @param type Type of tool_response
#' @param isError Whether the tool_response is an error
#'
#' @name tool_response
#' @return A tool_response object
#' @export
tool_response_text <- function(text) {
  tool_response(text = text, type = "text")
}

#' @rdname tool_response
#' @export
tool_response_image <- function(image, mime_type = "image/png") {
  tool_response(data = image, type = "image", mime_type = mime_type)
}

#' @rdname tool_response
#' @export
tool_response_audio <- function(audio, mime_type = "audio/mpeg") {
  tool_response(data = audio, type = "audio", mime_type = mime_type)
}

#' @rdname tool_response
#' @export
tool_response_video <- function(video, mime_type = "video/mp4") {
  tool_response(data = video, type = "video", mime_type = mime_type)
}

#' @rdname tool_response
#' @export
tool_response_file <- function(file, mime_type = "application/octet-stream") {
  tool_response(data = file, type = "file", mime_type = mime_type)
}

#' @rdname tool_response
#' @export
tool_response_resource <- function(resource) {
  tool_response(resource = resource, type = "resource")
}

#' @rdname tool_response
#' @export
tool_response_error <- function(text) {
  tool_response(text = text, type = "text", isError = TRUE)
}

#' @rdname tool_response
#' @export
tool_response <- function(
  ...,
  type = c("text", "image", "audio", "video", "file", "resource"),
  isError = FALSE
) {
  type <- match.arg(type)

  structure(
    list(
      content = list(type = type, ...),
      isError = isError
    ),
    class = c(
      "tool_response",
      sprintf("tool_response_%s", type),
      "list"
    )
  )
}
