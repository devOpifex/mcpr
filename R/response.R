#' Create a response object
#'
#' @param text Text content
#' @param image Image content
#' @param audio Audio content
#' @param video Video content
#' @param file File content
#' @param resource Resource content
#' @param type Type of response
#' @param isError Whether the response is an error
#' @param mime_type Mime type of the content
#' @param ... Mutliple `response` objects
#'
#' @name response
#' @return A response object
#' @export
response_text <- function(text) {
  response(text = as.character(text), type = "text")
}

#' @rdname response
#' @export
response_image <- function(image, mime_type = "image/png") {
  response(data = image, type = "image", mimeType = mime_type)
}

#' @rdname response
#' @export
response_audio <- function(audio, mime_type = "audio/mpeg") {
  response(data = audio, type = "audio", mimeType = mime_type)
}

#' @rdname response
#' @export
response_video <- function(video, mime_type = "video/mp4") {
  response(data = video, type = "video", mimeType = mime_type)
}

#' @rdname response
#' @export
response_file <- function(file, mime_type = "application/octet-stream") {
  response(data = file, type = "file", mimeType = mime_type)
}

#' @rdname response
#' @export
response_resource <- function(resource) {
  response(resource = resource, type = "resource")
}

#' @rdname response
#' @export
response_error <- function(text) {
  response(text = text, type = "text", isError = TRUE)
}

#' @rdname response
#' @export
response <- function(
  ...,
  type = c("text", "image", "audio", "video", "file", "resource"),
  isError = FALSE
) {
  type <- match.arg(type)

  structure(
    list(
      content = list(list(type = type, ...)),
      isError = isError
    ),
    class = c(
      "response",
      sprintf("response_%s", type),
      "list"
    )
  )
}

responses <- function(...) {
  structure(
    list(...),
    class = c("responses", "list")
  )
}
