#' Create a response object
#'
#' @param text Text content
#' @param image Image content
#' @param audio Audio content
#' @param video Video content
#' @param file File content
#' @param type Type of the content
#' @param resource Resource content
#' @param is_error Whether the response is an error
#' @param mime_type Mime type of the content
#' @param ... Mutliple `response` objects
#'
#' @details Use `response_item` to create a custom response item.
#'
#' @examples
#' response(
#'   response_text("Hello, world!"),
#'   response_image(system.file("extdata/logo.png", package = "mcpr")),
#'   response_audio(system.file("extdata/sound.mp3", package = "mcpr")),
#'   response_video(system.file("extdata/video.mp4", package = "mcpr")),
#'   response_file(system.file("extdata/file.txt", package = "mcpr")),
#'   response_resource(system.file("extdata/resource.json", package = "mcpr"))
#' )
#'
#' @name response
#' @return A response object
#' @export
response_text <- function(text) {
  if (missing(text)) {
    stop("'text' argument is required")
  }
  response_item(text = as.character(text), type = "text")
}

#' @rdname response
#' @export
response_image <- function(image, mime_type = "image/png") {
  if (missing(image)) {
    stop("'image' argument is required")
  }
  if (!is.character(mime_type)) {
    stop("'mime_type' must be a character string")
  }
  response_item(data = image, type = "image", mimeType = mime_type)
}

#' @rdname response
#' @export
response_audio <- function(audio, mime_type = "audio/mpeg") {
  if (missing(audio)) {
    stop("'audio' argument is required")
  }
  if (!is.character(mime_type)) {
    stop("'mime_type' must be a character string")
  }
  response_item(data = audio, type = "audio", mimeType = mime_type)
}

#' @rdname response
#' @export
response_video <- function(video, mime_type = "video/mp4") {
  if (missing(video)) {
    stop("'video' argument is required")
  }
  if (!is.character(mime_type)) {
    stop("'mime_type' must be a character string")
  }
  response_item(data = video, type = "video", mimeType = mime_type)
}

#' @rdname response
#' @export
response_file <- function(file, mime_type = "application/octet-stream") {
  if (missing(file)) {
    stop("'file' argument is required")
  }
  if (!is.character(mime_type)) {
    stop("'mime_type' must be a character string")
  }
  response_item(data = file, type = "file", mimeType = mime_type)
}

#' @rdname response
#' @export
response_resource <- function(resource) {
  if (missing(resource)) {
    stop("'resource' argument is required")
  }
  response_item(resource = resource, type = "resource")
}

#' @rdname response
#' @export
response_error <- function(text) {
  if (missing(text)) {
    stop("'text' argument is required")
  }
  response_item(text = as.character(text), type = "text", isError = TRUE)
}

#' @rdname response
#' @export
response_item <- function(
  ...,
  type = c("text", "image", "audio", "video", "file", "resource")
) {
  if (length(list(...)) == 0) {
    stop("At least one parameter must be provided")
  }

  if (length(type) > 1) {
    stop("Only one type can be specified")
  }

  # no mathc.arg to allow custom types
  type <- type[1]

  structure(
    list(
      type = type,
      ...
    ),
    class = c(
      "response_item",
      sprintf("response_item_%s", type),
      "list"
    )
  )
}

#' @rdname response
#' @export
response <- function(
  ...,
  is_error = FALSE
) {
  content <- list(...)

  if (length(content) == 0) {
    stop("At least one response item must be provided")
  }

  if (!is.logical(is_error)) {
    stop("'is_error' must be a logical value")
  }

  structure(
    list(
      content = content,
      isError = is_error
    ),
    class = c(
      "response",
      "list"
    )
  )
}
