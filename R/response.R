#' Create a response object
#'
#' @param text Text content
#' @param image Image content
#' @param audio Audio content
#' @param video Video content
#' @param file File content
#' @param resource Resource content
#' @param is_error Whether the response is an error
#' @param mime_type Mime type of the content
#' @param ... Mutliple `response` objects
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
  response_item(text = as.character(text), type = "text")
}

#' @rdname response
#' @export
response_image <- function(image, mime_type = "image/png") {
  response_item(data = image, type = "image", mimeType = mime_type)
}

#' @rdname response
#' @export
response_audio <- function(audio, mime_type = "audio/mpeg") {
  response_item(data = audio, type = "audio", mimeType = mime_type)
}

#' @rdname response
#' @export
response_video <- function(video, mime_type = "video/mp4") {
  response_item(data = video, type = "video", mimeType = mime_type)
}

#' @rdname response
#' @export
response_file <- function(file, mime_type = "application/octet-stream") {
  response_item(data = file, type = "file", mimeType = mime_type)
}

#' @rdname response
#' @export
response_resource <- function(resource) {
  response_item(resource = resource, type = "resource")
}

#' @rdname response
#' @export
response_error <- function(text) {
  response_item(text = text, type = "text", isError = TRUE)
}

response_item <- function(
  ...,
  type = c("text", "image", "audio", "video", "file", "resource")
) {
  type <- match.arg(type)

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
  structure(
    list(
      content = list(...),
      isError = is_error
    ),
    class = c(
      "response",
      "list"
    )
  )
}
