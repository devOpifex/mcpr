#' Create a new mcp client
#'
#' @param command The command to run
#' @param args Arguments to pass to the command
#' @param name The name of the client
#' @param version The version of the client
#'
#' @return A new mcp client
#' @export
new_client <- function(
  command,
  args = character(),
  name = command,
  version = "1.0.0"
) {
  stopifnot(is.character(command), length(command) == 1)

  p <- processx::process$new(
    command = command,
    args = args,
    stdin = "|",
    stdout = "|"
  )

  structure(
    p,
    name = name,
    version = version,
    class = c("client", class(p))
  )
}
