#' Initialize a mcp provider
#'
#' @param x A mcp provider
#'
#' @return The mcp provider
#' @export
#'
#' @examples
#' @name initialize
initialize <- function(x) UseMethod("initialize")

#' @method initialize mcp
#' @export
initialize.mcp <- function(x) {
  write(
    x,
    "initialize",
    list(
      protocolVersion = "2024-11-05",
      clientInfo = list(
        name = attr(x, "name"),
        version = attr(x, "version")
      ),
      capabilities = list(
        roots = list(
          listChanged = FALSE
        ),
        sampling = list()
      )
    )
  )
}

#' Get the list of tools
#'
#' @param x A mcp provider
#'
#' @return The list of tools
#' @export
#'
#' @examples
tools_list <- function(x) UseMethod("tools_list")

#' @method tools_list mcp
#' @export
tools_list.mcp <- function(x) {
  write(x, "tools.list", NULL)
}
