tools_list <- function(mcp) {
  # List all available tools
  tools <- list()
  for (tool_name in names(mcp$tools)) {
    tool <- mcp$tools[[tool_name]]
    tools <- c(tools, list(tool))
  }
  tools
}
