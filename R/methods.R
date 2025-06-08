tools_list <- function(mcp) {
  # List all available tools
  tools <- list()
  for (tool_name in names(mcp$tools)) {
    tool <- mcp$tools[[tool_name]]
    tools <- c(tools, list(tool))
  }
  tools
}

resources_list <- function(mcp) {
  # List all available resources
  resources <- list()
  for (resource_name in names(mcp$resources)) {
    resource <- mcp$resources[[resource_name]]
    resources <- c(resources, list(resource))
  }
  resources
}

prompts_list <- function(mcp) {
  # List all available prompts
  prompts <- list()
  for (prompt_name in names(mcp$prompts)) {
    prompt <- mcp$prompts[[prompt_name]]
    prompts <- c(prompts, list(prompt))
  }
  prompts
}
