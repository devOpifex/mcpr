devtools::load_all()

# Create an MCPR client connected to the calculator server
client <- new_client_io(
  "Rscript",
  "/home/john/Opifex/Packages/mcpr/examples/ellmer/server.R",
  name = "calculator",
  version = "1.0.0"
)

Sys.sleep(1)

res <- tools_call(
  client,
  params = list(
    name = "math_calculator",
    arguments = list(
      operation = "add",
      a = 5,
      b = 3
    )
  ),
  id = 42
)

# Create a Claude chat session with ellmer
chat <- ellmer::chat_anthropic()

## Convert MCPR tools to ellmer tools and register them with the chat
chat$set_tools(mcpr_to_ellmer_tools(client))

## Try using the tools in a chat
chat$chat(
  "Subtract 2 from 44"
)
