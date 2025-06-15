devtools::load_all()

# Create an MCPR client connected to the calculator server
client <- new_client_io(
  "Rscript",
  "/home/john/Opifex/Packages/mcpr/examples/calculator/server.R",
  name = "calculator",
  version = "1.0.0"
)

# Initialize the client
res <- initialize(client)
print(res)

# List available tools
res <- tools_list(client)
print(res)

# Create a Claude chat session with ellmer
chat <- ellmer::chat_anthropic()

print(mcpr_to_ellmer_tools(client))

# Convert MCPR tools to ellmer tools and register them with the chat
chat$set_tools(mcpr_to_ellmer_tools(client))

# Try using the tools in a chat
chat$chat(
  "Subtract 2 from 44"
)
