devtools::load_all()

client <- new_client(
  "Rscript",
  "/home/john/Opifex/Packages/mcpr/examples/calculator/server.R",
  name = "calculator",
  version = "1.0.0"
)

res <- initialize(client)

print(res)

res <- tools_list(client)

print(res)
