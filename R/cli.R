#' Run the CLI
#'
#' @return Nothing
#' @export
run <- function() {
  #!/usr/bin/env Rscript

  library(jsonlite)

  cat("JSON-RPC CLI started. Send JSON requests line by line.\n")

  while (TRUE) {
    line <- readLines(con = stdin(), n = 1, warn = FALSE)

    if (length(line) == 0) break

    tryCatch(
      {
        # Parse JSON
        request <- fromJSON(line)

        # Process JSON-RPC request
        if (!is.null(request$method)) {
          cat("Method:", request$method, "\n")
          # Handle your RPC methods here
        }
      },
      error = function(e) {
        cat("Error parsing JSON:", e$message, "\n")
      }
    )
  }
}
