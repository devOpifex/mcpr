#!/usr/bin/env Rscript

# R CLI Tool that keeps running and processes stdin
# Usage: Rscript cli_tool.R
# Or: echo "test input" | Rscript cli_tool.R

cat("R CLI Tool started. Listening for input...\n")
cat("Type 'quit' or 'exit' to stop, or use Ctrl+C\n")
cat("----------------------------------------\n")

# Function to process input
process_input <- function(input_text) {
  # Example processing - convert to uppercase and add timestamp
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  result <- list(
    timestamp = timestamp,
    original = input_text,
    processed = toupper(input_text),
    length = nchar(input_text)
  )
  return(result)
}

# Main loop - this is the key part that works with Rscript
tryCatch(
  {
    # Open stdin connection explicitly
    con <- file("stdin", open = "r")

    while (TRUE) {
      # Read line from stdin
      line <- readLines(con, n = 1, warn = FALSE)

      # Check for EOF or empty result
      if (length(line) == 0) {
        cat("Received EOF. Exiting...\n")
        break
      }

      # Trim whitespace
      line <- trimws(line)

      # Skip empty lines
      if (nchar(line) == 0) {
        next
      }

      # Check for quit commands
      if (tolower(line) %in% c("quit", "exit", "q")) {
        cat("Received quit command. Exiting...\n")
        break
      }

      # Process the input
      cat("Received:", line, "\n")
      result <- process_input(line)

      # Output result
      cat("Processed at:", result$timestamp, "\n")
      cat("Length:", result$length, "characters\n")
      cat("Result:", result$processed, "\n")
      cat("----------------------------------------\n")
    }
  },
  interrupt = function(e) {
    cat("\nReceived interrupt (Ctrl+C). Cleaning up...\n")
  },
  error = function(e) {
    cat("Error occurred:", e$message, "\n")
  },
  finally = {
    # Clean up
    if (exists("con")) {
      close(con)
    }
    cat("CLI tool stopped.\n")
  }
)
