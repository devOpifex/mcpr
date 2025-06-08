generate_id <- function() {
  trunc(as.numeric(Sys.time()) * 1000000) # microseconds
}
