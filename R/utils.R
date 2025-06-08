generate_id <- function() {
  as.numeric(Sys.time()) * 1000000 # microseconds
}
