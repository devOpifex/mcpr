#' OAuth Storage Interfaces
#'
#' Storage interfaces for OAuth clients, tokens, and authorization codes.
#' Supports both in-memory and file-based storage with extensible interface.
#'

#' Create OAuth storage interface
#'
#' @param type Storage type ("memory" or "file")
#' @param path Path for file storage (ignored for memory storage)
#' @return Storage object
#' @export
oauth_storage <- function(type = c("memory", "file"), path = NULL) {
  type <- match.arg(type)
  
  switch(type,
    "memory" = oauth_memory_storage(),
    "file" = oauth_file_storage(path)
  )
}

#' Create in-memory OAuth storage
#'
#' @return Memory storage object
#' @export
oauth_memory_storage <- function() {
  env <- new.env(parent = emptyenv())
  
  structure(list(
    type = "memory",
    env = env,
    
    get = function(key) {
      env[[key]]
    },
    
    set = function(key, value) {
      env[[key]] <- value
      invisible(TRUE)
    },
    
    delete = function(key) {
      if (exists(key, envir = env)) {
        rm(list = key, envir = env)
        TRUE
      } else {
        FALSE
      }
    },
    
    exists = function(key) {
      exists(key, envir = env)
    },
    
    list_keys = function() {
      ls(env)
    },
    
    clear = function() {
      rm(list = ls(env), envir = env)
      invisible(TRUE)
    },
    
    size = function() {
      length(ls(env))
    }
  ), class = c("oauth_storage", "oauth_memory_storage"))
}

#' Create file-based OAuth storage
#'
#' @param path Path to storage file
#' @return File storage object
#' @export
oauth_file_storage <- function(path) {
  if (is.null(path)) {
    stop("File path is required for file storage")
  }
  
  # Ensure directory exists
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  
  # Initialize empty file if it doesn't exist
  if (!file.exists(path)) {
    saveRDS(list(), path)
  }
  
  structure(list(
    type = "file",
    path = path,
    
    get = function(key) {
      data <- readRDS(path)
      data[[key]]
    },
    
    set = function(key, value) {
      data <- readRDS(path)
      data[[key]] <- value
      saveRDS(data, path)
      invisible(TRUE)
    },
    
    delete = function(key) {
      data <- readRDS(path)
      if (key %in% names(data)) {
        data[[key]] <- NULL
        saveRDS(data, path)
        TRUE
      } else {
        FALSE
      }
    },
    
    exists = function(key) {
      data <- readRDS(path)
      key %in% names(data)
    },
    
    list_keys = function() {
      data <- readRDS(path)
      names(data)
    },
    
    clear = function() {
      saveRDS(list(), path)
      invisible(TRUE)
    },
    
    size = function() {
      data <- readRDS(path)
      length(data)
    }
  ), class = c("oauth_storage", "oauth_file_storage"))
}

#' Store OAuth client information
#'
#' @param storage Storage object
#' @param client_id Client identifier
#' @param client_info Client information list
#' @export
oauth_store_client <- function(storage, client_id, client_info) {
  # Add metadata
  client_info$created_at <- Sys.time()
  client_info$client_id <- client_id
  
  storage$set(paste0("client:", client_id), client_info)
}

#' Retrieve OAuth client information
#'
#' @param storage Storage object
#' @param client_id Client identifier
#' @return Client information or NULL if not found
#' @export
oauth_get_client <- function(storage, client_id) {
  storage$get(paste0("client:", client_id))
}

#' Store authorization code
#'
#' @param storage Storage object
#' @param code Authorization code
#' @param code_info Code information (client_id, redirect_uri, etc.)
#' @param expires_in Expiration time in seconds (default: 600)
#' @export
oauth_store_authorization_code <- function(storage, code, code_info, expires_in = 600) {
  code_data <- list(
    code = code,
    client_id = code_info$client_id,
    redirect_uri = code_info$redirect_uri,
    scope = code_info$scope,
    code_challenge = code_info$code_challenge,
    code_challenge_method = code_info$code_challenge_method,
    user_id = code_info$user_id,
    created_at = Sys.time(),
    expires_at = Sys.time() + expires_in,
    used = FALSE
  )
  
  storage$set(paste0("code:", code), code_data)
}

#' Retrieve and consume authorization code
#'
#' @param storage Storage object
#' @param code Authorization code
#' @return Code information or NULL if not found/expired/used
#' @export
oauth_consume_authorization_code <- function(storage, code) {
  key <- paste0("code:", code)
  code_data <- storage$get(key)
  
  if (is.null(code_data)) {
    return(NULL)
  }
  
  # Check if already used
  if (code_data$used) {
    return(NULL)
  }
  
  # Check if expired
  if (Sys.time() > code_data$expires_at) {
    storage$delete(key)
    return(NULL)
  }
  
  # Mark as used
  code_data$used <- TRUE
  storage$set(key, code_data)
  
  code_data
}

#' Store access token
#'
#' @param storage Storage object
#' @param token_id Token identifier (jti claim)
#' @param token_info Token information
#' @export
oauth_store_access_token <- function(storage, token_id, token_info) {
  token_data <- list(
    token_id = token_id,
    client_id = token_info$client_id,
    user_id = token_info$user_id,
    scope = token_info$scope,
    audience = token_info$audience,
    created_at = Sys.time(),
    expires_at = Sys.time() + token_info$expires_in,
    revoked = FALSE
  )
  
  storage$set(paste0("token:", token_id), token_data)
}

#' Get access token information
#'
#' @param storage Storage object
#' @param token_id Token identifier
#' @return Token information or NULL if not found
#' @export
oauth_get_access_token <- function(storage, token_id) {
  storage$get(paste0("token:", token_id))
}

#' Revoke access token
#'
#' @param storage Storage object
#' @param token_id Token identifier
#' @return Logical indicating success
#' @export
oauth_revoke_access_token <- function(storage, token_id) {
  key <- paste0("token:", token_id)
  token_data <- storage$get(key)
  
  if (is.null(token_data)) {
    return(FALSE)
  }
  
  token_data$revoked <- TRUE
  token_data$revoked_at <- Sys.time()
  
  storage$set(key, token_data)
  TRUE
}

#' Clean expired tokens and codes
#'
#' @param storage Storage object
#' @return Number of items cleaned
#' @export
oauth_cleanup_expired <- function(storage) {
  all_keys <- storage$list_keys()
  now <- Sys.time()
  cleaned <- 0
  
  for (key in all_keys) {
    item <- storage$get(key)
    
    if (!is.null(item) && !is.null(item$expires_at) && now > item$expires_at) {
      storage$delete(key)
      cleaned <- cleaned + 1
    }
  }
  
  cleaned
}

#' Print method for OAuth storage
#'
#' @param x OAuth storage object
#' @param ... Additional arguments
#' @export
print.oauth_storage <- function(x, ...) {
  cat("OAuth Storage (", x$type, ")\n", sep = "")
  cat("Items:", x$size(), "\n")
  if (x$type == "file") {
    cat("Path:", x$path, "\n")
  }
  invisible(x)
}

#' Get storage statistics
#'
#' @param storage Storage object
#' @return List with storage statistics
#' @export
oauth_storage_stats <- function(storage) {
  keys <- storage$list_keys()
  
  clients <- sum(grepl("^client:", keys))
  codes <- sum(grepl("^code:", keys))
  tokens <- sum(grepl("^token:", keys))
  
  list(
    total_items = length(keys),
    clients = clients,
    authorization_codes = codes,
    access_tokens = tokens,
    storage_type = storage$type
  )
}