# Caching Utilities for fitzRoy
#
# Intelligent caching system for web requests and data processing

#' Get cache directory path
#'
#' @param create Whether to create directory if it doesn't exist
#' @keywords internal
#' @noRd
get_cache_dir <- function(create = TRUE) {
  cache_dir <- file.path(tempdir(), "fitzRoy_cache")
  
  if (create && !dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  
  return(cache_dir)
}

#' Generate cache key from parameters
#'
#' @param ... Parameters to include in cache key
#' @keywords internal
#' @noRd
generate_cache_key <- function(...) {
  params <- list(...)
  
  # Remove NULL values
  params <- params[!sapply(params, is.null)]
  
  # Sort parameters by name for consistency
  if (length(params) > 0) {
    params <- params[order(names(params))]
  }
  
  # Create hash of parameters
  param_string <- paste(
    names(params), 
    sapply(params, function(x) paste(x, collapse = "_")), 
    sep = "=", 
    collapse = "|"
  )
  
  cache_key <- digest::digest(param_string, algo = "md5")
  return(cache_key)
}

#' Check if cached data exists and is valid
#'
#' @param cache_key Unique cache key
#' @param max_age_hours Maximum age of cached data in hours
#' @keywords internal
#' @noRd
is_cache_valid <- function(cache_key, max_age_hours = 24) {
  cache_file <- file.path(get_cache_dir(), paste0(cache_key, ".rds"))
  
  if (!file.exists(cache_file)) {
    return(FALSE)
  }
  
  file_age_hours <- as.numeric(difftime(
    Sys.time(), 
    file.info(cache_file)$mtime,
    units = "hours"
  ))
  
  return(file_age_hours <= max_age_hours)
}

#' Retrieve data from cache
#'
#' @param cache_key Unique cache key
#' @keywords internal
#' @noRd
get_from_cache <- function(cache_key) {
  cache_file <- file.path(get_cache_dir(), paste0(cache_key, ".rds"))
  
  if (!file.exists(cache_file)) {
    return(NULL)
  }
  
  tryCatch({
    data <- readRDS(cache_file)
    fitzroy_inform("Using cached data (key: {substr(cache_key, 1, 8)}...)")
    return(data)
  }, error = function(e) {
    fitzroy_warn(paste("Failed to read cached data:", e$message), class = "cache_error")
    return(NULL)
  })
}

#' Save data to cache
#'
#' @param data Data to cache
#' @param cache_key Unique cache key
#' @keywords internal
#' @noRd
save_to_cache <- function(data, cache_key) {
  cache_dir <- get_cache_dir(create = TRUE)
  cache_file <- file.path(cache_dir, paste0(cache_key, ".rds"))
  
  tryCatch({
    saveRDS(data, cache_file)
    fitzroy_inform("Data cached (key: {substr(cache_key, 1, 8)}...)")
  }, error = function(e) {
    fitzroy_warn(paste("Failed to cache data:", e$message), class = "cache_error")
  })
  
  return(data)
}

#' Clear old cache files
#'
#' @param max_age_hours Maximum age to keep in hours
#' @param dry_run If TRUE, only report what would be deleted
#' @keywords internal
#' @noRd
clear_old_cache <- function(max_age_hours = 168, dry_run = FALSE) {  # Default 1 week
  cache_dir <- get_cache_dir(create = FALSE)
  
  if (!dir.exists(cache_dir)) {
    return(invisible(NULL))
  }
  
  cache_files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
  
  if (length(cache_files) == 0) {
    return(invisible(NULL))
  }
  
  file_info <- file.info(cache_files)
  old_files <- cache_files[as.numeric(difftime(
    Sys.time(), 
    file_info$mtime, 
    units = "hours"
  )) > max_age_hours]
  
  if (length(old_files) > 0) {
    if (dry_run) {
      fitzroy_inform("Would delete {length(old_files)} old cache files")
    } else {
      file.remove(old_files)
      fitzroy_inform("Deleted {length(old_files)} old cache files")
    }
  }
  
  return(invisible(length(old_files)))
}

#' Get cache statistics
#'
#' @keywords internal
#' @noRd
get_cache_stats <- function() {
  cache_dir <- get_cache_dir(create = FALSE)
  
  if (!dir.exists(cache_dir)) {
    return(list(
      files = 0,
      total_size_mb = 0,
      oldest_hours = NA,
      newest_hours = NA
    ))
  }
  
  cache_files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
  
  if (length(cache_files) == 0) {
    return(list(
      files = 0, 
      total_size_mb = 0,
      oldest_hours = NA,
      newest_hours = NA
    ))
  }
  
  file_info <- file.info(cache_files)
  total_size_mb <- sum(file_info$size) / (1024^2)
  
  ages_hours <- as.numeric(difftime(Sys.time(), file_info$mtime, units = "hours"))
  
  return(list(
    files = length(cache_files),
    total_size_mb = round(total_size_mb, 2),
    oldest_hours = round(max(ages_hours), 2),
    newest_hours = round(min(ages_hours), 2)
  ))
}

#' Cached function wrapper
#'
#' @param .f Function to wrap with caching
#' @param max_age_hours Maximum age of cached results
#' @param cache_prefix Prefix for cache keys
#' @keywords internal
#' @noRd
with_cache <- function(.f, max_age_hours = 24, cache_prefix = "") {
  function(...) {
    # Generate cache key from function name and arguments
    func_name <- deparse(substitute(.f))
    cache_key <- generate_cache_key(
      func = paste0(cache_prefix, func_name),
      ...
    )
    
    # Check cache first
    if (is_cache_valid(cache_key, max_age_hours)) {
      cached_result <- get_from_cache(cache_key)
      if (!is.null(cached_result)) {
        return(cached_result)
      }
    }
    
    # Execute function and cache result
    result <- .f(...)
    
    if (!is.null(result)) {
      save_to_cache(result, cache_key)
    }
    
    return(result)
  }
}

#' Create cached version of a fetch function
#'
#' @param fetch_func Fetch function to cache
#' @param max_age_hours Maximum age of cached results
#' @export
cached_fetch <- function(fetch_func, max_age_hours = 24) {
  if (!is.function(fetch_func)) {
    fitzroy_abort("fetch_func must be a function", class = "invalid_input")
  }
  
  return(with_cache(fetch_func, max_age_hours = max_age_hours, cache_prefix = "fetch_"))
}

#' Clear all fitzRoy cache
#'
#' @param confirm If TRUE, skip confirmation prompt
#' @export
clear_fitzroy_cache <- function(confirm = FALSE) {
  cache_dir <- get_cache_dir(create = FALSE)
  
  if (!dir.exists(cache_dir)) {
    fitzroy_inform("No cache directory found")
    return(invisible(NULL))
  }
  
  stats <- get_cache_stats()
  
  if (stats$files == 0) {
    fitzroy_inform("Cache is already empty")
    return(invisible(NULL))
  }
  
  if (!confirm) {
    message(sprintf(
      "This will delete %d cached files (%.2f MB). Continue? [y/N] ",
      stats$files, stats$total_size_mb
    ))
    
    response <- readline()
    if (!tolower(response) %in% c("y", "yes")) {
      fitzroy_inform("Cache clearing cancelled")
      return(invisible(NULL))
    }
  }
  
  unlink(cache_dir, recursive = TRUE)
  fitzroy_inform("Cache cleared: {stats$files} files ({stats$total_size_mb} MB)")
  
  return(invisible(stats))
}