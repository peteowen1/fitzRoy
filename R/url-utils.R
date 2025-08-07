# URL Construction Utilities for fitzRoy
#
# Secure URL building functions to prevent injection vulnerabilities

#' Safely build URLs with query parameters
#'
#' @param base_url Base URL
#' @param path Optional path to append
#' @param query Named list of query parameters
#' @param fragment Optional URL fragment
#' @keywords internal
#' @noRd
build_url <- function(base_url, path = NULL, query = NULL, fragment = NULL) {
  # Validate base URL
  if (!is.character(base_url) || length(base_url) != 1) {
    fitzroy_abort("Base URL must be a single character string", class = "invalid_url")
  }
  
  if (!grepl("^https?://", base_url)) {
    fitzroy_abort("Base URL must start with http:// or https://", class = "invalid_url")
  }
  
  # Use httr2's URL parsing and building
  req <- httr2::request(base_url)
  
  # Add path if provided
  if (!is.null(path)) {
    path <- sanitize_path(path)
    req <- httr2::req_url_path_append(req, path)
  }
  
  # Add query parameters if provided
  if (!is.null(query)) {
    query <- sanitize_query_params(query)
    req <- httr2::req_url_query(req, !!!query)
  }
  
  # Extract the URL from the request object
  return(req$url)
}

#' Sanitize URL path component
#'
#' @param path Path string to sanitize
#' @keywords internal
#' @noRd
sanitize_path <- function(path) {
  if (is.null(path)) return(NULL)
  
  if (!is.character(path)) {
    path <- as.character(path)
  }
  
  # Trim whitespace
  path <- trimws(path)
  
  # Don't URL encode the path here - httr2 will handle this properly
  # Encoding here causes double-encoding issues
  return(path)
}

#' Sanitize query parameters
#'
#' @param query Named list of query parameters
#' @keywords internal
#' @noRd
sanitize_query_params <- function(query) {
  if (is.null(query)) return(NULL)
  
  if (!is.list(query)) {
    fitzroy_abort("Query parameters must be a named list", class = "invalid_query")
  }
  
  # Ensure all values are character
  query <- lapply(query, function(x) {
    if (is.null(x)) return("")
    as.character(x)
  })
  
  # URL encode parameter values
  query <- lapply(query, utils::URLencode)
  
  return(query)
}

#' Sanitize URL fragment
#'
#' @param fragment Fragment string to sanitize
#' @keywords internal
#' @noRd
sanitize_fragment <- function(fragment) {
  if (is.null(fragment)) return(NULL)
  
  if (!is.character(fragment)) {
    fragment <- as.character(fragment)
  }
  
  # URL encode the fragment
  fragment <- utils::URLencode(fragment)
  
  return(fragment)
}

#' Build AFL API URL safely
#'
#' @param endpoint API endpoint
#' @param params Named list of parameters
#' @keywords internal
#' @noRd
build_afl_api_url <- function(endpoint, params = NULL) {
  base_url <- "https://aflapi.afl.com.au"
  
  # Validate endpoint
  if (!is.character(endpoint)) {
    endpoint <- as.character(endpoint)
  }
  
  # Ensure endpoint starts with /
  if (!grepl("^/", endpoint)) {
    endpoint <- paste0("/", endpoint)
  }
  
  return(build_url(base_url, path = endpoint, query = params))
}

#' Build Footywire URL safely  
#'
#' @param path URL path
#' @param params Named list of parameters
#' @keywords internal
#' @noRd
build_footywire_url <- function(path, params = NULL) {
  base_url <- "https://www.footywire.com"
  
  # Validate path
  if (!is.character(path)) {
    path <- as.character(path)
  }
  
  return(build_url(base_url, path = path, query = params))
}

#' Build AFL Tables URL safely
#'
#' @param path URL path  
#' @param params Named list of parameters
#' @keywords internal
#' @noRd
build_afltables_url <- function(path, params = NULL) {
  base_url <- "https://afltables.com"
  
  # Validate path
  if (!is.character(path)) {
    path <- as.character(path)
  }
  
  return(build_url(base_url, path = path, query = params))
}

#' Build Squiggle API URL safely
#'
#' @param endpoint API endpoint
#' @param params Named list of parameters  
#' @keywords internal
#' @noRd
build_squiggle_url <- function(endpoint = "", params = NULL) {
  base_url <- "https://api.squiggle.com.au"
  
  # Validate endpoint
  if (!is.character(endpoint)) {
    endpoint <- as.character(endpoint)
  }
  
  return(build_url(base_url, path = endpoint, query = params))
}

#' Validate match ID for security
#'
#' @param match_id Match ID to validate
#' @keywords internal
#' @noRd
validate_match_id <- function(match_id) {
  if (is.null(match_id)) return(NULL)
  
  # Convert to numeric if possible
  match_id <- suppressWarnings(as.numeric(match_id))
  
  if (is.na(match_id)) {
    fitzroy_abort("Match ID must be numeric", class = "invalid_match_id")
  }
  
  if (match_id < 1 || match_id > 999999) {
    fitzroy_abort("Match ID must be between 1 and 999999", class = "invalid_match_id")  
  }
  
  return(as.integer(match_id))
}

#' Validate season for URL construction
#'
#' @param season Season to validate
#' @keywords internal
#' @noRd
validate_season_for_url <- function(season) {
  if (is.null(season)) return(NULL)
  
  season <- validate_season(season, allow_multiple = FALSE)
  return(as.integer(season))
}

#' Validate round number for URL construction
#'
#' @param round_number Round number to validate
#' @keywords internal
#' @noRd
validate_round_for_url <- function(round_number) {
  if (is.null(round_number)) return(NULL)
  
  round_number <- validate_round_number(round_number, allow_null = FALSE)
  return(as.integer(round_number))
}