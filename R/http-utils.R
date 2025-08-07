# HTTP Utilities for fitzRoy
#
# Robust HTTP handling with retry logic and rate limiting using httr2

#' Safe HTTP GET with retry logic using httr2
#'
#' @param url URL to fetch
#' @param max_retries Maximum number of retries
#' @param retry_delay Base delay between retries in seconds
#' @param timeout Request timeout in seconds
#' @param user_agent User agent string
#' @param headers Additional headers as named list
#' @param query Query parameters as named list
#' @param ... Additional arguments passed to httr2 functions
#' @keywords internal
#' @noRd
safe_http_get <- function(url, 
                          max_retries = 3, 
                          retry_delay = 1,
                          timeout = 30,
                          user_agent = DEFAULT_USER_AGENT,
                          headers = NULL,
                          query = NULL,
                          ...) {
  
  req <- httr2::request(url) |>
    httr2::req_timeout(timeout) |>
    httr2::req_user_agent(user_agent) |>
    httr2::req_retry(
      max_tries = max_retries + 1,
      backoff = ~ retry_delay * (2 ^ (.x - 1)),
      is_transient = ~ httr2::resp_status(.x) %in% c(429, 500, 502, 503, 504)
    )
  
  # Add headers if provided
  if (!is.null(headers)) {
    req <- httr2::req_headers(req, !!!headers)
  }
  
  # Add query parameters if provided
  if (!is.null(query)) {
    req <- httr2::req_url_query(req, !!!query)
  }
  
  tryCatch({
    response <- httr2::req_perform(req, ...)
    return(response)
  }, error = function(e) {
    fitzroy_abort(
      paste("HTTP request to", url, "failed:", e$message),
      class = "http_error"
    )
  })
}

#' Safe HTML scraping with retry logic using httr2
#'
#' @param url URL to scrape
#' @param encoding Character encoding
#' @param max_retries Maximum number of retries
#' @param retry_delay Base delay between retries
#' @param ... Additional arguments passed to safe_http_get
#' @keywords internal
#' @noRd
safe_read_html <- function(url, 
                          encoding = "UTF-8",
                          max_retries = 3,
                          retry_delay = 1,
                          ...) {
  
  tryCatch({
    # Get response using safe_http_get
    response <- safe_http_get(url, max_retries = max_retries, retry_delay = retry_delay, ...)
    
    # Parse HTML from response body
    html_content <- response |>
      httr2::resp_body_raw() |>
      xml2::read_html(encoding = encoding)
      
    return(html_content)
    
  }, error = function(e) {
    fitzroy_abort(
      paste("Failed to parse HTML from", url, ":", e$message),
      class = "html_parse_error"
    )
  })
}

#' Safe JSON API call with retry logic using httr2
#'
#' @param url URL for API call
#' @param method HTTP method (GET, POST, etc.)
#' @param body Request body for POST requests
#' @param headers Additional headers as named list
#' @param query Query parameters as named list
#' @param max_retries Maximum number of retries
#' @param retry_delay Base delay between retries
#' @param timeout Request timeout in seconds
#' @param ... Additional arguments passed to httr2 functions
#' @keywords internal
#' @noRd
safe_api_call <- function(url,
                         method = "GET",
                         body = NULL,
                         headers = NULL,
                         query = NULL,
                         max_retries = 3,
                         retry_delay = 1,
                         timeout = 30,
                         ...) {
  
  # Start building request
  req <- httr2::request(url) |>
    httr2::req_timeout(timeout) |>
    httr2::req_user_agent(DEFAULT_USER_AGENT) |>
    httr2::req_retry(
      max_tries = max_retries + 1,
      backoff = ~ retry_delay * (2 ^ (.x - 1)),
      is_transient = ~ httr2::resp_status(.x) %in% c(429, 500, 502, 503, 504)
    )
  
  # Set method
  req <- switch(toupper(method),
    "GET" = req,  # GET is default
    "POST" = httr2::req_method(req, "POST"),
    "PUT" = httr2::req_method(req, "PUT"),
    "DELETE" = httr2::req_method(req, "DELETE"),
    stop("Unsupported HTTP method: ", method)
  )
  
  # Add headers if provided
  default_headers <- list(
    "Accept" = JSON_CONTENT_TYPE,
    "Content-Type" = JSON_CONTENT_TYPE
  )
  all_headers <- utils::modifyList(default_headers, headers %||% list())
  req <- httr2::req_headers(req, !!!all_headers)
  
  # Add query parameters if provided
  if (!is.null(query)) {
    req <- httr2::req_url_query(req, !!!query)
  }
  
  # Add body for POST/PUT requests
  if (!is.null(body) && method %in% c("POST", "PUT")) {
    if (is.character(body)) {
      req <- httr2::req_body_raw(req, body, type = JSON_CONTENT_TYPE)
    } else {
      req <- httr2::req_body_json(req, body)
    }
  } else if (method %in% c("POST", "PUT")) {
    # For POST/PUT requests without body, explicitly set empty body to ensure Content-Length: 0
    req <- httr2::req_body_raw(req, "", type = JSON_CONTENT_TYPE)
  }
  
  tryCatch({
    response <- httr2::req_perform(req, ...)
    
    # Handle API errors
    handle_api_error(response, context = paste(method, "request to", url))
    
    # Parse JSON response
    if (httr2::resp_has_body(response)) {
      json_data <- httr2::resp_body_json(response)
      return(json_data)
    } else {
      fitzroy_warn("Empty response from API", class = "empty_response")
      return(list())
    }
    
  }, error = function(e) {
    if (inherits(e, "fitzroy_api_error")) {
      stop(e)  # Re-throw fitzroy errors
    }
    fitzroy_abort(
      paste("API call to", url, "failed:", e$message),
      class = "api_error"
    )
  })
}

#' Rate-limited request wrapper
#'
#' @param requests_per_second Maximum requests per second
#' @keywords internal  
#' @noRd
with_rate_limit <- function(requests_per_second = 2) {
  last_request_time <- 0
  min_interval <- 1 / requests_per_second
  
  function(.f) {
    function(...) {
      current_time <- as.numeric(Sys.time())
      time_since_last <- current_time - last_request_time
      
      if (time_since_last < min_interval) {
        sleep_time <- min_interval - time_since_last
        Sys.sleep(sleep_time)
      }
      
      result <- .f(...)
      last_request_time <<- as.numeric(Sys.time())
      return(result)
    }
  }
}

#' Check if URL is valid and reachable using httr2
#'
#' @param url URL to validate
#' @param timeout Timeout in seconds
#' @keywords internal
#' @noRd
validate_url <- function(url, timeout = 5) {
  if (!is.character(url) || length(url) != 1) {
    fitzroy_abort("URL must be a single character string", class = "invalid_url")
  }
  
  if (!grepl("^https?://", url)) {
    fitzroy_abort("URL must start with http:// or https://", class = "invalid_url")
  }
  
  tryCatch({
    response <- httr2::request(url) |>
      httr2::req_method("HEAD") |>
      httr2::req_timeout(timeout) |>
      httr2::req_error(is_error = \(resp) FALSE) |>  # Don't error on HTTP errors
      httr2::req_perform()
    
    return(!httr2::resp_is_error(response))
  }, error = function(e) {
    fitzroy_warn(paste("URL validation failed:", e$message), class = "url_validation")
    return(FALSE)
  })
}

#' Create a cached HTTP GET request
#'
#' @param url URL to fetch
#' @param cache_hours Number of hours to cache the result
#' @param ... Additional arguments passed to safe_http_get
#' @keywords internal
#' @noRd
cached_http_get <- function(url, cache_hours = 24, ...) {
  cached_func <- with_cache(safe_http_get, max_age_hours = cache_hours)
  return(cached_func(url, ...))
}

#' Safe AFL API call with JSON flattening
#'
#' @param url URL for API call
#' @param method HTTP method (GET, POST, etc.)
#' @param body Request body for POST requests
#' @param headers Additional headers as named list
#' @param query Query parameters as named list
#' @param max_retries Maximum number of retries
#' @param retry_delay Base delay between retries
#' @param timeout Request timeout in seconds
#' @param ... Additional arguments passed to httr2 functions
#' @keywords internal
#' @noRd
safe_afl_api_call <- function(url,
                             method = "GET",
                             body = NULL,
                             headers = NULL,
                             query = NULL,
                             max_retries = 3,
                             retry_delay = 1,
                             timeout = 30,
                             ...) {
  
  # Get raw response
  response <- safe_http_get(
    url = url,
    max_retries = max_retries,
    retry_delay = retry_delay,
    timeout = timeout,
    headers = headers,
    query = query,
    ...
  )
  
  # Parse JSON with flatten=TRUE for AFL API compatibility
  tryCatch({
    json_data <- response |>
      httr2::resp_body_string() |>
      jsonlite::fromJSON(flatten = TRUE)
    
    return(json_data)
  }, error = function(e) {
    fitzroy_abort(
      paste("Failed to parse AFL API response from", url, ":", e$message),
      class = "afl_api_parse_error"
    )
  })
}

#' Helper for null-coalescing operator
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x