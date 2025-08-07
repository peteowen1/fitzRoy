# Error Handling Functions for fitzRoy
# 
# Consistent error handling patterns for the fitzRoy package

#' Throw a fitzRoy error
#'
#' @param message Error message
#' @param class Error class suffix (will be prefixed with "fitzroy_")
#' @param call The calling environment
#' @param ... Additional arguments passed to cli::cli_abort
#' @keywords internal
#' @noRd
fitzroy_abort <- function(message, class = "error", call = rlang::caller_env(), ...) {
  cli::cli_abort(
    message = message,
    class = paste0("fitzroy_", class),
    call = call,
    ...
  )
}

#' Issue a fitzRoy warning
#'
#' @param message Warning message
#' @param class Warning class suffix (will be prefixed with "fitzroy_")
#' @param ... Additional arguments passed to cli::cli_warn
#' @keywords internal
#' @noRd
fitzroy_warn <- function(message, class = "warning", ...) {
  cli::cli_warn(
    message = message,
    class = paste0("fitzroy_", class),
    ...
  )
}

#' Issue a fitzRoy informational message
#'
#' @param message Informational message
#' @param ... Additional arguments passed to cli::cli_inform
#' @keywords internal
#' @noRd
fitzroy_inform <- function(message, ...) {
  cli::cli_inform(message = message, ...)
}

#' Handle API errors consistently
#'
#' @param response httr or httr2 response object
#' @param context Context for the API call
#' @param call The calling environment
#' @keywords internal
#' @noRd
handle_api_error <- function(response, context = "API request", call = rlang::caller_env()) {
  if (inherits(response, "httr2_response")) {
    # httr2 response
    if (httr2::resp_is_error(response)) {
      status_code <- httr2::resp_status(response)
      fitzroy_abort(
        message = "{context} failed with status {status_code}",
        class = "api_error",
        call = call
      )
    }
  }
}

#' Handle missing data consistently
#'
#' @param data Data object to check
#' @param context Context for the data
#' @param call The calling environment
#' @keywords internal
#' @noRd
handle_missing_data <- function(data, context = "data", call = rlang::caller_env()) {
  if (is.null(data) || (is.data.frame(data) && nrow(data) == 0)) {
    fitzroy_warn(
      message = "No {context} found",
      class = "missing_data"
    )
    return(tibble::tibble())
  }
  return(data)
}

#' Validate season input
#'
#' @param season Season value to validate
#' @param allow_multiple Whether multiple seasons are allowed
#' @param call The calling environment
#' @keywords internal
#' @noRd
validate_season <- function(season, allow_multiple = TRUE, call = rlang::caller_env()) {
  if (is.null(season)) {
    return(CURRENT_SEASON)
  }
  
  if (!is.numeric(season)) {
    fitzroy_abort(
      message = "Season must be numeric",
      class = "invalid_input",
      call = call
    )
  }
  
  if (any(nchar(as.character(season)) != 4)) {
    fitzroy_abort(
      message = "Season should be in YYYY format",
      class = "invalid_input",
      call = call
    )
  }
  
  if (any(season < EARLIEST_AFL_SEASON | season > LATEST_VALID_SEASON)) {
    fitzroy_abort(
      message = "Season should be between {EARLIEST_AFL_SEASON} and {LATEST_VALID_SEASON}",
      class = "invalid_input",
      call = call
    )
  }
  
  if (!allow_multiple && length(season) > 1) {
    fitzroy_abort(
      message = "Multiple seasons not allowed for this function",
      class = "invalid_input",
      call = call
    )
  }
  
  return(season)
}

#' Validate round number input
#'
#' @param round_number Round number to validate
#' @param allow_null Whether NULL is allowed
#' @param call The calling environment
#' @keywords internal
#' @noRd
validate_round_number <- function(round_number, allow_null = TRUE, call = rlang::caller_env()) {
  if (is.null(round_number)) {
    if (allow_null) {
      return(NULL)
    } else {
      fitzroy_abort(
        message = "Round number is required",
        class = "invalid_input",
        call = call
      )
    }
  }
  
  if (!is.numeric(round_number) || any(round_number < MIN_ROUND_NUMBER) || any(round_number > MAX_ROUND_NUMBER)) {
    fitzroy_abort(
      message = "Round number must be between {MIN_ROUND_NUMBER} and {MAX_ROUND_NUMBER}",
      class = "invalid_input",
      call = call
    )
  }
  
  return(round_number)
}

#' Validate team name input
#'
#' @param team Team name to validate
#' @param valid_teams Vector of valid team names
#' @param call The calling environment
#' @keywords internal
#' @noRd
validate_team <- function(team, valid_teams = NULL, call = rlang::caller_env()) {
  if (is.null(team)) {
    return(NULL)
  }
  
  if (!is.character(team)) {
    fitzroy_abort(
      message = "Team must be a character string",
      class = "invalid_input", 
      call = call
    )
  }
  
  if (!is.null(valid_teams) && !all(team %in% valid_teams)) {
    invalid_teams <- team[!team %in% valid_teams]
    fitzroy_abort(
      message = "Invalid team name(s): {paste(invalid_teams, collapse = ', ')}",
      class = "invalid_input",
      call = call
    )
  }
  
  return(team)
}