#' Access Squiggle data using the squiggle API service.
#'
#' Use `fetch_squiggle_data` to access the [Squiggle](https://squiggle.com.au) API. See instructions at [api.squiggle.com.au](https://api.squiggle.com.au).
#'
#' Optional arguments can be provided to further restrict the data you are pulling.
#'
#' For full instructions, see [api.squiggle.com.au](https://api.squiggle.com.au)
#'
#' @param query A text string. The main query to use with the API. Please read the Squiggle documentation for information about valid queries
#' @param ... (optional) An optional argument provided to the [Squiggle API](https://api.squiggle.com.au). See details for more info.
#' @param user_agent (optional) Use this to set something meaningful so that Squiggle admin can contact you if needed.
#' @return A dataframe, with the resultant data that matches the query specified in `query`, as well as any optional filters.
#' @export
#'
#' @examples
#' \dontrun{
#' # Return a list of the sources, with ID's
#' sources <- fetch_squiggle_data("sources")
#'
#' # Get tips for Round 1, 2018
#' tips <- fetch_squiggle_data(query = "tips", round = 1, year = 2018)
#'
#' # Get tips from Squiggle 2019
#' squiggle <- fetch_squiggle_data(query = "tips", source = 1, year = 2019)
#' }
fetch_squiggle_data <- function(query,
                                ...,
                                user_agent = "fitzRoy Package https://github.com/jimmyday12/fitzRoy") {
  # Extract optional params
  params <- list(
    q = query,
    ...
  )

  api_url <- "https://api.squiggle.com.au"
  
  # Add format parameter
  params$format <- "JSON"
  
  # Make API call using safe_api_call
  api_url_display <- build_squiggle_url(endpoint = "", params = list(q = params$q))
  cli::cli_progress_step("Getting data from {.field {api_url_display}}")
  
  # Get raw response and parse with flatten=TRUE for consistency  
  response <- safe_http_get(
    url = api_url,
    query = params,
    headers = list("User-Agent" = user_agent)
  )
  
  # Parse JSON with flatten=TRUE like other APIs
  json_data <- response |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON(flatten = TRUE)
  
  # Check if we got HTML instead of JSON (error condition)
  if (is.character(json_data) && grepl("<!DOCTYPE html|<html", json_data)) {
    cli::cli_abort(c(
      "API did not return any data",
      "i" = "Did you check that the queries provided are valid?",
      "x" = "You've supplied the following queries: {.val {names(params)}}"
    ))
  }

  # Convert to tibble - with flatten=TRUE, should have proper structure
  if (is.list(json_data) && length(json_data) > 0) {
    first_element <- json_data[[1]]
    
    if (is.null(first_element) || length(first_element) == 0) {
      return(tibble::tibble())
    }
    
    # With flattening, should be able to convert directly
    tryCatch({
      return(tibble::as_tibble(first_element))
    }, error = function(e) {
      # If still failing, provide more debugging info
      fitzroy_warn(paste("Failed to convert Squiggle API response to tibble:", e$message), class = "api_parse_warning")
      return(tibble::tibble())
    })
  } else {
    return(tibble::tibble())
  }
}

#' Access Squiggle data using the squiggle API service.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' All `get_` functions were replaced with `fetch_*` functions.
#' Please use `fetch_squiggle_data()` instead
#'
#' @examples
#' #
#' \dontrun{
#' get_squiggle_data()
#' # ->
#' fetch_squiggle_data()
#' }
#' @keywords internal
get_squiggle_data <- function(query = c(
                                "teams",
                                "sources",
                                "games",
                                "tips",
                                "ladder",
                                "standings",
                                "virtual",
                                "pav"
                              ), ...) {
  lifecycle::deprecate_warn(
    "1.0.0",
    "get_squiggle_data()",
    "fetch_squiggle_data()"
  )
  fetch_squiggle_data(query = query, ...)
}
