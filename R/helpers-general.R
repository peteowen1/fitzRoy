
#' Check Season
#'
#' Checks the season for various things
#'
#' @param x Season in Year format
#'
#' @keywords internal
#' @noRd
check_season <- function(x) {
  if (is.null(x)) {
    x <- Sys.Date() %>%
      format("%Y") %>%
      as.numeric()
  }
  if (nchar(x) < 4) rlang::abort(glue::glue("Season should be in YYYY format. 
                                                Your season is only {nchar(x)} digits"))
  return(x)
}

#' Check comp
#'
#' Checks the comp for various things
#'
#' @param x Comp name
#'
#' @keywords internal
#' @noRd
check_comp <- function(x) {
  if (!x %in% c("AFLM", "AFLW")) rlang::abort(glue::glue("Comp should be either \"AFLW\" or \"AFL\". You supplied {x}"))
}

#' Check Source
#'
#' Checks the source for various things
#'
#' @param x Source name
#'
#' @keywords internal
#' @noRd
check_source <- function(x) {
  # if (!x %in% c("AFL", "footywire", "afltables", "squiggle", "fryzigg")) rlang::abort(glue::glue("Source should be either \"AFL\", \"footywire\" or \"afltables\". You supplied {x}"))

  valid <- c(
    "AFL",
    "footywire",
    "afltables",
    "squiggle",
    "fryzigg"
  )

  if (!x %in% valid) {
    rlang::abort(glue::glue(
      "Provided paramters must be one of {glue::glue_collapse(valid, sep = \", \", last = \" or \")}
    You provided the following: {x}"
    ))
  }
}

#' Check Comp Source
#'
#' Checks both comp and source for various things
#'
#' @param comp Comp name
#' @param source Source name
#'
#' @keywords internal
#' @noRd
check_comp_source <- function(comp, source) {
  check_comp(comp)
  check_source(source)

  if (comp == "AFLW" & source != "AFL") {
    rlang::abort(glue::glue("AFLW data only exists from source \"AFL\""))
  }
}

#' Verify Year
#'
#' Verifies year
#'
#' @param year Year in numeric format YYYY
#'
#' @keywords internal
#' @noRd
verify_year <- function(year) {
  year <- suppressWarnings(as.numeric(year))
  if (is.na(year)) {
    stop(paste("Not a year."))
  }
  else if (year >= 1897 & year <= as.numeric(format(Sys.Date(), "%Y"))) {
    return(year)
  }
  else {
    stop(paste("Not a valid year within available range."))
  }
}

#' Returns start and end dates given a season range
#'
#'
#' @param season Season in numeric format YYYY
#'
#' @keywords internal
#' @noRd
return_start_end_dates <- function(season) {
  season_checked <- season %>% purrr::map_int(check_season)

  if (is.null(season)) {
    start_date <- lubridate::ymd("1897-01-01", quiet = TRUE)
    end_date <- lubridate::parse_date_time(Sys.Date(), c("dmy", "ymd"), quiet = TRUE)
  } else {
    start_date <- lubridate::parse_date_time(
      paste0(min(season_checked), "-01-01"), c("ymd"),
      quiet = TRUE
    )

    end_date <- lubridate::parse_date_time(
      paste0(max(season_checked), "-12-31"), c("ymd"),
      quiet = TRUE
    )
  }

  if (end_date > Sys.Date()) {
    end_date <- lubridate::parse_date_time(Sys.Date(), c("dmy", "ymd"), quiet = TRUE)
  }

  if (is.na(start_date)) {
    stop(paste(
      "Date format not recognised",
      "Check that start_date is in dmy or ymd format"
    ))
  }

  if (is.na(end_date)) {
    stop(paste(
      "Date format not recognised",
      "Check that end_date is in dmy or ymd format"
    ))
  }

  return(list(
    start_date = start_date,
    end_date = end_date
  ))
}