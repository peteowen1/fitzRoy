#' Convert AFL Men's results into long format
#'
#' \code{convert_results} returns a dataframe containing the results in long format.
#'
#' The standard results returned by afltables.com will be in wide format.
#' This is useful for game based analysis but less so for team based ones. This function converts the data into long format for easier analysis.
#'
#' @param results A dataframe that has been returned from get_match_results
#'
#' @keywords internal
#' @noRd
convert_results <- function(results) {

  # Convert results to wide format
  results %>%
    tidyr::gather("variable", "value", .data$Home.Team:.data$Away.Points) %>%
    tidyr::separate(.data$variable, into = c("Status", "variable")) %>%
    tidyr::spread(.data$variable, .data$value) %>%
    dplyr::arrange(.data$Game) %>%
    dplyr::mutate(Margin = ifelse(.data$Status == "Home",
      .data$Margin,
      .data$Margin * -1
    ))
}


#' Helper function for \code{get_footywire_stats}
#'
#' @param x URL of the match
#' @param id Match ID number
#'
#' @keywords internal
#' @noRd
footywire_html <- function(x, id) {

  # First get extra information
  game_details <- x %>%
    rvest::html_node("tr:nth-child(2) .lnorm") %>%
    rvest::html_text()

  # We need to extract Round and venue from that text
  Round <- stringr::str_split(game_details, ",")[[1]][1] %>% trimws()
  venue <- stringr::str_split(game_details, ",")[[1]][2] %>% trimws()

  # Get Game date
  game_details_date <- x %>%
    rvest::html_node(".lnormtop tr:nth-child(3) .lnorm") %>%
    rvest::html_text()

  # Again, we have to extract the details
  game_date <- stringr::str_split(game_details_date, ",")[[1]][2] %>%
    trimws() %>%
    lubridate::dmy()
  season <- lubridate::year(game_date)

  # Get home and away team names
  home_team <- x %>%
    rvest::html_node("#matchscoretable tr:nth-child(2) a") %>%
    rvest::html_text()

  away_team <- x %>%
    rvest::html_node("#matchscoretable tr~ tr+ tr a") %>%
    rvest::html_text()

  # Now get the table data. The Home Team is in the 13th table
  home_stats <- x %>%
    rvest::html_nodes("table") %>%
    .[[12]] %>%
    rvest::html_table(header = TRUE) %>%
    dplyr::mutate(
      Team = home_team,
      Opposition = away_team,
      Status = "Home"
    )

  # Now get the table data
  away_stats <- x %>%
    rvest::html_nodes("table") %>%
    .[[16]] %>%
    rvest::html_table(header = TRUE) %>%
    dplyr::mutate(
      Team = away_team,
      Opposition = home_team,
      Status = "Away"
    )

  ## Add data to ind.table
  player_stats <- home_stats %>%
    dplyr::bind_rows(away_stats) %>%
    dplyr::mutate(
      Round = Round,
      Venue = venue,
      Season = season,
      Date = game_date,
      Match_id = id
    ) %>%
    dplyr::select(
      .data$Date,
      .data$Season,
      .data$Round,
      .data$Venue,
      .data$Player,
      .data$Team,
      .data$Opposition,
      .data$Status,
      dplyr::everything()
    )

  names(player_stats) <- make.names(names(player_stats))

  return(player_stats)
}



#' Helper function for \code{get_footywire_stats}
#'
#' @param id A match id from afltables
#'
#' @keywords internal
#' @noRd
get_match_data <- function(id) {

  # Create URL
  default_url <- "http://www.footywire.com/afl/footy/ft_match_statistics?mid="
  basic_url <- paste(default_url, id, sep = "")
  advanced_url <- paste(default_url, id, "&advv=Y", sep = "")

  # Check if URL exists
  footywire_basic <- tryCatch(
    xml2::read_html(basic_url),
    error = function(e) FALSE
  )

  if (!is.list(footywire_basic)) {
    stop("Couldn't Find basic table")
  } else {
    # Check if Advanced Page exist? If it doesn't, the script breaks
    # since the html tables have different nodes
    advanced_empty <- footywire_basic %>%
      rvest::html_nodes(".notice") %>%
      rvest::html_text() %>%
      stringr::str_detect("Advanced") %>%
      rlang::is_empty()

    # Check advanced exists
    if (advanced_empty) {
      stop("This function only works on matches from 2010 onwards")
    } else {

      # If it does, grab the basic data
      player_stats_basic <- footywire_html(footywire_basic, id)

      # If it does, create access the URL and create the data table.
      # Also merge with basic
      Sys.sleep(2)

      # Check if Advanced URL exists
      footywire_advanced <- tryCatch(
        xml2::read_html(advanced_url),
        error = function(e) FALSE
      )

      if (is.list(footywire_advanced)) {
        player_stats_advanced <- footywire_html(footywire_advanced, id)

        # Join them
        info_columns <- c(
          "Date", "Season", "Round", "Venue", "Player",
          "Team", "Opposition", "Status", "Match_id"
        )
        player_stats_table <- player_stats_advanced %>%
          dplyr::select(-dplyr::one_of(info_columns)) %>%
          dplyr::bind_cols(player_stats_basic) %>%
          dplyr::select(dplyr::one_of(info_columns), dplyr::everything())

        # Tidy Names
        player_stats_table <- player_stats_table %>%
          dplyr::rename(
            DE = .data$DE.,
            TOG = .data$TOG.,
            One.Percenters = .data$X1.
          )
      }
    }
  }
  return(player_stats_table)
}

#' GFetch aftables match ids
#'
#' Returns available match idds for a given season
#'
#' @param season A numeric value for season year
#'
#' @keywords internal
#' @noRd
fetch_footywire_match_ids <- function(season) {
  paste0("https://www.footywire.com/afl/footy/ft_match_list?", season) %>%
    xml2::read_html() %>%
    rvest::html_nodes(".data:nth-child(5) a") %>%
    rvest::html_attr("href") %>%
    stringr::str_extract("[0-9]+")
}

#' Extract match data
#'
#' Extracts match data from footywire given a valid match ID.
#'
#' @param match_id An XML file returned from `xml2::read_html`
#'
#' @keywords internal
#' @noRd
extract_match_data <- function(match_id) {
  # pb$tick()
  match_url <- paste0("https://www.footywire.com/afl/footy/ft_match_statistics?mid=", match_id)

  xml <- xml2::read_html(match_url)
  extract_footywire_match_table(xml)
}


#' Extract footywire match table
#'
#' Returns match results table from an XML file.
#'
#' @param xml An XML file returned from `xml2::read_html`
#'
#' @keywords internal
#' @noRd
extract_footywire_match_table <- function(xml) {
  tbl <- xml %>%
    rvest::html_nodes("#matchscoretable") %>%
    rvest::html_table() %>%
    .[[1]]

  tbl <- tbl %>%
    dplyr::rename(Points = .data$Final) %>%
    dplyr::select(.data$Team, .data$Points) %>%
    dplyr::mutate(Status = c("Home", "Away")) %>%
    tidyr::pivot_wider(
      names_from = .data$Status,
      values_from = c(.data$Team, .data$Points),
      names_sep = "."
    ) %>%
    dplyr::rename(
      Home.Team = .data$Team.Home,
      Away.Team = .data$Team.Away,
      Home.Points = .data$Points.Home,
      Away.Points = .data$Points.Away
    )

  match_details <- extract_footywire_match_details(xml)

  tbl <- tbl %>%
    dplyr::mutate(
      Date = match_details$date,
      Time = match_details$time,
      Round = match_details$round,
      Venue = match_details$venue
    ) %>%
    dplyr::select(
      .data$Date, .data$Time, .data$Round, .data$Venue,
      .data$Home.Team, .data$Away.Team,
      .data$Home.Points, .data$Away.Points
    )

  return(tbl)
}


#' Extract footywire match details
#'
#' Returns match details such as round, venue, date from an XML file.
#'
#' @param xml An XML file returned from `xml2::read_html`
#'
#' @keywords internal
#' @noRd
extract_footywire_match_details <- function(xml) {
  details <- xml %>%
    rvest::html_nodes(".lnorm") %>%
    rvest::html_text()

  date_time <- lubridate::dmy_hm(details[[2]])
  date <- date_time %>% as.Date()
  time <- date_time %>% strftime(format = "%H:%M", tz = "UTC")

  round <- stringr::str_split(details[[1]], ",")[[1]][1]
  venue <- stringr::str_split(details[[1]], ",")[[1]][2]

  list(
    date = date,
    time = time,
    round = round,
    venue = venue
  )
}


#' Parse round name
#'
#' Helper function to parse round name from footywire
#'
#' @param max_regular_round_number Max round regular round number for season
#'
#' @keywords internal
#' @noRd
parse_round_name <- function(max_regular_round_number) {
  FINALS_WEEK <- stringr::regex("Finals\\s+Week\\s+(\\d+)", ignore_case = TRUE)
  QUALIFYING_FINALS <- stringr::regex("qualifying", ignore_case = TRUE)
  ELIMINATION_FINALS <- stringr::regex("elimination", ignore_case = TRUE)
  # One bloody week in 2010 uses 'One' instead of '1' on
  # https://www.footywire.com/afl/footy/afl_betting
  FINALS_WEEK_ONE <- stringr::regex("Finals\\s+Week\\s+One", ignore_case = TRUE)
  SEMI_FINALS <- stringr::regex("semi", ignore_case = TRUE)
  PRELIMINARY_FINALS <- stringr::regex("preliminary", ignore_case = TRUE)
  GRAND_FINAL <- stringr::regex("grand", ignore_case = TRUE)


  return(
    function(round_name) {
      round_number <- stringr::str_match(round_name, DIGITS)[[2]]

      if (!is.na(round_number)) {
        return(round_number)
      }

      finals_week <- stringr::str_match(round_name, FINALS_WEEK)[[2]]

      if (!is.na(finals_week)) {
        # Betting data uses the format "YYYY Finals Week N" to label finals rounds
        # so we can just add N to max round to get the round number
        return(as.numeric(finals_week) + max_regular_round_number)
      }

      is_first_finals_week <- !is.na(stringr::str_match(round_name, QUALIFYING_FINALS)) ||
        !is.na(stringr::str_match(round_name, ELIMINATION_FINALS)) ||
        !is.na(stringr::str_match(round_name, FINALS_WEEK_ONE))

      if (is_first_finals_week) {
        return(max_regular_round_number + 1)
      }

      if (!is.na(stringr::str_match(round_name, SEMI_FINALS))) {
        return(max_regular_round_number + 2)
      }

      if (!is.na(stringr::str_match(round_name, PRELIMINARY_FINALS))) {
        return(max_regular_round_number + 3)
      }

      if (!is.na(stringr::str_match(round_name, GRAND_FINAL))) {
        return(max_regular_round_number + 4)
      }
    }
  )
}

#' Calculate round number for footywire data
#'
#' Helper function to parse round number from footywire
#'
#' @param round_names Names of rounds
#'
#' @keywords internal
#' @noRd
calculate_round_number <- function(round_names) {
  max_regular_round_number <- round_names %>%
    stringr::str_match_all(., DIGITS) %>%
    unlist(.) %>%
    as.numeric(.) %>%
    max(., na.rm = TRUE)

  round_names %>%
    purrr::map(., parse_round_name(max_regular_round_number)) %>%
    unlist(.)
}

#' Scrape footywire player statistics.
#'
#' \code{fetch_footywire_stats} returns a dataframe containing player match stats from footywire from 2010 onwards.
#'
#' The dataframe contains both basic and advanced player statistics from each match specified in the match_id input.
#' To find match ID, find the relevant matches on https://wwww.footywire.com
#'
#' @param ids A vector containing match id's to return. Can be a single value or vector of values.
#'
#'
#' @keywords internal
#' @noRd
fetch_footywire_stats <- function(ids) {
  if (missing(ids)) stop("Please provide an ID between 1 and 9999")
  if (!is.numeric(ids)) stop("ID must be numeric between 1 and 9999")

  # Initialise dataframe
  dat <- as.data.frame(matrix(ncol = 42, nrow = 44))

  # Now get data
  # First, only proceed if we've accessed the URL
  message("Getting data from https://www.footywire.com")

  # Create Progress Bar
  # nolint start
  pb <- dplyr::progress_estimated(length(ids), min_time = 5)

  # Loop through data using map
  dat <- ids %>%
    purrr::map_df(~ {
      pb$tick()$print() # update the progress bar (tick())
      get_match_data(id = .x) # do function
    })
  # nolint end

  # Rearrange
  dat <- dat %>%
    dplyr::arrange(.data$Date, .data$Match_id, dplyr::desc(.data$Status))

  # Finish and return
  message("Finished getting data")
  return(dat)
}

DIGITS <- stringr::regex("round\\s+(\\d+)", ignore_case = TRUE)