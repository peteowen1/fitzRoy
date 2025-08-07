#' Fetch Valid Teams from AFL API
#'
#' Returns a dataframe of teams
#'
#' @param comp "AFLM" or "AFLW"
#' @keywords internal
#' @noRd
fetch_teams_afl <- function(comp) {
  team_api <- function(page) {
    api <- "https://aflapi.afl.com.au/afl/v2/teams"

    cont <- safe_afl_api_call(
      url = api,
      query = list(
        "pageSize" = "1000",
        page = page
      )
    )
  }

  cont <- team_api(0)

  # check_for_more
  if (cont$meta$pagination$numPages > 1) {
    page_ind <- 0:(cont$meta$pagination$numPages - 1)

    teams <- page_ind %>%
      purrr::map(team_api) %>%
      purrr::map_dfr(purrr::pluck, "teams")
  } else {
    teams <- cont$teams
  }


  df <- teams %>%
    dplyr::select(
      "id", "abbreviation",
      "name", "teamType",
      "club.id", "club.providerId", "club.name", "club.abbreviation", "club.nickname"
    ) %>%
    stats::na.omit()

  type <- dplyr::case_when(
    comp == "AFLM" ~ "MEN",
    comp == "AFLW" ~ "WOMEN",
    comp == "VFL" ~ "VFL_MEN",
    comp == "VFLW" ~ "VFL_WOMEN",
    comp == "U18B" ~ "U18_BOYS",
    comp == "U18G" ~ "U18_GIRLS",
    comp == "WAFL" ~ "WAFL_MEN",
    is.null(comp) ~ "ALL",
    TRUE ~ ""
  )

  if (type == "ALL") {
    return(df)
  }

  df[df$teamType == type, ]
}



#' Find Team ID
#'
#' Returns the ID for the team
#'
#' @param team Afl team name
#' @param comp "AFLM" or "AFLW"
#' @keywords internal
#' @noRd
find_team_id <- function(team_abr, comp = "AFLM") {
  check_comp(comp)

  df <- fetch_teams_afl(comp)

  if (is.null(team_abr)) {
    return(df)
  }

  ids <- df$id[df$abbreviation == team_abr]
  min(ids, na.rm = TRUE)
}
#' Check if a team is valid for afl website
#'
#' @param team Team
#'
#' @keywords internal
#' @noRd
team_check_afl <- function(team) {
  cli::cli_warn("In future versions of `fetch_player_details`, teams will need to match the official AFL API teams.
              You can use `official_teams = TRUE` to test this behaviour and change your code before this breaking change",
    .frequency = "regularly",
    .frequency_id = "fpd_depr",
    id = "fpd_depr"
  )

  valid_teams <- c(
    "Adelaide", "Brisbane Lions",
    "Carlton", "Collingwood", "Essendon",
    "Fremantle", "GWS", "Geelong", "Gold Coast",
    "Hawthorn", "Melbourne", "North Melbourne",
    "Port Adelaide", "Richmond", "St Kilda",
    "Sydney", "West Coast",
    "Western Bulldogs"
  )

  valid <- team %in% valid_teams

  if (!valid) {
    cli::cli_abort("{team} is not a valid input for afl teams.
                            Should be one of {glue::glue_collapse(valid_teams, sep = \", \")} ")
  }
}

#' Internal function to return team name abbreviation for AFL API
#' @param team Team name
#' @param return_id Should we return the team ID used by the API instead of the abbreviation?
#' @export
team_abr_afl <- function(team, return_id = FALSE) {
  cli::cli_warn("In future versions of `fetch_player_details`, teams will need to match the official AFL API teams.
              You can use `official_teams = TRUE` to test this behaviour and change your code before this breaking change",
    .frequency = "regularly",
    .frequency_id = "fpd_depr",
    id = "fpd_depr"
  )

  if (return_id) {
    # Internal function
    dplyr::case_when(
      team == "Adelaide" ~ 1,
      team == "Kuwarna" ~ 1,
      team == "Brisbane Lions" ~ 2,
      team == "Collingwood" ~ 3,
      team == "Gold Coast" ~ 4,
      team == "Carlton" ~ 5,
      team == "North Melbourne" ~ 6,
      team == "Port Adelaide" ~ 7,
      team == "Yartapuulti" ~ 7,
      team == "Western Bulldogs" ~ 8,
      team == "Hawthorn" ~ 9,
      team == "Geelong" ~ 10,
      team == "St Kilda" ~ 11,
      team == "Euro-Yroke" ~ 11,
      team == "Essendon" ~ 12,
      team == "Sydney" ~ 13,
      team == "Fremantle" ~ 14,
      team == "Walyalup" ~ 14,
      team == "GWS" ~ 15,
      team == "Richmond" ~ 16,
      team == "Melbourne" ~ 17,
      team == "Narrm" ~ 17,
      team == "West Coast" ~ 18,
      team == "Waalitj Marawar" ~ 18,
      TRUE ~ -1
    )
  } else {
    # Internal function
    dplyr::case_when(
      team == "Adelaide" ~ "ADEL",
      team == "Kuwarna" ~ "KUW",
      team == "Brisbane Lions" ~ "BL",
      team == "Collingwood" ~ "COLL",
      team == "Gold Coast" ~ "GCFC",
      team == "Carlton" ~ "CARL",
      team == "North Melbourne" ~ "NMFC",
      team == "Port Adelaide" ~ "PORT",
      team == "Yartapuulti" ~ "YAR",
      team == "Western Bulldogs" ~ "WB",
      team == "Hawthorn" ~ "HAW",
      team == "Geelong" ~ "GEEL",
      team == "St Kilda" ~ "STK",
      team == "Euro-Yroke" ~ "EUR",
      team == "Essendon" ~ "ESS",
      team == "Sydney" ~ "SYD",
      team == "Fremantle" ~ "FRE",
      team == "Walyalup" ~ "WAL",
      team == "GWS" ~ "GWS",
      team == "Richmond" ~ "RICH",
      team == "Melbourne" ~ "MELB",
      team == "Narrm" ~ "NAR",
      team == "West Coast" ~ "WCE",
      team == "Waalitj Marawar" ~ "WAA",
      TRUE ~ team
    )
  }

}


#' Check if a team is valid for afl website
#'
#' @param team Team name
#' @param comp Competition
#' @keywords internal
#' @noRd
team_check_afl2 <- function(team, comp = "AFLM") {
  valid_teams <- fetch_teams_afl(comp)


  valid <- team %in% valid_teams$name
  
  if (!valid) {
    valid <- team %in% valid_teams$club.name
  }

  if (!valid) {
    cli::cli_abort("\"{team}\" is not a valid input for afl teams for the \"{comp}\" comp.
                            Run `fetch_teams_afl(\"{comp}\")` to see a list of valid teams")
  }
}

#' Internal function to return team name abbreviation for AFL API
#' @param team Team name
#' @param comp Competition
#' @keywords internal
#' @export
team_abr_afl2 <- function(team, comp = "AFLM") {
  teams <- fetch_teams_afl(comp)

  abr <- teams$abbreviation[teams$name == team]
  
  if (length(abr) < 1) {
    abr <- teams$abbreviation[teams$club.name == team]
  }
  
  return(abr)
}

#' Find Comp ID
#'
#' Returns the ID for the comp
#'
#' @param comp One of "AFLM" or "AFLW"
#' @keywords internal
#' @noRd
find_comp_id <- function(comp) {
  comp <- check_comp(comp)

  api_url <- "https://aflapi.afl.com.au/afl/v2/competitions?pageSize=50"

  cont <- safe_afl_api_call(api_url)

  cont$competitions <- cont$competitions %>%
    dplyr::filter(!stringr::str_detect(.data$name, "Legacy"))

  if (comp == "AFLM") comp <- "AFL"

  ids <- cont$competitions$id[cont$competitions$code == comp]
  min(ids, na.rm = TRUE)
}

#' Get AFL Stats cookie (internal function)
#'
#' Gets a cookie from http://www.afl.com.au/ to authenticate
#' further requests.
#'
#' @return token code
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' cookie <- get_afl_cookie()
#' }
#' @export
get_afl_cookie <- function() {
  json_data <- safe_api_call(
    url = "https://api.afl.com.au/cfs/afl/WMCTok",
    method = "POST"
  )
  json_data$token
}

#' Find Season ID
#'
#' Returns the ID for the season
#'
#' @param season Season, in YYYY format.
#' @keywords internal
#' @noRd
find_season_id <- function(season, comp = "AFLM") {
  # check inputs
  season <- check_season(season)
  comp <- check_comp(comp)

  comp_id <- find_comp_id(comp)

  api <- paste0("https://aflapi.afl.com.au/afl/v2/competitions/", comp_id, "/compseasons?pageSize=100")

  cont <- safe_afl_api_call(api)

  comp_ids <- cont$compSeasons %>%
    dplyr::filter(!stringr::str_detect(.data$name, "Legacy")) %>%
    dplyr::mutate(season = as.numeric(gsub("^.*([0-9]{4}).*", "\\1", .data$name)))

  id <- comp_ids$id[comp_ids$season == season]

  id <- id[!is.na(id)]

  if (length(id) < 1) {
    cli::cli_warn("Could not find a matching ID to the {comp} for {season}")
    return(NULL)
  }
  return(id)
}

#' Find Season ID
#'
#' Returns the ID for the season
#'
#' @param season Season, in YYYY format.
#' @keywords internal
#' @noRd
find_round_id <- function(round_number,
                          season = NULL,
                          season_id = NULL,
                          comp = "AFLM",
                          providerId = FALSE,
                          future_rounds = TRUE) {
  if (providerId) {
    id_name <- "providerId"
  } else {
    id_name <- "id"
  }

  # check inputs
  season <- check_season(season)
  comp <- check_comp(comp)

  if (is.null(season_id)) season_id <- find_season_id(season, comp)

  api <- paste0("https://aflapi.afl.com.au/afl/v2/compseasons/", season_id, "/rounds")

  cont <- safe_afl_api_call(
    url = api,
    query = list(pageSize = 30)
  )

  df <- cont$rounds

  if (!future_rounds) {
    df <- df[df$utcStartTime < Sys.Date() & df$utcStartTime != "", ]
  }

  if (is.null(round_number)) {
    id <- df[, id_name]
  } else {
    id <- df[df$roundNumber %in% round_number, id_name]
  }


  if (length(id) < 1) {
    cli::cli_warn("No data found for specified round number and season")
    return(NULL)
  }
  return(id)
}


#' Returns match roster
#'
#' @param id Match ID from AFL website
#' @param cookie cookie from AFL website, can be returned with `get_afl_cookie`
#' @keywords internal
#' @noRd
fetch_match_roster_afl <- function(id, cookie = NULL) {
  if (is.null(cookie)) cookie <- get_afl_cookie()

  api <- paste0("https://api.afl.com.au/cfs/afl/matchRoster/full/", id)

  # Use safe_afl_api_call with authentication header
  cont <- tryCatch({
    safe_afl_api_call(
      url = api,
      headers = list("x-media-mis-token" = cookie)
    )
  }, error = function(e) {
    if (grepl("404", e$message)) {
      cli::cli_warn("No match found for match ID {.val {id}}. Returning empty tibble")
      return(tibble::tibble())
    } else {
      stop(e)
    }
  })

  # If we got an empty tibble from 404 error, return it
  if (inherits(cont, "tbl_df") && nrow(cont) == 0) {
    return(cont)
  }

  # Check if matchRoster data is available
  if (is.null(cont$matchRoster)) {
    return(tibble::tibble())
  }

  cont$matchRoster$homeTeam$clubDebuts <- list()
  cont$matchRoster$homeTeam$milestones <- list()
  cont$matchRoster$homeTeam$ins <- list()
  cont$matchRoster$homeTeam$outs <- list()

  home_df <- cont$matchRoster$homeTeam %>%
    purrr::compact() %>%
    purrr::flatten_dfr() %>%
    dplyr::mutate(teamType = "home")

  cont$matchRoster$awayTeam$clubDebuts <- list()
  cont$matchRoster$awayTeam$milestones <- list()
  cont$matchRoster$awayTeam$ins <- list()
  cont$matchRoster$awayTeam$outs <- list()

  away_df <- cont$matchRoster$awayTeam %>%
    purrr::compact() %>%
    purrr::flatten_dfr() %>%
    dplyr::mutate(teamType = "away")

  dplyr::bind_rows(home_df, away_df)
}

#' Returns match stats for a given match ID
#'
#' @param id Match ID from AFL website
#' @param cookie cookie from AFL website, can be returned with `get_afl_cookie`
#' @keywords internal
#' @noRd
fetch_match_stats_afl <- function(id, cookie = NULL) {
  if (is.null(cookie)) cookie <- get_afl_cookie()
  
  api <- paste0("https://api.afl.com.au/cfs/afl/playerStats/match/", id)
  
  # 1. Catch any error (e.g. 404) and return NULL instead
  cont <- tryCatch({
    safe_afl_api_call(
      url = api,
      headers = list("x-media-mis-token" = cookie)
    )
  }, error = function(e) {
    message("AFL API call failed: ", e$message)
    return(NULL)
  })
  
  # 2. If parsing failed, return an empty tibble right away
  if (is.null(cont) | is.null(cont$homeTeamPlayerStats)) {
    return(tibble::tibble())
  }
  
  # 3. Otherwise proceed as before
  home_df <- cont$homeTeamPlayerStats %>%
    dplyr::select(-teamId, -"playerStats.lastUpdated") %>%
    clean_names_playerstats_afl() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(teamStatus = "home")
  
  away_df <- cont$awayTeamPlayerStats %>%
    dplyr::select(-teamId, -"playerStats.lastUpdated") %>%
    clean_names_playerstats_afl() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(teamStatus = "away")
  
  df <- dplyr::bind_rows(home_df, away_df) %>%
    dplyr::mutate(providerId = id)
  
  return(df)
}


#' Cleans names for player stats
#'
#' @param x data frame returned from `fetch_match_stats_afl`
#' @keywords internal
#' @noRd
clean_names_playerstats_afl <- function(x) {
  names(x) <- gsub(x = names(x), pattern = "playerStats\\.", replacement = "")
  # names(x) <- gsub(x = names(x), pattern = "player\\.", replacement = "")
  names(x) <- gsub(x = names(x), pattern = "stats\\.", replacement = "")
  names(x) <- gsub(x = names(x), pattern = "playerName\\.", replacement = "")
  return(x)
}

#' Fetches results of all matches in a round
#'
#' @param id round id returned by `find_round_id`
#' @param cookie cookie returned by `get_afl_cookie`
#' @keywords internal
#' @noRd
fetch_round_results_afl <- function(id, cookie = NULL) {
  if (is.null(cookie)) cookie <- get_afl_cookie()

  url_api <- paste0("http://api.afl.com.au/cfs/afl/matchItems/round/", id)

  cont <- safe_afl_api_call(
    url = url_api,
    headers = list("x-media-mis-token" = cookie)
  )

  df <- dplyr::as_tibble(cont$items)

  # Fix names
  names(df) <- gsub(x = names(df), pattern = "score\\.", replacement = "")

  # add date
  df <- df %>%
    dplyr::mutate(match.date = lubridate::ymd_hms(.data$match.date))

  # remove unwanted columns
  df <- df %>%
    dplyr::select(-dplyr::contains(c("url", "link", "Videos")))

  return(df)
}


#' Fetches squad
#'
#' @param teamId team id returned by `find_team_id`
#' @param compSeasonId comp season id returned by `find_season_id`
#' @keywords internal
#' @noRd

fetch_squad_afl <- function(teamId, team, season, compSeasonId) {
  cli::cli_progress_step("Fetching player details for {team}, {season}")
  api <- "https://aflapi.afl.com.au/afl/v2/squads"

  cont <- tryCatch({
    safe_afl_api_call(
      url = api,
      query = list(
        "teamId" = teamId,
        "compSeasonId" = compSeasonId,
        "pageSize" = "1000"
      )
    )
  }, error = function(e) {
    return(NULL)
  })

  df <- dplyr::as_tibble(cont$squad$players)

  names(df) <- gsub("player.", "", names(df))

  df %>%
    dplyr::mutate(
      season = season,
      team = team
    ) %>%
    dplyr::select(
      "firstName",
      "surname",
      "id",
      "team",
      "season",
      dplyr::everything()
    )
}

#' Parses afl response and checks for errors
#'
#' @param resp response object returned from POST/GET
#' @keywords internal
#' @noRd
parse_resp_afl <- function(resp) {
  # Updated to work with httr2 responses
  if (!httr2::resp_has_body(resp)) {
    cli::cli_abort("API response has no body")
  }
  
  content_type <- httr2::resp_content_type(resp)
  if (!grepl("application/json", content_type)) {
    cli::cli_abort("API did not return json")
  }

  parsed <- resp %>%
    httr2::resp_body_string() %>%
    jsonlite::fromJSON(flatten = TRUE)

  if (httr2::resp_is_error(resp)) {
    cli::cli_abort(
      "AFL API request failed
        {httr2::resp_status(resp)} - {parsed$techMessage}"
    )
  }
  return(parsed)
}
