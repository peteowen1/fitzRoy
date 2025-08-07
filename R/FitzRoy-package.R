#' @keywords internal
"_PACKAGE"

#' @importFrom tibble tibble

## quiets concerns of R CMD check re data
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    # Data objects
    "player_stats", ".",
    
    # Column names used in NSE contexts
    "Away.Team", "Games", "Games_Polled", "Home.Team", "Player", 
    "Players_With_Votes", "Polled", "Round", "Season", "Team", 
    "V/G", "Votes", "Votes_1", "Votes_2", "Votes_3", "teamId", "type",
    
    # Variables from other files
    "dictionary_afltables", "mapping_afltables", "x", "y", "round_mapping",
    "periodNumber", "periodSeconds",
    
    # dplyr helper functions
    "where"
  ))
}

## usethis namespace: start
#' @importFrom lifecycle deprecated
## usethis namespace: end
NULL
