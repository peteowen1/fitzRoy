# Constants for fitzRoy Package
#
# Centralized constants to reduce hardcoded values throughout the codebase

# API Base URLs
AFL_API_BASE_URL <- "https://aflapi.afl.com.au"
AFL_OLD_API_BASE_URL <- "https://api.afl.com.au" 
FOOTYWIRE_BASE_URL <- "https://www.footywire.com"
AFLTABLES_BASE_URL <- "https://afltables.com"
SQUIGGLE_API_BASE_URL <- "https://api.squiggle.com.au"

# AFL API Endpoints
AFL_MATCH_ENDPOINT <- "/cfs/afl/matches"
AFL_LADDER_ENDPOINT <- "/cfs/afl/ladder"
AFL_PLAYER_STATS_ENDPOINT <- "/cfs/afl/playerStats"
AFL_LINEUP_ENDPOINT <- "/cfs/afl/lineups"

# Footywire Paths
FOOTYWIRE_MATCH_STATS_PATH <- "/afl/footy/ft_match_statistics"
FOOTYWIRE_FIXTURE_PATH <- "/afl/footy/ft_match_list"
FOOTYWIRE_SUPERCOACH_PATH <- "/afl/footy/sc_round"
FOOTYWIRE_DREAMTEAM_PATH <- "/afl/footy/dt_round"

# Valid Competition Types
VALID_COMPETITIONS <- c(
  "AFLM",
  "AFLW", 
  "VFL",
  "VFLM",
  "VFLW", 
  "WAFL",
  "U18B",
  "U18G"
)

# Valid Data Sources
VALID_SOURCES <- c(
  "AFL",
  "footywire",
  "afltables", 
  "squiggle",
  "fryzigg",
  "vflstats"
)

# AFL Team Abbreviations
AFL_TEAM_ABBREVIATIONS <- c(
  "Adelaide" = "ADE",
  "Brisbane Lions" = "BL", 
  "Carlton" = "CAR",
  "Collingwood" = "COL",
  "Essendon" = "ESS",
  "Fremantle" = "FRE",
  "Geelong" = "GEE", 
  "Gold Coast" = "GC",
  "GWS" = "GWS",
  "Hawthorn" = "HAW",
  "Melbourne" = "MEL",
  "North Melbourne" = "NM",
  "Port Adelaide" = "PA", 
  "Richmond" = "RIC",
  "St Kilda" = "STK",
  "Sydney" = "SYD",
  "West Coast" = "WC",
  "Western Bulldogs" = "WB"
)

# Season Constraints  
EARLIEST_AFL_SEASON <- 1897
CURRENT_SEASON <- as.integer(format(Sys.Date(), "%Y"))
LATEST_VALID_SEASON <- CURRENT_SEASON + 1

# Round Constraints
MIN_ROUND_NUMBER <- 1
MAX_ROUND_NUMBER <- 30  # Includes finals

# HTTP Configuration
DEFAULT_TIMEOUT_SECONDS <- 30
DEFAULT_USER_AGENT <- "fitzRoy R package"
DEFAULT_MAX_RETRIES <- 3
DEFAULT_RETRY_DELAY <- 1

# Cache Configuration  
DEFAULT_CACHE_HOURS <- 24
DEFAULT_MAX_CACHE_AGE_HOURS <- 168  # 1 week

# Data Processing Constants
FOOTYWIRE_STATS_COLUMNS <- 42  # Expected number of columns
FOOTYWIRE_STATS_ROWS <- 44     # Expected number of rows per match

# AFL API Page Sizes
AFL_API_DEFAULT_PAGE_SIZE <- 50
AFL_API_MAX_PAGE_SIZE <- 1000

# Content Types
JSON_CONTENT_TYPE <- "application/json"
HTML_CONTENT_TYPE <- "text/html"

# AFL Venue Mappings (commonly used venues)
AFL_VENUE_ABBREVIATIONS <- c(
  "Marvel Stadium" = "Marvel",
  "Melbourne Cricket Ground" = "MCG",
  "Adelaide Oval" = "Adelaide Oval",
  "Optus Stadium" = "Optus",
  "The Gabba" = "Gabba",
  "Sydney Cricket Ground" = "SCG",
  "GMHBA Stadium" = "GMHBA"
)

# Data Source Capabilities Matrix
# Which sources support which competitions
SOURCE_COMP_SUPPORT <- list(
  "AFL" = c("AFLM", "AFLW", "VFL", "VFLW", "WAFL", "U18B", "U18G"),
  "footywire" = c("AFLM"),
  "afltables" = c("AFLM"),
  "squiggle" = c("AFLM"), 
  "fryzigg" = c("AFLM"),
  "vflstats" = c("VFL", "VFLW")
)

# Error Messages Templates
ERROR_MESSAGES <- list(
  invalid_season = "Season must be between {EARLIEST_AFL_SEASON} and {LATEST_VALID_SEASON}",
  invalid_round = "Round number must be between {MIN_ROUND_NUMBER} and {MAX_ROUND_NUMBER}",
  invalid_comp = "Competition must be one of: {paste(VALID_COMPETITIONS, collapse = ', ')}",
  invalid_source = "Source must be one of: {paste(VALID_SOURCES, collapse = ', ')}",
  no_data = "No data found for the specified parameters",
  network_error = "Network request failed. Please check your internet connection",
  api_error = "API request failed with status code {status_code}"
)