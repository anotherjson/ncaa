# Start ----
## Required packages:
##  testthat
##  lubridate
##  tidyverse
time_start <- Sys.time()
print("Starting script")

# Assumptions for exponent calculations
exp_calc_start <- 1
exp_calc_end <- 1000
exp_calc_by <- 1
exp_calc_place <- .001

# Functions ----
import_packages <- function() {
  # Import packages required for research
  # Req:
  #   testthat
  #   lubridate
  #   tidyverse
  # Arg:
  #   none
  # Return:
  #   none
  suppressPackageStartupMessages(library(testthat))
  suppressPackageStartupMessages(library(lubridate))
  suppressPackageStartupMessages(library(tidyverse))
}

rmse <- function(error) {
  # Finds the root mean square error
  # Req:
  #   none
  # Arg:
  #   error: the delta between actual and predicted values
  # Return:
  #   value as numeric
  stopifnot(is.integer(error) | is.numeric(error))

  as.numeric(sqrt(mean(error^2)))
}

calc_pyth <- function(points_for, points_against, exponent) {
  # Calculates the pyth expectations
  # Req:
  #   none
  # Arg:
  #   points_for: the goals scored for the team predicted
  #   points_against: the goals scoard against the team predicted
  #   exponent: ideal exponent based on individual sports research
  # Return:
  #   value as numeric
  stopifnot(
    is.numeric(points_for) | is.integer(points_for),
    is.numeric(points_against) | is.integer(points_against),
    is.numeric(exponent) | is.integer(exponent)
  )

  temp <- as.numeric(1 / (1 + ((points_against / points_for)^exponent)))

  return(temp)
}

calc_pyth_exp <- function(points_for, points_against, games, exponent) {
  # Calculates the exponent for pyth expectations
  # Req:
  #   none
  # Arg:
  #   points_for: the goals scored for the team predicted
  #   points_against: the goals scoard against the team predicted
  #   games: number of games played for range of games in points_for, against
  #   exponent: ideal exponent based on individual sports research
  # Return:
  #   value as numeric
  stopifnot(
    is.numeric(points_for) | is.integer(points_for),
    is.numeric(points_against) | is.integer(points_against),
    is.numeric(games) | is.integer(games),
    is.numeric(exponent) | is.integer(exponent)
  )

  temp <- as.numeric(((points_for + points_against) / games)^exponent)

  return(temp)
}

find_rmse_game <- function(df, df_spread, exponent_calc, numeric_place = 1) {
  # Calculates the exponent for pyth expectations by game
  # Req:
  #   tidyverse
  # Arg:
  #   df: data.frame with required column names
  #   df_spread: data.frame with estimated spreads to project against ScoreDiff
  #   exponent_calc: exponent to calc the exponent used in pyth expectations
  #   numeric_place: number to divide exponent_calc, default 1
  # Return:
  #   value as numeric
  cols_req <- c("TeamScore", "OppScore", "DayNum", "ScoreDiff")
  stopifnot(sum(cols_req %in% colnames(df)) == length(cols_req))

  exponent_calc <- exponent_calc * numeric_place

  df_pyth <- df %>%
    mutate(PythExp = calc_pyth(
      TeamScore,
      OppScore,
      calc_pyth_exp(
        TeamScore,
        OppScore,
        1,
        exponent_calc
      )
    )) %>%
    mutate(PythExpPercent = as.integer(round(PythExp, 2) * 100)) %>%
    left_join(df_spread, by = c("PythExpPercent" = "WinPercent")) %>%
    mutate(Error = ScoreDiff - Spread)

  df_rmse <- df_pyth %>%
    ungroup() %>%
    summarise(RMSEOutput = rmse(Error))

  return(df_rmse$RMSEOutput)
}

find_rmse_season <- function(df, exponent_calc, numeric_place = 1) {
  # Calculates the exponent for pyth expectations by season
  # Req:
  #   tidyverse
  # Arg:
  #   df: data.frame with required column names
  #   df_spread: data.frame with estimated spreads to project against ScoreDiff
  #   exponent_calc: exponent to calc the exponent used in pyth expectations
  #   numeric_place: number to divide exponent_calc, default 1
  # Return:
  #   value as numeric
  cols_req <- c("TotalTeamScore", "TotalOppScore", "Games", "TotalWon")
  stopifnot(sum(cols_req %in% colnames(df)) == length(cols_req))

  exponent_calc <- exponent_calc * numeric_place

  df_pyth <- df %>%
    mutate(PythExp = calc_pyth(
      TotalTeamScore,
      TotalOppScore,
      calc_pyth_exp(
        TotalTeamScore,
        TotalOppScore,
        Games,
        exponent_calc
      )
    )) %>%
    mutate(PythGames = PythExp * Games) %>%
    mutate(Error = TotalWon - PythGames)

  df_rmse <- df_pyth %>%
    ungroup() %>%
    summarise(RMSEOutput = rmse(Error))

  return(df_rmse$RMSEOutput)
}

find_rmse <- function(df,
                      exponent_start,
                      exponent_end,
                      exponent_by,
                      numeric_place,
                      season = TRUE,
                      df_spread = "None") {
  number_seq <- seq(exponent_start, exponent_end, by = exponent_by)
  pb <- progress::progress_bar$new(total = length(number_seq))
  df_temp <- tibble()
  number <- exponent_start

  bind_df <- function(df, df_bind, number_bind, number_place) {
    df_bind <- df_bind %>%
      rbind(tibble(
        "Exponent" = number_bind * numeric_place,
        "RMSEOutput" = ifelse(season,
          find_rmse_season(
            df,
            number_bind,
            number_place
          ),
          find_rmse_game(
            df,
            df_spread,
            number_bind,
            number_place
          )
        )
      ))
  }

  dummy_function <- function(df,
                             df_dummy,
                             number,
                             number_end,
                             number_by,
                             number_place) {
    if (number == number_end) {
      pb$tick()
      df_dummy <- bind_df(df, df_dummy, number, number_place)
      return(df_dummy)
    } else {
      pb$tick()
      df_dummy <- bind_df(df, df_dummy, number, number_place)
      number <- number + number_by
      dummy_function(df, df_dummy, number, number_end, number_by, number_place)
    }
  }
  df_return <- dummy_function(
    df,
    df_temp,
    number,
    exponent_end,
    exponent_by,
    numeric_place
  )
  return(df_return)
}

# Import packages ----
import_packages()

# Paths ----
working_dir <- getwd()
data_dir <- file.path(working_dir, "data")
outputs_dir <- file.path(working_dir, "outputs")
scripts_dir <- file.path(working_dir, "scripts")

# Data ----
data_list_sub <- dir(file.path(data_dir, "MDataFiles_Stage1"))
data_list <- dir(data_dir)
data_reg_compact <- read_csv(
  file.path(
    data_dir,
    "MDataFiles_Stage1/",
    "MRegularSeasonCompactResults.csv"
  )
)
data_win_spread <- readRDS(file.path(data_dir, "winSpread.RData"))

# Cleaning ----
df_reg_compact_won <- data_reg_compact %>%
  rename(TeamId = WTeamID) %>%
  rename_at(vars(matches("W")), ~ gsub("^(W)", "Team", .)) %>%
  rename_at(vars(matches("L")), ~ gsub("^(L)", "Opp", .)) %>%
  mutate(OppLoc = if_else(TeamLoc == "H", "A",
    if_else(TeamLoc == "A", "H", "N")
  ))

df_reg_compact_loss <- data_reg_compact %>%
  rename(TeamId = LTeamID) %>%
  rename_at(vars(matches("L")), ~ gsub("^(L)", "Team", .)) %>%
  rename_at(vars(matches("W")), ~ gsub("^(W)", "Opp", .)) %>%
  mutate(TeamLoc = if_else(OppLoc == "H", "A",
    if_else(OppLoc == "A", "H", "N")
  ))

df_reg_compact <- df_reg_compact_won %>%
  rbind(df_reg_compact_loss) %>%
  mutate(ScoreDiff = TeamScore - OppScore) %>%
  mutate(
    Won = ScoreDiff > 0,
    Tie = ScoreDiff == 0,
    Loss = ScoreDiff < 0,
    Result = if_else(ScoreDiff > 0, 1, if_else(ScoreDiff == 0, .5, 0))
  )

df_reg_summary <- df_reg_compact %>%
  group_by(Season, TeamId) %>%
  summarise(
    Games = length(DayNum),
    TotalTeamScore = sum(TeamScore),
    TotalOppScore = sum(OppScore),
    TotalResults = mean(Result),
    TotalWon = sum(Won)
  )

# Calculate best exponent for use in pyth exponent per game measurement ----
print("finding game exponent")
rmse_list_game <- find_rmse(df_reg_compact,
  exp_calc_start,
  exp_calc_end,
  exp_calc_by,
  exp_calc_place,
  season = FALSE,
  df_spread = data_win_spread
)

# Find game exponent where rmse is min ----
rmse_game_min <- rmse_list_game %>%
  filter(RMSEOutput == min(RMSEOutput))

# Calculate best exponent for use in pyth exponent per season measurement ----
print("finding season exponent")
rmse_list_season <- find_rmse(
  df_reg_summary,
  exp_calc_start,
  exp_calc_end,
  exp_calc_by,
  exp_calc_place
)

# Find season exponent where rmse is min ----
rmse_season_min <- rmse_list_season %>%
  filter(RMSEOutput == min(RMSEOutput))

# Write data ----
saveRDS(rmse_list_game, file.path(data_dir, "rmseListGame.RData"))
saveRDS(rmse_list_season, file.path(data_dir, "rmseListSeason.RData"))

# End ----
time_end <- Sys.time()
time_diff <- time_end - time_start
print(time_diff)
