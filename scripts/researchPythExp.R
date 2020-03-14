# Start ----
## Required packages:
##  testthat
##  lubridate
##  tidyverse
time_start <- Sys.time()
print("Starting script")

# Functions ----
import_packages <- function() {
  # Import packages required for research
  # Req:
  #   none
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
  #   value as numeric or int
  as.numeric(sqrt(mean(error^2)))
}

calc_pyth <- function(points_for, points_against, exponent) {
  # Calculates the pyth expectations
  # Req:
  #   none
  # Arg:
  #   goals_for: the goals scored for the team predicted
  #   goals_against: the goals scoard against the team predicted
  #   exponent: ideal exponent based on individual sports research
  # Return:
  #   value as numeric
  stopifnot(
    is.numeric(points_for) | is.integer(points_for),
    is.numeric(points_against) | is.integer(points_against)
  )

  temp <- 1 / (1 + ((points_against / points_for)^exponent))

  return(temp)
}

calc_pyth_exp <- function(points_for, points_against, games, exponent) {
  temp <- (((points_for + points_against) / games)^exponent)

  return(temp)
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
exp_calc_start <- .001
exp_calc_end <- 1
rmse_list_game <- tibble()

exp_calc_seq <- seq(exp_calc_start, exp_calc_end, by = 0.001)

pb <- progress::progress_bar$new(total = length(exp_calc_seq))
for (e in exp_calc_seq) {
  pb$tick()
  df_pyth <- df_reg_compact %>%
    select(Season, DayNum, TeamId, TeamScore, OppScore, ScoreDiff, Won) %>%
    mutate(Exponent = calc_pyth_exp(TeamScore, OppScore, 1, e))

  df_pyth_avg <- df_pyth %>%
    group_by(Season, TeamId) %>%
    summarise(ExpAvg = mean(Exponent))

  df_pyth <- df_pyth %>%
    left_join(df_pyth_avg, by = c("Season", "TeamId")) %>%
    mutate(PythExpGame = calc_pyth(TeamScore, OppScore, ExpAvg)) %>%
    mutate(PythExpPercent = as.integer(round(PythExpGame, 2) * 100)) %>%
    left_join(data_win_spread, by = c("PythExpPercent" = "WinPercent")) %>%
    mutate(Error = ScoreDiff - Spread)

  rmse_output_game <- df_pyth %>%
    ungroup() %>%
    summarise(RMSEOutput = rmse(Error))

  rmse_list_game <- rmse_list_game %>%
    rbind(tibble("Exponent" = e, "RMSEOutput" = rmse_output_game$RMSEOutput))
}

# Find exponent where rmse is min ----
rmse_game_min <- rmse_list_game %>%
  filter(RMSEOutput == min(RMSEOutput)) %>%
  select(Exponent)

# Calculate best exponent for use in pyth exponent per season measurement ----
exp_calc_start <- .001
exp_calc_end <- 1
rmse_list_season <- tibble()

exp_calc_seq <- seq(exp_calc_start, exp_calc_end, by = 0.001)
pb <- progress::progress_bar$new(total = length(exp_calc_seq))
for (e in exp_calc_seq) {
  pb$tick()
  df_pyth_season <- df_reg_summary %>%
    select(Season, TeamId, TotalTeamScore, TotalOppScore, Games, TotalWon) %>%
    mutate(Exponent = calc_pyth_exp(TotalTeamScore, TotalOppScore, Games, e))

  df_pyth_avg <- df_pyth_season %>%
    group_by(Season, TeamId) %>%
    summarise(ExpAvg = mean(Exponent))

  df_pyth_season <- df_pyth_season %>%
    left_join(df_pyth_avg, by = c("Season", "TeamId")) %>%
    mutate(PythExp = calc_pyth(TotalTeamScore, TotalOppScore, ExpAvg)) %>%
    mutate(PythGames = PythExp * Games) %>%
    mutate(Error = TotalWon - PythGames)

  rmse_output_season <- df_pyth_season %>%
    ungroup() %>%
    summarise(RMSEOutput = rmse(Error))

  rmse_list_season <- rmse_list_season %>%
    rbind(tibble(
      "Exponent" = e,
      "RMSEOutput" = rmse_output_season$RMSEOutput
    ))
}

rmse_season_min <- rmse_list_season %>%
  filter(RMSEOutput == min(RMSEOutput))

# Write data ----
saveRDS(rmse_list_game, file.path(data_dir, "rmseListGame.RData"))
saveRDS(rmse_list_season, file.path(data_dir, "rmseListSeason.RData"))

# End ----
time_end <- Sys.time()
time_diff <- time_end - time_start
print(time_diff)
