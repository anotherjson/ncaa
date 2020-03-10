# Start ----
## Required packages:
##  testthat
##  lubridate
##  tidyverse

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

  temp <- 1 / ((1 + (points_against / points_for))^exponent)

  return(temp)
}

calc_pyth_exp <- function(points_for, points_against, games, exponent) {
  temp <- (((points_for + points_against) / games) ^ exponent)

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
data_list <- dir(file.path(data_dir, "MDataFiles_Stage1"))
data_reg_compact <- read_csv(file.path(
  data_dir,
  "MDataFiles_Stage1/",
  "MRegularSeasonCompactResults.csv"
))

# Research ----
sample_data <- sample(10:1000, 50, replace = TRUE)
prediction <- sample_data / 10
actual <- sample(1:100, 50, replace = TRUE)

error <- actual - prediction
test <- rmse(error)

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
  mutate(Ratio = OppScore / TeamScore)

# Pyth expectations
exp_calc_pyth_start <- .001
exp_calc_pyth_end <- 1
error_list = tibble()

exp_calc_pyth_seq <- seq(exp_calc_pyth_start, exp_calc_pyth_end, by = 0.001)

for (exponent in exp_calc_pyth_seq) {
  df_reg_pyth <- df_reg_compact %>%
    select(Season, TeamId, TeamScore, OppScore)

  df_team_avg <- df_reg_pyth %>%
    mutate(exponent = calc_pyth_exp(TeamScore, OppScore, 1, exponent)) %>%
    group_by(Season, TeamId) %>%
    summarise(ExpAvg = mean(exponent))

  df_reg_pyth <- df_reg_pyth %>%
    left_join(df_team_avg, by = c("Season", "TeamId"))

  df_reg_pyth <-  df_reg_pyth %>%
    mutate(PythExp = calc_pyth(TeamScore, OppScore, ExpAvg),
           Diff = TeamScore - OppScore) %>%
    mutate(Result = if_else(Diff > 0, 1, if_else(Diff == 0, .5, 0)),
           Error = Result - PythExp)

  rmse_output <- df_reg_pyth %>%
    ungroup() %>%
    summarise(RMSE = rmse(Error))

  error_list <- error_list %>%
    rbind(tibble("exponent" = exponent, "rmse" = rmse_output$RMSE))
}
