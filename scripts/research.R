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
data_reg_compact <- read_csv(file.path(
  data_dir,
  "MDataFiles_Stage1/",
  "MRegularSeasonCompactResults.csv"
))
data_error <- readRDS(file.path(data_dir, "errorList.RData"))


# Cleaning ----
exponent <- data_error %>%
  filter(rmse == min(rmse)) %>%
  select(exponent)

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
  mutate(PythExponent = calc_pyth_exp(
    TeamScore,
    OppScore,
    1,
    exponent$exponent
  ), ScoreDiff = TeamScore - OppScore) %>%
  mutate(
    PythExp = calc_pyth(TeamScore, OppScore, PythExponent),
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
    MeanPythExp = mean(PythExp),
    TotalResults = mean(Result),
    TotalWon = sum(Won)
  ) %>%
  mutate(WinPercent = TotalWon / Games,
         PythExp2 = calc_pyth(TotalTeamScore, TotalOppScore, 2),
         PythExp13 = calc_pyth(TotalTeamScore, TotalOppScore, 13),
         PythExpExp = calc_pyth(TotalTeamScore,
                                TotalOppScore,
                                calc_pyth_exp(TotalTeamScore,
                                              TotalOppScore,
                                              Games,
                                              exponent$exponent))) %>%
  mutate(Error = WinPercent - MeanPythExp,
         Error2 = WinPercent - PythExp2,
         Error13 = WinPercent - PythExp13,
         ErrorExp = WinPercent - PythExpExp)

print(rmse(df_reg_summary$Error))
print(rmse(df_reg_summary$Error2))
print(rmse(df_reg_summary$Error13))
print(rmse(df_reg_summary$ErrorExp))
