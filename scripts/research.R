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
data_reg_compact <- read_csv(file.path(
  data_dir,
  "MDataFiles_Stage1/",
  "MRegularSeasonCompactResults.csv"
))

data_exponent_game <- readRDS(file.path(data_dir, "rmseListGame.RData"))
data_exponent_season <- readRDS(file.path(data_dir, "rmseListSeason.RData"))

# Cleaning and modeling ----
exponent_game <- tibble(Exponent = 2.17)

exponent_season <- data_exponent_season %>%
  filter(RMSEOutput == min(RMSEOutput)) %>%
  summarise(Exponent = min(Exponent))

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
    exponent_game$Exponent
  ), ScoreDiff = TeamScore - OppScore) %>%
  mutate(
    PythExpGame = calc_pyth(TeamScore, OppScore, PythExponent),
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
    PythExpGame = mean(PythExpGame),
    TotalResults = mean(Result),
    TotalWon = sum(Won)
  ) %>%
  mutate(PythExponent = calc_pyth_exp(
    TotalTeamScore,
    TotalOppScore,
    Games,
    exponent_season$Exponent
  )) %>%
  mutate(
    WinPercent = TotalWon / Games,
    PythExp2 = calc_pyth(TotalTeamScore, TotalOppScore, 2),
    PythExp13 = calc_pyth(TotalTeamScore, TotalOppScore, 13),
    PythExpSeason = calc_pyth(
      TotalTeamScore,
      TotalOppScore,
      PythExponent
    )
  ) %>%
  mutate(
    PythExpGameGames = PythExpGame * Games,
    PythExp2Games = PythExp2 * Games,
    PythExp13Games = PythExp13 * Games,
    PythExpSeasonGames = PythExpSeason * Games
  ) %>%
  mutate(
    ErrorGame = TotalWon - PythExpGameGames,
    Error2 = TotalWon - PythExp2Games,
    Error13 = TotalWon - PythExp13Games,
    ErrorSeason = TotalWon - PythExpSeasonGames
  )

# Errors ----
print(paste("Error Game:", rmse(df_reg_summary$ErrorGame)))
print(paste("Error 2:", rmse(df_reg_summary$Error2)))
print(paste("Error 13:", rmse(df_reg_summary$Error13)))
print(paste("Error Season:", rmse(df_reg_summary$ErrorSeason)))

# Correlations ----
print(paste("Cor Game:", cor(
  df_reg_summary$TotalWon,
  df_reg_summary$PythExpGameGames
)))
print(paste("Cor 2:", cor(
  df_reg_summary$TotalWon,
  df_reg_summary$PythExp2Games
)))
print(paste("Cor 13:", cor(
  df_reg_summary$TotalWon,
  df_reg_summary$PythExp13Games
)))
print(paste("Cor Season:", cor(
  df_reg_summary$TotalWon,
  df_reg_summary$PythExpSeasonGames
)))

# Standard deviation ----
print(paste("SD Game:", sd(df_reg_summary$ErrorGame)))
print(paste("SD 2:", sd(df_reg_summary$Error2)))
print(paste("SD 13:", sd(df_reg_summary$Error13)))
print(paste("SD Season", sd(df_reg_summary$ErrorSeason)))
