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
))
data_reg_detail <- read_csv(
  file.path(data_dir,
            "MDataFiles_Stage1/",
            "MRegularSeasonDetailedResults.csv"))

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
    TotalWon = sum(Won),
    MedianSpread = median(TeamScore - OppScore)
  ) %>%
  mutate(WinPercent = TotalWon / Games) %>%
  mutate(WinPercentRound = round(WinPercent, 2))

# Create spread chart by win percent ----
df_win_spread <- df_reg_summary %>%
  group_by(WinPercentRound) %>%
  summarise(Spread = median(MedianSpread)) %>%
  rename(WinPercent = WinPercentRound) %>%
  mutate(WinPercent = as.integer(WinPercent * 100))

df_win_spread <- tibble(WinPercent = seq(0, 100, by = 1)) %>%
  left_join(df_win_spread) %>%
  fill(Spread, .direction = c("down"))

# Write data ----
saveRDS(df_win_spread, file.path(data_dir, "winSpread.RData"))

# End ----
time_end <- Sys.time()
time_diff <- time_end - time_start
print(time_diff)