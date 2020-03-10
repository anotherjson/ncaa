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
  rbind(df_reg_compact_loss)

exp_calc_pyth <- .001

pyth_exp <- calc_pyth_exp(df_reg_compact$TeamScore,
                          df_reg_compact$OppScore,
                          1,
                          exp_calc_pyth)

df_reg_compact <- df_reg_compact %>%
  mutate(
    Diff = TeamScore - OppScore,
    PythExp = calc_pyth(TeamScore, OppScore, pyth_exp)
  ) %>%
  glimpse()


# mutate(
#   Result = if_else(Diff > 0, 1, if_else(Diff == 0, .5, 0)),
#   Error = Result - PythExp
# ) %>%
#   glimpse()
#
# test <- rmse(df_reg_compact$Error)
