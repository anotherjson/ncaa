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

# Import packages ----
import_packages()

# Setup paths ----
working_dir <- getwd()
data_dir <- file.path(working_dir, "data")
data_sub_dir <- file.path(data_dir, "MDataFiles_Stage1")
outputs_dir <- file.path(working_dir, "outputs")
scripts_dir <- file.path(working_dir, "scripts")

# Bring in data ----
data_reg_compact <- readr::read_csv(
  file.path(data_sub_dir, "MRegularSeasonCompactResults.csv"))

data_win_spread <- readRDS(file.path(data_dir, "winSpread.RData"))

# Clean data ----
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
    Loss = ScoreDiff < 0
  )

df_reg_summary <- df_reg_compact %>%
  group_by(Season, TeamId) %>%
  summarise(
    Games = length(DayNum),
    TotalTeamScore = sum(TeamScore),
    TotalOppScore = sum(OppScore),
    TotalWon = sum(Won)
  )

# Write data ----
saveRDS(df_reg_compact, file.path(data_dir, "regularSeasonCompactGame.RData"))
saveRDS(df_reg_summary, file.path(data_dir, "regularSeasonCompactSummary.RData"))