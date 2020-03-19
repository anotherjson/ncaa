# Start ----
## Required packages:
##  testthat
##  lubridate
##  tidyverse
time_start <- Sys.time()
print("Starting script")

# Assumptions ----

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

# Import functions ----
import_packages()

calc_rmse <- function(error) {
  # Finds the root mean square error
  # Req:
  #   none
  # Arg:
  #   error: the delta between actual and predicted values
  # Return:
  #   value as numeric or NA
  return(sqrt(mean(as.numeric(error)^2)))
}

# Setup paths ----
working_dir <- getwd()
data_dir <- file.path(working_dir, "data")
data_sub_dir <- file.path(data_dir, "MDataFiles_Stage1")
outputs_dir <- file.path(working_dir, "outputs")
scripts_dir <- file.path(working_dir, "scripts")

# Bring in data ----
data_reg_compact <- readRDS(
  file.path(data_dir, "regularSeasonCompactGame.RData"))

data_reg_summary <- readRDS(
  file.path(data_dir, "regularSeasonCompactSummary.RData"))

data_win_spread <- readRDS(file.path(data_dir, "winSpread.RData"))

# Cleaning data
data_reg_compact %>%
  glimpse()


