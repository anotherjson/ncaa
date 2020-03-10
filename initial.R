# Start script ----
message("starting ncaa project")

# Paths ----
working_dir <- getwd()
data_dir <- file.path(working_dir, "data")
outputs_dir <- file.path(working_dir, "outputs")
scripts_dir <- file.path(working_dir, "scripts")

# Primary functions ----

## Import packages
import_packages <- function() {
  # Import packages required for scripts
  # Req:
  #   none
  # Arg:
  #   none
  # Return:
  #   none
  suppressPackageStartupMessages(library(testthat))
  suppressPackageStartupMessages(library(jsonlite))
  suppressPackageStartupMessages(library(scrapeR))
  suppressPackageStartupMessages(library(rlang))
  suppressPackageStartupMessages(library(gridExtra))
  suppressPackageStartupMessages(library(pROC))
  suppressPackageStartupMessages(library(RColorBrewer))
  suppressPackageStartupMessages(library(zoo))
  suppressPackageStartupMessages(library(curl))
  suppressPackageStartupMessages(library(lubridate))
  suppressPackageStartupMessages(library(tidyverse))
}

prompt_user <- function(user_prompts, user_title) {
  # Prompt a user from a multiple choice vector
  # Req:
  #   purrr
  # Arg:
  #   user_prompts is a vector of length 1 or greater
  stopifnot(
    is_character(user_prompts), is_vector(user_prompts),
    length(user_prompts) >= 1, is_character(user_title),
    length(user_title) == 1
  )

  temp <- menu(user_prompts, title = user_title)

  return(temp)
}

# Load packages ----
import_packages()
