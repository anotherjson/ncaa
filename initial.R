# Start script ----
message("starting ncaa project")

# Paths ----
working.dir <- getwd()
data.dir <- file.path(working.dir, "data")
outputs.dir <- file.path(working.dir, "outputs")
scripts.dir <- file.path(working.dir, "scripts")

# Primary functions ----

## Import packages
import.packages <- function() {
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

prompt.user <- function(user.prompts, user.title) {
  # Prompt a user from a multiple choice vector
  # Req:
  #   purrr
  # Arg:
  #   user.prompts is a vector of length 1 or greater
  stopifnot(is_character(user.prompts), is_vector(user.prompts),
            length(user.prompts) >= 1, is_character(user.title),
            length(user.title) == 1)

  temp <- menu(user.prompts, title = user.title)

  return(temp)
}

# Load packages ----
import.packages()

# # Update or load datasets ----
# prompts.vector.update <- c("Update data", "Load data", "Cancel request")
# prompts.title.update <- "Would you like to update or load datasets?"
#
# user.answer.update <- prompt.user(prompts.vector.update, prompts.title.update)
#
# if (user.answer.update == 1) {
#   message("sourcing update data script")
#   # source(file.path(scripts.dir, "updateData.R"))
#
# } else if (user.answer.update == 2) {
#   message("loading datasets")
#   # source(file.path(scripts.dir, "loadData.R"))
#
# } else (
#   message("canceling")
#
# )
