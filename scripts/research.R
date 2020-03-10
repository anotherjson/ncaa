# Start ----
## Source initial.R before this script
## Required packages:
##  none

# Functions ----
rmse <- function(error) {
  sqrt(mean(error^2))
}

# Data ----
data.list <- dir(file.path(data.dir, "MDataFiles_Stage1"))
print(data.list)
data.reg.compact <- read_csv(
  file.path(data.dir, "MDataFiles_Stage1/", "MRegularSeasonCompactResults.csv"))

# Research ----
prediction <- sample((10:1000)/10, 50, replace = TRUE)
actual <- sample(1:100, 50, replace = TRUE)

error <- actual - prediction
test <- rmse(error)

# Cleaning ----
df.reg.compact.won <- data.reg.compact %>%
  rename(TeamId = WTeamID) %>%
  rename_at(vars(matches("W")), ~ gsub("^(W)", "Team", .)) %>%
  rename_at(vars(matches("L")), ~ gsub("^(L)", "Opp", .)) %>%
  mutate(OppLoc = if_else(TeamLoc == "H", "A",
                          if_else(TeamLoc == "A", "H", "N")))

df.reg.compact.loss <- data.reg.compact %>%
  rename(TeamId = LTeamID) %>%
  rename_at(vars(matches("L")), ~ gsub("^(L)", "Team", .)) %>%
  rename_at(vars(matches("W")), ~ gsub("^(W)", "Opp", .)) %>%
  mutate(TeamLoc = if_else(OppLoc == "H", "A",
                          if_else(OppLoc == "A", "H", "N")))

df.reg.compact <- df.reg.compact.won %>%
  rbind(df.reg.compact.loss) %>%
  glimpse()