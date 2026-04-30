library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(readr)
library(jsonlite)
library(here)

i_am("Studies/Study1/Code/S1_DataProcessing.R")

cond_dict <- c(
  "-2" = "Strongly Decreasing",
  "-1" = "Weakly Decreasing",
  "0" = "Flat",
  "1" = "Weakly Increasing",
  "2" = "Strongly Increasing"
)

# Loading data
game_data <- readRDS(here("Studies/Study1", "Data", "game_choices_raw.rds"))
  

game_data <- game_data %>%
  filter(nchar(clicks) != 2) %>%
  filter(!(turkid %in% c(NA, "TEST", "TESTTESTTEST"))) %>% 
  mutate(
    # Convert the JSON strings in "choices" to R lists
    choices_list = lapply(clicks, fromJSON),
    # Convert the JSON strings in "choicestimes" to R lists
    times_list   = lapply(clicktimes, fromJSON)
  )

df_time_long <- game_data %>%
  # We only need turkid and times_list for the time data,
  # plus any ID columns you need to merge later (e.g., turkid, condition, etc.)
  dplyr::select(turkid, condid, condition, times_list, startdate, enddate) %>%
  # unnest_longer(...) will replicate rows for each element of times_list
  unnest_longer(
    col = times_list,
    indices_include = TRUE,
    names_repair = "minimal"
  ) %>%
  # The .id from unnest_longer will be stored in times_list_id
  rename(round = times_list_id) %>%
  mutate(
    # Convert from ms to POSIXct
    timing = as.POSIXct(
      as.numeric(times_list) / 1000,
      origin = "1970-01-01"
    ),
    round = as.integer(round)
  ) %>%
  dplyr::select(-times_list)

df_choices_long <- game_data %>%
  dplyr::select(turkid, choices_list) %>%
  unnest_longer(
    col = choices_list,
    indices_include = TRUE,
    names_repair = "minimal"
  ) %>%
  rename(
    round = choices_list_id,
    choice = choices_list
  ) %>%
  mutate(
    round = as.integer(round)
  )

game_data_clean <- df_choices_long %>%
  left_join(df_time_long, by = join_by(turkid, round)) %>%
  group_by(turkid) %>%
  mutate(
    time_to_choose = difftime(timing, lag(timing), units = "sec") %>%
      as.numeric() %>%
      replace_na(0),
    time_to_completion = difftime(enddate, startdate, units = "sec")
  )

# Save cleaned data
write_csv(game_data_clean, here("Studies/Study1", "Data", "game_choices_clean.csv"))


attributes_data <- read_csv(here("Studies/Study1", "Materials", "game_attributes.csv")) %>%
  mutate(
    valuation = as.integer(gsub(",", "", substring(valuation, 2))),
    customers = as.integer(gsub(",", "", customers)),
    project = as.integer(substring(project, nchar(project))) - 1
  )

# Prepare df_choices
choices_data_clean <- game_data_clean %>%
  mutate(
    ones = 1,
    project = choice
  ) %>%
  pivot_wider(
    names_from = project,
    values_from = ones,
    values_fill = 0
  ) %>%
  pivot_longer(
    cols = c("2", "0", "1"),
    names_to = "project",
    values_to = "project_chosen"
  ) %>%
  mutate(project = as.integer(project)) %>%
  left_join(attributes_data, by = c("project", "round")) %>%
  janitor::clean_names()

# Save the resulting data frame
write_csv(choices_data_clean, here("Studies/Study1", "Data", "game_choices_clean_with_attributes.csv"))

cols_pre <- c(
  "turkid",
  "Duration - Pre",
  "StartDate - Pre",
  "EndDate - Pre",
  "DiffPredict"
)

cols_post <- c(
  "Duration - Post",
  "StartDate - Post",
  "EndDate - Post",
  "EasierOrHarder",
  "ProjectChoice",
  "Wager_1",
  "AttImport_MV",
  "AttImport_SA",
  "AttImport_MS",
  "AttImport_NC",
  "BecameConfident",
  "NotEnoughRound",
  "CouldPredict",
  "AttribUseful",
  "AttribDiff",
  "SubjSlope",
  "turkid",
  "condid",
  "bonusearned"
)

survey_data_pre <- read_csv(here("Studies/Study1", "Data", "survey_pre_raw.csv")) %>%
  # Rename columns
  rename(
    Duration_Pre = `Duration (in seconds)`,
    StartDate_Pre = `StartDate`,
    EndDate_Pre = `EndDate`
  ) %>% 
  dplyr::select(any_of(cols_pre))


survey_data_post <- read_csv(here("Studies/Study1", "Data", "survey_post_raw.csv")) %>%
  rename(
    Duration_Post = `Duration (in seconds)`,
    StartDate_Post = `StartDate`,
    EndDate_Post = `EndDate`
  ) %>%
  dplyr::select(any_of(cols_post))

survey_data <- left_join(survey_data_post, survey_data_pre, by = join_by(turkid)) %>% 
  write_csv(here("Studies/Study1", "Data", "survey_clean.csv"))

# ==============================================================================
# CODEBOOK GENERATION
# Auto-generated codebook for processed data
# ==============================================================================

codebook <- tibble::tribble(
  ~variable, ~description,
  "EasierOrHarder", "First, would you say that learning the factors associated with higher payoffs was harder/easier than expected?",
  "ProjectChoice", "Consider the three projects below. Based on what you have learned in the 20 rounds of the game, which project do you think has the highest payoff of all three?",
  "Wager_1", "How many cents from your 10c bonus would you bet on this answer? For every 1c that you bet, you will earn 3c if your answer is correct. The more confident you are that your answer is correct, the more money you should bet.",
  "AttImport_MV", "How predictive of the projects' payoffs do you think each the following characteristics were? - Market Valuation",
  "AttImport_SA", "How predictive of the projects' payoffs do you think each the following characteristics were? - Sector of Activity",
  "AttImport_MS", "How predictive of the projects' payoffs do you think each the following characteristics were? - Market Size",
  "AttImport_NC", "How predictive of the projects' payoffs do you think each the following characteristics were? - Number of Competitors",
  "BecameConfident", "The more the game went on, the more confident I was in which attribute(s) were useful.",
  "NotEnoughRound", "There were not enough rounds to learn about the attributes.",
  "CouldPredict", "Over time, I managed to learn which attribute(s) were good predictors of a project's payoff",
  "AttribUseful", "The attributes were good predictors of a project's payoff",
  "AttribDiff", "Some attributes were better predictors than others",
  "SubjSlope", "I earned more points at end of the game than at the beginning.",
  "turkid", "MTurk worker ID",
  "condid", "Condition ID (experimental condition assignment)",
  "bonusearned", "Bonus amount earned (computed from wager and correct answer)",
  "DiffPredict", "From the description of the game alone, how difficult do you think that it will be for you to identify the factors that are associated with better payoffs?"
)

write_csv(codebook, here("Studies/Study1", "Data", "survey_codebook.csv"))
cat("\nCodebook generated with", nrow(codebook), "variables\n")
