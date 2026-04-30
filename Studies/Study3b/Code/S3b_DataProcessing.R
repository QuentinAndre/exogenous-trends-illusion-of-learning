library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(readr)
library(jsonlite)
library(here)

i_am("Studies/Study3b/Code/S3b_DataProcessing.R")
source(here("custom_theme.R"))
library(glue)

# Data loading

## Loading game data
game_data <- readRDS(here("Studies/Study3b", "Data", "game_choices_raw.rds"))

## Survey data
survey_data <- read_csv(here("Studies/Study3b", "Data", "survey_post_raw.csv"))

# Pre-Processing

## Processing game data

game_data <- game_data %>%
  filter(nchar(choices) != 2) %>%
  mutate(
    # Convert the JSON strings in "choices" to R lists
    choices_list = lapply(choices, fromJSON),
    # Convert the JSON strings in "choicestimes" to R lists
    times_list = lapply(choicestimes, fromJSON)
  )

df_time_long <- game_data %>%
  # We only need turkid and times_list for the time data,
  # plus any ID columns you need to merge later (e.g., turkid, condition, etc.)
  dplyr::select(turkid, times_list) %>%
  # unnest_longer(...) will replicate rows for each element of times_list
  unnest_longer(
    col = times_list,
    indices_include = TRUE,
    names_repair = "minimal"
  ) %>%
  # The .id from unnest_longer will be stored in times_list_id
  rename(week = times_list_id) %>%
  mutate(
    # Convert from ms to POSIXct
    choicestimes = as.POSIXct(
      as.numeric(times_list) / 1000,
      origin = "1970-01-01"
    ),
    week = as.integer(week) # Make "week" an integer
  ) %>%
  dplyr::select(-times_list)

df_choices_long <- game_data %>%
  # Keep ID columns plus the new choices_list
  dplyr::select(turkid, condition, replicate, startdate, enddate, choices_list) %>%
  # First unnest: get one row per "week" (8 times)
  unnest_longer(
    col = choices_list,
    indices_include = TRUE,
    names_repair = "minimal"
  ) %>%
  rename(week = choices_list_id) %>%
  mutate(
    chosen_duration = `choices_list`[, 1],
    chosen_frequency = `choices_list`[, 2],
    chosen_content = `choices_list`[, 3],
    chosen_location = `choices_list`[, 4]
  ) %>%
  dplyr::select(-choices_list)

game_data_clean <- df_choices_long %>%
  left_join(df_time_long, by = join_by(turkid, week)) %>%
  mutate(week = as.integer(week)) %>%
  group_by(turkid) %>%
  mutate(
    time_to_choose = difftime(choicestimes, lag(choicestimes), units = "sec") %>%
      as.numeric() %>%
      replace_na(0),
    time_to_completion = difftime(enddate, startdate, units = "sec")
  )

## Isolating the duration of the game
game_duration <- game_data_clean %>%
  group_by(turkid) %>%
  summarise(
    time_to_completion = mean(time_to_completion)
  )

## Merging with survey data to perform pre-registered exclusions
survey_data_clean <-
  survey_data %>%
  left_join(game_duration) %>%
  drop_na(turkid) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    DV_4 = -DV_4_R,
    condition = factor(
      condid,
      levels = c(0, 1),
      labels = c("Flat", "Increasing")
    ),
    illusory_beliefs_index = mean(c_across(c(DV_1, DV_2, DV_3, DV_4))) + 3,
    included = time_to_completion > 50
  ) %>%
  rename(
    `time_to_survey_completion` = `Duration (in seconds)`,
    best_duration = `1_Duration`,
    worst_duration = `2_Duration`,
    best_frequency = `1_Frequency`,
    worst_frequency = `2_Frequency`,
    best_content = `1_Content`,
    worst_content = `2_Content`,
    best_location = `1_Location`,
    worst_location = `2_Location`,
    prob_superiority = EdgeProbability
  ) %>%
  filter(included)

## Subsetting the columns of interest for the survey
cols_survey <- c(
  "StartDate", "EndDate", "RecordedDate", "turkid", "surveyduration",
  "gameduration", "condid", "condition", "replicate", "included", "DV_1",
  "DV_2", "DV_3", "DV_4", "DV_4_R", "illusory_beliefs_index",
  "best_duration", "best_frequency", "best_content", "best_location",
  "worst_duration", "worst_frequency", "worst_content", "worst_location",
  "prob_superiority"
)

survey_data_clean <- survey_data_clean %>%
  dplyr::select(any_of(cols_survey))

write_csv(survey_data_clean, here("Studies/Study3b", "Data", "survey_clean.csv"))


## Re-merging survey data with choice data:

game_data_full <- game_data_clean %>%
  left_join(
    survey_data_clean %>%
      dplyr::select(
        turkid, best_duration, best_frequency, best_content,
        best_location, worst_duration, worst_frequency, worst_content,
        worst_location
      )
  ) %>%
  ungroup() %>%
  drop_na(best_location)

for (attribute in c("duration", "frequency", "content", "location")) {
  game_data_full[glue("is_best_{attribute}")] <- as.numeric(
    game_data_full[glue("best_{attribute}")] ==
      game_data_full[glue("chosen_{attribute}")]
  )
  game_data_full[glue("is_worst_{attribute}")] <- as.numeric(
    game_data_full[glue("worst_{attribute}")] ==
      game_data_full[glue("chosen_{attribute}")]
  )
}

game_data_full <- game_data_full %>%
  group_by(turkid) %>%
  mutate(across(
    c("chosen_duration", "chosen_frequency", "chosen_content", "chosen_location"),
    .fns = ~ as.numeric(.x != lag(.x)),
    .names = "changed_{.col}"
  )) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    n_matching_best = sum(c_across(starts_with("is_best"))),
    n_matching_worst = sum(c_across(starts_with("is_worst"))),
    n_changed = sum(c_across(starts_with("changed_")), na.rm = TRUE)
  )

cols_game <- c(
  "turkid", "condition", "replicate", "startdate",
  "enddate", "week", "chosen_duration", "chosen_frequency",
  "chosen_content", "chosen_location", "choicestimes", "time_to_choose",
  "n_matching_best", "n_matching_worst", "n_changed"
)

game_data_full %>%
  dplyr::select(any_of(cols_game)) %>%
  write_csv(here("Studies/Study3b", "Data", "game_data_clean.csv"))

# Creating codebooks

## For Game

labels_game <- c(
  "MTurk ID",
  "Condition",
  "Replicate",
  "Start Date",
  "End Date",
  "Week",
  "Chosen Duration",
  "Chosen Frequency",
  "Chosen Content",
  "Chosen Location",
  "Timing of Choice",
  "Time to Choose",
  "N. Attributes Matching 'Best'",
  "N. Attributes Matching 'Worst'",
  "N. of Attributes Changed"
)

df_codebook_game <- tibble(
  variable = cols_game,
  labels = labels_game,
)

write_csv(df_codebook_game, here("Studies/Study3b", "Data", "game_codebook.csv"))

## For Survey

# ==============================================================================
# CODEBOOK GENERATION
# Auto-generated codebook for processed data
# Extracted from QSF files and R preprocessing script
# ==============================================================================

# Read actual column names from the cleaned CSV
survey_clean_cols <- read_csv(
  here("Studies/Study3b", "Data", "survey_clean.csv"),
  n_max = 0,
  show_col_types = FALSE
) %>% names()

# Create codebook tibble with descriptions extracted from QSF files
# Pre-Study QSF: Contains attention checks (not in final dataset)
# Post-Study QSF: Contains dependent variables and meeting practice questions
codebook <- tibble::tribble(
  ~variable, ~description,
  "StartDate", "Survey start date and time",
  "EndDate", "Survey end date and time",
  "RecordedDate", "Date and time when response was recorded",
  "turkid", "Participant identifier (from embedded data field turkid)",
  "condid", "Condition ID: 0 = Flat, 1 = Increasing (from embedded data field condid)",
  "condition", "Experimental condition (computed from condid: 0 = Flat, 1 = Increasing)",
  "replicate", "Replicate number (from embedded data field replicate)",
  "included", "Whether participant met inclusion criteria (computed: time_to_completion > 50 seconds)",
  "DV_1", "Changing the meeting practices had an impact on the performance of the team (from QID64 matrix question, DataExportTag: Questions_DV_1)",
  "DV_2", "Some meeting practices resulted in better performance than others (from QID64 matrix question, DataExportTag: Questions_DV_2)",
  "DV_3", "Overall, meeting practices mattered for the performance of the team (from QID64 matrix question, DataExportTag: Questions_DV_3)",
  "DV_4_R", "The performance of the team didn't seem to be affected by meeting practices (from QID64 matrix question, DataExportTag: Questions_DV_4_R)",
  "DV_4", "The performance of the team didn't seem to be affected by meeting practices - reversed (computed: -DV_4_R)",
  "illusory_beliefs_index", "Index of illusory beliefs (computed: mean of DV_1, DV_2, DV_3, DV_4) + 3",
  "best_duration", "Duration of meetings identified as best practice (renamed from 1_Duration, from QID67: 0=15 min, 1=30 min, 2=45 min, 3=60 min)",
  "best_frequency", "Frequency of meetings identified as best practice (renamed from 1_Frequency, from QID68: 0=Once a week, 1=Twice a week, 2=Once a day)",
  "best_content", "Content of meetings identified as best practice (renamed from 1_Content, from QID69: 0=Unstructured, 1=Focus on 'Wins', 2=Focus on 'Woes')",
  "best_location", "Location of meetings identified as best practice (renamed from 1_Location, from QID70: 0=In-Person, 1=Online, 2=Hybrid)",
  "worst_duration", "Duration of meetings identified as worst practice (renamed from 2_Duration, from QID67: 0=15 min, 1=30 min, 2=45 min, 3=60 min)",
  "worst_frequency", "Frequency of meetings identified as worst practice (renamed from 2_Frequency, from QID68: 0=Once a week, 1=Twice a week, 2=Once a day)",
  "worst_content", "Content of meetings identified as worst practice (renamed from 2_Content, from QID69: 0=Unstructured, 1=Focus on 'Wins', 2=Focus on 'Woes')",
  "worst_location", "Location of meetings identified as worst practice (renamed from 2_Location, from QID70: 0=In-Person, 1=Online, 2=Hybrid)",
  "prob_superiority", "Probability estimate that team following \"best\" meeting practices will generate more leads than team following \"worst\" practices (renamed from EdgeProbability, from QID71, range: 50-100%)"
)

# Filter to only include variables that exist in the cleaned CSV
codebook <- codebook %>%
  filter(variable %in% survey_clean_cols)

write_csv(codebook, here("Studies/Study3b", "Data", "survey_codebook.csv"))
cat("\nCodebook generated with", nrow(codebook), "variables\n")
