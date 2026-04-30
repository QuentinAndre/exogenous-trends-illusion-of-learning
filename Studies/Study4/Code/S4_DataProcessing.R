library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(readr)
library(jsonlite)
library(purrr)
library(here)

i_am("Studies/Study4/Code/S4_DataProcessing.R")

cond_dict <- c(
  "0" = "Flat",
  "1" = "Increasing"
)

# Loading data
game_data <- readRDS(here("Studies/Study4", "Data", "game_choices_raw.rds"))
  
game_data <- game_data %>%
  filter(nchar(choices) != 4) %>%
  mutate(
    choices_list = str_remove_all(choices, "[{}]") %>% 
      str_split(",") %>% 
      map(as.numeric),
    times_list = str_remove_all(choicestimes, "[{}]") %>% 
      str_split(",") %>% 
      map(as.numeric),
  )

df_time_long <- game_data %>%
  # We only need turkid and times_list for the time data,
  # plus any ID columns you need to merge later (e.g., turkid, condition, etc.)
  dplyr::select(turkid, condid, condition, message_type, times_list, startdate, enddate,
         could_learn_interim_1, could_learn_interim_2, could_learn_interim_3,
         belief_valuation, belief_market_size, belief_competitors, belief_sector_of_activity) %>%
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
write_csv(game_data_clean, here("Studies/Study4", "Data", "game_choices_clean.csv"))


attributes_data <- read_csv(here("Studies/Study4", "Materials", "game_attributes.csv")) %>%
  mutate(
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
write_csv(choices_data_clean, here("Studies/Study4", "Data", "game_choices_clean_with_attributes.csv"))

cols_post <- c(
  "Duration_Post",
  "StartDate_Post",
  "EndDate_Post",
  "CouldLearn_1",
  "CouldLearn_2",
  "CouldLearn_3",
  "Confidence_NC",
  "Confidence_MS",
  "Confidence_CV",
  "Confidence_SA",
  "Slope",
  "Positive_Test_First",
  "Negative_Test_First",
  "Positive_Test_Second",
  "Negative_Test_Second",
  "turkid",
  "condid"
)

survey_data_post <- read_csv(here("Studies/Study4", "Data", "survey_post_raw.csv")) %>%
  rename(
    Duration_Post = `Duration (in seconds)`,
    StartDate_Post = `StartDate`,
    EndDate_Post = `EndDate`
  ) %>%
  dplyr::select(any_of(cols_post))

survey_data_game_clean <- game_data_clean %>% 
  dplyr::select(
  turkid, condition, condition, message_type,
  startdate, enddate, could_learn_interim_1,
  could_learn_interim_2, could_learn_interim_3,
  belief_valuation, belief_market_size, belief_competitors,
  belief_sector_of_activity, time_to_completion
)


survey_data <- left_join(survey_data_game_clean, survey_data_post, by = join_by(turkid)) %>%
  drop_na("CouldLearn_1") %>%
  distinct(turkid, .keep_all=TRUE) %>%
  write_csv(here("Studies/Study4", "Data", "survey_clean.csv"))

# ==============================================================================
# CODEBOOK GENERATION
# Auto-generated codebook for processed data
# ==============================================================================

library(readr)

survey_clean_cols <- read_csv(
  here("Studies/Study4", "Data", "survey_clean.csv"),
  n_max = 0,
  show_col_types = FALSE
) %>% names()

codebook <- tibble::tribble(
  ~variable, ~description,
  "turkid", "Participant identifier (MTurk or Prolific ID)",
  "condition", "Experimental condition: Flat or Increasing (from embedded data in game)",
  "condid", "Condition ID: 0=Flat, 1=Increasing (from post-survey embedded data)",
  "message_type", "Type of performance message shown during the game (from game data)",
  "startdate", "Game start date and time",
  "enddate", "Game end date and time",
  "time_to_completion", "Time to complete the game in seconds (computed from enddate - startdate)",
  "could_learn_interim_1", "Interim subjective learning measure at first checkpoint: confidence that attributes could be learned (from game data)",
  "could_learn_interim_2", "Interim subjective learning measure at second checkpoint (from game data)",
  "could_learn_interim_3", "Interim subjective learning measure at third checkpoint (from game data)",
  "belief_valuation", "Post-game belief about predictiveness of Market Valuation attribute (from game data)",
  "belief_market_size", "Post-game belief about predictiveness of Market Size attribute (from game data)",
  "belief_competitors", "Post-game belief about predictiveness of Number of Competitors attribute (from game data)",
  "belief_sector_of_activity", "Post-game belief about predictiveness of Sector of Activity attribute (from game data)",
  "Duration_Post", "Post-survey duration in seconds (renamed from 'Duration (in seconds)')",
  "StartDate_Post", "Post-survey start date and time (renamed from StartDate)",
  "EndDate_Post", "Post-survey end date and time (renamed from EndDate)",
  "CouldLearn_1", "Agreement: I managed to learn which characteristics of companies had an impact on their ROI (post-survey, 1=Strongly disagree to 7=Strongly agree)",
  "CouldLearn_2", "Agreement: I understood how the characteristics of companies relate to their ROIs (post-survey, 1=Strongly disagree to 7=Strongly agree)",
  "CouldLearn_3", "Agreement: I know which characteristics of companies affected their ROIs, and in which direction (post-survey, 1=Strongly disagree to 7=Strongly agree)",
  "Confidence_NC", "Confidence that Number of Competitors affected ROI (post-survey, 1=Not at all confident to 7=Extremely confident)",
  "Confidence_MS", "Confidence that Market Size affected ROI (post-survey, 1=Not at all confident to 7=Extremely confident)",
  "Confidence_CV", "Confidence that Market Valuation (Company Valuation) affected ROI (post-survey, 1=Not at all confident to 7=Extremely confident)",
  "Confidence_SA", "Confidence that Sector of Activity affected ROI (post-survey, 1=Not at all confident to 7=Extremely confident)",
  "Slope", "Perceived improvement in performance over the course of the game (post-survey)",
  "Positive_Test_First", "First attribute selected when performing a positive test strategy (post-survey)",
  "Negative_Test_First", "First attribute selected when performing a negative test strategy (post-survey)",
  "Positive_Test_Second", "Second attribute selected when performing a positive test strategy (post-survey)",
  "Negative_Test_Second", "Second attribute selected when performing a negative test strategy (post-survey)"
)

codebook <- codebook %>%
  dplyr::filter(variable %in% survey_clean_cols)

write_csv(codebook, here("Studies/Study4", "Data", "survey_codebook.csv"))
cat("\nCodebook generated with", nrow(codebook), "variables\n")
