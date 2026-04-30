library(tidyverse)
library(glue)
library(jsonlite)
library(here)

i_am("Studies/Study3/Code/S3_DataProcessing.R")
source(here("custom_theme.R"))

# Data loading

## Loading game data
game_data <- readRDS(here("Studies/Study3", "Data", "game_choices_raw.rds"))

## Survey data
survey_data <- read_csv(here("Studies/Study3", "Data", "survey_post_raw.csv"))

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
  dplyr::select(turkid, times_list) %>%
  # unnest_longer(...) will replicate rows for each element of times_list
  unnest_longer(col = times_list, indices_include = TRUE, names_repair = "minimal") %>%
  # The .id from unnest_longer will be stored in times_list_id
  rename(day = times_list_id) %>%
  mutate(
    # Convert from ms to POSIXct
    choicestimes = as.POSIXct(as.numeric(times_list) / 1000, origin = "1970-01-01"),
    day = as.integer(day)
  ) %>%
  dplyr::select(-times_list)

df_choices_long <- game_data %>%
  # Keep ID columns plus the new choices_list
  dplyr::select(turkid, condition, startdate, enddate, choices_list) %>%
  # First unnest: get one row per "day" (8 times)
  unnest_longer(col = choices_list, indices_include = TRUE, names_repair = "minimal") %>%
  rename(day = choices_list_id) %>%
  mutate(
    chosen_hook = `choices_list`[, 1],
    chosen_length = `choices_list`[, 2],
    chosen_tone = `choices_list`[, 3],
    chosen_content = `choices_list`[, 4],
    chosen_cta = `choices_list`[, 5]
  ) %>%
  dplyr::select(-choices_list)

game_data_clean <- df_choices_long %>%
  left_join(df_time_long, by = join_by(turkid, day)) %>%
  mutate(day = as.integer(day)) %>%
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
survey_data_clean <- survey_data %>%
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
    confidence_in_learning_index = mean(c_across(c(CouldLearn_1, CouldLearn_2, CouldLearn_3))),
    included = time_to_completion > 50
  ) %>%
  rename(
    `time_to_survey_completion` = `Duration (in seconds)`,
    best_hook = `1_Hook`,
    worst_hook = `2_Hook`,
    best_length = `1_Length`,
    worst_length = `2_Length`,
    best_tone = `1_Tone`,
    worst_tone = `2_Tone`,
    best_content = `1_Content`,
    worst_content = `2_Content`,
    best_cta = `1_CTA`,
    worst_cta = `2_CTA`,
    prob_superiority = EdgeProbability
  ) %>%
  filter(included)

## Subsetting the columns of interest for the survey
cols_survey <- c(
  "StartDate", "EndDate", "RecordedDate", "turkid", "surveyduration", "gameduration", "condid", "condition", "replicate", 
  "included", "DV_1", "DV_2", "DV_3", "DV_4", "DV_4_R", "illusory_beliefs_index", "CouldLearn_1", "CouldLearn_2",
  "CouldLearn_3", "confidence_in_learning_index", "best_hook", "best_length", "best_tone", "best_content",
  "best_cta", 
  "worst_hook", "worst_length", "worst_tone", "worst_content",
  "worst_cta", "prob_superiority"
)

survey_data_clean <- survey_data_clean %>%
  dplyr::select(any_of(cols_survey))

survey_data_clean %>% write_csv(here("Studies/Study3", "Data", "survey_clean.csv"))

## Re-merging survey data with choice data:

game_data_full <- game_data_clean %>%
  left_join(
    survey_data_clean %>%
      dplyr::select(
        c(
          turkid, best_hook, best_length, best_tone, best_content,
          best_cta, worst_hook, worst_length, worst_tone, worst_content,
          worst_cta
        )
      )
  ) %>%
  ungroup() %>%
  drop_na(best_cta)

for (attribute in c("hook", "length", "tone", "content", "cta")) {
  game_data_full[glue("is_best_{attribute}")] <- as.numeric(
    game_data_full[glue("best_{attribute}")] == game_data_full[glue("chosen_{attribute}")]
  )
  game_data_full[glue("is_worst_{attribute}")] <- as.numeric(
    game_data_full[glue("worst_{attribute}")] == game_data_full[glue("chosen_{attribute}")]
  )
}

game_data_full <- game_data_full %>%
  group_by(turkid) %>%
  mutate(
    across(
      c("chosen_hook", "chosen_length", "chosen_tone", "chosen_content", "chosen_cta"),
      .fns = ~ as.numeric(.x != lag(.x)),
      .names = "changed_{.col}"
    )
  ) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    n_matching_best = sum(c_across(starts_with("is_best"))),
    n_matching_worst = sum(c_across(starts_with("is_worst"))),
    n_changed = sum(c_across(starts_with("changed_")), na.rm = T)
  )

cols_game <- c(
  "turkid", "condition", "startdate",
  "enddate", "day", "chosen_hook", "chosen_length", "chosen_tone",
  "chosen_content", "chosen_cta", "choicestimes", "time_to_choose",
  "n_matching_best", "n_matching_worst", "n_changed"
)

game_data_full %>%
  dplyr::select(any_of(cols_game)) %>%
  write_csv(here("Studies/Study3", "Data", "game_data_clean.csv"))

# Creating codebooks 

## For Game

labels_game <- c(
  "MTurk ID",
  "Condition",
  "Start Date",
  "End Date",
  "Day",
  "Chosen Hook",
  "Chosen Length",
  "Chosen Tone",
  "Chosen Content",
  "Chosen Call to Action",
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

write_csv(df_codebook_game, here("Studies/Study3", "Data", "game_codebook.csv"))

## For Survey

# ==============================================================================
# CODEBOOK GENERATION
# Auto-generated codebook for processed data
# Generated from Qualtrics QSF files and R preprocessing script
# ==============================================================================

codebook <- tibble::tribble(
  ~variable, ~description,
  "StartDate", "Survey start date and time",
  "EndDate", "Survey end date and time",
  "RecordedDate", "Date and time when response was recorded",
  "turkid", "Prolific participant ID (embedded data)",
  "condid", "Experimental condition ID: 0=Flat, 1=Increasing (embedded data)",
  "condition", "Experimental condition as factor: Flat or Increasing (computed from condid)",
  "replicate", "Replicate number (embedded data)",
  "included", "Whether participant met inclusion criteria: time_to_completion > 50 seconds (computed)",
  "DV_1", "Agreement: Changing the features of posts had an impact on the engagement they received (1=Strongly disagree to 5=Strongly agree)",
  "DV_2", "Agreement: Some features of posts resulted in stronger engagement than others (1=Strongly disagree to 5=Strongly agree)",
  "DV_3", "Agreement: Overall, the features of posts mattered for how much engagement they received (1=Strongly disagree to 5=Strongly agree)",
  "DV_4", "Agreement: The engagement received didn't seem affected by the features of the posts, reversed (1=Strongly disagree to 5=Strongly agree, computed as -DV_4_R)",
  "DV_4_R", "Agreement: The engagement received didn't seem affected by the features of the posts, original (1=Strongly disagree to 5=Strongly agree)",
  "illusory_beliefs_index", "Index of illusory beliefs: Mean of DV_1, DV_2, DV_3, DV_4 plus 3 (computed)",
  "CouldLearn_1", "Agreement: I managed to learn which features of posts (if any) had an impact on the engagement they received (1=Strongly disagree to 5=Strongly agree)",
  "CouldLearn_2", "Agreement: I understood how the features of posts relate to their engagement (1=Strongly disagree to 5=Strongly agree)",
  "CouldLearn_3", "Agreement: I know which features affected the engagement of posts, and in which direction (1=Strongly disagree to 5=Strongly agree)",
  "confidence_in_learning_index", "Index of confidence in learning: Mean of CouldLearn_1, CouldLearn_2, CouldLearn_3 (computed)",
  "best_hook", "Hook feature expected to generate STRONG engagement: 0=Question, 1=Cliffhanger, 2=Anecdote (renamed from 1_Hook)",
  "best_length", "Length feature expected to generate STRONG engagement: 0=Less than 400 characters, 1=400-1400 characters, 2=1400-2400 characters (renamed from 1_Length)",
  "best_tone", "Tone feature expected to generate STRONG engagement: 0=First-Person, 1=Third-Person (renamed from 1_Tone)",
  "best_content", "Content feature expected to generate STRONG engagement: 0=Text, 1=Text + Picture, 2=Text + Poll (renamed from 1_Content)",
  "best_cta", "Call to Action feature expected to generate STRONG engagement: 0='Like and Comment', 1=No Call to Action (renamed from 1_CTA)",
  "worst_hook", "Hook feature expected to generate WEAK engagement: 0=Question, 1=Cliffhanger, 2=Anecdote (renamed from 2_Hook)",
  "worst_length", "Length feature expected to generate WEAK engagement: 0=Less than 400 characters, 1=400-1400 characters, 2=1400-2400 characters (renamed from 2_Length)",
  "worst_tone", "Tone feature expected to generate WEAK engagement: 0=First-Person, 1=Third-Person (renamed from 2_Tone)",
  "worst_content", "Content feature expected to generate WEAK engagement: 0=Text, 1=Text + Picture, 2=Text + Poll (renamed from 2_Content)",
  "worst_cta", "Call to Action feature expected to generate WEAK engagement: 0='Like and Comment', 1=No Call to Action (renamed from 2_CTA)",
  "prob_superiority", "Probability (50-100%) that a post with \"strong engagement\" features will receive more likes than a post with \"weak engagement\" features (renamed from EdgeProbability)"
)

write_csv(codebook, here("Studies/Study3", "Data", "survey_codebook.csv"))
cat("\nCodebook generated with", nrow(codebook), "variables\n")

