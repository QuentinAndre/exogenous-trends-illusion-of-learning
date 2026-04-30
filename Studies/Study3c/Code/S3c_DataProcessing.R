library(tidyverse)
library(glue)
library(jsonlite)
library(here)

i_am("Studies/Study3c/Code/S3c_DataProcessing.R")
source(here("custom_theme.R"))

# Data loading

## Loading game data
game_data <- readRDS(here("Studies/Study3c", "Data", "game_choices_raw.rds"))

## Survey data
survey_data <- read_csv(here("Studies/Study3c", "Data", "survey_post_raw.csv"))

# Pre-Processing

## Processing game data

game_data <- game_data %>%
  filter(nchar(choices) != 2) %>%
  mutate(
    # Convert the JSON strings in "choices" to R lists
    choices_list = lapply(choices, fromJSON),
    # Convert the JSON strings in "choicestimes" to R lists
    times_list   = lapply(choicestimes, fromJSON)
  )

df_time_long <- game_data %>%
  # We only need turkid and times_list for the time data,
  dplyr::select(turkid, times_list) %>%
  # unnest_longer(...) will replicate rows for each element of times_list
  unnest_longer(
    col = times_list,
    indices_include = TRUE,
    names_repair = "minimal"
  ) %>%
  # The .id from unnest_longer will be stored in times_list_id
  rename(day = times_list_id) %>%
  mutate(
    # Convert from ms to POSIXct
    choicestimes = as.POSIXct(
      as.numeric(times_list) / 1000,
      origin = "1970-01-01"
    ), day = as.integer(day)
  ) %>%
  dplyr::select(-times_list)

df_choices_long <- game_data %>%
  # Keep ID columns plus the new choices_list
  dplyr::select(turkid, condition, replicate, startdate, enddate, choices_list) %>%
  # First unnest: get one row per "day" (8 times)
  unnest_longer(
    col = choices_list,
    indices_include = TRUE,
    names_repair = "minimal"
  ) %>%
  rename(day = choices_list_id) %>%
  mutate(
    chosen_shape = `choices_list`[, 1],
    chosen_color = `choices_list`[, 2],
    chosen_size = `choices_list`[, 3],
    chosen_texture = `choices_list`[, 4]
  ) %>%
  dplyr::select(-choices_list)

game_data_clean <- df_choices_long %>%
  left_join(df_time_long, by = join_by(turkid, day)) %>%
  mutate(day = as.integer(day)) %>%
  group_by(turkid) %>%
  mutate(
    time_to_choose = difftime(
      choicestimes, lag(choicestimes),
      units = "sec"
    ) %>%
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
    confidence_in_learning_index = mean(
      c_across(c(CouldLearn_1, CouldLearn_2, CouldLearn_3))
    ),
    included = time_to_completion > 50
  ) %>%
  rename(
    `time_to_survey_completion` = `Duration (in seconds)`,
    best_color = `1_Color`,
    worst_color = `2_Color`,
    best_shape = `1_Shape`,
    worst_shape = `2_Shape`,
    best_size = `1_Size`,
    worst_size = `2_Size`,
    best_texture = `1_Texture`,
    worst_texture = `2_Texture`,
    prob_superiority = EdgeProbability
  ) %>%
  filter(included)

## Subsetting the columns of interest for the survey
cols_survey <- c(
  "StartDate", "EndDate", "RecordedDate", "turkid", "surveyduration",
  "gameduration", "condid", "condition", "replicate", "included", "DV_1",
  "DV_2", "DV_3", "DV_4", "DV_4_R", "illusory_beliefs_index",
  "CouldLearn_1", "CouldLearn_2", "CouldLearn_3",
  "confidence_in_learning_index",
  "best_color", "best_shape", "best_size", "best_texture",
  "worst_color", "worst_shape", "worst_size", "worst_texture",
  "prob_superiority"
)

survey_data_clean <- survey_data_clean %>%
  dplyr::select(any_of(cols_survey))

(
  survey_data_clean %>%
    write_csv(here("Studies/Study3c", "Data", "survey_clean.csv"))
)

## Re-merging survey data with choice data:

game_data_full <- game_data_clean %>%
  left_join(
    survey_data_clean %>%
      dplyr::select(
        c(
          turkid, best_color, best_shape, best_size, best_texture,
          worst_color, worst_shape, worst_size, worst_texture
        )
      )
  ) %>%
  ungroup() %>%
  drop_na(best_color)

for (attribute in c("color", "shape", "size", "texture")) {
  game_data_full[glue("is_best_{attribute}")] <- as.numeric(
    (game_data_full[glue("best_{attribute}")] ==
      game_data_full[glue("chosen_{attribute}")])
  )
  game_data_full[glue("is_worst_{attribute}")] <- as.numeric(
    (game_data_full[glue("worst_{attribute}")] ==
      game_data_full[glue("chosen_{attribute}")])
  )
}

game_data_full <- game_data_full %>%
  group_by(turkid) %>%
  mutate(
    across(
      c("chosen_color", "chosen_shape", "chosen_size", "chosen_texture"),
      .fns = ~ as.numeric(.x != lag(.x)),
      .names = "changed_{.col}"
    )
  ) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    n_matching_best = sum(c_across(starts_with("is_best"))),
    n_matching_worst = sum(c_across(starts_with("is_worst"))),
    n_changed = sum(c_across(starts_with("changed_")), na.rm = TRUE)
  )

cols_game <- c(
  "turkid", "condition", "replicate", "startdate",
  "enddate", "day", "chosen_color", "chosen_shape",
  "chosen_size", "chosen_texture", "choicestimes", "time_to_choose",
  "n_matching_best", "n_matching_worst", "n_changed"
)

game_data_full %>%
  dplyr::select(any_of(cols_game)) %>%
  write_csv(here("Studies/Study3c", "Data", "game_data_clean.csv"))

# Creating codebooks

## For Game

labels_game <- c(
  "MTurk ID",
  "Condition",
  "Replicate",
  "Start Date",
  "End Date",
  "Day",
  "Chosen Color",
  "Chosen Shape",
  "Chosen Size",
  "Chosen Texture",
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

write_csv(df_codebook_game, here("Studies/Study3c", "Data", "game_codebook.csv"))

## For Survey

# ==============================================================================
# CODEBOOK GENERATION
# Auto-generated codebook for processed data
# ==============================================================================

codebook <- tibble::tribble(
  ~variable, ~description,
  "StartDate", "Survey start date and time (Qualtrics metadata)",
  "EndDate", "Survey end date and time (Qualtrics metadata)",
  "RecordedDate", "Date and time when response was recorded (Qualtrics metadata)",
  "turkid", "Prolific participant ID (embedded data from pre-survey)",
  "condid", "Experimental condition ID: 0=Flat, 1=Increasing (embedded data from pre-survey)",
  "condition", "Experimental condition as factor: Flat or Increasing (computed from condid)",
  "replicate", "Replicate number: 0-4 (embedded data from pre-survey)",
  "included", "Whether participant met inclusion criteria: time_to_completion > 50 seconds (computed)",
  "DV_1", "Agreement: \"Changing the characteristics of items had an impact on the number of points they received\" (QID64, -2=Strongly disagree to 2=Strongly agree)",
  "DV_2", "Agreement: \"Some characteristics of items resulted in more points than others\" (QID64, -2=Strongly disagree to 2=Strongly agree)",
  "DV_3", "Agreement: \"Overall, the characteristics of items mattered for how many points they received\" (QID64, -2=Strongly disagree to 2=Strongly agree)",
  "DV_4", "Agreement: \"The number of points received didn't seem affected by the characteristics of the items\", reversed (QID64, -2=Strongly disagree to 2=Strongly agree, computed as -DV_4_R)",
  "DV_4_R", "Agreement: \"The number of points received didn't seem affected by the characteristics of the items\", original (QID64, -2=Strongly disagree to 2=Strongly agree)",
  "illusory_beliefs_index", "Index of illusory beliefs: Mean of DV_1, DV_2, DV_3, DV_4 plus 3 (computed)",
  "CouldLearn_1", "Agreement: \"I managed to learn which characteristics of items (if any) had an impact on the number of points they received\" (QID77, -2=Strongly disagree to 2=Strongly agree)",
  "CouldLearn_2", "Agreement: \"I understood how the characteristics of items relate to the number of points they received\" (QID77, -2=Strongly disagree to 2=Strongly agree)",
  "CouldLearn_3", "Agreement: \"I know which characteristics affected the number of points item received, and in which direction\" (QID77, -2=Strongly disagree to 2=Strongly agree)",
  "confidence_in_learning_index", "Index of confidence in learning: Mean of CouldLearn_1, CouldLearn_2, CouldLearn_3 (computed)",
  "best_color", "Color characteristic expected to generate HIGHER number of points: 0=Black, 1=Grey, 2=White (QID68 loop 1, renamed from 1_Color)",
  "best_shape", "Shape characteristic expected to generate HIGHER number of points: 0=Circle, 1=Square, 2=Triangle (QID67 loop 1, renamed from 1_Shape)",
  "best_size", "Size characteristic expected to generate HIGHER number of points: 0=Small, 1=Medium, 2=Large (QID69 loop 1, renamed from 1_Size)",
  "best_texture", "Texture characteristic expected to generate HIGHER number of points: 0=Smooth, 1=Rugged (QID70 loop 1, renamed from 1_Texture)",
  "worst_color", "Color characteristic expected to generate LOWER number of points: 0=Black, 1=Grey, 2=White (QID68 loop 2, renamed from 2_Color)",
  "worst_shape", "Shape characteristic expected to generate LOWER number of points: 0=Circle, 1=Square, 2=Triangle (QID67 loop 2, renamed from 2_Shape)",
  "worst_size", "Size characteristic expected to generate LOWER number of points: 0=Small, 1=Medium, 2=Large (QID69 loop 2, renamed from 2_Size)",
  "worst_texture", "Texture characteristic expected to generate LOWER number of points: 0=Smooth, 1=Rugged (QID70 loop 2, renamed from 2_Texture)",
  "prob_superiority", "Probability estimate: How likely it is that the \"higher points\" item will receive more points than the \"lower points\" item (QID71, renamed from EdgeProbability, 50-100%)"
)

write_csv(codebook, here("Studies/Study3c", "Data", "survey_codebook.csv"))
cat("\nCodebook generated with", nrow(codebook), "variables\n")
