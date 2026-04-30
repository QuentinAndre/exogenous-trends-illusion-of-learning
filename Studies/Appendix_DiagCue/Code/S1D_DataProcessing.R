library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)
library(janitor)
library(readr)
library(jsonlite)
library(here)

i_am("Studies/Appendix_DiagCue/Code/S1D_DataProcessing.R")

cond_dict <- c(
  "-2" = "Strongly Decreasing",
  "-1" = "Weakly Decreasing",
  "0" = "Flat",
  "1" = "Weakly Increasing",
  "2" = "Strongly Increasing"
)

# Loading data
game_data <- readRDS(here("Studies/Appendix_DiagCue", "Data", "game_choices_raw.rds"))

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
  # plus any ID columns you need to merge later (e.g., turkid, condition, etc.)
  dplyr::select(turkid, condid, condition, times_list, startdate, enddate) %>%
  # unnest_longer(...) will replicate rows for each element of times_list
  unnest_longer(col = times_list, indices_include = TRUE, names_repair = "minimal") %>%
  # The .id from unnest_longer will be stored in times_list_id
  rename(round = times_list_id) %>%
  mutate(
    # Convert from ms to POSIXct
    timing = as.POSIXct(as.numeric(times_list) / 1000, origin = "1970-01-01"),
    round = as.integer(round)
  ) %>%
  dplyr::select(-times_list)

df_choices_long <- game_data %>%
  dplyr::select(turkid, choices_list) %>%
  unnest_longer(col = choices_list, indices_include = TRUE, names_repair = "minimal") %>%
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
    time_to_choose = difftime(timing, lag(timing), units = "sec")
    %>% as.numeric() %>% replace_na(0),
    time_to_completion = difftime(enddate, startdate, units = "sec")
  )

# Save cleaned data
write.csv(game_data_clean, here("Studies/Appendix_DiagCue", "Data", "game_choices_clean.csv"), row.names = FALSE, fileEncoding = "UTF-8")

attributes_data <- read.csv(here("Studies/Appendix_DiagCue", "Materials", "game_attributes.csv")) %>%
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
write_csv(choices_data_clean, here("Studies/Appendix_DiagCue", "Data", "game_choices_clean_with_attributes.csv"))

cols_post <- c(
  "Duration",
  "StartDate",
  "EndDate",
  "CouldLearn_1",
  "CouldLearn_2",
  "CouldLearn_3",
  "subjective_learning",
  "confidence",
  "turkid",
  "condition",
  "target"
)

survey_data <- read_csv(here("Studies/Appendix_DiagCue", "Data", "survey_raw.csv"))
survey_data_clean <- survey_data %>%
  rowwise() %>%
  mutate(
    condition = case_when(condid == -2 ~ "Decreasing", condid == 2 ~ "Increasing"),
    subjective_learning = mean(c(CouldLearn_1, CouldLearn_2, CouldLearn_3)) + 3,
    confidence = mean(c_across(c(contains("Confidence_CV"), contains("Confidence_NC"), contains("Confidence_MS"))), na.rm = TRUE),
  ) %>%
  ungroup() %>%
  rename(Duration = `Duration (in seconds)`) %>%
  dplyr::select(any_of(cols_post))

survey_data_clean %>% write_csv(here("Studies/Appendix_DiagCue", "Data", "survey_clean.csv"))

# ==============================================================================
# CODEBOOK GENERATION
# Auto-generated codebook for processed data
# ==============================================================================

codebook <- tibble::tribble(
  ~variable, ~description,
  "Duration", "Survey duration in seconds",
  "StartDate", "Survey start date and time",
  "EndDate", "Survey end date and time",
  "CouldLearn_1", "I managed to learn which characteristics of companies (if any) had an impact on the ROI they generated",
  "CouldLearn_2", "I understood how the characteristics of companies relate to their ROIs",
  "CouldLearn_3", "I know which characteristics of companies affected their ROIs, and in which direction",
  "subjective_learning", "Subjective Learning Index: Mean of CouldLearn_1, CouldLearn_2, and CouldLearn_3, plus 3",
  "confidence", "Confidence in False Statements: Mean confidence rating that non-diagnostic attributes (Company Valuation, Number of Competitors, or Market Size) had an impact on ROI",
  "turkid", "Participant identifier (MTurk ID or Prolific ID)",
  "condition", "Experimental condition: \"Decreasing\" (condid = -2) or \"Increasing\" (condid = 2)",
  "target", "Diagnostic attribute name: \"customers\", \"valuation\", or \"competitors\""
)

write_csv(codebook, here("Studies/Appendix_DiagCue", "Data", "survey_codebook.csv"))
cat("\nCodebook generated with", nrow(codebook), "variables\n")

