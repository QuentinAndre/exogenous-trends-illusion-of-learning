library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(janitor)
library(readr)
library(here)

i_am("Studies/Appendix_Exec/Code/S1R_DataProcessing.R")

survey_data <- read_csv(here("Studies/Appendix_Exec", "Data", "survey_post_raw.csv")) %>%
  # Rename columns
  rename(
    Duration = `Duration (in seconds)`,
  ) %>%
  rowwise() %>%
  mutate(
    Extremity_Bets = mean(abs(c_across(c(Bet1, Bet2, Bet3, Bet4)))),
    N_Correct_Responses = sum(c_across(c(MV, RoboVsBio, BioVsElec, RoboVsElec, MS, NC)) == 0),
    Condition = ifelse(condid == 2, "Increasing", "Decreasing")
  )

# Define attributes
variables <- c(
  "ResponseId",
  "Condition",
  "Duration",
  "StartDate",
  "EndDate",
  "Extremity_Bets",
  "SubjInfluence",
  "EdgeOver",
  "BecameConfident",
  "NotEnoughRound",
  "CouldPredict",
  "AttribDiff",
  "SubjSlope",
  "N_Correct_Responses",
  "Confidence"
)

survey_data %>% dplyr::select(variables) %>% write_csv(here("Studies/Appendix_Exec", "Data", "survey_clean.csv"))

# ==============================================================================
# CODEBOOK GENERATION
# Auto-generated codebook for processed data
# ==============================================================================

codebook <- tibble::tribble(
  ~variable, ~description,
  "ResponseId", "Unique response identifier",
  "Condition", "Experimental condition (computed from condid: 2=Increasing, -2=Decreasing)",
  "Duration", "Survey duration in seconds (renamed from \"Duration (in seconds)\")",
  "StartDate", "Survey start date and time",
  "EndDate", "Survey end date and time",
  "Extremity_Bets", "Mean of absolute values of Bet1, Bet2, Bet3, Bet4 predictions (computed)",
  "SubjInfluence", "Suppose you play the same simulation again. How much influence do you think you will have on the ROI that you obtain? (1=No influence at all to 5=A very large influence)",
  "EdgeOver", "Suppose you play the same simulation again, and compete against someone who has never played the simulation before. How likely it is that your average ROI will be larger than the new player's? (1=Very unlikely to 5=Very likely)",
  "BecameConfident", "The more the game went on, the more confident I was in which attribute(s) were predictive of the ROIs. (1=Strongly disagree to 7=Strongly agree)",
  "NotEnoughRound", "There were not enough rounds to learn about the relationship between attributes and ROIs. (1=Strongly disagree to 7=Strongly agree)",
  "CouldPredict", "Over time, I managed to learn which attribute(s) were good predictors of the ROI. (1=Strongly disagree to 7=Strongly agree)",
  "AttribDiff", "Some attributes were better predictors of the ROIs than others. (1=Strongly disagree to 7=Strongly agree)",
  "SubjSlope", "I earned more points at end of the game than at the beginning. (1=Strongly Disagree to 7=Strongly Agree)",
  "N_Correct_Responses", "Number of correct attribute comparisons (sum of MV, RoboVsBio, BioVsElec, RoboVsElec, MS, NC where response equals 0, computed)",
  "Confidence", "On a scale from 1 (Not at all confident) to 7 (Extremely confident), how confident are you in the accuracy of the judgements that you just reported?"
)

write_csv(codebook, here("Studies/Appendix_Exec", "Data", "survey_codebook.csv"))
cat("\nCodebook generated with", nrow(codebook), "variables\n")
