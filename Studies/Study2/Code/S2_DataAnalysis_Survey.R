library(tidyverse)
library(tidymodels)
library(ltm)
library(effectsize)
library(glue)
library(here)

i_am("Studies/Study2/Code/S2_DataAnalysis_Survey.R")
source(here("custom_theme.R"))

cols_subj_learn <- c(
  "CouldLearn_1",
  "CouldLearn_2",
  "CouldLearn_3"
)

cols_illusory_beliefs <- c(
  "Confidence_NC",
  "Confidence_MS",
  "Confidence_CV"
)

game_data <- read_csv(here("Studies/Study2", "Data", "game_choices_clean.csv"))
runtime <- game_data %>%
  distinct(turkid, .keep_all = TRUE) %>%
  dplyr::select(c(turkid, time_to_completion))

survey_data_all <- read_csv(here("Studies/Study2", "Data", "survey_clean.csv")) %>%
  mutate(Slope = 2 - Slope) %>%
  rowwise() %>%
  mutate(
    SubjectiveLearning = mean(c_across(cols_subj_learn)),
    IllusoryBeliefs = mean(c_across(cols_illusory_beliefs)),
    condition = factor(
      case_when(
        condid == 0 ~ "Flat",
        condid == 1 ~ "Increasing"
      ),
      levels =
        c("Flat", "Increasing")
    )
  )

survey_data <- survey_data_all %>%
  left_join(runtime, by = join_by(turkid)) %>%
  dplyr::filter(time_to_completion > 90)

# Subjective Learning (Reported)
cronbach.alpha(survey_data %>% dplyr::select(cols_subj_learn))

tidy(
  lm(
    scale(SubjectiveLearning) ~ factor(
      condition,
      levels = c("Flat", "Increasing")
    ),
    data = survey_data
  )
)


inc_vs_flat <- cohens_d(
  SubjectiveLearning ~ condition,
  data = survey_data
) %>%
  pull(Cohens_d) %>%
  abs()

loc_inc_vs_flat <- survey_data %>%
  group_by(condition) %>%
  summarize(m = mean(SubjectiveLearning)) %>%
  pull(m) %>%
  mean()

ggplot(survey_data, aes(x = condition, y = SubjectiveLearning, color = condition)) +
  stat_summary(
    fun = mean, geom = "point", position = position_dodge(width = 0.5),
    size = 4
  ) +
  stat_summary(
    fun.data = mean_cl_boot, geom = "errorbar", width = 0,
    position = position_dodge(width = 0.5), show.legend = FALSE,
    size = 1.2
  ) +
  # theme_matplotlib() +
  scale_color_manual(values = c("#1f1e1e", "#3f69c9")) +
  annotate(
    "text",
    x = 1.5, y = loc_inc_vs_flat,
    label = glue("Incr. vs. Flat:\nd = {round(inc_vs_flat, 2)}, p < .001"),
    hjust = .5, size = 4
  ) +
  labs(
    x = element_blank(),
    y = "Subjective Sense of Learning",
    color = element_blank()
  ) +
  guides(color = "none")


# Illusory Beliefs (Reported)

cronbach.alpha(survey_data %>% dplyr::select(cols_illusory_beliefs))

tidy(
  lm(
    scale(IllusoryBeliefs) ~ factor(
      condition,
      levels = c("Flat", "Increasing")
    ),
    data = survey_data
  )
)


inc_vs_flat <- cohens_d(
  IllusoryBeliefs ~ condition,
  data = survey_data
) %>%
  pull(Cohens_d) %>%
  abs()

loc_inc_vs_flat <- survey_data %>%
  group_by(condition) %>%
  summarize(m = mean(IllusoryBeliefs)) %>%
  pull(m) %>%
  mean()

ggplot(survey_data, aes(x = condition, y = IllusoryBeliefs, color = condition)) +
  stat_summary(
    fun = mean, geom = "point", position = position_dodge(width = 0.5),
    size = 4
  ) +
  stat_summary(
    fun.data = mean_cl_boot, geom = "errorbar", width = 0,
    position = position_dodge(width = 0.5), show.legend = FALSE,
    size = 1.2
  ) +
  # theme_matplotlib() +
  scale_color_manual(values = c("#b63441", "#1f1e1e", "#3f69c9")) +
  annotate(
    "text",
    x = 1.5, y = loc_inc_vs_flat,
    label = glue("Incr. vs. Flat:\nd = {round(inc_vs_flat, 2)}, p < .001"),
    hjust = .5, size = 4
  ) +
  labs(
    x = element_blank(),
    y = "Endorsement of Illusory Beliefs",
    color = element_blank()
  ) +
  guides(color = "none")

# =============================================================================
# Export Statistical Results for Manuscript
# =============================================================================

library(jsonlite)

format_welch_result <- function(dv, data, group1, group2) {
  d <- data %>% filter(condition %in% c(group1, group2))
  tt <- t.test(d[[dv]][d$condition == group2], d[[dv]][d$condition == group1])
  cd <- cohens_d(as.formula(paste(dv, "~ condition")), data = d) %>%
    pull(Cohens_d) %>%
    abs()
  p_formatted <- if (tt$p.value < .001) "p < .001" else sprintf("p = %.3f", tt$p.value)
  sprintf("t(%.1f) = %.2f, %s, d = %.2f", tt$parameter, abs(tt$statistic), p_formatted, cd)
}

manuscript_results <- list()

# Sample sizes
manuscript_results$collected_n <- survey_data_all %>% nrow()
manuscript_results$final_n <- survey_data %>% nrow()
n_by_cond <- survey_data %>% count(condition)
manuscript_results$n_flat <- n_by_cond %>%
  filter(condition == "Flat") %>%
  pull(n)
manuscript_results$n_increasing <- n_by_cond %>%
  filter(condition == "Increasing") %>%
  pull(n)

# Cronbach's alpha values
manuscript_results$alpha_subjective_learning <- survey_data %>%
  dplyr::select(all_of(cols_subj_learn)) %>%
  cronbach.alpha() %>%
  pluck("alpha") %>%
  round(2)

manuscript_results$alpha_illusory_beliefs <- survey_data %>%
  dplyr::select(all_of(cols_illusory_beliefs)) %>%
  cronbach.alpha() %>%
  pluck("alpha") %>%
  round(2)

# Descriptive statistics (M, SD) by condition
for (dv in c("SubjectiveLearning", "IllusoryBeliefs")) {
  dv_label <- ifelse(dv == "SubjectiveLearning", "subjective_learning", "illusory_beliefs")
  for (cond in c("Flat", "Increasing")) {
    vals <- survey_data %>%
      filter(condition == cond) %>%
      pull(!!sym(dv))
    prefix <- paste0(dv_label, "_", tolower(cond))
    manuscript_results[[paste0(prefix, "_m")]] <- round(mean(vals), 2)
    manuscript_results[[paste0(prefix, "_sd")]] <- round(sd(vals), 2)
  }
}

# Welch t-tests
manuscript_results$subjective_learning_comparison <- format_welch_result(
  "SubjectiveLearning", survey_data, "Flat", "Increasing"
)

manuscript_results$illusory_correlations_comparison <- format_welch_result(
  "IllusoryBeliefs", survey_data, "Flat", "Increasing"
)

# Slope perception by condition
slope_props <- survey_data %>%
  group_by(condition) %>%
  summarise(
    n = n(),
    n_increasing = sum(Slope == 1),
    pct_increasing = round(mean(Slope == 1) * 100, 1)
  )

manuscript_results$slopeprop_increasing <- slope_props %>%
  filter(condition == "Increasing") %>%
  pull(pct_increasing)

manuscript_results$slopeprop_flat <- slope_props %>%
  filter(condition == "Flat") %>%
  pull(pct_increasing)

# Chi-square test for proportion difference
slope_table <- table(survey_data$condition, survey_data$Slope)
chi_slope <- chisq.test(slope_table)

manuscript_results$slopeprop_comparison <- sprintf(
  "\u03c7\u00b2(%d) = %.2f, p %s",
  chi_slope$parameter,
  chi_slope$statistic,
  if_else(chi_slope$p.value < 0.001, "< .001", sprintf("= %.3f", chi_slope$p.value))
)

write_json(manuscript_results, here("Results", "Study2_statistical_results.json"), pretty = TRUE, auto_unbox = TRUE)
