library(tidyverse)
library(effsize)
library(glue)
library(here)

i_am("Studies/Appendix_Exec/Code/S1R_DataAnalysis_Survey.R")
source(here("custom_theme.R"))

survey_data <- read_csv(here("Studies/Appendix_Exec", "Data", "survey_clean.csv"))

items <- c(
  "SubjInfluence",
  "EdgeOver",
  "BecameConfident",
  "CouldPredict",
  "AttribDiff",
  "NotEnoughRound",
  "N_Correct_Responses",
  "Confidence",
  "Extremity_Bets"
)

labels <- list(
  "Pred. Influence on ROI",
  "Pred. Edge over New Player",
  "Confident in Ability to Predict ROI",
  "Could Predict ROI by End Game",
  "Some Attrib. Were More Predictive",
  "Not Enough Rounds to Learn",
  "# Correct Attributes Comparisons",
  "Confidence in Attributes Comps.",
  "Extremity of Bets"
)

# Generating t-stats, p-values and effect sizes for all variables.
t_stats <- c()
p_values <- c()
cohens_ds <- c()
y_pos <- c()
for (s in items) {
  formula <- as.formula(paste(s, "~Condition"))
  results <- t.test(formula, data = survey_data)
  t_stats <- c(t_stats, results$statistic)
  p_values <- c(p_values, results$p.value)
  cohens_ds <- c(cohens_ds, cohen.d(formula, data = survey_data)$estimate)
  y_pos <- c(y_pos, survey_data %>% pull(s) %>% mean())
}

df_results <- data.frame(
  item = items,
  t_stat = -t_stats,
  p_value = p_values,
  cohens_d = -cohens_ds,
  y_pos = y_pos,
  x = 1.5
) %>%
  mutate(
    formatted_p = ifelse(p_value < .001,
      "p < .001",
      glue("p = {round(p_value, 3)}")
    ),
    label = glue("t(29) = {round(t_stat, 3)}\n{formatted_p}\nd = {round(cohens_d, 2)}"),
    item = factor(item, levels = items, labels = labels)
  )

# Pivoting survey results in long form.
survey_data_long <- survey_data %>%
  pivot_longer(cols = all_of(items), names_to = "item") %>%
  dplyr::select(ResponseId, Condition, item, value) %>%
  mutate(item = factor(item, levels = items, labels = labels))


# Plotting them.
ggplot(survey_data_long, aes(x = Condition, y = value, color = Condition)) +
  facet_wrap(~item, scales = "free_y") +
  stat_summary(
    fun = mean, geom = "point", position = position_dodge(width = 0.5),
    size = 4
  ) +
  stat_summary(
    fun.data = mean_cl_boot, geom = "errorbar", width = 0,
    position = position_dodge(width = 0.5), show.legend = FALSE,
    size = 1.2
  ) +
  theme_matplotlib() +
  scale_color_manual(values = c("#b63441", "#3f69c9")) +
  labs(
    x = element_blank(),
    color = element_blank(),
    y = "Survey Response"
  ) +
  guides(color = "none") +
  geom_text(
    data = df_results,
    mapping = aes(x = x, y = y_pos, label = label),
    inherit.aes = FALSE,
    color = "black"
  )

# Saving them.


# =============================================================================
# Export Statistical Results for Manuscript
# =============================================================================

library(jsonlite)
library(effectsize)

manuscript_results <- list()

# Sample sizes
manuscript_results$collected_n <- survey_data %>% nrow()
manuscript_results$final_n <- survey_data %>% nrow()
conditions <- survey_data %>%
  pull(Condition) %>%
  unique() %>%
  sort()
n_by_cond <- survey_data %>% count(Condition)
for (cond in conditions) {
  manuscript_results[[paste0("n_", tolower(cond))]] <- n_by_cond %>%
    filter(Condition == cond) %>%
    pull(n)
}

# Descriptive statistics and Welch t-tests for each DV
item_labels <- c(
  "SubjInfluence" = "subj_influence",
  "EdgeOver" = "edge_over",
  "BecameConfident" = "became_confident",
  "CouldPredict" = "could_predict",
  "AttribDiff" = "attrib_diff",
  "NotEnoughRound" = "not_enough_round",
  "N_Correct_Responses" = "n_correct_responses",
  "Confidence" = "confidence",
  "Extremity_Bets" = "extremity_bets"
)

for (item in items) {
  label <- item_labels[[item]]
  for (cond in conditions) {
    vals <- survey_data %>%
      filter(Condition == cond) %>%
      pull(!!sym(item))
    prefix <- paste0(label, "_", tolower(cond))
    manuscript_results[[paste0(prefix, "_m")]] <- round(mean(vals), 2)
    manuscript_results[[paste0(prefix, "_sd")]] <- round(sd(vals), 2)
  }

  # Welch t-test
  tt <- t.test(as.formula(paste(item, "~ Condition")), data = survey_data)
  cd <- effectsize::cohens_d(as.formula(paste(item, "~ Condition")), data = survey_data) %>%
    pull(Cohens_d) %>%
    abs()
  p_formatted <- if (tt$p.value < .001) "p < .001" else sprintf("p = %.3f", tt$p.value)
  manuscript_results[[paste0(label, "_comparison")]] <- sprintf(
    "t(%.1f) = %.2f, %s, d = %.2f", tt$parameter, abs(tt$statistic), p_formatted, cd
  )
}

write_json(
  manuscript_results,
  here("Results", "Studies/Appendix_Exec_statistical_results.json"),
  pretty = TRUE, auto_unbox = TRUE
)
cat("Statistical results exported to Results/Studies/Appendix_Exec_statistical_results.json\n")
