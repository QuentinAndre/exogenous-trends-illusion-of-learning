library(tidyverse)
library(tidymodels)
library(glue)
library(broom)
library(here)

i_am("Studies/Appendix_DiagCue/Code/S1D_DataAnalysis_Survey.R")
source(here("custom_theme.R"))

generate_title <- function(model) {
  coeffs <- model %>% broom::tidy()
  summ <- model %>% broom::glance()
  df <- summ %>%
    dplyr::slice(1) %>%
    dplyr::pull(df.residual)
  t <- coeffs %>%
    dplyr::slice(2) %>%
    dplyr::pull(statistic)
  pval <- coeffs %>%
    dplyr::slice(2) %>%
    dplyr::pull(p.value)
  glue(
    "t({df}) = {round(t, 3)}, {ifelse(pval < .001, 'p < .001', glue('p = {round(pval, 3)}'))}"
  )
}

survey_data <- read_csv(here("Studies/Appendix_DiagCue", "Data", "survey_clean.csv"))
game_data <- read_csv(here("Studies/Appendix_DiagCue", "Data", "game_choices_clean.csv"))
runtime <- game_data %>%
  distinct(turkid, .keep_all = TRUE) %>%
  dplyr::select(c(turkid, time_to_completion))
survey_data_all <- survey_data %>%
  left_join(runtime, by = join_by(turkid)) %>%
  dplyr::filter(time_to_completion > 90)


model <- lm(subjective_learning ~ condition, data = survey_data_all)
survey_data_all %>%
  ggplot(aes(
    x = factor(condition), y = subjective_learning,
    color = condition
  )) +
  stat_summary(
    fun = mean, geom = "point", alpha = 1,
    position = position_dodge(width = 0.5), size = 4
  ) +
  stat_summary(
    fun.data = mean_cl_boot, geom = "errorbar",
    width = 0, position = position_dodge(width = 0.5),
    show.legend = FALSE, linewidth = 1.2
  ) +
  labs(
    x = element_blank(),
    y = "Subjective Understanding\nin Cue-Outcome Link (1-5)",
    title = generate_title(model)
  ) +
  theme_matplotlib() +
  scale_color_manual(values = c("#b63441", "#3f69c9")) +
  guides(color = "none")

model <- lm(confidence ~ condition, data = survey_data_all)
survey_data_all %>%
  ggplot(aes(
    x = factor(condition), y = confidence,
    color = condition
  )) +
  stat_summary(
    fun = mean, geom = "point", alpha = 1,
    position = position_dodge(width = 0.5),
    size = 4
  ) +
  stat_summary(
    fun.data = mean_cl_boot, geom = "errorbar", width = 0,
    position = position_dodge(width = 0.5),
    show.legend = FALSE,
    size = 1.2
  ) +
  labs(
    x = element_blank(),
    y = "Confidence in\nFalse Statements (1-5)",
    title = generate_title(model)
  ) +
  theme_matplotlib() +
  scale_color_manual(values = c("#b63441", "#3f69c9")) +
  guides(color = "none")


tidy(t.test(subjective_learning ~ condition, data = survey_data_all))
tidy(t.test(confidence ~ condition, data = survey_data_all))

# =============================================================================
# Export Statistical Results for Manuscript
# =============================================================================

library(jsonlite)
library(effectsize)

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

# Sample Sizes
manuscript_results$collected_n <- survey_data %>% nrow()
manuscript_results$final_n <- survey_data_all %>% nrow()

# Descriptive statistics (M, SD) by condition
conditions <- survey_data_all %>%
  pull(condition) %>%
  unique() %>%
  sort()
for (dv in c("subjective_learning", "confidence")) {
  for (cond in conditions) {
    vals <- survey_data_all %>%
      filter(condition == cond) %>%
      pull(!!sym(dv))
    prefix <- paste0(
      ifelse(dv == "confidence", "illusory_correlation", "subjective_learning"),
      "_", tolower(gsub(" ", "_", cond))
    )
    manuscript_results[[paste0(prefix, "_m")]] <- round(mean(vals), 2)
    manuscript_results[[paste0(prefix, "_sd")]] <- round(sd(vals), 2)
  }
}

# Welch t-tests
manuscript_results$test_subjective_learning <- format_welch_result(
  "subjective_learning", survey_data_all, conditions[1], conditions[2]
)

manuscript_results$test_illusory_correlation <- format_welch_result(
  "confidence", survey_data_all, conditions[1], conditions[2]
)

write_json(manuscript_results, here("Results", "Studies/Appendix_DiagCue_statistical_results.json"), pretty = TRUE, auto_unbox = TRUE)
