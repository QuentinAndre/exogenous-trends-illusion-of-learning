library(tidyverse)
library(tidymodels)
library(glue)
library(ltm)
library(lme4)
library(lmerTest)
library(purrr)
library(lubridate)
library(forcats)
library(effsize)
library(here)

i_am("Studies/Study3c/Code/S3c_DataAnalysis.R")
source(here("custom_theme.R"))

# Declare global variables to suppress warnings
globalVariables(c("statistic", "p.value"))

generate_title <- function(model) {
  coeffs <- model %>% tidy()
  summ <- model %>% glance()
  df <- summ %>%
    slice(1) %>%
    pull(df.residual)
  t <- coeffs %>%
    slice(2) %>%
    pull(statistic)
  pval <- coeffs %>%
    slice(2) %>%
    pull(p.value)
  glue(
    "t({df}) = {round(t, 3)}, {ifelse(pval < .001, 'p < .001', glue('p = {round(pval, 3)}'))}"
  )
}

# Data loading
game_data <- read_csv(here("Studies/Study3c", "Data", "game_data_clean.csv")) %>%
  mutate(condition = factor(condition, levels = c("Flat", "Increasing")))

# Analysis of Game Choices

## Number of Attribute Changes

### Descriptives
game_data %>%
  filter(day != 1) %>%
  group_by(day, condition) %>%
  summarise(
    mean = mean(n_changed),
    median = median(n_changed)
  )


### Graph
game_data %>%
  filter(day != 1) %>%
  ggplot(aes(
    x =
      day, y = n_changed, fill = condition, shape = condition, color = condition
  )) +
  stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.5), size = 4) +
  stat_summary(
    fun.data = mean_cl_boot, geom = "errorbar", width = 0, position = position_dodge(width = 0.5), show.legend = FALSE,
    size = 1.2
  ) +
  geom_smooth(method = "lm", alpha = 0.1, linetype = 0, span = .5, show.legend = FALSE) +
  geom_line(stat = "smooth", method = "lm", linetype = "dashed", alpha = .5, show.legend = FALSE) +
  labs(
    y = "Number of Attributes Changed", x = element_blank(),
    color = "Trend", fill = "Trend", shape = "Trend"
  ) +
  scale_x_continuous(
    breaks = seq(2, 8, 1),
    labels = glue("Day {seq(1, 7)}\nto {seq(2, 8)}")
  ) +
  coord_cartesian(ylim = c(1, 2.1)) +
  theme_matplotlib() +
  scale_color_manual(values = c("#1f1e1e", "#3f69c9")) +
  scale_fill_manual(values = c("#1f1e1e", "#3f69c9")) +
  scale_shape_manual(values = c(21, 24))


### Model
summary(lmer(
  formula = n_changed ~ day * condition + (1 + day |
    turkid),
  data = game_data %>% filter(day != 1)
))


## Time to Choose

### Descriptives
game_data %>%
  filter(day != 1) %>%
  group_by(day, condition) %>%
  summarise(
    mean = mean(time_to_choose),
    median = median(time_to_choose)
  )

### Graph
game_data %>%
  filter(day != 1) %>%
  ggplot(aes(
    x =
      day, y = time_to_choose, fill = condition, shape = condition, color = condition
  )) +
  stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.5), size = 4) +
  stat_summary(
    fun.data = mean_cl_boot, geom = "errorbar", width = 0, position = position_dodge(width = 0.5), show.legend = FALSE,
    size = 1.2
  ) +
  geom_smooth(method = "lm", alpha = 0.1, linetype = 0, span = .5, show.legend = FALSE) +
  geom_line(stat = "smooth", method = "lm", linetype = "dashed", alpha = .5, show.legend = FALSE) +
  labs(
    y = "Time to Choose (seconds)", x = element_blank(),
    color = "Trend", fill = "Trend", shape = "Trend"
  ) +
  scale_x_continuous(
    breaks = seq(2, 8, 1),
    labels = glue("Day {seq(1, 7)}\nto {seq(2, 8)}")
  ) +
  theme_matplotlib() +
  scale_color_manual(values = c("#1f1e1e", "#3f69c9")) +
  scale_fill_manual(values = c("#1f1e1e", "#3f69c9")) +
  scale_shape_manual(values = c(21, 24))


## Match to Best and Worst Cues

data_cue_matches <- game_data %>%
  dplyr::select(c(turkid, condition, day, n_matching_best, n_matching_worst)) %>%
  pivot_longer(
    cols = c(n_matching_best, n_matching_worst),
    values_to = "n_matches"
  ) %>%
  mutate(feature_type = glue('{str_to_title(gsub("n_matching_", "", name))} Practices')) %>%
  mutate(condition = factor(condition, levels = c("Flat", "Increasing")))

### Descriptives
data_cue_matches %>%
  group_by(feature_type, day, condition) %>%
  summarise(
    mean = mean(n_matches),
    median = median(n_matches)
  )



data_cue_matches %>%
  ggplot(aes(
    x =
      day, y = n_matches, color = feature_type, fill = feature_type
  )) +
  stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.5), size = 4) +
  stat_summary(
    fun.data = mean_cl_boot, geom = "errorbar", width = 0, position = position_dodge(width = 0.5),
    show.legend = FALSE, size = 1.2
  ) +
  geom_smooth(method = "lm", alpha = 0.1, linetype = 0, span = .5, show.legend = FALSE) +
  geom_line(stat = "smooth", method = "lm", linetype = "dashed", alpha = .5, show.legend = FALSE) +
  scale_x_continuous(
    breaks = seq(1, 8, 1),
    labels = glue("{seq(1, 8)}")
  ) +
  theme_matplotlib() +
  facet_wrap(. ~ condition,
    scales = "free",
    labeller = as_labeller(function(x) glue("{x} Condition"))
  ) +
  scale_shape_manual(values = c(21, 24)) +
  labs(
    y = "Number of Chosen Cues Matching...", x = "Day",
    color = element_blank(), fill = element_blank()
  ) +
  theme(legend.position = c(.2, .85)) +
  scale_fill_manual(
    values = c("Best Practices" = "#E69F00", "Worst Practices" = "#4D4D4D")
  ) +
  scale_color_manual(
    values = c("Best Practices" = "#E69F00", "Worst Practices" = "#4D4D4D")
  ) +
  coord_cartesian(ylim = c(0.5, 3.5))


### Model
summary(lmer(
  formula = n_matches ~ day * condition + (1 + day |
    turkid),
  data = data_cue_matches %>% filter(feature_type == "Best Practices")
))

summary(lmer(
  formula = n_matches ~ day * condition + (1 + day |
    turkid),
  data = data_cue_matches %>% filter(feature_type == "Worst Practices")
))


# Analysis of Survey Data
survey_data_clean <- read_csv(here("Studies/Study3c", "Data", "survey_clean.csv"))

## Illusory Beliefs Index
cronbach.alpha(survey_data_clean %>% drop_na(c(DV_1, DV_2, DV_3, DV_4)) %>% dplyr::select(DV_1, DV_2, DV_3, DV_4))

model <- lm(illusory_beliefs_index ~ condid, data = survey_data_clean)

survey_data_clean %>%
  ggplot(aes(
    x = condition, y = illusory_beliefs_index,
    color = condition
  )) +
  stat_summary(fun = mean, geom = "point", alpha = 1, position = position_dodge(width = 0.5), size = 4) +
  stat_summary(
    fun.data = mean_cl_boot, geom = "errorbar", width = 0, position = position_dodge(width = 0.5), show.legend = FALSE,
    size = 1.2
  ) +
  labs(
    x = element_blank(),
    y = "Belief Cues Affect Outcomes (1-5)",
    title = generate_title(model)
  ) +
  theme_matplotlib() +
  scale_color_manual(values = c("#1f1e1e", "#3f69c9")) +
  guides(color = "none")


cohen.d(illusory_beliefs_index ~ condition, data = survey_data_clean)

## Confidence in Learning
cronbach.alpha(survey_data_clean %>% drop_na(c(CouldLearn_1, CouldLearn_2, CouldLearn_3)) %>% dplyr::select(CouldLearn_1, CouldLearn_2, CouldLearn_3))

model <- lm(confidence_in_learning_index ~ condid, data = survey_data_clean)

survey_data_clean %>%
  ggplot(aes(
    x = condition, y = confidence_in_learning_index,
    color = condition
  )) +
  stat_summary(fun = mean, geom = "point", alpha = 1, position = position_dodge(width = 0.5), size = 4) +
  stat_summary(
    fun.data = mean_cl_boot, geom = "errorbar", width = 0, position = position_dodge(width = 0.5), show.legend = FALSE,
    size = 1.2
  ) +
  labs(
    x = element_blank(),
    y = "Subjective Understanding (1-5)",
    title = generate_title(model)
  ) +
  theme_matplotlib() +
  scale_color_manual(values = c("#1f1e1e", "#3f69c9")) +
  guides(color = "none")

cohen.d(confidence_in_learning_index ~ condition, data = survey_data_clean)

## Probability of Superiority of 'Best' Practices

model <- lm(prob_superiority ~ condid, data = survey_data_clean)

survey_data_clean %>%
  ggplot(aes(
    x = condition, y = prob_superiority,
    color = condition
  )) +
  stat_summary(fun = mean, geom = "point", alpha = 1, position = position_dodge(width = 0.5), size = 4) +
  stat_summary(
    fun.data = mean_cl_boot, geom = "errorbar", width = 0, position = position_dodge(width = 0.5), show.legend = FALSE,
    size = 1.2
  ) +
  labs(
    x = element_blank(),
    y = "Likelihood of 'Best'\nOutperforming 'Worst'",
    title = generate_title(model)
  ) +
  theme_matplotlib() +
  scale_color_manual(values = c("#1f1e1e", "#3f69c9")) +
  guides(color = "none")

cohen.d(prob_superiority ~ condition, data = survey_data_clean)


## Analysis of best attributes

grid <- expand.grid(c(0, 1, 2), c(0, 1, 2), c(0, 1, 2), c(0, 1))
levs <- glue("{grid$Var1}_{grid$Var2}_{grid$Var3}_{grid$Var4}")
labs <- 1:length(levs)
survey_data_choices <- survey_data_clean %>%
  unite(
    Best_Items,
    c(`best_color`, `best_shape`, `best_size`, `best_texture`),
    remove = FALSE
  ) %>%
  unite(
    Worst_Items,
    c(`worst_color`, `worst_shape`, `worst_size`, `worst_texture`),
    remove = FALSE
  ) %>%
  mutate(
    Best_Items_Labeled = fct_infreq(factor(Best_Items, levels = levs, labels = levs)),
    Worst_Items_Labeled = fct_infreq(factor(Worst_Items, levels = levs, labels = levs))
  )

beliefs_dist_best <- survey_data_choices %>%
  group_by(Best_Items_Labeled, .drop = FALSE) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  mutate(is_default = ifelse((Best_Items_Labeled == "0_0_0_0") |
    (Best_Items_Labeled == "2_2_2_1"), "Yes", "No"))

beliefs_dist_best %>% ggplot(aes(x = Best_Items_Labeled, y = prop, fill = is_default)) +
  geom_bar(stat = "identity", width = .7, position = position_dodge()) +
  geom_hline(yintercept = 1 / 54) +
  labs(
    y = "% Identifying as 'Best'",
    x = "Unique Feature Combinations (Sorted by Popularity)",
    title = "Beliefs About Best Features",
    fill = "Default Choice?",
    subtitle = "A 'Default Choice' means 'selecting the first (or last) option on all features'."
  ) +
  annotate("text", x = 40, y = 1 / 50, label = "Expected Under Uniform Choices") +
  theme(axis.text.x = element_blank())


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

# Sample sizes
manuscript_results$collected_n <- survey_data_clean %>% nrow()
manuscript_results$final_n <- survey_data_clean %>% nrow()

# Cronbach's alpha
alpha_subjlearn <- cronbach.alpha(
  survey_data_clean %>%
    drop_na(c(CouldLearn_1, CouldLearn_2, CouldLearn_3)) %>%
    dplyr::select(CouldLearn_1, CouldLearn_2, CouldLearn_3)
)
manuscript_results$alpha_subjective_learning <- round(alpha_subjlearn$alpha, 2)

alpha_illusory <- cronbach.alpha(
  survey_data_clean %>%
    drop_na(c(DV_1, DV_2, DV_3, DV_4)) %>%
    dplyr::select(DV_1, DV_2, DV_3, DV_4)
)
manuscript_results$alpha_illusory_beliefs <- round(alpha_illusory$alpha, 2)

# Descriptive statistics (M, SD) by condition
for (dv in c("confidence_in_learning_index", "illusory_beliefs_index", "prob_superiority")) {
  dv_label <- switch(dv,
    "confidence_in_learning_index" = "subjective_learning",
    "illusory_beliefs_index" = "illusory_beliefs",
    "prob_superiority" = "prob_superiority"
  )
  for (cond in c("Flat", "Increasing")) {
    vals <- survey_data_clean %>%
      filter(condition == cond) %>%
      pull(!!sym(dv))
    prefix <- paste0(dv_label, "_", tolower(cond))
    manuscript_results[[paste0(prefix, "_m")]] <- round(mean(vals, na.rm = TRUE), 2)
    manuscript_results[[paste0(prefix, "_sd")]] <- round(sd(vals, na.rm = TRUE), 2)
  }
}

# Welch t-tests
manuscript_results$subjective_learning_comparison <- format_welch_result(
  "confidence_in_learning_index", survey_data_clean, "Flat", "Increasing"
)

manuscript_results$illusory_correlations_comparison <- format_welch_result(
  "illusory_beliefs_index", survey_data_clean, "Flat", "Increasing"
)

manuscript_results$prob_superiority_comparison <- format_welch_result(
  "prob_superiority", survey_data_clean, "Flat", "Increasing"
)

write_json(manuscript_results, here("Results", "Study3c_statistical_results.json"), pretty = TRUE, auto_unbox = TRUE)
