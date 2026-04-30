library(tidyverse)
library(tidymodels)
library(glue)
library(ltm)
library(lme4)
library(lmerTest)
library(purrr)
library(lubridate)
library(here)

i_am("Studies/Study3b/Code/S3b_DataAnalysis.R")
source(here("custom_theme.R"))

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
game_data <- read_csv(here("Studies/Study3b", "Data", "game_data_clean.csv")) %>%
  mutate(condition = factor(condition, levels = c("Flat", "Increasing")))

# Analysis of Game Choices

## Number of Attribute Changes

### Descriptives
game_data %>%
  filter(week != 1) %>%
  group_by(week, condition) %>%
  summarise(
    mean = mean(n_changed),
    median = median(n_changed)
  )

### Graph
game_data %>%
  filter(week != 1) %>%
  ggplot(aes(
    x =
      week, y = n_changed, fill = condition, shape = condition, color = condition
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
    labels = glue("Week {seq(1, 7)}\nto {seq(2, 8)}")
  ) +
  theme_matplotlib() +
  scale_color_manual(values = c("#1f1e1e", "#3f69c9")) +
  scale_fill_manual(values = c("#1f1e1e", "#3f69c9")) +
  scale_shape_manual(values = c(21, 24))


### Model
summary(lmer(
  formula = n_changed ~ week * condition + (1 + week |
    turkid),
  data = game_data %>% filter(week != 1) # %>% mutate(condition=fct_relevel(condition, "Increasing", "Flat"))
))


## Time to Choose

### Descriptives
game_data %>%
  filter(week != 1) %>%
  group_by(week, condition) %>%
  summarise(
    mean = mean(time_to_choose),
    median = median(time_to_choose)
  )

### Graph
game_data %>%
  filter(week != 1) %>%
  ggplot(aes(
    x =
      week, y = time_to_choose, fill = condition, shape = condition, color = condition
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
    labels = glue("Week {seq(1, 7)}\nto {seq(2, 8)}")
  ) +
  theme_matplotlib() +
  scale_color_manual(values = c("#1f1e1e", "#3f69c9")) +
  scale_fill_manual(values = c("#1f1e1e", "#3f69c9")) +
  scale_shape_manual(values = c(21, 24))



summary(lmer(
  formula = time_to_choose ~ week * condition + (1 + week |
    turkid),
  data = game_data %>%
    filter(week != 1)
))


## Match to Best and Worst Cues

data_cue_matches <- game_data %>%
  dplyr::select(c(turkid, condition, week, n_matching_best, n_matching_worst)) %>%
  pivot_longer(
    cols = c(n_matching_best, n_matching_worst),
    values_to = "n_matches"
  ) %>%
  mutate(feature_type = glue('{str_to_title(gsub("n_matching_", "", name))} Practices')) %>%
  mutate(condition = factor(condition, levels = c("Flat", "Increasing")))

### Descriptives
summary <- data_cue_matches %>%
  group_by(feature_type, week, condition) %>%
  summarise(
    mean = mean(n_matches),
    median = median(n_matches)
  )

data_cue_matches %>%
  mutate(condition = fct_relevel(condition, "Increasing", "Flat")) %>%
  ggplot(aes(
    x = week, y = n_matches, color = feature_type, fill = feature_type
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
    y = "Number of Attributes Matching...", x = "Round",
    color = element_blank(), fill = element_blank()
  ) +
  theme(legend.position = c(.11, .7)) +
  scale_fill_manual(
    values = c("Best Practices" = "#E69F00", "Worst Practices" = "#4D4D4D")
  ) +
  scale_color_manual(
    values = c("Best Practices" = "#E69F00", "Worst Practices" = "#4D4D4D")
  ) +
  coord_cartesian(ylim = c(0.5, 3.2))



### Model
summary(lmer(
  formula = n_matches ~ week * condition + (1 + week | turkid),
  data = data_cue_matches %>% filter(feature_type == "Best Practices") %>%
    mutate(condition = fct_relevel(condition, "Increasing", "Flat"))
))
summary(lmer(
  formula = n_matches ~ week * condition + (1 + week | turkid),
  data = data_cue_matches %>% filter(feature_type == "Worst Practices") %>%
    mutate(condition = fct_relevel(condition, "Increasing", "Flat"))
))


# Analysis of Survey Data
survey_data_clean <- read_csv(here("Studies/Study3b", "Data", "survey_clean.csv"))

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
    y = "Illusory Beliefs Index",
    title = generate_title(model)
  ) +
  theme_matplotlib() +
  scale_color_manual(values = c("#b63441", "#3f69c9")) +
  guides(color = "none")


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
    y = "Probability of Superiority of\n'Best' Practices over 'Worst' Practices",
    title = generate_title(model)
  ) +
  theme_matplotlib() +
  scale_color_manual(values = c("#b63441", "#3f69c9")) +
  guides(color = "none")


## Analysis of best/worst attributes

expand_combinations <- function(a, b, sep = "_") {
  grid <- expand.grid(a, b)
  glue("{grid$Var1}{sep}{grid$Var2}")
}

grid <- expand.grid(c(0, 1, 2, 3), c(0, 1, 2), c(0, 1, 2), c(0, 1, 2))
levs <- glue("{grid$Var1}_{grid$Var2}_{grid$Var3}_{grid$Var4}")
labs <- 1:length(levs)

survey_data_choices <- survey_data_clean %>%
  unite(
    Best_Meetings,
    c(`best_duration`, `best_frequency`, `best_content`, `best_location`),
    remove = FALSE
  ) %>%
  unite(
    Worst_Meetings,
    c(`worst_duration`, `worst_frequency`, `worst_content`, `worst_location`),
    remove = FALSE
  ) %>%
  unite(
    Best_Meetings_DurFreq,
    c(`best_duration`, `best_frequency`),
    remove = FALSE
  ) %>%
  unite(
    Best_Meetings_ContLoc,
    c(`best_content`, `best_location`),
    remove = FALSE
  ) %>%
  unite(
    Worst_Meetings_DurFreq,
    c(`worst_duration`, `worst_frequency`),
    remove = FALSE
  ) %>%
  unite(
    Worst_Meetings_ContLoc,
    c(`worst_content`, `worst_location`),
    remove = FALSE
  ) %>%
  mutate(
    Best_Meetings_DurFreq = factor(Best_Meetings_DurFreq,
      levels = expand_combinations(c(0, 1, 2, 3), c(0, 1, 2)),
      labels = expand_combinations(c("15min", "30min", "45min", "60min"), c("1/w", "2/w", "1/d"), sep = " - ")
    ),
    Best_Meetings_ContLoc = factor(Best_Meetings_ContLoc,
      levels = expand_combinations(c(0, 1, 2), c(0, 1, 2)),
      labels = expand_combinations(c("Unstr.", "Wins", "Woes"), c("In-Pers.", "Online", "Hybrid"), sep = " - ")
    ),
    Worst_Meetings_DurFreq = factor(Worst_Meetings_DurFreq,
      levels = expand_combinations(c(0, 1, 2, 3), c(0, 1, 2)),
      labels = expand_combinations(c("15min", "30min", "45min", "60min"), c("1/w", "2/w", "1/d"), sep = " - ")
    ),
    Worst_Meetings_ContLoc = factor(Worst_Meetings_ContLoc,
      levels = expand_combinations(c(0, 1, 2), c(0, 1, 2)),
      labels = expand_combinations(c("Unstr.", "Wins", "Woes"), c("In-Pers.", "Online", "Hybrid"), sep = " - ")
    ),
  ) %>%
  mutate(
    Best_Meetings_Labeled = fct_infreq(factor(Best_Meetings, levels = levs, labels = labs)),
    Worst_Meetings_Labeled = fct_infreq(factor(Worst_Meetings, levels = levs, labels = labs))
  )


### Visualization of best combinations

table_best <- as.data.frame(table(survey_data_choices$Best_Meetings_DurFreq, survey_data_choices$Best_Meetings_ContLoc))
ggplot(table_best, aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq)) +
  labs(
    x = "Location and Content",
    y = "Duration and Frequency"
  ) +
  guides(fill = "none")

### Visualization of all combinations.

beliefs_dist_best <- survey_data_choices %>%
  group_by(Best_Meetings_Labeled, .drop = FALSE) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

beliefs_dist_best %>% ggplot(aes(x = Best_Meetings_Labeled, y = prop)) +
  geom_bar(stat = "identity", width = .7, position = position_dodge()) +
  geom_hline(yintercept = 1 / 108) +
  labs(y = "% Identifying as 'Best'", x = "Unique Feature Combinations (Sorted by Popularity)", title = "Beliefs About Best Features") +
  annotate("text", x = 50, y = 1 / 80, label = "Expected Under Uniform Beliefs") +
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

# Helper function to format coefficient results
format_coef <- function(model_summary, term) {
  coefs <- model_summary$coefficients
  b <- coefs[term, "Estimate"]
  p <- coefs[term, "Pr(>|t|)"]
  sprintf(
    "b = %.3f, p %s",
    b,
    if_else(p < 0.001, "< .001", sprintf("= %.3f", p))
  )
}

manuscript_results <- list()

# Sample sizes
manuscript_results$collected_n <- survey_data_clean %>% nrow()
manuscript_results$final_n <- survey_data_clean %>% nrow()

# Number of Attribute Changes model (Flat as reference)
model_nchanged <- lmer(
  formula = n_changed ~ week * condition + (1 + week | turkid),
  data = game_data %>% filter(week != 1)
)
summ_nchanged <- summary(model_nchanged)

manuscript_results$b_round_flat <- format_coef(summ_nchanged, "week")
manuscript_results$b_round_x_condition_nchanged <- format_coef(summ_nchanged, "week:conditionIncreasing")

# For p-value of b_round_increasing, re-fit with Increasing as reference
model_nchanged_inc <- lmer(
  formula = n_changed ~ week * condition + (1 + week | turkid),
  data = game_data %>%
    filter(week != 1) %>%
    mutate(condition = fct_relevel(condition, "Increasing", "Flat"))
)
summ_nchanged_inc <- summary(model_nchanged_inc)
manuscript_results$b_round_increasing <- format_coef(summ_nchanged_inc, "week")

# Best Practices matching model (Increasing as reference)
model_best <- lmer(
  formula = n_matches ~ week * condition + (1 + week | turkid),
  data = data_cue_matches %>%
    filter(feature_type == "Best Practices") %>%
    mutate(condition = fct_relevel(condition, "Increasing", "Flat"))
)
summ_best <- summary(model_best)
manuscript_results$b_round_increasing_best <- format_coef(summ_best, "week")
manuscript_results$b_round_x_condition_best <- format_coef(summ_best, "week:conditionFlat")

# Worst Practices matching model (Increasing as reference)

model_worst <- lmer(
  formula = n_matches ~ week * condition + (1 + week | turkid),
  data = data_cue_matches %>%
    filter(feature_type == "Worst Practices") %>%
    mutate(condition = fct_relevel(condition, "Increasing", "Flat"))
)
summ_worst <- summary(model_worst)
manuscript_results$b_round_increasing_worst <- format_coef(summ_worst, "week")
manuscript_results$b_round_x_condition_worst <- format_coef(summ_worst, "week:conditionFlat")

# Cronbach's alpha for illusory beliefs index
alpha_illusory <- cronbach.alpha(
  survey_data_clean %>%
    drop_na(c(DV_1, DV_2, DV_3, DV_4)) %>%
    dplyr::select(DV_1, DV_2, DV_3, DV_4)
)
manuscript_results$alpha_illusory_beliefs <- round(alpha_illusory$alpha, 2)

# Descriptive statistics (M, SD) by condition
for (dv in c("illusory_beliefs_index", "prob_superiority")) {
  dv_label <- switch(dv,
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
manuscript_results$test_illusory_beliefs <- format_welch_result(
  "illusory_beliefs_index", survey_data_clean, "Flat", "Increasing"
)

manuscript_results$test_prob_superiority <- format_welch_result(
  "prob_superiority", survey_data_clean, "Flat", "Increasing"
)


write_json(manuscript_results, here("Results", "Study3b_statistical_results.json"), pretty = TRUE, auto_unbox = TRUE)
