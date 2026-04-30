library(tidyverse)
library(tidymodels)
library(ltm)
library(glue)
library(effectsize)
library(here)

# Set working directory to script location
i_am("Studies/Study1/Code/S1_DataAnalysis_Survey.R")

source(here("custom_theme.R"))

cols_sol <- c(
  "BecameConfident",
  "EnoughRound",
  "CouldPredict",
  "AttribUseful",
  "AttribDiff"
)

cols_subj_learn <- c(
  "BecameConfident",
  "EnoughRound",
  "CouldPredict"
)

cols_illusory_beliefs <- c("AttribUseful",
                           "AttribDiff")

survey_data <- read_csv(here("Studies/Study1", "Data", "survey_clean.csv")) %>%
  mutate(EnoughRound = 8 - NotEnoughRound) %>%
  rowwise() %>%
  mutate(
    SoL = mean(c_across(cols_sol)),
    SubjectiveLearning = mean(c_across(cols_subj_learn)),
    IllusoryBeliefs = mean(c_across(cols_illusory_beliefs)),
    BetAmount = Wager_1,
    condition = factor(
      case_when(
        condid == -2 ~ "Decreasing",
        condid == 0 ~ "Flat",
        condid == 2 ~ "Increasing"
      ),
      levels =
        c("Decreasing", "Flat", "Increasing")
    )
  )


# Subjective Learning (Reported)
cronbach.alpha(survey_data %>% dplyr::select(cols_subj_learn))

tidy(
  lm(
    scale(SubjectiveLearning) ~ factor(
      condition, levels = c("Flat", "Decreasing", "Increasing")
    ),
    data = survey_data
  )
)

flat_vs_dec <- cohens_d(
  SubjectiveLearning ~ condition, 
  data=survey_data %>% filter(condition != "Increasing")
  ) %>% 
  pull(Cohens_d) %>% abs()

loc_flat_vs_dec <- survey_data %>% 
  filter(condition != "Increasing") %>% 
  group_by(condition) %>% summarize(m=mean(SubjectiveLearning)) %>% 
  pull(m) %>% 
  mean()

inc_vs_flat <- cohens_d(
  SubjectiveLearning ~ condition, 
  data=survey_data %>% filter(condition != "Decreasing")
) %>% 
  pull(Cohens_d) %>% abs()

loc_inc_vs_flat <- survey_data %>% 
  filter(condition != "Decreasing") %>% 
  group_by(condition) %>% summarize(m=mean(SubjectiveLearning)) %>% 
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
  theme_matplotlib() +
  scale_color_manual(values = c("#b63441", "#1f1e1e", "#3f69c9")) +
  annotate(
    "text",
    x = 1.5, y = loc_flat_vs_dec,
    label = glue("Flat vs. Decr.:\nd = {round(flat_vs_dec, 2)}, p < .001"),
    hjust = .5, size = 4
  ) +
  annotate(
    "text",
    x = 2.5, y = loc_inc_vs_flat,
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
      condition, levels = c("Flat", "Decreasing", "Increasing")
    ),
    data = survey_data
  )
)

flat_vs_dec <- cohens_d(
  IllusoryBeliefs ~ condition, 
  data=survey_data %>% filter(condition != "Increasing")
) %>% 
  pull(Cohens_d) %>% abs()

loc_flat_vs_dec <- survey_data %>% 
  filter(condition != "Increasing") %>% 
  group_by(condition) %>% summarize(m=mean(IllusoryBeliefs)) %>% 
  pull(m) %>% 
  mean()

inc_vs_flat <- cohens_d(
  IllusoryBeliefs ~ condition, 
  data=survey_data %>% filter(condition != "Decreasing")
) %>% 
  pull(Cohens_d) %>% abs()

loc_inc_vs_flat <- survey_data %>% 
  filter(condition != "Decreasing") %>% 
  group_by(condition) %>% summarize(m=mean(IllusoryBeliefs)) %>% 
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
  theme_matplotlib() +
  scale_color_manual(values = c("#b63441", "#1f1e1e", "#3f69c9")) +
  annotate(
    "text",
    x = 1.5, y = loc_flat_vs_dec,
    label = glue("Flat vs. Decr.:\nd = {round(flat_vs_dec, 2)}, p < .001"),
    hjust = .5, size = 4
  ) +
  annotate(
    "text",
    x = 2.5, y = loc_inc_vs_flat,
    label = glue("Incr. vs. Flat:\nd = {round(inc_vs_flat, 2)}, p < .001"),
    hjust = .5, size = 4
  ) +
  labs(
    x = element_blank(),
    y = "Endorsement of Illusory Beliefs",
    color = element_blank()
  ) +
  guides(color = "none")


# Sense of Learning (Originally pre-reged)


tidy(
  lm(
    scale(SoL) ~ factor(condition,
                        levels = c("Flat", "Decreasing", "Increasing")
    ),
    data = survey_data
  )
)

flat_vs_dec <- cohens_d(
  SoL ~ condition, 
  data=survey_data %>% filter(condition != "Increasing")
) %>% 
  pull(Cohens_d) %>% abs()

loc_flat_vs_dec <- survey_data %>% 
  filter(condition != "Increasing") %>% 
  group_by(condition) %>% summarize(m=mean(SoL)) %>% 
  pull(m) %>% 
  mean()

inc_vs_flat <- cohens_d(
  SoL ~ condition, 
  data=survey_data %>% filter(condition != "Decreasing")
) %>% 
  pull(Cohens_d) %>% abs()

loc_inc_vs_flat <- survey_data %>% 
  filter(condition != "Decreasing") %>% 
  group_by(condition) %>% summarize(m=mean(SoL)) %>% 
  pull(m) %>% 
  mean()

ggplot(survey_data, aes(x = condition, y = SoL, color = condition)) +
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
  scale_color_manual(values = c("#b63441", "#1f1e1e", "#3f69c9")) +
  annotate(
    "text",
    x = 1.5, y = loc_flat_vs_dec,
    label = glue("Flat vs. Decr.:\nd = {round(flat_vs_dec, 2)}, p < .001"),
    hjust = .5, size = 4
  ) +
  annotate(
    "text",
    x = 2.5, y = loc_inc_vs_flat,
    label = glue("Incr. vs. Flat:\nd = {round(inc_vs_flat, 2)}, p < .001"),
    hjust = .5, size = 4
  ) +
  labs(
    x = element_blank(),
    y = "Subjective Learning",
    color = element_blank()
  ) +
  guides(color = "none")


# Bet Amounts
offsets <- c(Decreasing = -0.1, Flat = 0, Increasing = 0.1)
survey_data <- (survey_data %>%
    mutate(Bet_Offset = BetAmount + offsets[condition])
)
ggplot(
  survey_data,
  aes(x = Bet_Offset, color = condition, linetype = condition)
) +
  stat_ecdf(geom = "step", position = position_nudge(x = .1)) +
  theme_matplotlib() +
  scale_color_manual(values = c("#b63441", "#1f1e1e", "#3f69c9")) +
  labs(
    y = "Cumulative % of Participants",
    x = "Bet Amount",
    color = element_blank(),
    linetype = element_blank()
  ) +
  scale_linetype_manual(values = c("dotdash", "dashed", "solid"))

tidy(
  lm(
    scale(BetAmount) ~ factor(condition,
      levels = c("Flat", "Decreasing", "Increasing")
    ),
    data = survey_data
  )
)
ggplot(
  survey_data,
  aes(x = EasierOrHarder, color = condition, fill = condition)
) +
  geom_bar(position = position_dodge()) +
  theme_matplotlib() +
  scale_color_manual(values = c("#b63441", "#1f1e1e", "#3f69c9")) +
  scale_fill_manual(values = c("#b63441", "#1f1e1e", "#3f69c9")) +
  scale_x_continuous(
    breaks = seq(1, 7),
    labels = c(
      "Much\nHarder",
      "Harder",
      "Slightly\nHarder",
      "As\nHard/Easy",
      "Slightly\nEasier",
      "Easier",
      "Much\nEasier"
    )
  ) +
  labs(
    y = "% of Participants",
    x = "Task was [...] than anticipated",
    color = element_blank(),
    fill = element_blank()
  ) +
  scale_linetype_manual(values = c("dotdash", "dashed", "solid"))


# Export Statistical Results to JSON for manuscript writing
library(jsonlite)

# Helper function to format OLS regression results (pre-registered analysis)
format_ols_result <- function(dv, data, target_condition) {
  model <- lm(
    as.formula(paste(
      dv,
      "~ factor(condition, levels = c('Flat', 'Decreasing', 'Increasing'))"
    )),
    data = data
  )
  tidied <- tidy(model)
  row <- tidied %>% filter(str_ends(term, target_condition))

  df_resid <- glance(model)$df.residual
  t_val <- abs(row$statistic)
  p_val <- row$p.value

  filter_out <- if (target_condition == "Increasing") "Decreasing" else "Increasing"
  cd <- cohens_d(
    as.formula(paste(dv, "~ condition")),
    data = data %>% filter(condition != filter_out)
  ) %>% pull(Cohens_d) %>% abs()

  p_formatted <- if (p_val < .001) "p < .001" else glue("p = {format(round(p_val, 3), nsmall = 3)}")

  glue(
    "t({df_resid}) = {format(round(t_val, 2), nsmall = 2)}, ",
    "{p_formatted}, ",
    "d = {format(round(cd, 2), nsmall = 2)}"
  )
}

# Sample information
sample_counts <- survey_data %>% 
  count(condition) %>% 
  pivot_wider(names_from = condition, values_from = n, names_prefix = "n_") %>%
  rename_with(tolower)

results_export <- list(
  collected_n = survey_data %>% nrow(),
  final_n = survey_data %>% nrow(),
  n_decreasing = sample_counts %>% pull(n_decreasing),
  n_flat = sample_counts %>% pull(n_flat),
  n_increasing = sample_counts %>% pull(n_increasing)
)

# Cronbach's alpha values
results_export$alpha_subjective_learning <- survey_data %>% 
  dplyr::select(all_of(cols_subj_learn)) %>% 
  cronbach.alpha() %>% 
  pluck("alpha") %>% 
  round(2)

results_export$alpha_illusory_beliefs <- survey_data %>% 
  dplyr::select(all_of(cols_illusory_beliefs)) %>% 
  cronbach.alpha() %>% 
  pluck("alpha") %>% 
  round(2)

# Descriptive statistics (M, SD) by condition for each DV
dvs <- c("SubjectiveLearning", "IllusoryBeliefs", "BetAmount")
dv_labels <- c("subjective_learning", "illusory_beliefs", "bet_amount")

descriptives <- survey_data %>%
  group_by(condition) %>%
  summarize(
    across(all_of(dvs), list(m = mean, sd = sd)),
    .groups = "drop"
  )

for (i in seq_along(dvs)) {
  for (cond in c("Decreasing", "Flat", "Increasing")) {
    row <- descriptives %>% filter(condition == cond)
    prefix <- glue("{dv_labels[i]}_{tolower(cond)}")
    results_export[[glue("{prefix}_m")]] <- round(row[[glue("{dvs[i]}_m")]], 2)
    results_export[[glue("{prefix}_sd")]] <- round(row[[glue("{dvs[i]}_sd")]], 2)
  }
}

# Subjective Learning OLS (pre-registered: two dummies, Flat as reference)
results_export$subjective_learning_inc_vs_flat <- format_ols_result(
  "SubjectiveLearning", survey_data, "Increasing"
)

results_export$subjective_learning_flat_vs_dec <- format_ols_result(
  "SubjectiveLearning", survey_data, "Decreasing"
)

# Illusory Beliefs OLS (pre-registered: two dummies, Flat as reference)
results_export$illusory_beliefs_inc_vs_flat <- format_ols_result(
  "IllusoryBeliefs", survey_data, "Increasing"
)

results_export$illusory_beliefs_flat_vs_dec <- format_ols_result(
  "IllusoryBeliefs", survey_data, "Decreasing"
)

# Bet Amount OLS (pre-registered: two dummies, Flat as reference)
results_export$bet_amount_inc_vs_flat <- format_ols_result(
  "BetAmount", survey_data, "Increasing"
)

results_export$bet_amount_flat_vs_dec <- format_ols_result(
  "BetAmount", survey_data, "Decreasing"
)

# Write to JSON file
results_export %>%
  write_json(
    here("Results", "Study1_statistical_results.json"),
    pretty = TRUE,
    auto_unbox = TRUE
  )

cat("Statistical results exported to Results/Study1_statistical_results.json\n")