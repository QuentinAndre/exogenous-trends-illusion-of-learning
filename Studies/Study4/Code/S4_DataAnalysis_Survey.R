# 1. Preamble

library(tidyverse)
library(tidymodels)
library(ltm)
library(effectsize)
library(glue)
library(here)

i_am("Studies/Study4/Code/S4_DataAnalysis_Survey.R")
source(here("custom_theme.R"))

cols_subj_learn_final <- c(
  "CouldLearn_1",
  "CouldLearn_2",
  "CouldLearn_3"
)

cols_illusory_beliefs_final <- c("Confidence_NC",
                           "Confidence_MS",
                           "Confidence_CV",
                           "Confidence_SA")

cols_subj_learn_interim <- c(
  "could_learn_interim_1",
  "could_learn_interim_2",
  "could_learn_interim_3"
)

cols_illusory_beliefs_interim <- c("belief_valuation",
                                "belief_market_size",
                                "belief_sector_of_activity",
                                "belief_competitors")


survey_data_all <- read_csv(here("Studies/Study4", "Data", "survey_clean.csv")) %>%
  mutate(Slope = 2 - Slope) %>%
  rowwise() %>%
  mutate(
    SubjectiveLearning_Interim = mean(c_across(cols_subj_learn_interim)),
    IllusoryBeliefs_Interim = mean(c_across(cols_illusory_beliefs_interim)),
    SubjectiveLearning_Final = mean(c_across(cols_subj_learn_final)),
    IllusoryBeliefs_Final = mean(c_across(cols_illusory_beliefs_final)),
    SubjectiveLearning_Delta = SubjectiveLearning_Final-SubjectiveLearning_Interim,
    IllusoryBeliefs_Delta = IllusoryBeliefs_Final-IllusoryBeliefs_Interim,    
    condition = factor(
      case_when(
        condid == 0 ~ "Flat",
        condid == 1 ~ "Increasing"
      ),
      levels =
        c("Flat", "Increasing")
    ),
    message_type = factor(message_type, levels=c("control", 
                                                 "positive_test", "negative_test"),
                          labels = c("Control", "Positive\nTest", "Negative\nTest"))
  )

survey_data <- survey_data_all %>%
  dplyr::filter(time_to_completion > 110)


# 2. Subjective Learning (Interim)
cronbach.alpha(survey_data %>% dplyr::select(cols_subj_learn_interim))

tidy(
  lm(
    scale(SubjectiveLearning_Interim) ~ factor(
      condition, levels = c("Flat", "Increasing")
    ),
    data = survey_data
  )
)


inc_vs_flat <- cohens_d(
  SubjectiveLearning_Interim ~ condition, 
  data=survey_data
) %>% 
  pull(Cohens_d) %>% abs()

loc_inc_vs_flat <- survey_data %>% 
  group_by(condition) %>% summarize(m=mean(SubjectiveLearning_Interim)) %>% 
  pull(m) %>% 
  mean()


ggplot(survey_data, aes(x = condition, y = SubjectiveLearning_Interim)) +
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
  scale_color_manual(values = c("#1f1e1e", "red", "green")) +
  labs(
    x = element_blank(),
    y = "Subjective Sense of Learning\n(Round 14, Before Prompt)",
    color = element_blank()
  ) + 
  annotate(
    "text",
    x = 1.5, y = loc_inc_vs_flat,
    label = glue("Incr. vs. Flat:\nd = {round(inc_vs_flat, 2)}, p < .001"),
    hjust = .5, size = 4
  )



# 3. IllusoryBeliefs (Interim)
cronbach.alpha(survey_data %>% dplyr::select(cols_illusory_beliefs_interim))

tidy(
  lm(
    scale(IllusoryBeliefs_Interim) ~ factor(
      condition, levels = c("Flat", "Increasing")
    ),
    data = survey_data
  )
)


inc_vs_flat <- cohens_d(
  IllusoryBeliefs_Interim ~ condition, 
  data=survey_data
) %>% 
  pull(Cohens_d) %>% abs()

loc_inc_vs_flat <- survey_data %>% 
  group_by(condition) %>% summarize(m=mean(IllusoryBeliefs_Interim)) %>% 
  pull(m) %>% 
  mean()


ggplot(survey_data, aes(x = condition, y = IllusoryBeliefs_Interim)) +
  stat_summary(
    fun = mean, geom = "point", position = position_dodge(width = 0.5),
    size = 4
  ) +
  stat_summary(
    fun.data = mean_cl_boot, geom = "errorbar", width = 0,
    position = position_dodge(width = 0.5), show.legend = FALSE,
    size = 1.2
  ) +
  #theme_matplotlib() +
  scale_color_manual(values = c("#1f1e1e", "red", "green")) +
  labs(
    x = element_blank(),
    y = "Endorsement of Illusory Corr.\n(Round 14, Before Prompt)",
    color = element_blank()
  ) + 
  annotate(
    "text",
    x = 1.5, y = loc_inc_vs_flat,
    label = glue("Incr. vs. Flat:\nd = {round(inc_vs_flat, 2)}, p = .026"),
    hjust = .5, size = 4
  )


ggplot(survey_data, aes(x = condition, y = SubjectiveLearning_Final, color = message_type)) +
  stat_summary(
    fun = mean, geom = "point", position = position_dodge(width = 0.5),
    size = 4
  ) +
  stat_summary(
    fun.data = mean_cl_boot, geom = "errorbar", width = 0,
    position = position_dodge(width = 0.5), show.legend = FALSE,
    size = 1.2
  ) +
  #theme_matplotlib() +
  scale_color_manual(values = c("blue3",  "green3", "red3")) +
  labs(
    x = element_blank(),
    y = "Subjective Sense of Learning\n(Round 20, After Prompt)",
    color = element_blank()
  )



ggplot(survey_data, aes(x = condition, y = IllusoryBeliefs_Final, color = message_type)) +
  stat_summary(
    fun = mean, geom = "point", position = position_dodge(width = 0.5),
    size = 4
  ) +
  stat_summary(
    fun.data = mean_cl_boot, geom = "errorbar", width = 0,
    position = position_dodge(width = 0.5), show.legend = FALSE,
    size = 1.2
  ) +
  #theme_matplotlib() +
  scale_color_manual(values = c("blue3",  "green3", "red3")) +
  labs(
    x = element_blank(),
    y = "Endorsement of Illusory Corr.\n(Round 20, After Prompt)",
    color = element_blank()
  )


ggplot(survey_data, aes(x = condition, y = SubjectiveLearning_Delta, color = message_type)) +
  stat_summary(
    fun = mean, geom = "point", position = position_dodge(width = 0.5),
    size = 4
  ) +
  stat_summary(
    fun.data = mean_cl_boot, geom = "errorbar", width = 0,
    position = position_dodge(width = 0.5), show.legend = FALSE,
    size = 1.2
  ) +
  #theme_matplotlib() +
  scale_color_manual(values = c("blue3",  "green3", "red3")) +
  labs(
    x = element_blank(),
    y = "Change in Subjective\nLearning (Final - Interim)",
    color = element_blank()
  ) + geom_hline(aes(yintercept = 0), linetype="dashed")

ggplot(survey_data, aes(x = condition, y = IllusoryBeliefs_Delta, color = message_type)) +
  stat_summary(
    fun = mean, geom = "point", position = position_dodge(width = 0.5),
    size = 4
  ) +
  stat_summary(
    fun.data = mean_cl_boot, geom = "errorbar", width = 0,
    position = position_dodge(width = 0.5), show.legend = FALSE,
    size = 1.2
  ) +
  #theme_matplotlib() +
  scale_color_manual(values = c("blue3",  "green3", "red3")) +
  labs(
    x = element_blank(),
    y = "Change in Endorsement of\nIllusory Correlations (Final - Interim)",
    color = element_blank()
  )+ geom_hline(aes(yintercept = 0), linetype="dashed")





survey_data_prepost <- survey_data %>% pivot_longer(c(IllusoryBeliefs_Interim,
                                                      IllusoryBeliefs_Final,
                                                      SubjectiveLearning_Interim,
                                                      SubjectiveLearning_Final)) %>% 
  mutate(Variable=
           str_split(name, "_") %>% 
           map_chr(~ .x[[1]]) %>% 
           factor(levels=c("SubjectiveLearning", "IllusoryBeliefs"),
                  labels=c("Subj. Sense of Learning",
                           "Endorsement of Illusory Corr.")),
         Timing=str_split(name, "_") %>% map_chr(~ .x[[2]]) %>% 
           factor(levels=c("Interim", "Final"),
                  labels=c("Interim\nMeasure", "Final\nMeasure"))
  )



ggplot(survey_data_prepost, 
       aes(x = Timing, y = scale(value), color=message_type)) +
  stat_summary(
    fun = mean, geom = "point", position = position_dodge(width = 0.25),
    size = 4
  ) +
  stat_summary(
    fun.data = mean_cl_boot, geom = "errorbar", width = 0,
    position = position_dodge(width = 0.25), show.legend = FALSE,
    size = 1.2
  ) +
  stat_summary(
    fun = mean, geom = "line",
    aes(group=message_type),
    position = position_dodge(width = 0.25), show.legend = FALSE,
    size = .8, linetype="dashed"
  ) +
  
  theme_matplotlib() +
  scale_color_manual(values = c("#1f1e1e", "green3", "red3")) +
  labs(
    x = element_blank(),
    y = "Subjective Sense of Learning",
    color = element_blank()
  ) + 
  facet_grid(Variable~condition, scales="free_y")



testing_strategy <- survey_data %>% 
  dplyr::select(turkid, condid, message_type,
                Positive_Test_First, Positive_Test_Second,
                Negative_Test_First, Negative_Test_Second) %>% 
  pivot_longer(cols = c(Positive_Test_First, Positive_Test_Second,
                        Negative_Test_First, Negative_Test_Second)) %>% 
  mutate(Strategy=str_split(name, "_") %>% map_chr(~ .x[[1]]),
         Timing=str_split(name, "_") %>% map_chr(~ .x[[3]])) 

testing_strategy %>%
  group_by(message_type, Timing, Strategy) %>%
  count(value) %>%
  ggplot(aes(x=value, y=n, fill=message_type)) +
  geom_col(position=position_dodge2(), width=.2) +
  facet_grid(Strategy~Timing) +
  scale_fill_manual(values = c("blue3",  "green3", "red3"))


# 6. Slope Detection Analysis

# Recode Slope: 1 = Increasing, 2 = Flat, 3 = Decreasing
survey_data <- survey_data %>%
  mutate(
    perceived_slope = factor(Slope, levels = c(1, 2, 3),
                             labels = c("Increasing", "Flat", "Decreasing")),
    correct_detection = case_when(
      condition == "Increasing" & Slope == 1 ~ 1,
      condition == "Flat" & Slope == 2 ~ 1,
      TRUE ~ 0
    )
  )

# Overall detection rates by condition
survey_data %>%
  group_by(condition, perceived_slope) %>%
  summarize(n = n(), .groups = "drop") %>%
  group_by(condition) %>%
  mutate(pct = n / sum(n) * 100) %>%
  print(n = Inf)

# Correct detection rate by condition
survey_data %>%
  group_by(condition) %>%
  summarize(
    n = n(),
    n_correct = sum(correct_detection),
    pct_correct = mean(correct_detection) * 100,
    .groups = "drop"
  )

# Chi-squared test: correct detection ~ condition
chisq.test(table(survey_data$condition, survey_data$correct_detection))

# Correct detection by condition x message_type
detection_by_prompt <- survey_data %>%
  group_by(condition, message_type) %>%
  summarize(
    n = n(),
    n_correct = sum(correct_detection),
    pct_correct = mean(correct_detection) * 100,
    .groups = "drop"
  )
print(detection_by_prompt)

# Logistic regression: correct_detection ~ condition * message_type
detection_model <- glm(
  correct_detection ~ condition * message_type,
  data = survey_data,
  family = binomial
)
summary(detection_model)

# Simple effects: effect of testing prompt within each condition
detection_model_inc <- glm(
  correct_detection ~ message_type,
  data = survey_data %>% filter(condition == "Increasing"),
  family = binomial
)
summary(detection_model_inc)

detection_model_flat <- glm(
  correct_detection ~ message_type,
  data = survey_data %>% filter(condition == "Flat"),
  family = binomial
)
summary(detection_model_flat)

# Simple effects: effect of slope condition within each testing prompt
detection_model_control <- glm(
  correct_detection ~ condition,
  data = survey_data %>% filter(message_type == "Control"),
  family = binomial
)
summary(detection_model_control)

detection_model_postest <- glm(
  correct_detection ~ condition,
  data = survey_data %>% filter(message_type == "Positive\nTest"),
  family = binomial
)
summary(detection_model_postest)

detection_model_negtest <- glm(
  correct_detection ~ condition,
  data = survey_data %>% filter(message_type == "Negative\nTest"),
  family = binomial
)
summary(detection_model_negtest)

# Plot: correct detection rate by condition x prompt
ggplot(survey_data, aes(x = message_type, y = correct_detection, fill = condition)) +
  stat_summary(
    fun = mean, geom = "bar",
    position = position_dodge(width = 0.7), width = 0.6
  ) +
  stat_summary(
    fun.data = mean_cl_normal, geom = "errorbar", width = 0.15,
    position = position_dodge(width = 0.7)
  ) +
  theme_matplotlib() +
  scale_fill_manual(values = c("grey60", "steelblue")) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(
    x = "Testing Prompt",
    y = "Correct Slope Detection Rate",
    fill = "Slope Condition"
  )


# =============================================================================
# Export Statistical Results for Manuscript
# =============================================================================

library(jsonlite)

format_welch_result <- function(dv, data, group1, group2) {
  d <- data %>% filter(condition %in% c(group1, group2))
  tt <- t.test(d[[dv]][d$condition == group2], d[[dv]][d$condition == group1])
  cd <- cohens_d(as.formula(paste(dv, "~ condition")), data = d) %>%
    pull(Cohens_d) %>% abs()
  p_formatted <- if (tt$p.value < .001) "p < .001" else sprintf("p = %.3f", tt$p.value)
  sprintf("t(%.1f) = %.2f, %s, d = %.2f", tt$parameter, abs(tt$statistic), p_formatted, cd)
}

manuscript_results <- list()

# Sample sizes
manuscript_results$collected_n <- survey_data_all %>% nrow()
manuscript_results$final_n <- survey_data %>% nrow()
n_by_cond <- survey_data %>% count(condition)
manuscript_results$n_flat <- n_by_cond %>% filter(condition == "Flat") %>% pull(n)
manuscript_results$n_increasing <- n_by_cond %>% filter(condition == "Increasing") %>% pull(n)

# Cronbach's alpha values
manuscript_results$alpha_subjective_learning_interim <- survey_data %>%
  dplyr::select(all_of(cols_subj_learn_interim)) %>%
  cronbach.alpha() %>% pluck("alpha") %>% round(2)

manuscript_results$alpha_illusory_beliefs_interim <- survey_data %>%
  dplyr::select(all_of(cols_illusory_beliefs_interim)) %>%
  cronbach.alpha() %>% pluck("alpha") %>% round(2)

manuscript_results$alpha_subjective_learning_final <- survey_data %>%
  dplyr::select(all_of(cols_subj_learn_final)) %>%
  cronbach.alpha() %>% pluck("alpha") %>% round(2)

manuscript_results$alpha_illusory_beliefs_final <- survey_data %>%
  dplyr::select(all_of(cols_illusory_beliefs_final)) %>%
  cronbach.alpha() %>% pluck("alpha") %>% round(2)

# Descriptive statistics (M, SD) by condition for interim measures
for (dv in c("SubjectiveLearning_Interim", "IllusoryBeliefs_Interim")) {
  dv_label <- tolower(gsub("([A-Z])", "_\\1", sub("_Interim", "", dv))) %>%
    sub("^_", "", .) %>% paste0("_interim")
  for (cond in c("Flat", "Increasing")) {
    vals <- survey_data %>% filter(condition == cond) %>% pull(!!sym(dv))
    prefix <- paste0(dv_label, "_", tolower(cond))
    manuscript_results[[paste0(prefix, "_m")]] <- round(mean(vals), 2)
    manuscript_results[[paste0(prefix, "_sd")]] <- round(sd(vals), 2)
  }
}

# Descriptive statistics (M, SD) by condition for final measures
for (dv in c("SubjectiveLearning_Final", "IllusoryBeliefs_Final")) {
  dv_label <- tolower(gsub("([A-Z])", "_\\1", sub("_Final", "", dv))) %>%
    sub("^_", "", .) %>% paste0("_final")
  for (cond in c("Flat", "Increasing")) {
    vals <- survey_data %>% filter(condition == cond) %>% pull(!!sym(dv))
    prefix <- paste0(dv_label, "_", tolower(cond))
    manuscript_results[[paste0(prefix, "_m")]] <- round(mean(vals), 2)
    manuscript_results[[paste0(prefix, "_sd")]] <- round(sd(vals), 2)
  }
}

# Welch t-tests for interim measures (Increasing vs. Flat)
manuscript_results$subjective_learning_interim_comparison <- format_welch_result(
  "SubjectiveLearning_Interim", survey_data, "Flat", "Increasing"
)

manuscript_results$illusory_beliefs_interim_comparison <- format_welch_result(
  "IllusoryBeliefs_Interim", survey_data, "Flat", "Increasing"
)

# Welch t-tests for final measures (Increasing vs. Flat)
manuscript_results$subjective_learning_final_comparison <- format_welch_result(
  "SubjectiveLearning_Final", survey_data, "Flat", "Increasing"
)

manuscript_results$illusory_beliefs_final_comparison <- format_welch_result(
  "IllusoryBeliefs_Final", survey_data, "Flat", "Increasing"
)

# ANCOVA models (pre-registered specification)
model_sl_json <- lm(
  scale(SubjectiveLearning_Final) ~ condition * message_type + SubjectiveLearning_Interim,
  data = survey_data
)
model_ib_json <- lm(
  scale(IllusoryBeliefs_Final) ~ condition * message_type + IllusoryBeliefs_Interim,
  data = survey_data
)
model_sl_inc_json <- lm(scale(SubjectiveLearning_Final) ~ message_type + SubjectiveLearning_Interim,
                        data = survey_data %>% filter(condition == "Increasing"))
model_sl_flat_json <- lm(scale(SubjectiveLearning_Final) ~ message_type + SubjectiveLearning_Interim,
                         data = survey_data %>% filter(condition == "Flat"))
model_ib_inc_json <- lm(scale(IllusoryBeliefs_Final) ~ message_type + IllusoryBeliefs_Interim,
                        data = survey_data %>% filter(condition == "Increasing"))
model_ib_flat_json <- lm(scale(IllusoryBeliefs_Final) ~ message_type + IllusoryBeliefs_Interim,
                         data = survey_data %>% filter(condition == "Flat"))

fmt_coef <- function(model, term_name) {
  row <- tidy(model) %>% filter(term == term_name)
  p <- row$p.value
  p_str <- if (p < .001) "p < .001" else sprintf("p = %.3f", p)
  sprintf("b = %.2f, %s", row$estimate, p_str)
}

pt <- "message_typePositive\nTest"
nt <- "message_typeNegative\nTest"
pt_int <- "conditionIncreasing:message_typePositive\nTest"
nt_int <- "conditionIncreasing:message_typeNegative\nTest"

manuscript_results$ancova_sl_increasing_pt       <- fmt_coef(model_sl_inc_json,  pt)
manuscript_results$ancova_sl_increasing_nt       <- fmt_coef(model_sl_inc_json,  nt)
manuscript_results$ancova_sl_flat_pt             <- fmt_coef(model_sl_flat_json, pt)
manuscript_results$ancova_sl_flat_nt             <- fmt_coef(model_sl_flat_json, nt)
manuscript_results$ancova_sl_interaction_pt_trend <- fmt_coef(model_sl_json,     pt_int)
manuscript_results$ancova_sl_interaction_nt_trend <- fmt_coef(model_sl_json,     nt_int)

manuscript_results$ancova_ib_increasing_pt       <- fmt_coef(model_ib_inc_json,  pt)
manuscript_results$ancova_ib_increasing_nt       <- fmt_coef(model_ib_inc_json,  nt)
manuscript_results$ancova_ib_flat_pt             <- fmt_coef(model_ib_flat_json, pt)
manuscript_results$ancova_ib_flat_nt             <- fmt_coef(model_ib_flat_json, nt)
manuscript_results$ancova_ib_interaction_pt_trend <- fmt_coef(model_ib_json,     pt_int)
manuscript_results$ancova_ib_interaction_nt_trend <- fmt_coef(model_ib_json,     nt_int)

write_json(manuscript_results, here("Results", "Study4_statistical_results.json"), pretty = TRUE, auto_unbox = TRUE)

# =============================================================================
# Cell Means Table (2 Slope x 3 Testing Strategy) for Final Beliefs
# =============================================================================

cell_means <- survey_data %>%
  group_by(condition, message_type) %>%
  summarize(
    n = n(),
    sl_m = mean(SubjectiveLearning_Final),
    sl_sd = sd(SubjectiveLearning_Final),
    ib_m = mean(IllusoryBeliefs_Final),
    ib_sd = sd(IllusoryBeliefs_Final),
    .groups = "drop"
  ) %>%
  mutate(
    `Subjective Learning` = sprintf("%.2f (%.2f)", sl_m, sl_sd),
    `Illusory Correlations` = sprintf("%.2f (%.2f)", ib_m, ib_sd),
    N = n
  ) %>%
  dplyr::select(Slope = condition, `Testing Strategy` = message_type,
                N, `Subjective Learning`, `Illusory Correlations`)

print(cell_means, n = Inf)

# =============================================================================
# Summary Figure: Final Beliefs by Slope x Testing Strategy (Faceted by DV)
# =============================================================================

library(marginaleffects)

# Fit ANCOVA models (pre-registered specification)
model_sl <- lm(
  scale(SubjectiveLearning_Final) ~ condition * message_type +
    SubjectiveLearning_Interim,
  data = survey_data
)

model_ib <- lm(
  scale(IllusoryBeliefs_Final) ~ condition * message_type +
    IllusoryBeliefs_Interim,
  data = survey_data
)

# Get estimated marginal means from each model
pred_sl <- predictions(model_sl) %>%
  mutate(DV = "Subjective Learning")

pred_ib <- predictions(model_ib) %>%
  mutate(DV = "Endorsement of Illusory Correlations")

pred_combined <- bind_rows(pred_sl, pred_ib) %>%
  mutate(
    DV = factor(DV, levels = c(
      "Subjective Learning",
      "Endorsement of Illusory Correlations"
    )),
    message_type = factor(message_type,
      levels = levels(survey_data$message_type),
      labels = c("Control", "Positive\nTest", "Negative\nTest")
    )
  )

ggplot(
  pred_combined,
  aes(x = condition, y = estimate, color = message_type, shape = message_type)
) +
  stat_summary(
    fun = mean,
    geom = "line",
    aes(group = message_type),
    position = position_dodge(width = 0.5),
    linetype = "dashed",
    alpha = .5
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    position = position_dodge(width = 0.5),
    size = 4
  ) +
  stat_summary(
    fun.data = mean_cl_boot,
    geom = "errorbar",
    width = 0,
    position = position_dodge(width = 0.5),
    show.legend = FALSE,
    linewidth = 1.2
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.4) +
  facet_wrap(~DV) +
  theme_matplotlib() +
  scale_color_manual(values = c("#808080", "#e6960a", "#6a3d9a")) +
  scale_shape_manual(values = c(16, 18, 15)) +
  labs(
    x = element_blank(),
    y = "Change in Beliefs from Interim\n(Standardized Marginal Means from Model)",
    color = element_blank(),
    shape = element_blank()
  ) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.12, 0.82),
    legend.direction = "vertical",
    legend.background = element_rect(fill = "white", color = NA)
  )
ggsave(
  here("Figures", "Figure 5.png"),
  dpi = 400, width = 9, height = 5, units = "in", bg = "white"
)
