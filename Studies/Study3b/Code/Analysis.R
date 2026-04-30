library(tidyverse)
library(tidymodels)
library(glue)
library(ltm)
library(lme4)
library(lmerTest)
library(purrr)
library(lubridate)
library(vctrs)
library(here)

i_am("Studies/Study3b/Code/Analysis.R")

mean_and_ci <- function(grouped_data, var) {
  grouped_data %>%
    summarise(
      mean = mean({{ var }}, na.rm = TRUE),
      sd = sd({{ var }}, na.rm = TRUE),
      n = n()
    ) %>%
    mutate(
      se = sd / sqrt(n),
      llci = mean - qt(1 - (0.05 / 2), n - 1) * se,
      ulci = mean + qt(1 - (0.05 / 2), n - 1) * se
    )
}

# Data Processing
game_data <- read_csv(here("Studies/Study3b", "Data", "game_data_clean.csv"))
game_data_weekly <- game_data %>%
  mutate(gameduration = enddate - startdate) %>%
  group_by(turkid) %>%
  mutate(across(
    c(Hook, Length, Tone, Content, CallToAction),
    ~ ifelse(.x != lag(.x), 1, 0),
    .names = "Changed_{col}"
  )) %>%
  mutate(timetochoose = as.numeric(choicestimes - lag(choicestimes))) %>%
  ungroup() %>%
  mutate(n_changes = rowSums(dplyr::select(., starts_with("Changed_")))) %>%
  drop_na(Changed_Hook, turkid)

game_data_turkid <-
  game_data_weekly %>%
  group_by(turkid, week) %>%
  summarise(
    gameduration = mean(gameduration),
    n_changes =
      mean(n_changes),
    n_zero_changes =
      mean(n_changes == 0)
  ) %>%
  group_by(turkid) %>%
  summarise(
    gameduration = mean(gameduration),
    n_changes =
      mean(n_changes),
    n_zero_changes =
      mean(n_zero_changes)
  )

# data_exclusions <- read_csv("data_exclusions.csv")
survey_data <-
  read_csv(here("Studies/Study3b", "Data", "survey_clean.csv")) %>%
  left_join(game_data_turkid) %>%
  drop_na(turkid) %>%
  ungroup()

survey_data_clean <-
  survey_data %>%
  mutate(
    DV_4 = -DV_4_R,
    condition = factor(
      condid,
      levels = c(-1, 0, 1),
      labels = c("Decreasing", "Flat", "Increasing")
    )
  ) %>%
  mutate(
    AttributesPredict = rowMeans(dplyr::select(., c(DV_1, DV_2, DV_3, DV_4))) + 3,
    SubjectiveKnowledge = rowMeans(dplyr::select(., c(CouldLearn_1, CouldLearn_2, CouldLearn_3))) + 3,
    included = gameduration > 50
  ) %>%
  filter(included)


# Analysis of Game Choices

# Number of Attribute Changes
game_data_weekly %>% ggplot(aes(x = week, y = n_changes, color = condition)) +
  geom_smooth(method = "lm") +
  labs(y = "Number of Attributes Changed", x = element_blank()) +
  scale_x_continuous(
    breaks = seq(2, 8, 1),
    labels = glue("Week {seq(1, 7)}\nto {seq(2, 8)}")
  )

game_data_weekly %>%
  group_by(condition, week) %>%
  mean_and_ci(n_changes) %>%
  ggplot(aes(
    x =
      week, y = mean, color = condition
  )) +
  geom_errorbar(aes(ymin = llci, ymax = ulci),
    width = .1,
    position = position_dodge(width = .3)
  ) +
  geom_point(position = position_dodge(
    width =
      .3
  )) +
  labs(y = "Number of Attributes Changed", x = element_blank()) +
  scale_x_continuous(
    breaks = seq(2, 8, 1),
    labels = glue("Week {seq(1, 7)}\nto {seq(2, 8)}")
  )


summary(lmer(
  formula = n_changes ~ week * condition + (1 + week |
    turkid),
  data = game_data_weekly
))

game_data_weekly %>%
  group_by(week, condition) %>%
  summarise(m = median(n_changes))

# Time to Choose

game_data_weekly %>% ggplot(aes(x = week, y = timetochoose, color = condition)) +
  geom_smooth(method = "lm") +
  labs(y = "Time to Choose", x = element_blank()) +
  scale_x_continuous(
    breaks = seq(2, 8, 1),
    labels = glue("Week {seq(1, 7)}\nto {seq(2, 8)}")
  )


game_data_weekly %>%
  group_by(condition, week) %>%
  mean_and_ci(timetochoose) %>%
  ggplot(aes(
    x =
      week, y = mean, color = condition
  )) +
  geom_errorbar(aes(ymin = llci, ymax = ulci),
    width = .1,
    position = position_dodge(width = .3)
  ) +
  geom_point(position = position_dodge(
    width =
      .3
  )) +
  labs(y = "Time Between Choices", x = element_blank()) +
  scale_x_continuous(
    breaks = seq(2, 8, 1),
    labels = glue("Week {seq(1, 7)}\nto {seq(2, 8)}")
  )




summary(lmer(
  formula = timetochoose ~ week * condition + (1 + week |
    turkid),
  data = game_data_weekly
))




# Analysis of Survey Data

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
cronbach.alpha(survey_data_clean %>% drop_na(c(DV_1, DV_2, DV_3, DV_4)) %>% dplyr::select(DV_1, DV_2, DV_3, DV_4))

model <- lm(AttributesPredict ~ condid, data = survey_data_clean)
survey_data_clean %>%
  group_by(condition) %>%
  mean_and_ci(AttributesPredict) %>%
  ggplot(aes(x = factor(condition), y = mean)) +
  geom_errorbar(aes(ymin = llci, ymax = ulci), width = .1) +
  geom_point() +
  labs(
    x = element_blank(),
    y = "Beliefs Attribute Predict Outcomes (1-5)",
    title = generate_title(model)
  )


ggplot(survey_data_clean, aes(x = AttributesPredict, color = condition)) +
  stat_ecdf(geom = "step")

model <-
  lm(EdgeProbability ~ condid, data = survey_data_clean)

ggplot(survey_data_clean, aes(x = EdgeProbability, color = condition)) +
  stat_ecdf(geom = "step")
survey_data_clean %>%
  group_by(condid) %>%
  mean_and_ci(EdgeProbability) %>%
  ggplot(aes(x = factor(condid), y = mean)) +
  geom_errorbar(aes(ymin = llci, ymax = ulci), width = .1) +
  geom_point() +
  labs(
    x = element_blank(),
    y = "% Chance of Better Perf.",
    title = generate_title(model)
  )



model <-
  lm(SubjectiveKnowledge ~ condid, data = survey_data_clean)

ggplot(survey_data_clean, aes(x = SubjectiveKnowledge, color = condition)) +
  stat_ecdf(geom = "step")
survey_data_clean %>%
  group_by(condid) %>%
  mean_and_ci(SubjectiveKnowledge) %>%
  ggplot(aes(x = factor(condid), y = mean)) +
  geom_errorbar(aes(ymin = llci, ymax = ulci), width = .1) +
  geom_point() +
  labs(
    x = element_blank(),
    y = "% Chance of Better Perf.",
    title = generate_title(model)
  )


get_pval <- function(i) {
  pval <- survey_data_clean %>%
    slice(0:i) %>%
    lm(formula = EdgeProbability ~ condid) %>%
    tidy() %>%
    slice(2) %>%
    pull(statistic)
}

## Analysis of best/worst attributes


expand_combinations <- function(a, b, c = NULL, d = NULL, e = NULL, sep = "_") {
  if (is.null(c)) {
    grid <- expand.grid(a, b)
    glue("{grid$Var2}{sep}{grid$Var1}")
  } else {
    grid <- expand.grid(a, b, c)
    glue(grid, sep = "_")
  }
}

expand_combinations <- function(..., sep = "_") {
  grid <- expand.grid(...)
  grid %>%
    tidyr::unite("Result", everything(), sep = sep) %>%
    pull(Result)
}

survey_data_choices <- survey_data_clean %>%
  unite(
    Best_Posts,
    c(`1_Hook`, `1_Length`, `1_Tone`, `1_Content`, `1_CTA`),
    remove = FALSE
  ) %>%
  unite(
    Worst_Posts,
    c(`1_Hook`, `1_Length`, `1_Tone`, `1_Content`, `1_CTA`),
    remove = FALSE
  ) %>%
  unite(
    Best_Posts_HookLength,
    c(`1_Hook`, `1_Length`),
    remove = FALSE
  ) %>%
  unite(
    Best_Posts_ToneContentCTA,
    c(`1_Tone`, `1_Content`, `1_CTA`),
    remove = FALSE
  ) %>%
  unite(
    Worst_Posts_HookLength,
    c(`2_Hook`, `2_Length`),
    remove = FALSE
  ) %>%
  unite(
    Worst_Posts_ToneContentCTA,
    c(`2_Tone`, `2_Content`, `2_CTA`),
    remove = FALSE
  ) %>%
  mutate(
    Best_Posts = factor(Best_Posts,
      levels = expand_combinations(c(0, 1, 2), c(0, 1, 2), c(0, 1), c(0, 1, 2), c(0, 1))
    ),
    Worst_Posts = factor(Worst_Posts,
      levels = expand_combinations(c(0, 1, 2), c(0, 1, 2), c(0, 1), c(0, 1, 2), c(0, 1))
    ),
    Best_Posts_HookLength = factor(Best_Posts_HookLength,
      levels = expand_combinations(c(0, 1, 2), c(0, 1, 2)),
      labels = expand_combinations(c("Question", "Cliffhanger", "Anecdote"),
        c("< 400 words", "400-1400 words", "1400-2400 words"),
        sep = " - "
      )
    ),
    Best_Posts_ToneContentCTA = factor(Best_Posts_ToneContentCTA,
      levels = expand_combinations(c(0, 1), c(0, 1, 2), c(0, 1)),
      labels = expand_combinations(c("First P.", "Third P."), c("Text", "Text+Pic", "Text+Poll"), c("CTA", "No CTA"), sep = " - ")
    ),
    Worst_Posts_HookLength = factor(Worst_Posts_HookLength,
      levels = expand_combinations(c(0, 1, 2), c(0, 1, 2)),
      labels = expand_combinations(c("Question", "Cliffhanger", "Anecdote"),
        c("< 400 words", "400-1400 words", "1400-2400 words"),
        sep = " - "
      )
    ),
    Worst_Posts_ToneContentCTA = factor(Worst_Posts_ToneContentCTA,
      levels = expand_combinations(c(0, 1), c(0, 1, 2), c(0, 1)),
      labels = expand_combinations(c("First P.", "Third P."),
        c("Text", "Text+Pic", "Text+Poll"),
        c("CTA", "No CTA"),
        sep = " - "
      )
    )
  )


table_best <- as.data.frame(table(survey_data_choices$Best_Posts_HookLength, survey_data_choices$Best_Posts_ToneContentCTA))
table_best$Judgment <- "Best"

table_worst <- as.data.frame(table(survey_data_choices$Worst_Posts_HookLength, survey_data_choices$Worst_Posts_ToneContentCTA))
table_worst$Judgment <- "Worst"

tables <- rbind(table_best, table_worst)

ggplot(tables, aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq)) +
  labs(
    x = "Length and Hook",
    y = "CTA, Content and Tone"
  ) +
  guides(fill = "none") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  facet_grid(Judgment ~ .)


# Selecting winner
set.seed(242204) # Date
survey_data %>%
  filter(EdgeProbability == 50) %>%
  sample_n(1) %>%
  dplyr::select(turkid, condid)


b <- survey_data_choices %>%
  group_by(Best_Posts, .drop = FALSE) %>%
  count() %>%
  ungroup() %>%
  mutate(z = factor(fct_reorder(Best_Posts, desc(n)), labels = seq(1:108)), prop = n / nrow(survey_data_choices))
b %>% ggplot(aes(x = z, y = prop)) +
  geom_bar(stat = "identity", width = .7) +
  geom_hline(yintercept = 1 / 108) +
  labs(y = "% Identifying as 'Best Feature Combination'", x = "Unique Feature Combinations (Sorted)", title = "End Beliefs - Social Media Study") +
  annotate("text", x = 65, y = 1 / 90, label = "Expected Under Random Beliefs") +
  theme(axis.text.x = element_blank())
