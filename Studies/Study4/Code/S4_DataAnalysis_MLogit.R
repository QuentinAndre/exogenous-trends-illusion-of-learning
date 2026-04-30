library(dplyr)
library(readr)
library(tidyr)
library(janitor)
library(haven)
library(scales)
library(mlogit)
library(stringr)
library(glue)
library(marginaleffects)
library(tidymodels)
library(here)

i_am("Studies/Study4/Code/S4_DataAnalysis_MLogit.R")
source(here("custom_theme.R"))

# Load the data
choices_data <- read_csv(here("Studies/Study4", "Data", "game_choices_clean_with_attributes.csv")) %>%
  arrange(condition) %>% # Sort by condition
  rename_with(
    ~ recode(
      .,
      "valuation" = "Valuation",
      "customers" = "Customers",
      "competitors" = "Competitors",
      "round" = "Round",
      "project" = "Project",
      "condition" = "Condition",
      "project_chosen" = "Project_Chosen"
    )
  )


choices_data <- choices_data %>%
  # Standardize Valuation, Customers, and Competitors
  mutate(
    Valuation = as.numeric(scale(Valuation)),
    Customers = as.numeric(scale(Customers)),
    Competitors = as.numeric(scale(Competitors))
  ) %>%
  # Add dummy variables for activity and Condition
  mutate(
    `Biotech` = as.numeric(activity == "Biotechnology"),
    `Electronics` = as.numeric(activity == "Electronics"),
    `Robotics` = as.numeric(activity == "Robotics"),
    Flat = as.numeric(Condition == "Flat"),
    Inc = as.numeric(Condition == "Increasing"),
    Pos = as.numeric(message_type == "positive_test"),
    Neg = as.numeric(message_type == "negative_test"),
    Option_0 = as.numeric(Project == 0),
    Option_1 = as.numeric(Project == 1),
    Option_2 = as.numeric(Project == 2),
    Project_Chosen = as.numeric(Project_Chosen)
  ) %>%
  dplyr::arrange(turkid, Round, Project) %>%
  # Adjust rounds and slope
  mutate(
    Slope = condid,
    Round = Round - 1
  )

# Create interaction terms
interaction_columns <- c(
  "Option_1", "Option_2", "Valuation", "Customers", "Competitors",
  "Electronics", "Robotics"
)

for (c in interaction_columns) {
  for (m in c("Pos", "Neg")) {
    choices_data <- choices_data %>%
      mutate(
        !!paste0(m, "_", c) := !!sym(c) * !!sym(m),
        !!paste0("ThreeWay_", c) := !!sym(c) * Round * Slope
      )
  }
  choices_data <- choices_data %>%
    mutate(
      !!paste0("Slope_", c) := !!sym(c) * Slope,
      !!paste0("Round_", c) := !!sym(c) * Round,
      !!paste0("ThreeWay_", c) := !!sym(c) * Round * Slope
    )
}

# Sort the final data
choices_data_logit <- choices_data %>%
  mutate(choice_occasion = glue("{turkid}_{Round}"),
         turkid = as.integer(factor(turkid))) %>%
  arrange(turkid, Round, Project)


df_mxl <- mlogit.data(
  choices_data_logit %>% 
    filter(Round > 14 & Inc == 1) %>% 
    as.data.frame() |> 
    dplyr::select(
      turkid, choice_occasion, Project, Project_Chosen,
      Customers, Competitors, Valuation, Electronics, Robotics, 
      Pos_Customers, Pos_Competitors, Pos_Valuation, Pos_Electronics, Pos_Robotics, 
      Neg_Customers, Neg_Competitors, Neg_Valuation, Neg_Electronics, Neg_Robotics
    ),
  choice   = "Project_Chosen",
  shape    = "long",
  alt.var  = "Project",
  chid.var = "choice_occasion",   # task / round
  id.var   = "turkid"      # respondent id  (panel indicator)
)


# Model:
mxl_int <- mlogit(
  Project_Chosen ~                  
    Customers + Competitors + Valuation +
    Electronics + Robotics +
    Pos_Customers + Pos_Competitors + Pos_Valuation +
    Pos_Electronics + Pos_Robotics +
    Neg_Customers + Neg_Competitors + Neg_Valuation +
    Neg_Electronics + Neg_Robotics
  | 0,
  data        = df_mxl,
  rpar        = c(
    Customers = "n", Competitors = "n", Valuation = "n",
    Electronics = "n", Robotics = "n",
    Pos_Customers = "n", Pos_Competitors = "n", Pos_Valuation = "n",
    Pos_Electronics = "n", Pos_Robotics = "n",
    Neg_Customers = "n", Neg_Competitors = "n", Neg_Valuation = "n",
    Neg_Electronics = "n", Neg_Robotics = "n"
  ),
  correlation = TRUE,
  panel       = TRUE,
  R           = 1000,
  halton      = NA
)

model_inter_results <- mxl_int %>% tidy() %>% head(15)
model_inter_results

# Define parameters
b <- mxl_int$coefficients[1:15] # Coefficients
vcv <- (-solve(mxl_int$hessian[1:15, 1:15])) # Variance-Covariance Matrix

# Compute marginals
k <- 5 # Number of base parameters
contrasts <- rbind(
  # Control: base only
  cbind(diag(k), 0 * diag(k), 0 * diag(k)),
  # Positive_Test: base + pos
  cbind(diag(k), diag(k),     0 * diag(k)),
  # Negative_Test: base + neg
  cbind(diag(k), 0 * diag(k), diag(k))
)

conts <- as.numeric(contrasts %*% b)
errs <- sqrt(diag(contrasts %*% vcv %*% t(contrasts))) # Standard errors
levels <- c(
  "Customers", "Competitors", "Valuation",
  "Electronics vs. Biotech", "Robotics vs. Biotech"
)
names <- rep(levels, times = 3)
message_type <- factor(
  rep(c("Control", "Positive_Test", "Negative_Test"), each = length(levels)),
  levels = c("Control", "Positive_Test", "Negative_Test")
)

df_contrasts <- data.frame(
  estimate  = conts,
  std.error = errs,
  term      = names,
  message_type = message_type,
  term_names = factor(names, levels = levels, labels = labels),
)


df_contrasts |> write_csv(here("Studies/Study4", "Data", "LogitResults.csv"))


# Into a dataframe

pvals_pos <- ifelse(model_inter_results$p.value < .001, "p < .001",
  glue("p = {sprintf('%.3f',model_inter_results$p.value)}")
)[6:10]	

pvals_neg <- ifelse(model_inter_results$p.value < .001, "p < .001",
  glue("p = {sprintf('%.3f',model_inter_results$p.value)}")
)[11:15]	

labels <- glue("{levels}\n(Inter_Pos: {pvals_pos})\n(Inter_Neg: {pvals_neg})")




# Plotting the results
ggplot(
  df_contrasts,
  aes(x = estimate, y = term, color = message_type, shape = message_type, fill = message_type)
) +
  geom_point(size = 3, position = position_dodge(width = -.35)) +
  geom_errorbarh(
    aes(xmin = estimate - 1.96 * std.error,
        xmax = estimate + 1.96 * std.error),
    height = 0, linewidth = 1,
    position = position_dodge(width = -.35)
  ) +
  labs(
    title = element_blank(),
    x     = "Standardized Marginal Effect",
    y     = element_blank(),
    color = element_blank(),
    shape = element_blank(),
    fill  = element_blank()
  ) +
  theme_matplotlib() +
  scale_x_continuous(breaks = seq(-36, 26, 4) / 10) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 11),
    legend.position = "top",
    panel.grid.major.y = element_blank(), # Remove major horizontal grid lines
    panel.grid.minor.y = element_blank(), # Remove minor horizontal grid lines
    legend.background = element_rect(
      fill = "white", # Background color
      color = "white", # Border color
      size = 0 # Border size
    )
  ) +
  scale_y_discrete(limits = rev) +
  scale_color_manual(values = c("darkgrey", "green4", "red4")) +
  scale_fill_manual(values = c("darkgrey", "green4", "red4")) +
  scale_shape_manual(values = c(21, 24, 25))
