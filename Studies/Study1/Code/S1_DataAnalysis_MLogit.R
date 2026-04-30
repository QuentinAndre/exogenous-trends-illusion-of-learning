library(dplyr)
library(readr)
library(tidyr)
library(janitor)
library(scales)
library(mlogit)
library(stringr)
library(glue)
library(marginaleffects)
library(tidymodels)
library(here)

i_am("Studies/Study1/Code/S1_DataAnalysis_MLogit.R")
source(here("custom_theme.R"))

# Load the data
choices_data <- read_csv(here("Studies/Study1", "Data", "game_choices_clean_with_attributes.csv")) %>%
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

# Standardize Valuation, Customers, and Competitors
choices_data <- choices_data %>%
  mutate(
    Valuation = as.numeric(scale(Valuation)),
    Customers = as.numeric(scale(Customers)),
    Competitors = as.numeric(scale(Competitors))
  )

# Add dummy variables for activity and Condition
choices_data <- choices_data %>%
  mutate(
    AI = as.numeric(activity == "Artificial Intelligence"),
    `Biotech` = as.numeric(activity == "Biotechnology"),
    `Electronics` = as.numeric(activity == "Electronics"),
    `Robotics` = as.numeric(activity == "Robotics"),
    Flat = as.numeric(Condition == "Flat"),
    SDec = as.numeric(Condition == "Strongly Decreasing"),
    SInc = as.numeric(Condition == "Strongly Increasing"),
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
  "Biotech", "Electronics", "Robotics"
)

for (c in interaction_columns) {
  choices_data <- choices_data %>%
    mutate(
      !!paste0("Slope_", c) := !!sym(c) * Slope,
      !!paste0("Round_", c) := !!sym(c) * Round,
      !!paste0("ThreeWay_", c) := !!sym(c) * Round * Slope
    )
}

# Sort the final data
choices_data_logit <- choices_data %>%
  mutate(
    choice_occasion = glue("{turkid}_{Round}"),
    turkid = as.integer(factor(turkid))
  ) %>%
  arrange(turkid, Round, Project)


df_mxl <- mlogit.data(
  choices_data_logit %>% filter(Round > 9) %>% as.data.frame(),
  choice   = "Project_Chosen",
  shape    = "long",
  alt.var  = "Project",
  chid.var = "choice_occasion", # task / round
  id.var   = "turkid" # respondent id  (panel indicator)
)


# Model:
mxl_int <- mlogit(
  Project_Chosen ~
    Customers + Competitors + Valuation +
      `Biotech` + `Electronics` + `Robotics` +
      Slope_Customers + Slope_Competitors + Slope_Valuation +
      Slope_Biotech + Slope_Electronics + Slope_Robotics |
      0,
  data = df_mxl,
  rpar = c(
    Customers = "n", Competitors = "n", Valuation = "n",
    `Biotech` = "n", `Electronics` = "n", `Robotics` = "n",
    Slope_Customers = "n", Slope_Competitors = "n",
    Slope_Valuation = "n",
    Slope_Biotech = "n", Slope_Electronics = "n",
    Slope_Robotics = "n"
  ),
  correlation = TRUE,
  panel = TRUE,
  R = 1000,
  halton = NA
)

model_inter_results <- mxl_int %>%
  tidy() %>%
  head(12)
model_inter_results

# Define the contrasts matrix
contrasts <- do.call(rbind, lapply(c(-2, 0, 2), function(i) {
  cbind(diag(6), diag(6) * i) # Combine diagonal matrices for each i
}))

# Define parameters
b <- mxl_int$coefficients[1:12] # Coefficients
vcv <- (-solve(mxl_int$hessian)[1:12, 1:12]) # Variance-Covariance Matrix

# Compute marginals
conts <- t(b %*% t(contrasts)) # Effects
errs <- sqrt(diag(contrasts %*% vcv %*% t(contrasts))) # Standard errors

# Into a dataframe
names <- rep(c(
  "Customers", "Competitors", "Valuation", "Biotech vs. AI",
  "Electronics vs. AI", "Robotics vs. AI"
), 3)
slope <- rep(c(-2, 0, 2), each = 6)
levels <- c(
  "Customers", "Competitors", "Valuation", "Biotech vs. AI",
  "Electronics vs. AI", "Robotics vs. AI"
)
pvals <- tail(ifelse(model_inter_results$p.value < .001, "p < .001",
  glue("p = {sprintf('%.3f',model_inter_results$p.value)}")
), 6)
labels <- glue("{levels}\n(Inter: {pvals})")
df_contrasts <- data.frame(
  estimate = conts, std.error = errs,
  term = factor(names,
    levels = levels,
    labels = labels
  ),
  slope = factor(slope, levels = c(2, 0, -2), labels = c(
    "Increasing",
    "Flat", "Decreasing"
  ))
)

# Plotting the results
ggplot(df_contrasts, aes(x = estimate, y = term, color = slope, shape = slope, fill = slope)) +
  geom_point(size = 3, position = position_dodge(width = -.35)) + # Points for coefficients
  geom_errorbarh(aes(xmin = estimate - std.error * 1.96, xmax = estimate + std.error * 1.96),
    height = 0, linewidth = 1,
    position = position_dodge(width = -.35)
  ) + # Error bars
  labs(
    title = element_blank(),
    x = "Standardized Marginal Effect",
    y = element_blank(),
    color = element_blank(),
    shape = element_blank(),
    fill = element_blank()
  ) +
  theme_matplotlib() +
  scale_x_continuous(breaks = seq(-14, 12, 4) / 10) +
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
  scale_color_manual(values = rev(c("#b63441", "#1f1e1e", "#3f69c9"))) +
  scale_fill_manual(values = rev(c("#b63441", "#1f1e1e", "#3f69c9"))) +
  scale_shape_manual(values = rev(c(25, 21, 24)))

