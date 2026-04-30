library(dplyr)
library(readr)
library(tidyr)
library(mlogit)
library(stringr)
library(glue)
library(here)

i_am("Study1_DiagCueReplication/Code/S1D_DataAnalysis_MLogit.R")
source(here("custom_theme.R"))

# Load the data
choices_data <- read_csv(here("Studies/Appendix_DiagCue", "Data", "game_choices_clean_with_attributes.csv")) %>%
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
    Biotech = as.numeric(activity == "Biotechnology"),
    `Electronics vs. Biotech` = as.numeric(activity == "Electronics"),
    `Robotics vs. Biotech` = as.numeric(activity == "Robotics"),
    condid = condid / 2,
    Option_0 = as.numeric(Project == 0),
    Option_1 = as.numeric(Project == 1),
    Option_2 = as.numeric(Project == 2),
    Project_Chosen = as.numeric(Project_Chosen)
  ) %>%
  dplyr::arrange(turkid, Round, Project)

# Adjust Round and Slope
choices_data <- choices_data %>%
  mutate(
    Slope = condid,
    Round = Round - 1
  )

# Create interaction terms
interaction_columns <- c(
  "Option_1", "Option_2", "Valuation", "Customers", "Competitors",
  "Electronics vs. Biotech", "Robotics vs. Biotech"
)

for (c in interaction_columns) {
  choices_data <- choices_data %>%
    mutate(
      !!paste0("Slope on ", c) := !!sym(c) * Slope,
      !!paste0("Round on ", c) := !!sym(c) * Round,
      !!paste0("ThreeWay on ", c) := !!sym(c) * Round * Slope
    )
}

# Sort the final data
choices_data_logit <- choices_data %>%
  mutate(choice_occasion = glue("{turkid}_{Round}")) %>%
  arrange(choice_occasion, Project)

# Restrict to choices made during the second half of the game
df_mxl <- mlogit.data(
  choices_data_logit %>% filter(Round > 9) %>% as.data.frame(),
  choice   = "Project_Chosen",
  shape    = "long",
  alt.var  = "Project",
  chid.var = "choice_occasion",   # task / round
  id.var   = "turkid"      # respondent id  (panel indicator)
)

# Interaction model: Cues + Cues:Condition interaction.

mxl_int <- mlogit(
  Project_Chosen ~                  
    Customers + Competitors + Valuation +
    `Electronics` + `Robotics` +
    Slope_Customers + Slope_Competitors + Slope_Valuation + 
    Slope_Electronics + Slope_Robotics
  | 0,
  data        = df_mxl,
  rpar        = c(Customers = "n", Competitors = "n", Valuation = "n",
                  `Electronics` = "n", `Robotics` = "n",
                  Slope_Customers   = "n",  Slope_Competitors = "n", 
                  Slope_Valuation  = "n",
                  Slope_Electronics  = "n", 
                  Slope_Robotics = "n"),
  correlation = TRUE,
  panel       = TRUE,
  R           = 1000,
  halton      = NA
)

model_inter_results <- mxl_int %>% tidy() %>% head(10) %>% mutate(
  term = c("Customers", "Competitors", "Valuation",
           "Robotics vs. Biotech", "Electronics vs. Biotech",
           "Slope x Customers", "Slope x Competitors",
           "Slope x Valuation",
           "Slope x (Robo vs. Bio)",
           "Slope x (Elec vs. Bio"))
model_inter_results

# Plotting the marginal effects:
## Define the contrasts matrix
contrasts <- do.call(rbind, lapply(c(-1, 1), function(i) {
  cbind(diag(5), diag(5) * i) # Combine diagonal matrices for each i
}))

## Define parameters
b <- mxl_int$coefficients[1:10] # Coefficients
vcv <- (-solve(mxl_int$hessian[1:10, 1:10])) # Variance-Covariance Matrix

## Compute marginals
conts <- t(b %*% t(contrasts)) # Effects
errs <- sqrt(diag(contrasts %*% vcv %*% t(contrasts))) # Standard errors

## Into a dataframe
names <- rep(
  c("Customers", "Competitors", "Valuation", 
    "Robotics vs. Biotech", "Electronics vs. Biotech"), 
  2)
slope <- rep(c(-1, 1), each = 5)
levels <- c("Customers", "Competitors", "Valuation", 
            "Robotics vs. Biotech", "Electronics vs. Biotech")
pvals <- tail(ifelse(model_inter_results$p.value < .001, "p < .001", 
                     glue("p = {sprintf('%.3f',model_inter_results$p.value)}")), 5)
labels <- glue("{levels}\n(Inter: {pvals})")
df_contrasts <- data.frame(
  estimate = conts, std.error = errs,
  term = factor(names,
                levels = levels,
                labels = labels
  ),
  slope = factor(slope, 
                 levels = c(1, -1), 
                 labels = c("Increasing", "Decreasing")
  )
)

## Plotting the results
ggplot(df_contrasts, 
       aes(x = estimate, y = term, color = slope, shape = slope, fill = slope)
) +
  geom_point(size = 3, position = position_dodge(width = -.35)) +
  geom_errorbarh(
    aes(
      xmin = estimate - std.error * 1.96, 
      xmax = estimate + std.error * 1.96),
    height = 0, size = 1,
    position = position_dodge(width = -.35)
  ) +
  labs(
    title = element_blank(),
    x = "Standardized Marginal Effect",
    y = element_blank(),
    color = element_blank(),
    shape = element_blank(),
    fill = element_blank()
  ) +
  theme_matplotlib() +
  scale_x_continuous(breaks = seq(-16, 12, 4) / 10) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_discrete(limits = rev) +
  scale_color_manual(values = rev(c("#b63441", "#3f69c9"))) +
  scale_fill_manual(values = rev(c("#b63441", "#3f69c9"))) +
  scale_shape_manual(values = rev(c(25, 24)))