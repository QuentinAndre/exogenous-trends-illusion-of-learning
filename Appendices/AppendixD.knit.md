---
title: "Appendix D - Replication of Study 1 with a Diagnostic Cue"
format:
  html:
    code-fold: true
    code-summary: "Show Code"
  pdf:
    toc: false
    number-sections: false

execute:
  warning: false
  message: false
  error: false
  fig-width: 10
  fig-height: 4
  fig-dpi: 400
knitr:
  opts_chunk:
    collapse: true
---






We conducted a conceptual replication of Study 1 in which one of the cues reliably predicted participants' outcomes. This design tested whether exogenous trends can still foster illusory beliefs about non-diagnostic cues when participants have the opportunity to identify a genuinely predictive cue.

Participants played the same investment simulation as in Study 1, with two modifications. First, one of the three continuous company attributes (Company Valuation, Market Size, or Number of Competitors) was randomly selected to influence returns, while the other two continuous attributes and the categorical attribute (Sector of Activity) remained non-diagnostic. Second, we included only the Increasing and Decreasing conditions—the Flat condition was dropped to ensure a matched signal-to-noise ratio for detecting the diagnostic cue across conditions.

After 20 rounds, participants completed a post-game survey measuring (1) their subjective sense of learning—using the same three items as Study 1 (α = .94)—and (2) their endorsement of cue-outcome relationships involving the two non-diagnostic continuous attributes. Following the pre-registered interim analysis plan, data were collected in two waves of 400 participants each. The study was pre-registered on AsPredicted.org ([#db3p-fnz6](https://aspredicted.org/db3p-fnz6)).

## Preamble: Data Loadings and Wrangling






::: {.cell}

```{.r .cell-code}
library(readr)
library(tidyr)
library(mlogit)
library(stringr)
library(glue)
library(gt)
library(gtsummary)
library(tidyverse)
library(tidymodels)
library(broom)
library(here)

i_am("Appendices/AppendixD.qmd")
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
        "t({df}) = {round(t, 3)}, ",
        "{ifelse(pval < .001, 'p < .001',
      glue('p = {round(pval, 3)}'))}"
    )
}

# Load survey data
survey_data <- read_csv(
    here("Studies/Appendix_DiagCue", "Data", "survey_clean.csv")
)
game_data <- read_csv(
    here(
        "Studies/Appendix_DiagCue", "Data",
        "game_choices_clean.csv"
    )
)
runtime <- game_data %>%
    distinct(turkid, .keep_all = TRUE) %>%
    dplyr::select(c(turkid, time_to_completion))
survey_data_all <- survey_data %>%
    left_join(runtime, by = join_by(turkid)) %>%
    dplyr::filter(time_to_completion > 90)
```
:::






## Sample






::: {.cell}

```{.r .cell-code}
n_excluded <- nrow(survey_data) - nrow(survey_data_all)
n_total <- nrow(survey_data_all)
n_per <- survey_data_all %>% count(condition)
```
:::






After excluding 15 participants who completed the cue-learning task in under 90 seconds, the final sample consisted of 785 participants (393 in the Increasing condition, 392 in the Decreasing condition).

## Pre-Registered Dependent Variables

### Subjective Learning






::: {.cell}

```{.r .cell-code}
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
```

::: {.cell-output-display}
![](AppendixD_files/figure-pdf/unnamed-chunk-3-1.pdf){fig-pos='H'}
:::
:::






### Confidence in False Statements






::: {.cell}

```{.r .cell-code}
model <- lm(confidence ~ condition, data = survey_data_all)
survey_data_all %>%
    ggplot(aes(
        x = factor(condition), y = confidence,
        color = condition
    )) +
    stat_summary(
        fun = mean, geom = "point", alpha = 1,
        position = position_dodge(width = 0.5), size = 4
    ) +
    stat_summary(
        fun.data = mean_cl_boot, geom = "errorbar", width = 0,
        position = position_dodge(width = 0.5),
        show.legend = FALSE, linewidth = 1.2
    ) +
    labs(
        x = element_blank(),
        y = "Confidence in\nFalse Statements (1-5)",
        title = generate_title(model)
    ) +
    theme_matplotlib() +
    scale_color_manual(values = c("#b63441", "#3f69c9")) +
    guides(color = "none")
```

::: {.cell-output-display}
![](AppendixD_files/figure-pdf/unnamed-chunk-4-1.pdf){fig-pos='H'}
:::
:::






## Model: Cue by Condition Interactions






::: {.cell}

```{.r .cell-code}
# Load the choice data for the multinomial logit model
choices_data <- read_csv(
    here(
        "Studies/Appendix_DiagCue", "Data",
        "game_choices_clean_with_attributes.csv"
    )
) %>%
    arrange(condition) %>%
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
        `Electronics` = as.numeric(activity == "Electronics"),
        `Robotics` = as.numeric(activity == "Robotics"),
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
    "Electronics", "Robotics"
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
    mutate(choice_occasion = glue("{turkid}_{Round}")) %>%
    arrange(choice_occasion, Project)

# Restrict to choices made during the second half of the game
df_mxl <- mlogit.data(
    choices_data_logit %>% filter(Round > 9) %>% as.data.frame(),
    choice   = "Project_Chosen",
    shape    = "long",
    alt.var  = "Project",
    chid.var = "choice_occasion", # task / round
    id.var   = "turkid" # respondent id  (panel indicator)
)
```
:::






### Coefficients






::: {.cell}

```{.r .cell-code}
# Interaction model: Cues + Cues:Condition interaction.
# Cache the full model object to avoid re-running the slow mlogit estimation.

cache_path <- here("Studies/Appendix_DiagCue", "Data", "mxl_int_model.rds")

if (file.exists(cache_path)) {
    mxl_int <- readRDS(cache_path)
} else {
    mxl_int <- mlogit(
        Project_Chosen ~
            Customers + Competitors + Valuation +
                `Electronics` + `Robotics` +
                Slope_Customers + Slope_Competitors + Slope_Valuation +
                Slope_Electronics + Slope_Robotics |
                0,
        data = df_mxl,
        rpar = c(
            Customers = "n", Competitors = "n", Valuation = "n",
            `Electronics` = "n", `Robotics` = "n",
            Slope_Customers = "n", Slope_Competitors = "n",
            Slope_Valuation = "n",
            Slope_Electronics = "n",
            Slope_Robotics = "n"
        ),
        correlation = TRUE,
        panel = TRUE,
        R = 1000,
        halton = NA
    )
    saveRDS(mxl_int, cache_path)
}

model_inter_results <- mxl_int %>%
    tidy() %>%
    head(10) %>%
    mutate(
        term = c(
            "Customers", "Competitors", "Valuation",
            "Robotics vs. Biotech", "Electronics vs. Biotech",
            "Slope x Customers", "Slope x Competitors",
            "Slope x Valuation",
            "Slope x (Robo vs. Bio)",
            "Slope x (Elec vs. Bio"
        )
    )
model_inter_results %>%
    gt(rowname_col = "term") %>%
    fmt_number(columns = c(estimate, std.error, statistic)) %>%
    fmt(columns = p.value, fns = style_pvalue) %>%
    tab_row_group(label = "Interaction Effects", rows = 6:10) %>%
    tab_row_group(label = "Main Effects", rows = 1:5) %>%
    cols_label(
        term = "Coeff.", estimate = "Estimate", std.error = "Std. Err.",
        statistic = "t-stats", p.value = "p-value"
    ) %>%
    as_raw_html()
```

::: {.cell-output-display}

```{=html}
<div id="ltfumuvxrb" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  
  <table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false" style="-webkit-font-smoothing: antialiased; -moz-osx-font-smoothing: grayscale; font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji'; display: table; border-collapse: collapse; line-height: normal; margin-left: auto; margin-right: auto; color: #333333; font-size: 16px; font-weight: normal; font-style: normal; background-color: #FFFFFF; width: auto; border-top-style: solid; border-top-width: 2px; border-top-color: #A8A8A8; border-right-style: none; border-right-width: 2px; border-right-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #A8A8A8; border-left-style: none; border-left-width: 2px; border-left-color: #D3D3D3;" bgcolor="#FFFFFF">
  <thead style="border-style: none;">
    <tr class="gt_col_headings" style="border-style: none; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3;">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="a::stub" style="border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: left;" bgcolor="#FFFFFF" valign="bottom" align="left"></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="estimate" style="border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" bgcolor="#FFFFFF" valign="bottom" align="right">Estimate</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="std.error" style="border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" bgcolor="#FFFFFF" valign="bottom" align="right">Std. Err.</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="statistic" style="border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" bgcolor="#FFFFFF" valign="bottom" align="right">t-stats</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="p.value" style="border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" bgcolor="#FFFFFF" valign="bottom" align="right">p-value</th>
    </tr>
  </thead>
  <tbody class="gt_table_body" style="border-style: none; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3;">
    <tr class="gt_group_heading_row" style="border-style: none;">
      <th colspan="5" class="gt_group_heading" scope="colgroup" id="Main Effects" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: initial; text-transform: inherit; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; text-align: left;" bgcolor="#FFFFFF" valign="middle" align="left">Main Effects</th>
    </tr>
    <tr class="gt_row_group_first" style="border-style: none;"><th id="stub_1_1" scope="row" class="gt_row gt_left gt_stub" style="border-style: none; padding-top: 8px; padding-bottom: 8px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: initial; text-transform: inherit; border-right-style: solid; border-right-width: 2px; border-right-color: #D3D3D3; padding-left: 5px; padding-right: 5px; text-align: left; border-top-width: 2px;" valign="middle" bgcolor="#FFFFFF" align="left">Customers</th>
<td headers="Main Effects stub_1_1 estimate" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;" valign="middle" align="right">0.57</td>
<td headers="Main Effects stub_1_1 std.error" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;" valign="middle" align="right">0.04</td>
<td headers="Main Effects stub_1_1 statistic" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;" valign="middle" align="right">16.07</td>
<td headers="Main Effects stub_1_1 p.value" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;" valign="middle" align="right"><0.001</td></tr>
    <tr style="border-style: none;"><th id="stub_1_2" scope="row" class="gt_row gt_left gt_stub" style="border-style: none; padding-top: 8px; padding-bottom: 8px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: initial; text-transform: inherit; border-right-style: solid; border-right-width: 2px; border-right-color: #D3D3D3; padding-left: 5px; padding-right: 5px; text-align: left;" valign="middle" bgcolor="#FFFFFF" align="left">Competitors</th>
<td headers="Main Effects stub_1_2 estimate" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">−0.75</td>
<td headers="Main Effects stub_1_2 std.error" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">0.03</td>
<td headers="Main Effects stub_1_2 statistic" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">−21.77</td>
<td headers="Main Effects stub_1_2 p.value" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right"><0.001</td></tr>
    <tr style="border-style: none;"><th id="stub_1_3" scope="row" class="gt_row gt_left gt_stub" style="border-style: none; padding-top: 8px; padding-bottom: 8px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: initial; text-transform: inherit; border-right-style: solid; border-right-width: 2px; border-right-color: #D3D3D3; padding-left: 5px; padding-right: 5px; text-align: left;" valign="middle" bgcolor="#FFFFFF" align="left">Valuation</th>
<td headers="Main Effects stub_1_3 estimate" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">0.66</td>
<td headers="Main Effects stub_1_3 std.error" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">0.03</td>
<td headers="Main Effects stub_1_3 statistic" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">22.67</td>
<td headers="Main Effects stub_1_3 p.value" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right"><0.001</td></tr>
    <tr style="border-style: none;"><th id="stub_1_4" scope="row" class="gt_row gt_left gt_stub" style="border-style: none; padding-top: 8px; padding-bottom: 8px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: initial; text-transform: inherit; border-right-style: solid; border-right-width: 2px; border-right-color: #D3D3D3; padding-left: 5px; padding-right: 5px; text-align: left;" valign="middle" bgcolor="#FFFFFF" align="left">Robotics vs. Biotech</th>
<td headers="Main Effects stub_1_4 estimate" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">−0.23</td>
<td headers="Main Effects stub_1_4 std.error" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">0.06</td>
<td headers="Main Effects stub_1_4 statistic" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">−4.20</td>
<td headers="Main Effects stub_1_4 p.value" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right"><0.001</td></tr>
    <tr style="border-style: none;"><th id="stub_1_5" scope="row" class="gt_row gt_left gt_stub" style="border-style: none; padding-top: 8px; padding-bottom: 8px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: initial; text-transform: inherit; border-right-style: solid; border-right-width: 2px; border-right-color: #D3D3D3; padding-left: 5px; padding-right: 5px; text-align: left;" valign="middle" bgcolor="#FFFFFF" align="left">Electronics vs. Biotech</th>
<td headers="Main Effects stub_1_5 estimate" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">0.00</td>
<td headers="Main Effects stub_1_5 std.error" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">0.06</td>
<td headers="Main Effects stub_1_5 statistic" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">−0.05</td>
<td headers="Main Effects stub_1_5 p.value" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">>0.9</td></tr>
    <tr class="gt_group_heading_row" style="border-style: none;">
      <th colspan="5" class="gt_group_heading" scope="colgroup" id="Interaction Effects" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: initial; text-transform: inherit; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; text-align: left;" bgcolor="#FFFFFF" valign="middle" align="left">Interaction Effects</th>
    </tr>
    <tr class="gt_row_group_first" style="border-style: none;"><th id="stub_1_6" scope="row" class="gt_row gt_left gt_stub" style="border-style: none; padding-top: 8px; padding-bottom: 8px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: initial; text-transform: inherit; border-right-style: solid; border-right-width: 2px; border-right-color: #D3D3D3; padding-left: 5px; padding-right: 5px; text-align: left; border-top-width: 2px;" valign="middle" bgcolor="#FFFFFF" align="left">Slope x Customers</th>
<td headers="Interaction Effects stub_1_6 estimate" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;" valign="middle" align="right">0.08</td>
<td headers="Interaction Effects stub_1_6 std.error" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;" valign="middle" align="right">0.03</td>
<td headers="Interaction Effects stub_1_6 statistic" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;" valign="middle" align="right">2.36</td>
<td headers="Interaction Effects stub_1_6 p.value" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;" valign="middle" align="right">0.018</td></tr>
    <tr style="border-style: none;"><th id="stub_1_7" scope="row" class="gt_row gt_left gt_stub" style="border-style: none; padding-top: 8px; padding-bottom: 8px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: initial; text-transform: inherit; border-right-style: solid; border-right-width: 2px; border-right-color: #D3D3D3; padding-left: 5px; padding-right: 5px; text-align: left;" valign="middle" bgcolor="#FFFFFF" align="left">Slope x Competitors</th>
<td headers="Interaction Effects stub_1_7 estimate" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">−0.21</td>
<td headers="Interaction Effects stub_1_7 std.error" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">0.03</td>
<td headers="Interaction Effects stub_1_7 statistic" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">−6.60</td>
<td headers="Interaction Effects stub_1_7 p.value" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right"><0.001</td></tr>
    <tr style="border-style: none;"><th id="stub_1_8" scope="row" class="gt_row gt_left gt_stub" style="border-style: none; padding-top: 8px; padding-bottom: 8px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: initial; text-transform: inherit; border-right-style: solid; border-right-width: 2px; border-right-color: #D3D3D3; padding-left: 5px; padding-right: 5px; text-align: left;" valign="middle" bgcolor="#FFFFFF" align="left">Slope x Valuation</th>
<td headers="Interaction Effects stub_1_8 estimate" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">0.15</td>
<td headers="Interaction Effects stub_1_8 std.error" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">0.03</td>
<td headers="Interaction Effects stub_1_8 statistic" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">5.30</td>
<td headers="Interaction Effects stub_1_8 p.value" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right"><0.001</td></tr>
    <tr style="border-style: none;"><th id="stub_1_9" scope="row" class="gt_row gt_left gt_stub" style="border-style: none; padding-top: 8px; padding-bottom: 8px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: initial; text-transform: inherit; border-right-style: solid; border-right-width: 2px; border-right-color: #D3D3D3; padding-left: 5px; padding-right: 5px; text-align: left;" valign="middle" bgcolor="#FFFFFF" align="left">Slope x (Robo vs. Bio)</th>
<td headers="Interaction Effects stub_1_9 estimate" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">−0.11</td>
<td headers="Interaction Effects stub_1_9 std.error" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">0.06</td>
<td headers="Interaction Effects stub_1_9 statistic" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">−1.99</td>
<td headers="Interaction Effects stub_1_9 p.value" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">0.046</td></tr>
    <tr style="border-style: none;"><th id="stub_1_10" scope="row" class="gt_row gt_left gt_stub" style="border-style: none; padding-top: 8px; padding-bottom: 8px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: initial; text-transform: inherit; border-right-style: solid; border-right-width: 2px; border-right-color: #D3D3D3; padding-left: 5px; padding-right: 5px; text-align: left;" valign="middle" bgcolor="#FFFFFF" align="left">Slope x (Elec vs. Bio</th>
<td headers="Interaction Effects stub_1_10 estimate" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">−0.09</td>
<td headers="Interaction Effects stub_1_10 std.error" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">0.06</td>
<td headers="Interaction Effects stub_1_10 statistic" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">−1.64</td>
<td headers="Interaction Effects stub_1_10 p.value" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">0.10</td></tr>
  </tbody>
  
</table>
</div>
```

:::
:::








### Plotting the Marginal Effects







```{.r .cell-code}
# Plotting the marginal effects:
## Define the contrasts matrix
contrasts <- do.call(rbind, lapply(c(-1, 1), function(i) {
    cbind(diag(5), diag(5) * i) # Combine diagonal matrices for each i
}))

## Define parameters
b <- mxl_int$coefficients[1:10] # Coefficients
vcv <- (-solve(mxl_int$hessian)[1:10, 1:10]) # Variance-Covariance Matrix

## Compute marginals
conts <- t(b %*% t(contrasts)) # Effects
errs <- sqrt(diag(contrasts %*% vcv %*% t(contrasts))) # Standard errors

## Into a dataframe
names <- rep(
    c(
        "Customers", "Competitors", "Valuation",
        "Robotics vs. Biotech", "Electronics vs. Biotech"
    ),
    2
)
slope <- rep(c(-1, 1), each = 5)
levels <- c(
    "Customers", "Competitors", "Valuation",
    "Robotics vs. Biotech", "Electronics vs. Biotech"
)
pvals <- tail(ifelse(model_inter_results$p.value < .001, "p < .001",
    glue("p = {sprintf('%.3f',model_inter_results$p.value)}")
), 5)
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
ggplot(
    df_contrasts,
    aes(x = estimate, y = term, color = slope, shape = slope, fill = slope)
) +
    geom_point(size = 3, position = position_dodge(width = -.35)) +
    geom_errorbarh(
        aes(
            xmin = estimate - std.error * 1.96,
            xmax = estimate + std.error * 1.96
        ),
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
```

![](AppendixD_files/figure-pdf/unnamed-chunk-7-1.pdf){fig-pos='H'}






## Summary

Replicating the pattern observed in Study 1, participants who experienced an increasing sequence of outcomes reported a significantly greater subjective sense of learning than those in the Decreasing condition. Critically, participants in the Increasing condition also expressed stronger endorsement of false statements about the non-diagnostic cues, confirming that exogenous trends can foster illusory beliefs even when one cue is genuinely predictive of outcomes. The analysis of participants' choices in the second half of the game further reveals that those in the Increasing condition attributed a greater weight to the cue-outcome relationships than those in the Decreasing condition—consistent with the behavioral signature of the positive test strategy documented in Study 1.
