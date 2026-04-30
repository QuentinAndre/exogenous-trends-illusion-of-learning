library(tidyverse)
library(here)

i_am("Simulation/Simulation_Analysis.R")
source(here("custom_theme.R"))

# Reading and visualizing the payoffs
df_payoffs <- read_csv(here("Simulation", "Simulation_Payoffs_Main.csv"))

ggplot(df_payoffs, aes(x = Round, y = Payoff, fill = Trend, color=Trend, shape=Trend)) +
  geom_smooth(method = "lm", se = FALSE, linetype="dashed", show.legend = FALSE) +
  geom_point(size=3) +
  scale_x_continuous(breaks = 1:20) +
  scale_shape_manual(values = c(25, 21, 24)) +
  labs(y = "Reward") +
  scale_color_manual(values = c("#b63441", "#1f1e1e", "#3f69c9")) + 
  scale_fill_manual(values = c("#b63441", "#1f1e1e", "#3f69c9")) + 
  theme_matplotlib()

ggsave(here("Figures", "Figure 1.png"), dpi = 400, width = 12, height = 4, units = "in")


df <- read_csv(here("Simulation", "Simulation_Results_Main.csv"))

densities <- df %>% 
  group_split(Choice_Strategy, Trend) %>% 
  lapply(function(df) {
    dens <- density(df$Strength_Beliefs, bw=.2)  # Compute density for each group
    data.frame(x = dens$x, y = dens$y, 
               Choice_Strategy = unique(df$Choice_Strategy),
               Trend = unique(df$Trend))  # Convert to data frame
  }) %>% 
  bind_rows() %>% 
  mutate(Choice_Strategy = factor(Choice_Strategy, levels=c("100% Positive Test", "Mixed", "100% Random"))) 

annotation_df <- data.frame(
  x = c(.5, .8, 2.15),
  y = c(1.75, 1.4, 1.15), 
  Trend = c("Decreasing", "Flat", "Increasing"),
  Choice_Strategy = factor(c("100% Positive Test", "100% Positive Test","100% Positive Test"),
                           levels = c("100% Positive Test", "Mixed", "100% Random"))
)

densities %>% ggplot(aes(x=x, y=y, color=Trend, linetype = Trend)) + 
  geom_line(linewidth=.8) + 
  scale_color_manual(values = c("#b63441", "#1f1e1e", "#3f69c9")) + 
  facet_wrap(Choice_Strategy~.) + 
  theme_matplotlib() +
  scale_y_continuous() +
  scale_linetype_manual(values=c("dotdash", "dashed", "solid")) +
  labs(x="Strength of Beliefs", y="Density") +
  geom_text(data = annotation_df, aes(x=x, y=y, label = Trend, color=Trend), inherit.aes = FALSE,
            hjust=0) +
  guides(color="none", linetype="none")

ggsave(here("Figures", "Figure 2.png"), dpi = 400, width = 12, height = 4, units = "in")
