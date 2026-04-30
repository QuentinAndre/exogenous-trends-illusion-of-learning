library(ggplot2)
theme_matplotlib <- function(base_size = 12, base_family = "") {
  theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      axis.line = element_line(color = "black", linewidth =  0.8),  # Darker, thicker axis lines
      axis.ticks = element_line(color = "black", linewidth = 0.8),  # Thicker ticks
      axis.ticks.length = unit(5, "pt"),  # Slightly longer tick marks
      axis.text = element_text(color = "black", size = base_size),  # Dark axis labels
      axis.title = element_text(face = "bold", size = base_size + 2),  # Emphasized axis labels
      legend.background = element_blank(),  # No legend background
      legend.key = element_blank(),  # No box around legend keys
      panel.background = element_blank(),
      strip.background = element_blank(), # Ensures a clean white background,
      strip.text = element_text(size = 10, face = "bold", hjust = 0.5) 
    )
}
