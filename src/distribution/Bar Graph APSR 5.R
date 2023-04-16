# Bar plot, horiz, grouped

# Hertel-Fernandez, A., Mildenberger, M., & Stokes, L. C. (2019).
#   Legislative staff and representation in
#   congress. The American Political Science Review, 113(1), 5.
#   doi:10.1017/S0003055418000606

# Data: USA 2020 (Protected)

library(ggplot2)
library(dplyr)
library(gtools)
library(ggpubr)
library(magrittr)

load("data/protected/usa-2020-cleaned.Rda")
output_dir <- "output"

df <- df %>%
  group_by(pride, except) %>%
  summarise(
    mean = mean(hawk, na.rm = T),
    se = sd(hawk, na.rm = T)/sqrt(n())
  )

df %<>% na.omit()

ggplot(df,aes(factor(except), mean, fill = factor(pride))) +
  geom_bar(stat = "identity", position = "dodge")+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position = position_dodge(.9), width = 0.2) +
  scale_fill_brewer(
    name = "Legend",
    palette = "Set1",
    labels = c("Proud to be American", "Not Proud to be American")
  ) +
  labs(
    y = "Preference",
    x = "Group"
  ) +
  scale_x_discrete(
    labels = c("Control", "Peace-loving", "Hardworking", "Unique Qualities")
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) + coord_flip()

# Save individual Panels
ggsave(path=output_dir, filename="Bar Graph APSR 5.png", width=8, height=6)

# Example
# ggsave(path="src/distribution", filename="Bar Graph APSR 5-example.png", width=8, height=6)
