# Bar plot, grouped

# Alesina, A., Stantcheva, S., & Teso, E. (2018).
#   Intergenerational mobility and preferences for redistribution.
#   The American Economic Review, 108(2), 537,

# Data: Quek (2017) Rationalist Experiments on War, Table 4

library(haven)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(gtools)
library(ggpubr)

master <- read_dta("data/quek-rw-2017.dta")
output_dir <- "output"

df <- master %>%
  filter(period > 10) %>%
  group_by(session, enforce11) %>%
  summarise(
    mean = mean(war),
    se = sd(war)/sqrt(n()),
    n = n()
  )

df$session <- paste(df$session)

df_overall <- master %>%
  filter(period > 10) %>%
  group_by(enforce11) %>%
  summarise(
    session = "overall",
    mean = mean(war),
    se = sd(war)/sqrt(n()),
    n = n()
  )
df_overall <- df_overall[, c("session","enforce11","mean","se","n")]


ggplot(pdf,aes(x=session, y=mean, fill = ifelse(session == "overall", paste(session,enforce11), paste(enforce11)))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position = position_dodge(.9), width = 0.2) +
  scale_y_continuous(labels = scales::percent, lim = c(0,1)) +
  scale_fill_manual(
    name = "Legend",
    values = c('lightblue','pink', 'black', 'grey'),
    labels = c("", "", "No Enforcement", "Enforcement"),
    guide  = guide_legend(nrow = 2)
    ) +
  labs(
    y = "Incidence of War",
    x = "Session"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.spacing.x = unit(0.1,"cm")
  )

# Save individual Panels
ggsave(path=output_dir, filename="Bar Graph AER 3.png", width=8, height=6)

# Example
# ggsave(path="src/distribution", filename="Bar Graph AER 3-example.png", width=8, height=6)
