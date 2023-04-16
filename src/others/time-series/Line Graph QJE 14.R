# Multiple time series, multiple panels

# Scott R. Baker, Nicholas Bloom, and Steven J. Davis,
#   “Measuring Economic Policy Uncertainty,”
#   The Quarterly Journal of Economics 131, no. 4 (2016): 1601

# Data: ibid Figure II

library(ggplot2)
library(dplyr)
library(magrittr)
library(ggsignif)
output_dir="output"

# Plot 1
df <- read.csv("data/baker-2016.csv")

df$x <- as.Date(df$d, format = "%d/%m/%Y")

df <- df %>% filter(x < as.Date("2016-01-01"))

ggplot(df) +
  geom_line(aes(x=x, y=health, color = '1', linetype = '1')) +
  geom_line(aes(x=x, y=security, color = '2', linetype = '2')) +
  geom_text(aes(x=as.Date("1992-06-01"), y=650), label = "Gulf\nWar I") +
  geom_text(aes(x=as.Date("1995-01-01"), y=430), label = "Clinton healthcare\nreform effort") +
  geom_text(aes(x=as.Date("2001-01-01"), y=560), label = "9/11") +
  geom_text(aes(x=as.Date("2003-01-01"), y=620), label = "Gulf War II") +
  scale_color_manual(
    name = "Legend",
    values = c(
      '1' = 'red',
      '2' = 'blue'
    ),
    labels = c("Healthcare Policy Uncertainty", "National Security Policy Uncertainty")
  ) +
  scale_linetype_manual(
    name = "Legend",
    values = c(
      '1' = 2,
      '2' = 1
    ),
    labels = c("Healthcare Policy Uncertainty", "National Security Policy Uncertainty")
  ) +
  labs(
    y = "Index Value"
  )+
  theme_bw() +
  theme(
    legend.position = c(.5,.95),
    legend.title =  element_blank(),
    legend.background = element_rect(fill = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size=0.5),
    panel.border=element_blank(),
    axis.title.x = element_blank()
  ) +
  geom_bracket (
              y.position = 600,
              xmin = as.Date("2008-06-01"),
              xmax = as.Date("2015-01-01"),
              label = "Affordable\ncare act",
              tip.length = 0.01
              )

# Save individual Panels
ggsave(path=output_dir, filename="Line Graph QJE 14.png", width=8, height=6)


# Save as Example
# ggsave(path="src/others/time-series", filename="Line Graph QJE 14-example.png", width=8, height=6)
