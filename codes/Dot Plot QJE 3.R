# Dot Plot, horizontal, control = coloured

# Isaiah Andrews, Matthew Gentzkow, and Jesse M. Shapiro,
#   “Measuring the Sensitivity of Parameter Estimates to Estimation Moments,”
#   The Quarterly Journal of Economics 132, no. 4 (2017): 1573,
#   DOI:10.1093/qje/qjx023.

# Data: USA 2020 (protected)

library(ggplot2)
library(dplyr)
df <- read.csv("data/protected/usa-2020.csv")
output_dir="output"

# Clean data
df <- master %>%
  filter(spend<11) %>%
  group_by(spend) %>%
  summarize(
    bell_mean = mean(bell, na.rm = TRUE),
    bell_se = sd(bell, na.rm=TRUE)/sqrt(n()),
  ) %>%
  rename(
    group = spend,
    pt = bell_mean,
    error = bell_se
  )

df$label =
  c("Control Group",
    "China's Defence Improves and\nOffence Remains Same",
    "China Spends More Due to Others",
    "War Is Costly Due to Nuclear Weapons",
    "War Is Costly Due to Dependent Economies"
  )

# Plot
ggplot(df, aes(factor(group), pt)) +
  geom_errorbar(aes(ymax = pt + error, ymin = pt - error), width = 0.2, color = 'darkgrey') +
  geom_point(aes(fill=factor(group),colour = factor(group)),shape = 21, size=5) +
  geom_hline(aes(yintercept =4), linetype = 'dotted') +
  scale_x_discrete(
    labels = df$label
  ) +
  scale_fill_manual(
    values = c('blue',rep('white',4))
  ) +
  scale_color_manual(
    values = c('blue',rep('black',4))
  ) +
  labs(
    x = "Groups",
    y = "Preferences"
    ) +
  coord_flip(ylim = c(4,5.5)) +
  theme_bw() +
  theme(
    axis.title.y.left = element_text(angle=0,vjust=0.5),
    axis.title.x.top = element_text(size=12),
    axis.text.x.top = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  )

# Note: set height > width for best experience
ggsave(path=output_dir, filename="Dot Plot QJE 3.png",  width=6, height=8)

# Save example to src folder
ggsave(path='src/others/others', filename='Dot Plot QJE 3-example.png', width=6, height = 8)
