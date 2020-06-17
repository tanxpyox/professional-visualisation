# Line graph, horizontal, with lines of diff thickness to indicate spread

# BROOCKMAN, D. E., & Skovron, C. (2018). Bias in perceptions of public opinion among
#   political elites. The American Political Science Review, 112(3), 557.
#   doi:10.1017/S0003055418000011

# Data: USA 2020 (protected)

library(ggplot2)
library(dplyr)
master <- read.csv("data/protected/usa-2020.csv")
output_dir="output"

# Clean data
df <- master %>%
  filter(spend<11) %>%
  group_by(spend) %>%
  dplyr::summarize(
    bell_mean = mean(bell, na.rm = TRUE),
    bell_se = sd(bell, na.rm=TRUE)/sqrt(n()),
    sd = sd(bell,na.rm=TRUE),
    iqr = IQR(bell, na.rm = TRUE),
    median = median(bell, na.rm = TRUE)
  ) %>%
  dplyr::rename(
    group = spend,
    pt = bell_mean,
    error = bell_se
  )

label =
  c("Control",
    "China's Defence Improves",
    "China Spends More",
    "Nuclear Weapons",
    "Dependent Economies"
  )

# Plot
ggplot(df, aes(factor(group), pt)) +
  geom_linerange(aes(ymax = pt + error, ymin = pt - error), size = 2, color = 'blue') +
  geom_linerange(aes(ymax = pt + 2*error, ymin = pt - 2*error), size = 0.2, color = 'blue') +
  geom_point(fill = 'blue', shape = 21, size=4) +
  geom_hline(aes(yintercept =4), linetype = 'dotted') +
  scale_x_discrete(
    labels = label
  ) +
  labs(
    x = "Groups",
    y = "Preferences"
  ) +
  coord_flip(ylim = c(4,5.5)) +
  theme_bw() +
  theme(
    axis.title.y.left = element_blank(),
    axis.text.x.top = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  )

# Note: set height > width for best experience
ggsave(path=output_dir, filename="Line Graph APSR 3.png",  width=6, height=8)

# Save example to src folder
# ggsave(path='src/distribution', filename='Line Graph APSR 3-example.png', width=6, height = 8)
