# Graphical Regression Table

# Amy Finkelstein, Matthew Gentzkow, Heidi Williams, 
#   Sources of Geographic Variation in Health Care: Evidence From Patient Migration,
#   The Quarterly Journal of Economics, Volume 131, Issue 4, November 2016, Pages 1681–1726,
#   https://doi.org/10.1093/qje/qjw023 

# Data: USA 2020 (protected)

library(ggplot2)
library(dplyr)
library(magrittr)

master <- read.csv("data/protected/usa-2020.csv")

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
    "China's Defence Improves\nand Offence Remains Same",
    "China Spends More\nDue to Others",
    "War Is Costly Due to\nNuclear Weapons",
    "War Is Costly Due to\nDependent Economies"
  )

p <- ggplot(df, aes(factor(group), pt)) +
  geom_errorbar(aes(ymax = pt + error, ymin = pt - error), width = 0.2, color = 'darkgrey') +
  geom_point() +
  scale_y_continuous(
    sec.axis = sec_axis(~., "Whether US Military Spending Should Increase\n(1 = Decrease Greatly, 7 = Increase Greatly)")
  ) +
  annotate(x=5.8,y=3.7,geom="text" , label = "Treatment", size = 3.5) +
  scale_x_discrete(
    labels = df$label
  ) +
  geom_hline(aes(yintercept =4), linetype = 'dotted') +
  coord_flip(ylim = c(4,5.5), clip = 'off') +
  theme_bw() +
  theme(
    axis.title.y.left = element_blank(),
    axis.title.x.bottom = element_blank(),
    axis.title.x.top = element_text(size=12),
    axis.text.x.top = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# Note: do not change the width/height. Changing them could cause the axis title to shift.
ggsave(path=output_dir, filename="Regression Table QJE 1B.png", width=6, height=4)
# Save as Example
ggsave(path="src/relationship/", filename="Regression Table QJE 1B-example.png", width=6, height=4)
