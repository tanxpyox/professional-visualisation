# Bar graph with trendline

# Lovo, S., & Spaenjers, C. (2018). A model of trading in the art market.
# The American Economic Review, 108(3), 764.
# doi:10.1257/aer.20160522

# Data: Quek (2017) Rationalist Experiments on War, Fig 4

library(ggplot2)
library(dplyr)
library(magrittr)
library(haven)

master <- read_dta("data/quek-rw-2017.dta")
output_dir="output"

df <- master %>%
  filter(enforce==0 & period < 11) %>%
  group_by(period) %>%
  summarise(
    mean = mean(war1, na.rm = T) / mean(war, na.rm=T)
  )

ggplot(df, aes(period,mean)) +
  geom_bar(stat = "identity", alpha = .6) +
  geom_smooth(method = "lm",se = F, color = 'black') +
  xlab("Round") + ylab("Percentage of Wars in Stage 1") +
  scale_y_continuous(labels = scales::percent, limits=c(0,1)) +
  scale_x_continuous(breaks = 1:10) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line = element_line(size=0.5),
    panel.border=element_blank()
  )

ggsave(path=output_dir, filename="Bar Graph AER 4.png", width=8, height=6)

# Save as Example
# ggsave(path="src/distribution", filename="Bar Graph AER 4-example.png", width=8, height=6)
