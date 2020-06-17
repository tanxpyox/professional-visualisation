# Line graph, feeling thermometer

# RUNDLETT, A., & SVOLIK, M. W. (2016). Deliver the vote! micromotives and
#   macrobehavior in electoral fraud. The American Political Science Review, 110(1), 191.
#   doi:10.1017/S0003055415000635

# Data: USA 2020 (protected)

library(haven)
library(ggplot2)
library(dplyr)

load("data/protected/usa-2020-cleaned.Rda")
output_dir="output"

df <- df %>%
  filter(hand==1 & untie>0) %>%
  group_by(untie) %>%
  dplyr::summarise(
    val = n()
  )

# build plot
ggplot(df,aes(x=untie,y=val)) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(
    breaks = 1:7
  ) +
  labs(
    x = "Do you approve or disapprove of the way the U.S. president handled the situation?\n(1 = Greatly Disapprove, 7 = Greatly Approve)",
    y = "Number of Participants in Control Group",
    linetype = "Legend",
    shape = "Legend"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.background = element_rect(fill = 'grey90'),
    axis.line = element_line(size=0.5),
    axis.text.y = element_text(angle = 90),
    panel.border=element_blank()
  )

ggsave(path=output_dir, filename="Line Graph APSR 2.png", width=8, height=6)

# Save as Example
# ggsave(path='src/distribution', filename='Line Graph APSR 2-example.png', width=8, height=6)
