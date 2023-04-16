# Bar graph, stacked time series

# Colantone, I., & STANIG, P. (2018). Global competition and brexit.
#   The American Political Science Review, 112(2), 203.
#   doi:10.1017/S0003055417000685

# Data: ibid, Fig 1

library(ggplot2)
library(dplyr)
library(magrittr)

df <- read.csv("data/calantone-2018.csv")
output_dir = "output"

ggplot(df,aes(year,val,fill = class)) +
  geom_bar(stat = "identity", position = position_stack(reverse = T), color = 'black') +
  scale_fill_manual(values = c('grey40', 'grey90')) +
  scale_y_continuous(
    limits = c(0,12),
    breaks = c(0,5,10,12),
    expand = c(0,0)
  ) +
  scale_x_continuous(
    breaks = seq(1988,2006,2)
  ) +
  ggtitle("Imports from China and other low income countries as share of total imports") +
  theme_minimal() +
  labs(
    y = "%",
    x = "Year"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.y = element_line(size = 0.5),
    axis.ticks.y = element_line(size = 0.5),
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.1,.9),
    legend.background = element_rect(color='black')
  )



ggsave(path=output_dir, filename="Bar Graph APSR 7.png", width=8, height=6)

# Example
# ggsave(path="src/distribution", filename="Bar Graph APSR 7-example.png", width=8, height=6)
