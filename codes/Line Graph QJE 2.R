# Line graph, Multiple curves (real)

# Sarah Baird, Joan Hamory Hicks, Michael Kremer, and Edward Miguel, 
#   “Worms at Work: Long-run Impacts of a Child Health Investment,” 
#   The Quarterly Journal of Economics 131, no. 4 (2016): 1660,

# Data: emulated from ibid, Fig II

library(ggplot2)
library(dplyr)
library(tidyr)

df <- read.csv("data/baird-2016.csv")

fmt_dcimals <- function(decimals=0){
  function(x) format(x,nsmall = decimals,scientific = FALSE)
}

# build plot
p <- ggplot(df, aes(x = hours, y = density, color = type)) +
  geom_line(size=2) +
  theme_bw() +
  labs(
    x = "Hours worked in self-employment, males",
    y = "Kernel density",
    color = "Legend"
  ) + 
  scale_y_continuous(labels=fmt_dcimals(2)) + 
  scale_color_manual(values = c('darkgrey','black')) + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.direction = "vertical",        
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black")
  )

ggsave(path=output_dir, filename="Line Graph QJE 2.png", width=8, height=6)

# Save as Example
ggsave(path='src/distribution', filename='Line Graph QJE 2-example.png', width=8, height=6)
