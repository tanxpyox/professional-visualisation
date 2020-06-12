# Line graph (dotted), multiple

# Emmanuel Saez, Gabriel Zucman, Wealth Inequality in the United States since 1913: 
#   Evidence from Capitalized Income Tax Data , The Quarterly Journal of Economics, 
#   Volume 131, Issue 2, May 2016, Pages 519–578, https://doi.org/10.1093/qje/qjw004 

# Data: ibid Figure 8B

library(ggplot2)
library(dplyr)
library(magrittr)
library(ggsignif)

# Plot
df <- read.csv("data/saez-2016-fig8B.csv")

ggplot(df, aes(year, top0.1, shape = class, fill = class)) + 
  geom_line() + 
  geom_point(size = 2) + 
  labs(
    y = "Share of total pre-tax income",
    shape = "Legend",
    fill = "Legend"
  )+ 
  scale_x_continuous(
    breaks = seq(1960,2015,5)
  ) + 
  scale_y_continuous(label = scales::percent) + 
  scale_shape_manual(
    values = c(23,24),
    labels = (c("Labor income","National income"))
  ) + 
  scale_fill_manual(
    values = c('white','black'),
    labels = (c("Labor income","National income"))
  ) +
  coord_cartesian(ylim = c(0,.08), xlim = c(1960,2015)) + 
  theme_bw() + 
  theme(
    panel.grid.minor = element_blank(),
    axis.line = element_line(size=0.5),
    panel.border=element_blank(),
    axis.title.x = element_blank() 
  ) 

# Save individual Panels
ggsave(path=output_dir, filename="Line Graph QJE 16.png", width=8, height=6)


# Save as Example
ggsave(path="src/others/time-series", filename="Line Graph QJE 16-example.png", width=8, height=6)
