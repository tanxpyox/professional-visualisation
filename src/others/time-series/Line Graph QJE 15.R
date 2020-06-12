# Line graph (dotted)

# Emmanuel Saez, Gabriel Zucman, Wealth Inequality in the United States since 1913: 
#   Evidence from Capitalized Income Tax Data , The Quarterly Journal of Economics, 
#   Volume 131, Issue 2, May 2016, Pages 519–578, https://doi.org/10.1093/qje/qjw004 

# Data: ibid Figure 1

library(ggplot2)
library(dplyr)
library(magrittr)
library(ggsignif)

# Plot 1
df <- read.csv("data/saez-2016-fig1.csv")

ggplot(df, aes(year, top0.1)) + 
  geom_point() + 
  geom_line()+
  labs(
    y = "Share of Household Wealth"
  )+ 
  scale_x_continuous(
    breaks = seq(1913,2013,5)
  ) + 
  scale_y_continuous(label = scales::percent) + 
  theme_bw() + 
  theme(
    panel.grid.minor = element_blank(),
    axis.line = element_line(size=0.5),
    panel.border=element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90) 
  ) 

# Save individual Panels
# ggsave(path=output_dir, filename="Line Graph QJE 15.png", width=8, height=6)


# Save as Example
# ggsave(path="src/others/time-series", filename="Line Graph QJE 15-example.png", width=8, height=6)
