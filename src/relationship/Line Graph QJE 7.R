# Line graph Multiple overlay from given formula

# Thomas Sampson, “Dynamic Selection: An Idea Flows Theory of Entry, Trade, and Growth,”
# The Quarterly Journal of Economics 131, no. 1 (2016): 353

# Data: emulated from ibid, Figure I

library(ggplot2)
library(magrittr)
library(haven)
library(tidyverse)
output_dir="output"

#Define Function
f <- function(x, a) a * (x**2)

#Main frame
df <- data.frame(
  param = c(120,80,50),
  label = c("Total Gains", "Dynamic Gains", "Static Gains")
)

lc = c("Total Gains" = 1, "Dynamic Gains" = 4, "Static Gains" = 3)

# Dataset for plot
p1df <- df %>%
  crossing(x = seq(0,0.5,0.01)) %>%
  mutate(y = f(x,param))

# Plot
p1 <- ggplot(p1df, aes(x=x,y=y,linetype=label)) +
  geom_line (size = 1) +
  coord_cartesian(
    ylim = c(0,35)
  ) +
  scale_linetype_manual(values = lc) +
  labs(
    x = "Import Penetration Ratio",
    y = "Welfare gains from trade (%)",
    linetype = "Legend"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.2,0.8)
  )

print(p1)

ggsave(path=output_dir, filename="Line Graph QJE 7.png", width=8, height=6)
# Save as Example
ggsave(path="src/relationship/", filename="Line Graph QJE 7-example.png", width=8, height=6)
