# Line graph Multiple overlay from given formula

# Ulrike Malmendier and Stefan Nagel, 
#   “Learning from Inflation Experiences,” 
#   The Quarterly Journal of Economics 131, no. 1 (2016): 61

# Data: emulated from ibid, Figure II

library(ggplot2)
library(magrittr)
library(haven)
library(tidyverse)
library(gridExtra)

#Define Function
f <- function(x, theta)  (500*theta)/(x**2)
g <- function(x, p) 0.005*exp((p-1)*(x-20)/100)

#Main frame
df <- data.frame(
  theta = c(3,1,0.8),
  cc = c('red', 'blue', 'black'),
  lc = c('solid', 'longdash', 'dotted')  
)

# Dataset for first plot
p1df <- df %>%
  mutate(curve = theta, line = lc) %>%
  crossing(x = seq(0.1,200,0.1)) %>%
  mutate(y = f(x,theta))

# plot 1
p1 <- ggplot(p1df, aes(x,y,color = factor(theta), linetype=factor(theta))) + 
  geom_line (size = 1) + 
  coord_cartesian(
    ylim = c(0,8)
  ) + 
  scale_color_manual(values = cc) + 
  scale_linetype_manual(values = lc) + 
  labs(
    x = "Age (qtrs)",
    y = "Gain",
    color = "Legend",
    linetype = "Legend"
  ) +
  theme_bw() + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size=0.5),
    panel.border=element_blank()
  ) 

# Dataset for plot 2
p2df <- df %>%
  mutate(curve = theta, line = lc) %>%
  crossing(x = seq(0,200,0.1)) %>%
  mutate(y = g(x,theta))

# plot 2
p2 <- ggplot(p2df, aes(x,y,color = factor(theta), linetype=factor(theta))) + 
  geom_line (size=1) + 
  coord_cartesian(
    ylim = c(0,0.015)
  ) + 
  scale_color_manual(values = cc) + 
  scale_linetype_manual(values = lc) + 
  labs(
    x = "Time lag (qtrs)",
    y = "Weights",
    color = "Legend",
    linetype = "Legend"
  ) +
  theme_bw() + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size=0.5),
    panel.border=element_blank()
  )


ggsave(path=output_dir, filename="Line Graph QJE 6A.png", p1, width=8, height=6)
ggsave(path=output_dir, filename="Line Graph QJE 6B.png", p2, width=8, height=6)
ggsave(path=output_dir, filename="Line Graph QJE 6 Combined.png", arrangeGrob(p1,p2), width=8, height=10)
# Save as Example
ggsave(path="src/relationship/", filename="Line Graph QJE 6-example.png", arrangeGrob(p1,p2), width=8, height=10)
