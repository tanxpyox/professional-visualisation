# Multiple line graphs, shared legend

# Thomas Sampson, “Dynamic Selection: An Idea Flows Theory of Entry, Trade, and Growth,” 
# The Quarterly Journal of Economics 131, no. 1 (2016): 353


library(ggplot2)
library(magrittr)
library(haven)
library(tidyverse)
library(ggpubr)

#Define Function
f <- function(x, a, b) a * exp(b*x)
g <- function(x, a, b) a * exp(-b*x)

#Main frame
df <- data.frame(
  para1 = c(1.65,1),
  para2 = c(0,2*log(2.5)),
  label = c("Welfare (right)", "Growth (left)")
)

lc = c("Welfare (right)" = 3, "Growth (left)" = 1)

# Dataset for plot 1
p1df <- df %>%
  crossing(x = seq(0,0.5,0.01)) %>%
  mutate(y = f(x,para1,para2))

# plot 1
p1 <- ggplot(p1df, aes(x=x,y=y,linetype=label)) + 
  geom_line (size = 1) + 
  coord_cartesian(
    ylim = c(0,2.5)
  ) +
  scale_linetype_manual(values = lc) + 
  scale_y_continuous(
    sec.axis = sec_axis(~ (.-1) * 3 +1, name = "Gains")
  ) +
  labs(
    x = "Ratio",
    y = "Growth",
    linetype = "Legend"
  ) +
  theme_bw() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.2,0.8)
  ) 

# Dataset for plot 2
p2df <- p1df %>%
  mutate(y = g(x,para1,para2))

# plot 2
p2 <- ggplot(p2df, aes(x=x,y=y,linetype=label)) + 
  geom_line (size = 1) + 
  coord_cartesian(
    ylim = c(0,2.5)
  ) +
  scale_linetype_manual(values = lc) + 
  scale_y_continuous(
    sec.axis = sec_axis(~ (.-1) * 3 +1, name = "Gains")
  ) +
  labs(
    x = "Ratio",
    y = "Growth",
    linetype = "Legend"
  ) +
  theme_bw() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.2,0.8)
  ) 

# Emulate grid using plots 1 and 2
ggarrange(p1, p2, p1, p2, p1, p2, 
          ncol=3, nrow=2, 
          labels = "AUTO",
          align = "hv",
          common.legend = TRUE, legend="bottom")

ggsave(path=output_dir, filename="Line Graph QJE 9.png", width=8, height=6)
# Save as Example
# ggsave(path="src/relationship/", filename="Line Graph QJE 9-example.png", width=8, height=6)

