# Scatter Plot, discrete, jitter

# Abeler, J., Nosenzo, D. and Raymond, C. (2019), 
#   Preferences for Truth‐Telling. Econometrica,
#   87: 1115-1153. doi:10.3982/ECTA14673

# Data: Ibid, FIg 7

library(haven)
library(ggplot2)
library(dplyr)
master <- read_dta("data/abeler-2019-lab.dta")
output_dir="output"

df <- master %>%
  filter(treatment == "OBSERVABLE") %>%
  group_by(draw,report) 

# PLot
p <- ggplot(data = df, aes(x=factor(draw), y=factor(report))) + 
  geom_point(colour = '#5E5E5E') +
  geom_jitter() + 
  scale_size_continuous(range = c(1,15)) + 
  theme_bw() + 
  labs(
    x = "true number drawn on computer",
    y = "report",
    size = "Legend"
  ) + theme(
    panel.grid.minor = element_blank(),
    #    panel.grid.major.x = element_blank(),
    axis.line = element_line(size=0.5),
    panel.border=element_blank(),
    legend.position = "none"
  )

# ggsave(path=output_dir, filename="Scatter Plot Econometrica 2.png", width=8, height=6)

# Save as Example
# ggsave(path='src/distribution', filename='Scatter Plot Econometrica 2-example.png', width=8, height=6)
