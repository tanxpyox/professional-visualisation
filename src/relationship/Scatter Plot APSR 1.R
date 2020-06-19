# Scatter plot, texts and trendline

# LAITIN, D. D., & RAMACHANDRAN, R. (2016). Language policy and human development.
#   The American Political Science Review, 110(3), 471.
#   doi:10.1017/S0003055416000265

# Data: Generated

library(ggplot2)
library(magrittr)
library(gridExtra)

output_dir = "output"
total = 100

# Generate dataset
df <- data.frame(
  x = rep(0,total),
  y = rep(0,total),
  name = rep(0,total))

for (i in 1:total){
  df$x[i] = rnorm(1)
  df$y[i] = rnorm(1)
  df$name[i] = paste(LETTERS[sample(26,1)],LETTERS[sample(26,1)],LETTERS[sample(26,1)],sep = "")
}

# Plot
ggplot(df, aes(x,y)) +
  geom_point() +
  geom_text(aes(label = name), position = position_nudge(0.25)) +
  geom_smooth(method = "lm", se=F, color = 'red') +
  labs(
    x = "Democratic Index",
    y = "GDP Growth Rate"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size=0.5),
    panel.border=element_blank()
  )

# Save
ggsave(path=output_dir, filename="Scatter Plot APSR 1.png",width=8, height=6)

# Save as example
# ggsave(path="src/relationship", filename="Scatter Plot APSR 1Combined.png", width=8, height=6)
