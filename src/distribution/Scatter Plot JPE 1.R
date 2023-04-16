# Scatter Plot, Bubbles

# Matteo Maggiori, Brent Neiman, and Jesse Schreger,
#   "International Currencies and Capital Allocation,"
#   Journal of Political Economy 128, no. 6 (June 2020): 2019-2066.

# Data: Generated

library(ggplot2)
output_dir="output"

# Initialise
x = NA
x[100] = NA
y = NA
y[100] = NA
val= NA
val[100] = NA

# Generate Data
for (i in 1:100){
  x[i] = i + rnorm(1,sd=10)
  y[i] = i + rnorm(1,sd=10)
  val[i] = sample(1:20,1)
}
df <- data.frame(iv = x, dv=y, value = val)

# PLot
p <- ggplot(data = df, aes(x=iv, y=dv, size=value)) +
  geom_point(shape = 1, colour = 'blue') +
  scale_size_continuous(range = c(1,15)) +
  theme_bw() +
  labs(
    x = "Independent Variable",
    y = "Dependent Variable",
    size = "Legend"
  ) + theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line = element_line(size=0.5),
    panel.border=element_blank(),
    legend.position = "none"
  )

ggsave(path=output_dir, filename="Scatter Plot JPE 1.png", width=8, height=6)

# Save as Example
# ggsave(path='src/distribution', filename='Scatter Plot JPE 1-example.png', width=8, height=6)
