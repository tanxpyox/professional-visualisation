# Scatter plot, texts and trendline

# LAITIN, D. D., & RAMACHANDRAN, R. (2016). Language policy and human development.
#   The American Political Science Review, 110(3), 471.
#   doi:10.1017/S0003055416000265

# Data: Eddy's Democracy Paper (protected)

library(ggplot2)
library(magrittr)
library(gridExtra)
library(haven)
library(dplyr)
library(countrycode)

output_dir = "output"

# total = 100
# Generate dataset
# df <- data.frame(
#   x = rep(0,total),
#   y = rep(0,total),
#   name = rep(0,total))
#
# for (i in 1:total){
#   df$x[i] = rnorm(1)
#   df$y[i] = rnorm(1)
#   df$name[i] = paste(LETTERS[sample(26,1)],LETTERS[sample(26,1)],LETTERS[sample(26,1)],sep = "")
# }

df <- read_dta("data/protected/eddy-democracy.dta") %>%
  group_by(name) %>%
  summarise(
    x = mean(polity2),
    y = mean(V141)
  )

# Convert country name to ISO country code
df$name = countrycode(df$name, "country.name", "iso3c")

# Plot
ggplot(df, aes(x,y)) +
  geom_point() +
  geom_text(aes(label = name), position = position_nudge(0.7)) +
  geom_smooth(method = "gam", color = 'red') +
  labs(
    y = "Perceived Level of Democracy\n(1 = Least Democratic, 10 = Most Democratic)",
    x = "Polity V (-10 = Least Democratic, 10 = Most Democratic)"
  ) +
  scale_x_continuous(limits = c(-10,11)) +
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
