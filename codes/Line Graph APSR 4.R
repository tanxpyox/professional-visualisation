# Line graph, horizontal, control = coloured

# CHRISTENSON, D., & KRINER, D. (2019). Does Public Opinion Constrain
#   Presidential Unilateralism? American Political Science Review, 113(4), 1073.
#   doi:10.1017/S0003055419000327

# ibid. Fig 1

library(ggplot2)
library(haven)
library(dplyr)
df <- read_dta("data/christenson-2019.dta")
output_dir="output"
df$date=df$year + (df$month-1)/12
df <- df %>% filter(df$date>1952)

pres_names = c("Eisenhower", "JFK", "LBJ", "Nixon", "Ford", "Carter", "Reagan", "GHWB", "Clinton", "GWB", "Obama", "Trump")
pres_start=c(1953, 1961, 1963, 1969, 1974, 1977, 1981, 1989, 1993, 2001, 2009, 2017)
pres_end=c(1961, 1963, 1969, 1974, 1977, 1981, 1989, 1993, 2001, 2009, 2017, 2021)
pres_mid = (pres_start+pres_end)/2
colors = c("red", "white", "blue", "white", "red", "white", "red", "white", "blue", "white", "blue", "white")

# Plot
ggplot(df, aes(date,eosnytwithinyear))  +
  geom_line() +
  labs(
    x = "Year",
    y = "Executive Orders in NY Times"
  ) +
  annotate("rect",
           xmin = pres_start,
           xmax = pres_end,
           ymin = rep(0,12),
           ymax=rep(15.5,12),
           fill = colors,
           alpha = .2) +
  annotate("text", label = pres_names, x = pres_mid, y = rep(c(14,13),6)) +
  scale_y_continuous(
    limits = c(0,15.5),
    expand = c(0,0)
  ) +
  scale_x_continuous(
    breaks = seq(1959,2019,10)
  )  +
  theme_bw()

# Note: set height > width for best experience
ggsave(path=output_dir, filename="Line Graph APSR 4.png",  width=10, height=6)

# Save example to src folder
ggsave(path='src/distribution', filename='Line Graph APSR 4-example.png', width=10, height = 6)
