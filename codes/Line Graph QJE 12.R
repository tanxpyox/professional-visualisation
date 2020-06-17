# Multiple time series

# Emmanuel Saez and Gabriel Zucman, “Wealth Inequality in the United States since 1913:
#   Evidence from Capitalized Income Tax Data,”
#   The Quarterly Journal of Economics 131, no. 2 (2016): 548,
#   DOI:10.1093/qje/qjw004

# Data: ibid, Fig VIII panel A

library(ggplot2)
library(magrittr)
library(dplyr)

df <- read.csv("data/saez-2016-fig8.csv")
output_dir = "output"

ggplot(df, aes(x=year))+
  geom_line(aes(y=top0.1, color = '1', linetype = '1'), size=1) +
  geom_line(aes(y=bottom90, color = '2', linetype = '2'),  size=1) +
  geom_line(aes(y=pop, color = '3', linetype = '3'), size=1) +
  ggtitle("Share of Wealth held by elderly households (aged 65+)") +
  scale_linetype_manual(
    name = "Legend",
    values = c(
      '1'='solid',
      '2'='dotted',
      '3'='solid'
    ),
    labels = c(
      "Top 0.1%",
      "Total Population",
      "Bottom 90%"
    )
  ) +
  scale_color_manual(
    name = "Legend",
    values = c(
      '1'='black',
      '2'='black',
      '3'='darkgrey'
    ),
    labels = c(
      "Top 0.1%",
      "Total Population",
      "Bottom 90%"
    )
  ) +
  coord_cartesian(
    ylim = c(0,0.6)
  )+
  labs(
    y = "Share of each group's total wealth",
    x = "Year"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()

ggsave(path=output_dir, filename="Line Graph QJE 12.png", width=8, height=6)
# Save as Example
ggsave(path="src/others/time-series", filename="Line Graph QJE 12-example.png", width=8, height=6)

