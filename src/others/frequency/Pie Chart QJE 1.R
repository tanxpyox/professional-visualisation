# Pie Chart

# Emmanuel Farhi and Matteo Maggiori, “A Model of the International Monetary System,”
# The Quarterly Journal of Economics 133, no. 1 (2018): 299,
# DOI:10.1093/qje/qjx031.

# Data: USA 2020 (protected)

library(ggplot2)
library(dplyr)
df <- read.csv("data/protected/usa-2020.csv")
output_dir="output"

df<- df %>%
  filter(!is.na(bell)) %>%
  group_by(bell) %>%
  summarise(
    count = n()
  ) %>%
  mutate(
    p = count/sum(count)
  )

ggplot(df,aes(x=0, y=p, fill  = factor(bell))) +
  geom_bar(stat="identity",width = 1) +
  geom_text(aes(x=0.25,label = scales::percent(round(p,4), accuracy=0.1)), position = position_stack(vjust = 0.5), size = 4) +
  coord_polar("y",start=0) +
  labs(
    fill = "Choice"
  ) +
  scale_fill_brewer(palette=4,
                    labels = c("Decrease Greatly (1)",
                               "2",
                               "3",
                               "4",
                               "5",
                               "6",
                               "Decrease Greatly (7)")
                    ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  ggtitle("Do you think that U.S. spending on the military should
          increase, decrease or be kept about the same?")

# Save individual Panels
ggsave(path=output_dir, filename="Pie Chart QJE 1.png", width=8, height=6)

# Save as Example
# ggsave(path="src/others/frequency", filename="Pie Chart QJE 1-example.png", width=8, height=6)
