# Bar graph, left and right

# Matteo Maggiori, Brent Neiman, and Jesse Schreger, 
#   "International Currencies and Capital Allocation," 
#   Journal of Political Economy 128, no. 6 (June 2020): 2019-2066.
#   https://doi.org/10.1086/705688 

# Data: ibid, fig 2

library(ggplot2)

df <- read.csv("data/maggiori-2020-fig2.csv", stringsAsFactors = F)

df$val <- ifelse(df$foreign == 0, -df$val, df$val)
df$country <- factor(df$country, levels = df$country)

ggplot(df,aes(country,val,fill = factor(foreign), color = factor(foreign)))+
  geom_bar(stat="identity") +
  scale_fill_manual(values = c('red', 'white')) + 
  scale_color_manual(values = c('red', 'blue')) +
  scale_y_continuous(
    breaks = seq(-1,1,.25),
    labels = abs(seq(-1,1,.25)),
    limits = c(-1,1)
  ) + 
  labs(
    x= "Issuing Country"
  ) +
  coord_flip(clip='off') + theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.line = element_line(size=0.5),
    panel.border=element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) + 
  ggtitle("Doemstic Investors                    Foreign Investors")

# Save individual Panels
ggsave(path=output_dir, filename="Bar Graph JPE 2.png", width=8, height=6)

# Save as Example
# ggsave(path="src/others/frequency", filename="Bar Graph JPE 2-example.png", width=8, height=6)
