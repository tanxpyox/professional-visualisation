# Bar graph, stacked

# Matteo Maggiori, Brent Neiman, and Jesse Schreger, 
#   "International Currencies and Capital Allocation," 
#   Journal of Political Economy 128, no. 6 (June 2020): 2019-2066.
#   https://doi.org/10.1086/705688 

# Data: ibid, fig 3

library(ggplot2)

df <- read.csv("data/maggiori-2020-fig3.csv", stringsAsFactors = F)

for (i in 1:6){
  df$val[i+6] <- df$val[i+6] - df$val[i]
}

df$country <- factor(df$country, levels = unique(df$country))

ggplot(df,aes(country,val,fill = factor(usd), color = factor(usd)))+
  geom_bar(stat="identity") +
  scale_fill_manual(values = c('white', 'red'), labels = c("Home Currency", "USD")) + 
  scale_color_manual(values = c('black', 'red'), labels = c("Home Currency", "USD")) +
  scale_y_continuous(
    breaks = seq(-1,1,.25),
    labels = abs(seq(-1,1,.25)),
    limits = c(0,1),
    expand = c(0,0)
  ) + 
  labs(
    x= "Country",
    y= "Share of External Portfolio"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.background = element_rect(color= 'black'),
    axis.title.x = element_blank(),
    axis.line = element_line(size=0.5),
    panel.border=element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) 

# Save individual Panels
ggsave(path=output_dir, filename="Bar Graph JPE 3.png", width=8, height=6)

# Save as Example
# ggsave(path="src/others/frequency", filename="Bar Graph JPE 3-example.png", width=8, height=6)
