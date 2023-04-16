# Bar Graph Grouped

# Javier Cravino, Andrei A. Levchenko, Multinational Firms
#   and International Business Cycle Transmission,
#   The Quarterly Journal of Economics,
#   Volume 132, Issue 2, May 2017, Pages 921–962,
#   DOI:10.1093/qje/qjw043

# Data: emulated from ibid, Fig 1

library(ggplot2)
output_dir="output"

df <- read.csv("data/cravino-2016-fig1.csv", stringsAsFactors = FALSE)

df$class = factor(df$class, levels = rev(unique(df$class)))
df$country = factor(df$country, levels = rev(unique(df$country)))

ggplot(df,aes(country,val,fill = class, color = class)) +
  geom_bar(stat="identity",position = position_dodge()) +
  scale_fill_manual(values = c('darkgrey', 'white')) +
  scale_color_manual(values = c(NA, 'black')) +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip() + theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom",
    axis.title = element_blank(),
    axis.line = element_line(size=0.5),
    panel.border=element_blank(),
    legend.background = element_rect(color = 'black')
  )

# Save individual Panels
ggsave(path=output_dir, filename="Bar Graph QJE 3.png", width=8, height=6)

# Save as Example
# ggsave(path="src/others/frequency", filename="Bar Graph QJE 3-example.png", width=8, height=6)
