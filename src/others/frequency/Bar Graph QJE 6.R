# Bar graph, grouped with CI

# Tessa Bold, Kayuki C. Kaizzi, 
#   Jakob Svensson, David Yanagizawa-Drott, Lemon Technologies and Adoption: 
#   Measurement, Theory and Evidence from Agricultural Markets in Uganda, 
#   The Quarterly Journal of Economics, Volume 132, Issue 3, August 2017, 
#   Pages 1055–1100, DOI:10.1093/qje/qjx009 

# Data: emulated from ibid, fig 3

library(ggplot2)

df <- read.csv("data/bold-2017.csv", stringsAsFactors = F)

df$x <- factor(df$x, levels = unique(df$x))

ggplot(df,aes(x,val,fill = factor(N)))+
  geom_bar(stat="identity", position = position_dodge(), color = 'black') + 
  geom_errorbar(aes(ymax=val+.5,ymin=val-.5),position=position_dodge(.9),width=.2) +
  scale_fill_brewer(
    palette=6,
    name = "%N",
    labels = c("0%N",
               "11%N",
               "23%N",
               "34%N",
               "46%N"
               )
    ) +
  theme_bw() +
  scale_y_continuous(
    limits = c(0,6),
    expand = c(0,0)
  ) +
  labs(
    y= "Yield"
  ) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom",
    axis.title.x = element_blank(),
    axis.line = element_line(size=0.5),
    panel.border=element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) 

# Save individual Panels
# ggsave(path=output_dir, filename="Bar Graph QJE 6.png", width=8, height=6)

# Save as Example
# ggsave(path="src/others/frequency", filename="Bar Graph QJE 6-example.png", width=8, height=6)
