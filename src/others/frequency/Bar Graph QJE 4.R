# Bar Graph, multi

# David J. Deming, “The Growing Importance of Social Skills
#   in the Labor Market,” The Quarterly Journal of Economics 132, no. 4 (2017):
#   1596, DOI:10.1093/qje/qjx022.

# Data: emulated from ibid, Fig 1

library(ggplot2)
library(dplyr)
library(gridExtra)
library(ggpubr)
output_dir="output"

df <- read.csv("data/deming-2017-emulated.dta", stringsAsFactors = F)

df$occ <- factor(df$occ, levels = rev(unique(df$occ)))

df1 <- filter(df, stem == 1)
df2 <- filter(df, stem == 0)

p1 <- ggplot(df1,aes(occ,diff)) +
  geom_bar(stat="identity", fill = 	'#194670') +
  ggtitle("STEM Occupations") +
  coord_flip(ylim = c(-.2,.6)) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_blank(),
    axis.line = element_line(size=0.5),
    panel.border=element_blank()
  )


p2<- ggplot(df2,aes(occ,diff)) +
  geom_bar(stat="identity", fill = 	'#194670') +
  ggtitle("All Other Managerial or Professional Occupations") +
  coord_flip(ylim = c(-.2,.6)) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_blank(),
    axis.line = element_line(size=0.5),
    panel.border=element_blank()
  )

pc <- ggarrange(p1, p2,
          ncol=1, nrow=2,
          labels = "AUTO",
          align = 'v')

# Save individual Panels
ggsave(path=output_dir, filename="Bar Graph QJE 4A.png", p1, width=8, height=6)
ggsave(path=output_dir, filename="Bar Graph QJE 4B.png", p2, width=8, height=6)
ggsave(path=output_dir, filename="Bar Graph QJE 4Combined.png", pc, width=8, height=12)

# Save as Example
ggsave(path="src/others/frequency", filename="Bar Graph QJE 4A-example.png", p1, width=8, height=6)
ggsave(path="src/others/frequency", filename="Bar Graph QJE 4B-example.png", p2, width=8, height=6)
ggsave(path="src/others/frequency", filename="Bar Graph QJE 4Combined-example.png", pc, width=8, height=12)
