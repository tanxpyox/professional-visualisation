# Histogram with Vertical Line 2

# Andreas Fagereng, Luigi Guiso, Davide Malacrino, and Luigi Pistaferri,
#   “Heterogeneity and Persistence in Returns to Wealth,” Econometrica 88,
#   no. 1 (2020): 152, DOI:10.3982/ECTA14835.

# Data: Quek (2017) "Rationalist Experiments on War" (Figure 4)

library(haven)
master <- read_dta("data/quek-rw-2017.dta")
output_dir="output"

library(dplyr)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(ggsignif)

df <- master %>%
  filter(enforce==0 & period < 11) %>%
  group_by(period) %>%
  summarise(
    mean = mean(war1, na.rm = T) / mean(war, na.rm=T)
  )

p<- ggplot(df, aes(x=factor(period),y=mean)) +
    geom_col(fill='lightblue',width=1) +
    geom_vline(aes(xintercept=5.5)) +
    xlab("Round") + ylab("Percentage of Wars in Stage 1") +
    theme_bw() +
    scale_y_continuous(labels = scales::percent, limits=c(0,1)) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.line = element_line(size=0.5),
      panel.border=element_blank()
    )


ggsave(path=output_dir, filename="Histogram 2.png",  width=9, height=6)

# Save example to src folder
# ggsave(path='src/distribution/', filename='Histogram Econometrica 1-example.png', width=9, height = 6)
