# Histogram with Vertical Line 2

# Andreas Fagereng, Luigi Guiso, Davide Malacrino, and Luigi Pistaferri,
#   “Heterogeneity and Persistence in Returns to Wealth,” Econometrica 88,
#   no. 1 (2020): 152, DOI:10.3982/ECTA14835.

# Data: Quek (2017) "Rationalist Experiments on War" (Figure 3 and 4)

library(haven)
master <- read_dta("data/quek-rw-2017.dta")
output_dir="output"

library(dplyr)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(ggsignif)
library(gridExtra)

master$reneged = ifelse(master$offer1!=-1 & master$offer2!=-1,
                        master$offer1 > master$offer2,
                        NA)

df <- master %>%
  filter(enforce==0 & period < 11) %>%
  group_by(period) %>%
  summarise(
    mean_reneged = mean(reneged, na.rm=T),
    mean_war = mean(war1, na.rm = T) / mean(war, na.rm=T)
  )

fills <- c("Wars in Stage 1" = "lightblue", "Reneged Offers" = NA)
colours <- c("Wars in Stage 1" = NA, "Reneged Offers" = 'Black')

p<- ggplot(df, aes(x=factor(period))) +
    geom_col(aes(y=mean_war, fill='Wars in Stage 1', colour = "Wars in Stage 1"),width=1) +
    geom_col(aes(y=mean_reneged, fill='Reneged Offers', colour = "Reneged Offers"),width=1) +
    geom_vline(aes(xintercept=5.5)) +
    labs(
      x = "Round",
      y= "Percentage"
    ) +
    theme_bw() +
    scale_y_continuous(labels = scales::percent, limits=c(0,1)) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.line = element_line(size=0.5),
      panel.border=element_blank(),
      legend.title = element_blank(),
      legend.position = "bottom"
    ) +
    scale_fill_manual(
      name="Legend",
      values=fills
      ) +
    scale_color_manual(
      name="Legend",
      values=colours
      )

ggsave(path=output_dir, filename="Histogram 3.png",  width=9, height=6)

# Tile graphs (after saving as plot_1, plot_2, plot_3, plot_4)
# ggsave(path=output_dir,
#        filename="Histogram 3-combined.png",
#        arrangeGrobe(plot_1, plot_2, plot_3, plot_4),
#        width=18, height=12)

# Save example to src folder
# ggsave(path='src/distribution/', filename='Histogram Econometrica 2-example.png', width=9, height = 6)
