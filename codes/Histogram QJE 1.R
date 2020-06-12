# Histogram with Vertical Line

# Mitchell Hoffman, Lisa B Kahn, Danielle Li, 
#   Discretion in Hiring, 
#   The Quarterly Journal of Economics, Volume 133, Issue 2, May 2018, 
#   Pages 765–800, https://doi-org.eproxy.lib.hku.hk/10.1093/qje/qjx042

# Data: Quek (2017) "Rationalist Experiments on War" (Figure 3) 

library(haven)
master <- read_dta("data/quek-rw-2017.dta")
output_dir="output"

library(dplyr)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(ggsignif)

master$reneged = ifelse(master$offer1!=-1 & master$offer2!=-1,
                        master$offer1 > master$offer2,
                        NA)

df <- master %>%
      filter(enforce==0 & period < 11) %>%
      group_by(period) %>%
      summarise(
        mean = mean(reneged, na.rm = T)
      )

p<- ggplot(df, aes(x=factor(period),y=mean)) + 
    geom_col(fill='darkgrey', colour="black",width=1) + 
    geom_vline(aes(xintercept=5.5)) + 
    xlab("Round") + ylab("Percentage of Reneged Offers") +
    theme_bw() +
    scale_y_continuous(labels = scales::percent, limits=c(0,1)) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.line = element_line(size=0.5),
      panel.border=element_blank()
    )


ggsave(path=output_dir, filename="Histogram.png",  width=9, height=6)

# Save example to src folder
# ggsave(path='src/distribution/', filename='Histogram QJE 1-example.png', width=9, height = 6)
