# Event Study Plot w/ CI

# Proto, Rustichini, and Sofianos, "Intelligence, Personality, 
#  and Gains from Cooperation in Repeated Interactions",
#  Journal of Political Economy, 1363.

# Data from Quek (2017) "Rationalist Experiments on War" (Figure 6)

library(haven)
master <- read_dta("data/quek-rw-2017.dta")
output_dir="output"

library(dplyr)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(ggsignif)

df <- master %>%
            group_by(enforce11,period) %>%
            summarise(
              mean = mean(war),
              se = sd(war)/sqrt(n())
            ) %>%
            filter(period<16 & period > 10)

df$label <- ifelse(df$enforce11==0, "No Enforcement", "Enforcement")

p <- ggplot(df,aes(x=period, y=mean, group=label, color=label, shape=label)) + 
     geom_point(size=3) + 
     geom_line(size=1) + 
     geom_errorbar(aes(ymax=mean+se,ymin=mean-se),width=0.1) + 
     theme_bw() + 
     scale_y_continuous(labels = scales::percent, limits=c(0,1)) +
     scale_colour_manual(values=c('darkgrey','black')) + 
     scale_shape_manual(values=c(1,19)) + 
     xlab("Round") + ylab("Incidence of War") +
     theme(
       legend.title = element_blank(), 
       legend.position = "bottom",
       panel.grid.major.x = element_blank(),
       panel.grid.minor.x = element_blank(),
       panel.grid.minor.y = element_blank(),
       axis.line = element_line(size=0.5),
       panel.border=element_blank(),
     )

ggsave(path=output_dir, filename="Bar Plot.png",  width=9, height=6)

# Save example to src folder
# ggsave(path='src/central-tendency/mean', filename='Event Study Plot JPE-example.png', width=9, height = 6)
