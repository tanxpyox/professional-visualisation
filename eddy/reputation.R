# 2020 US Survey: Module Reputation (Eddy)
# 28 January 2020

df <- read.csv("eddy/reputation.csv")
output_dir="output"

library(ggplot2)
library(magrittr)
library(ggpubr)
library(plotrix)
library(ggsignif)


### DV: cred ###
p_temp <- ggplot(subset(df, group == 0 | group == 1 | group == 3 | group == 4 | group == 5 | group == 7), 
                 aes(x=group, y=mean)) + 
          geom_line() + 
          geom_errorbar(width=.1, aes(ymin=lower_bound, ymax=upper_bound)) + 
          geom_point(size=3, shape=21, fill="white") + 
          xlab("\nNumber of Past Crises Provided\n(Leader Reneged on Threat to Use Force in Fourth Crisis)") + 
          ylab("Mean Credibility (1 = Least Credible, 7 = Most Credible)\n") + 
          scale_y_continuous(expand=c(0,0), breaks=c(3,4,5,6,7)) + 
          scale_x_continuous(breaks=c(0, 1, 3, 4, 5, 7)) + coord_cartesian(ylim=c(3, 7)) + 
          theme_bw() + 
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

p_temp + geom_vline(xintercept=3.5, linetype="dashed", color="blue")

ggsave(path=output_dir, filename="Module Reputation - Figure 1A.jpg", width=6, height=6)
