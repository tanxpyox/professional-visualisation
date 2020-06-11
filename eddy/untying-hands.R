# 2020 US Survey: Module Untying Hands (Eddy)
# 25 February 2020

df <- read.csv("eddy/untying-hands.csv")
output_dir="output"
library(ggplot2)
library(magrittr)
library(ggpubr)
library(ggsignif)
library(scales)


### DV1: untie ###
custom <- c("Control", "Opponent\nThreat", "Opponent\nReassurance", "Opponent\nNon-Interference", "United Nations\nTreatment", "Military Experts\nTreatment")

p_temp <- ggplot(subset(df, group == 1 | group == 2 | group == 3 | group == 4 | group == 5 | group == 6), 
                 aes(x=factor(group), y=mean_1)) + 
          geom_errorbar(width=.1, aes(ymin=lower_bound_1, ymax=upper_bound_1)) + 
          geom_point(size=4, shape=21, fill=c("black", "white", "white", "white", "white", "white")) + 
          xlab("\nExperimental Group") + ylab("Approval Rate (1 = Lowest, 7 = Highest)\n") + 
          scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6"), labels=custom) + 
          coord_cartesian(xlim=c(0.75, 6.25), ylim=c(2.2, 5.2)) + 
          theme_bw() + 
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
          scale_y_continuous(breaks = pretty_breaks(n=6))

p_temp + geom_hline(yintercept = df$mean_1[1], linetype = "dashed", color = "grey")

ggsave(path=output_dir, filename="Module Untying Hands - Figure 1.jpg", width=9, height=6)


### DV2: damage ###
p_temp <- ggplot(subset(df, group == 1 | group == 2 | group == 3 | group == 4 | group == 5 | group == 6), 
                 aes(x=factor(group), y=mean_2)) + 
          geom_errorbar(width=.1, aes(ymin=lower_bound_2, ymax=upper_bound_2)) + 
          geom_point(size=4, shape=21, fill=c("black", "white", "white", "white", "white", "white")) + 
          xlab("\nExperimental Group") + 
          ylab("America's Reputation\n(1 = Damaged a Lot, 7 = Improved a Lot)\n") + 
          scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6"), labels=custom) + 
          coord_cartesian(xlim=c(0.75, 6.25), ylim=c(1.8, 4.8)) + 
          theme_bw() + 
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
          scale_y_continuous(breaks = pretty_breaks(n=6))

p_temp +  geom_hline(yintercept = df$mean_2[1], linetype = "dashed", color = "grey")

ggsave(path=output_dir, filename="Module Untying Hands - Figure 2A.jpg", width=9, height=6)
