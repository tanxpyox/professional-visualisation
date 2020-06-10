# 2020 US Survey: Module Exceptionalism (Eddy)
# 28 January 2020

df <- read.csv("eddy/exceptionalism.csv")
output_dir="output"

library(ggplot2)
library(magrittr)
library(ggpubr)
library(plotrix)
library(ggsignif)


### DV1: nativist ###
custom <- c("Control Group", "Peace-Loving Treatment", "Hardworking Treatment", "Unique Quality Treatment")

p_temp <- ggplot(subset(df, group == 1 | group == 2 | group == 3 | group == 4), 
                 aes(x=factor(group), y=mean_1)) +
          # geom_bar(stat="identity", aes(y=mean_1, fill=group), position="dodge") +
          geom_errorbar(width=.1, aes(ymin=lower_bound_1, ymax=upper_bound_1)) + 
          geom_point(size=4, shape=21, fill="white") + 
          xlab("\nExperimental Group") + 
          ylab("Nativism (1 = Least Nativist, 5 = Most Nativist)\n") + 
          scale_x_discrete(breaks=c("1", "2", "3", "4"), labels=custom) + 
          coord_cartesian(xlim=c(0.75, 4.25), ylim=c(2, 3.5)) + theme_bw() + 
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

p_temp + geom_hline(yintercept=2.923077, linetype="dashed", color="red") + 
            geom_signif(comparisons=list(c("1", "4")), 
                        annotations="**", y_position=2.4, tip_length=-0.07, vjust=3.4)

ggsave(path=output_dir, filename="Module Exceptionalism - Figure 1A.jpg", width=8, height=6)


### DV2: hawk1 + hawk2 ###
p_temp <- ggplot(subset(df, group == 1 | group == 2 | group == 3 | group == 4), 
                aes(x=factor(group), y=mean_2)) + 
          geom_errorbar(width=.1, aes(ymin=lower_bound_2, ymax=upper_bound_2)) + 
          geom_point(size=4, shape=21, fill="white") + 
          xlab("\nExperimental Group") + 
          ylab("Hawkishness (0 = Least Hawkish, 8 = Most Hawkish)\n") + 
          scale_x_discrete(breaks=c("1", "2", "3", "4"), labels=custom) + 
          coord_cartesian(xlim=c(0.75, 4.25), ylim=c(4.4, 5.8)) + 
          theme_bw() + 
          theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank())

p_temp + geom_hline(yintercept=5.026923, linetype="dashed", color="red")

ggsave(path=output_dir, filename="Module Exceptionalism - Figure 2A.jpg", width=8, height=6)


### DV3: trust ###
p_temp <- ggplot(subset(df, group == 1 | group == 2 | group == 3 | group == 4), 
                aes(x=factor(group), y=mean_3)) + 
          geom_errorbar(width=.1, aes(ymin=lower_bound_3, ymax=upper_bound_3)) + 
          geom_point(size=4, shape=21, fill="white") + 
          xlab("\nExperimental Group") + 
          ylab("International Trust (1 = Least Trust, 7 = Most Trust)\n") + 
          scale_x_discrete(breaks=c("1", "2", "3", "4"), labels=custom) + 
          coord_cartesian(xlim=c(0.75, 4.25), ylim=c(3.65, 4.5)) + 
          theme_bw() + 
          theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank())

p_temp + geom_hline(yintercept=4.057915, linetype="dashed", color="red")
ggsave(path=output_dir, filename="Module Exceptionalism - Figure 3A.jpg", width=8, height=6)
