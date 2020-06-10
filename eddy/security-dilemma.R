# 2020 US Survey: Module Security Dilemma (Eddy)
# 28 January 2020

df <- read.csv("eddy/security-dilemma.csv")
output_dir="output"
library(ggplot2)
library(magrittr)
library(ggpubr)
library(plotrix)
library(ggsignif)


##### Groups 1 to 5 only #####
### DV1: bell ###
custom <- c("Control Group", "China's Defence Improves\nand Offence Remains Same", "China Spends More\nDue to Others", "War Is Costly Due to\nNuclear Weapons", "War Is Costly Due to\nDependent Economies")

p_temp <- ggplot(subset(df, group == 1 | group == 2 | group == 3 | group == 4 | group == 5), 
                 aes(x=factor(group), y=mean_1)) + 
          geom_errorbar(width=.1, aes(ymin=lower_bound_1, ymax=upper_bound_1)) + 
          geom_point(size=4, shape=21, fill="white") + 
          xlab("\nExperimental Group") + 
          ylab("Whether US Military Spending Should Increase\n(1 = Decrease Greatly, 7 = Increase Greatly)\n") + 
          scale_x_discrete(breaks=c("1", "2", "3", "4", "5"), labels=custom) + 
          coord_cartesian(xlim=c(0.75, 5.25), ylim=c(4, 5.5)) + 
          theme_bw() + 
          theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank())

p_temp + geom_hline(yintercept=4.688372, linetype="dashed", color="red")

ggsave(path=output_dir, filename="Module Security Dilemma - Figure 1A.jpg", width=9, height=6)


### DV2: offense ###
p_temp <- ggplot(subset(df, group == 1 | group == 2 | group == 3 | group == 4 | group == 5), 
                 aes(x=factor(group), y=mean_2)) + 
          geom_errorbar(width=.1, aes(ymin=lower_bound_2, ymax=upper_bound_2)) + 
          geom_point(size=4, shape=21, fill="white") + 
          xlab("\nExperimental Group") + 
          ylab("China's Intent of Increasing Military Spending\n(1 = Purely for Defensive Reasons, 7 = Purely for Offensive Reasons)\n") + 
          scale_x_discrete(breaks=c("1", "2", "3", "4", "5"), labels=custom) + 
          coord_cartesian(xlim=c(0.75, 5.25), ylim=c(3.5, 5)) + 
          theme_bw() + 
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

p_temp + geom_hline(yintercept=4.191589, linetype="dashed", color="red")

ggsave(path=output_dir, filename="Module Security Dilemma - Figure 2A.jpg", width=9, height=6)


### DV3: threat ###
p_temp <- ggplot(subset(df, group == 1 | group == 2 | group == 3 | group == 4 | group == 5), 
                 aes(x=factor(group), y=mean_3)) + 
          geom_errorbar(width=.1, aes(ymin=lower_bound_3, ymax=upper_bound_3)) + 
          geom_point(size=4, shape=21, fill="white") + 
          xlab("\nExperimental Group") + 
          ylab("Whether China Is a Threat to US Security\n(1 = No Threat, 7 = Major Threat)\n") + 
          scale_x_discrete(breaks=c("1", "2", "3", "4", "5"), labels=custom) + 
          coord_cartesian(xlim=c(0.75, 5.25), ylim=c(4, 5)) + theme_bw() + 
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

p_temp + geom_hline(yintercept=4.562791, linetype="dashed", color="red")

ggsave(path=output_dir, filename="Module Security Dilemma - Figure 3A.jpg", width=9, height=6)


##### Groups 11 to 12 only #####
### DV1: stg2 ###
custom <- c("Produce Security for Own\nCountry As a Good Strategy", "Consider Opponent's Strategic\nResponse As a Good Strategy")

p_temp <- ggplot(subset(df, group == 11 | group == 12), aes(x=factor(group), y=mean_1)) + 
          geom_errorbar(width=.1, aes(ymin=lower_bound_1, ymax=upper_bound_1)) + 
          geom_point(size=4, shape=21, fill="white") + 
          xlab("\nExperimental Group") + 
          ylab("Whether US Military Spending Should Increase\n(1 = Decrease Greatly, 7 = Increase Greatly)\n") + 
          scale_x_discrete(breaks=c("11", "12"), labels=custom) + 
          coord_cartesian(xlim=c(0.75, 2.25), ylim=c(4, 5.5)) + theme_bw() + 
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(path=output_dir, filename="Module Security Dilemma - Figure 4A.jpg", width=6, height=6)


### DV2: offense ###
p_temp <- ggplot(subset(df, group == 11 | group == 12), aes(x=factor(group), y=mean_2)) + 
          geom_errorbar(width=.1, aes(ymin=lower_bound_2, ymax=upper_bound_2)) + 
          geom_point(size=4, shape=21, fill="white") + 
          xlab("\nExperimental Group") + 
          ylab("China's Intent of Increasing Military Spending\n(1 = Purely for Defensive Reasons, 7 = Purely for Offensive Reasons)\n") + 
          scale_x_discrete(breaks=c("11", "12"), labels=custom) + 
          coord_cartesian(xlim=c(0.75, 2.25), ylim=c(3.5, 5)) + theme_bw() + 
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(path=output_dir, filename="Module Security Dilemma - Figure 5A.jpg", width=6, height=6)


### DV3: threat ###
p_temp <- ggplot(subset(df, group == 11 | group == 12), aes(x=factor(group), y=mean_3)) + 
          geom_errorbar(width=.1, aes(ymin=lower_bound_3, ymax=upper_bound_3)) + 
          geom_point(size=4, shape=21, fill="white") + 
          xlab("\nExperimental Group") + 
          ylab("Whether China Is a Threat to US Security\n(1 = No Threat, 7 = Major Threat)\n") + 
          scale_x_discrete(breaks=c("11", "12"), labels=custom) + 
          coord_cartesian(xlim=c(0.75, 2.25), ylim=c(4, 5)) + theme_bw() + 
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(path=output_dir, filename="Module Security Dilemma - Figure 6A.jpg", width=6, height=6)
