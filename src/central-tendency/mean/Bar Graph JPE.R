# Bar graph with CI

# Arthur Blouin and Sharun W. Mukand, "Erasing Ethnicity?
#   Propaganda, Nation Building, and Identity in Rwanda,"
#   Journal of Political Economy 127, no. 3 (2019): 1046

# Data USA 2020 (not released); Security Dilemma

master <- read.csv("data/protected/usa-2020.csv")
output_dir="output"

library(ggplot2)
library(magrittr)
library(ggpubr)
library(ggsignif)
library(scales)
library(dplyr)
library(gridExtra)

df <- master %>%
      group_by(spend) %>%
      summarize(
        bell_mean = mean(bell, na.rm = TRUE),
        bell_se = sd(bell, na.rm=TRUE)/sqrt(n()),
        bell_lb = bell_mean - bell_se,
        bell_ub = bell_mean + bell_se,
        offense_mean = mean(offense, na.rm=TRUE),
        offense_se = sd(offense, na.rm = TRUE)/sqrt(n()),
        offense_lb = offense_mean - offense_se,
        offense_ub = offense_mean + offense_se
      ) %>%
      rename(
        group = spend,
        mean_1 = bell_mean,
        lower_bound_1 = bell_lb,
        upper_bound_1 = bell_ub,
        mean_2 = offense_mean,
        lower_bound_2 = offense_lb,
        upper_bound_2 = offense_ub
      )


custom <- c("Control Group", "China's Defence Improves\nand Offence Remains Same", "China Spends More\nDue to Others", "War Is Costly Due to\nNuclear Weapons", "War Is Costly Due to\nDependent Economies")

# Plot 1
plot_1 <- ggplot(subset(df, group == 1 | group == 2 | group == 3 | group == 4 | group == 5),
                 aes(x=factor(group), y=mean_1)) +
          geom_bar(stat="identity", aes(y=mean_1), fill="white", colour="black", position="dodge") +
          geom_errorbar(width=.1, aes(ymin=lower_bound_1, ymax=upper_bound_1)) +
          geom_point(size=4, shape=21, fill="white") +
          xlab("\nExperimental Group") +
          ylab("Whether US Military Spending Should Increase\n(1 = Decrease Greatly, 7 = Increase Greatly)\n") +
          scale_x_discrete(breaks=c("1", "2", "3", "4", "5"), labels=custom) +
          coord_cartesian(xlim=c(0.75, 5.25), ylim=c(4, 5.5)) +
          theme_bw() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())

# p_temp + geom_hline(yintercept=df$mean_1[1], linetype="dashed", color="red")

ggsave(path=output_dir, filename="Plot 1.jpg", width=8, height=6)

# Plot 2
plot_2 <- ggplot(subset(df, group == 1 | group == 2 | group == 3 | group == 4 | group == 5),
                 aes(x=factor(group), y=mean_2)) +
          geom_bar(stat="identity", aes(y=mean_2), fill="white", colour="black", position="dodge") +
          geom_errorbar(width=.1, aes(ymin=lower_bound_2, ymax=upper_bound_2)) +
          geom_point(size=4, shape=21, fill="white") +
          xlab("\nExperimental Group") +
          ylab("China's Intent of Increasing Military Spending\n(1 = Purely for Defensive Reasons, 7 = Purely for Offensive Reasons)\n") +
          scale_x_discrete(breaks=c("1", "2", "3", "4", "5"), labels=custom) +
          coord_cartesian(xlim=c(0.75, 5.25), ylim=c(3.5, 5)) +
          theme_bw() +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(path=output_dir, filename="Plot 2.jpg", width=9, height=6)

# Multiple panels
ggsave(path=output_dir, filename="combined.png", arrangeGrob(plot_1, plot_2), width=9, height = 10)

# Save example to src folder
# ggsave(path='src/central-tendency/mean', filename='Bar Graph JPE-example.png', arrangeGrob(plot_1, plot_2), width=9, height = 10)
