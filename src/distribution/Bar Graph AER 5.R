# Bar Graph with trendline, multiple panels
# For individual panels, see Bar Graph AER 4.R

# Muralidharan, K., Niehaus, P., & Sukhtankar, S. (2016).
# Building state capacity: Evidence from biometric
# smartcards in india. The American Economic Review, 106(10), 2909.
# doi:10.1257/aer.20141346

# Data: Quek (2017) Rationalist Experiments on War, Figs 3, 4

library(ggplot2)
library(dplyr)
library(magrittr)
library(haven)
library(ggpubr)

master <- read_dta("data/quek-rw-2017.dta")
output_dir="output"

# Generate datasets
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

something = "NA" # Placeholder

# Plot 1: fig 4
p1 <- ggplot(df, aes(period,mean_war, fill = something)) +
  geom_bar(stat = "identity") +
  geom_smooth(aes(color = something), method = "lm",se = F, size = 1, linetype = 2) +
  xlab("Round") + ylab("Percentage of Wars in Stage 1") +
  scale_fill_manual(name = "Legend", values = "cadetblue3", labels = "Percentage") +
  scale_color_manual(name = "Legend", values = 'cyan4', labels = "Trendline") +
  scale_y_continuous(labels = scales::percent, limits=c(0,1)) +
  scale_x_continuous(breaks = 1:10) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line = element_line(size=0.5),
    panel.border=element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

# Plot 2: Fig 3
p2 <- ggplot(df, aes(period,mean_reneged, fill = something)) +
  geom_bar(stat = "identity") +
  geom_smooth(aes(color = something), method = "lm",se = F, size = 1, linetype = 2) +
  xlab("Round") + ylab("Percentage of Reneged Offers") +
  scale_fill_manual(name = "Legend", values = "cadetblue3", labels = "Percentage") +
  scale_color_manual(name = "Legend", values = 'cyan4', labels = "Trendline") +
  scale_y_continuous(labels = scales::percent, limits=c(0,1)) +
  scale_x_continuous(breaks = 1:10) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line = element_line(size=0.5),
    panel.border=element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

pc <- ggarrange(p1 + ggtitle("Panel A:  Percentage of wars in Stage 1")+ theme(plot.title = element_text(size = 10)),
             p2 + ggtitle("Panel B: Percentage of reneged offers") + theme(plot.title = element_text(size = 10)),
             nrow =1, common.legend = T, legend = "bottom")

# Save individual panels
ggsave(path=output_dir, filename="Bar Graph AER 5A.png", p1, width=8, height=6)
ggsave(path=output_dir, filename="Bar Graph AER 5B.png", p2, width=8, height=6)
# Save combined graph
ggsave(path=output_dir, filename="Bar Graph AER 5Combined.png", pc, width=8, height=6)

# Save as Example
# ggsave(path="src/distribution", filename="Bar Graph AER 5A.png", p1, width=8, height=6)
# ggsave(path="src/distribution", filename="Bar Graph AER 5B.png", p2, width=8, height=6)
# ggsave(path="src/distribution", filename="Bar Graph AER 5Combined.png", pc, width=8, height=6)

