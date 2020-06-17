# Bar Graph with t-test bar

# Bracic, A. (2016). Reaching the individual: EU accession, NGOs, and human rights.
#   The American Political Science Review, 110(3), 541.
#   doi:10.1017/S000305541600023X

# Data: USA 2020 (protected)

master <- read.csv("data/protected/usa-2020-cleaned.csv")
output_dir <- "output"

library(ggplot2)
library(ggsignif)
library(rstatix)
library(dplyr)

# Build dataset
df <- master %>% pairwise_t_test(untie ~ hand) %>% filter(group1==1)

summary_table <- master %>% group_by(hand) %>% get_summary_stats(untie)
summary_table$name <- c("Control",
                        "Opponent\nThreat",
                        "Opponent\nReassurance",
                        "Opponent\nNon-Interference",
                        "UN\nTreatment",
                        "Military Experts\nTreatment")

# Please ignore warnings
ggplot(master,aes(x=factor(hand),y=untie)) +
  geom_signif(comparison = list(c(1,4)), map_signif_level = T, y_position = 5, tip_length = 0.5)+
  geom_bar(stat="summary", fill = 'white', color = 'black', width = 0.9) +
  geom_text(label = summary_table$mean, stat="summary", position = position_nudge(y=-0.5)) +
  geom_text(label = summary_table$name, stat="summary", position = position_fill(0.5), size = 3.5) +
  scale_y_continuous(limits = c(0,7), expand = c(0,0)) +
  theme_bw() +
  labs(
    x = "Experimental Group",
    y = "Approval Rate (1 = Lowest, 7 = Highest)",
    caption = "*** - difference significant at the p<0.01 level"
  ) +
  theme(
      axis.text.x = element_blank(),
      panel.grid.major.x = element_blank()
  )

# Save individual Panels
ggsave(path=output_dir, filename="Bar Graph APSR 1.png", width=8, height=6)

# Example
# ggsave(path="src/central-tendency/mean", filename="Bar Graph APSR 1-example.png", width=8, height=6)
