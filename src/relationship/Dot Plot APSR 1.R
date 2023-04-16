# Dot Plot, diff colors indicating binary statistics

# Lowande, K. (2018). Who polices the administrative state?
#   The American Political Science Review, 112(4), 882.
#   doi:10.1017/S0003055418000497

# Data: USA 2020 (protected), untying hands

library(ggplot2)
library(dplyr)
library(magrittr)
load("data/protected/usa-2020-cleaned.Rda")
output_dir="output"


df %<>%
  group_by(gender, hand) %>%
  summarise(mean_untie = mean(untie)) %>%
  na.omit()

df$gender = ifelse(df$gender==1, "Male", "Female")

df$end = NA
for (i in 1:6) {
  df$end[i] = df$mean_untie[i+6]
}

clabels =
  c("Control",
    "Opponent Threat",
    "Opponent Reassurance",
    "Opponent Non-Interference",
    "UN Treatment",
    "Military Experts Treatment"
  )

ggplot(df,aes(factor(hand),mean_untie,color = gender)) +
  geom_point(size =4) +
  geom_segment(aes(xend = factor(hand), yend = end), arrow = arrow(length = unit(0.03, "npc")), color = 'black') +
  scale_colour_manual(name = "Legend", values = c("red","blue")) +
  scale_x_discrete(
    labels = clabels
  ) +
  labs(
    x = "Groups",
    y = "Approval Rate (1 = Loweset, 7 = Highest)"
  ) +
  theme_bw() +
  theme(
    axis.title.y.left = element_blank(),
    axis.text.x.top = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.title = element_blank()
  ) + coord_flip()


# Note: set height > width for best experience
ggsave(path=output_dir, filename="Dot Plot APSR 1.png",  width=6, height=8)

# Save example to src folder
# ggsave(path='src/relationship', filename='Dot Plot APSR 1-example.png', width=6, height = 8)
