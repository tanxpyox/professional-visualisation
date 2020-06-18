# Bar plot, multiple

# Imas, A. (2016). The realization effect: Risk-taking after realized versus paper losses.
#   The American Economic Review, 106(8), 2102.
#   doi:10.1257/aer.20140386

# Data: USA 2020 (protected)

library(ggplot2)
library(magrittr)
library(scales)
library(gridExtra)
output_dir = "output"
load("data/protected/usa-2020-cleaned.Rda")

df %<>%
  group_by(spend) %>%
  summarise(
    mean = mean(threat,na.rm=T),
    se = sd(threat,na.rm=T)/sqrt(n())
  )

# df$mean <- df$mean - df$mean[1]
# df$se <- df$se + df$se[1]

df1 <- df %>% filter(spend < 9)
df2 <- df %>% filter(spend > 9)

p1 <- ggplot(df1,aes(x = factor(spend), fill = factor(spend))) +
  geom_bar(aes(y=mean), stat = "identity") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  scale_fill_grey() +
  geom_text(aes(y = mean+(mean/abs(mean))*.5, label = round(mean,2)), position = position_nudge(0,0)) +
  geom_hline(yintercept = 0) +
  coord_cartesian(
    ylim = c(0,7)
  ) +
  theme_minimal() +
  labs(
    y = "Whether China is a Threat to US Security\n(1 = No Threat, 7 = Major Threat)",
    x = "Treatment Group"
  ) +  theme(
    axis.line.y = element_line(size = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.5,.85),
    legend.direction = "horizontal",
    legend.background = element_rect(color = 'black'),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) + ggtitle("Groups 1-5")

p2 <- ggplot(df2,aes(x = factor(spend), fill = factor(spend))) +
  geom_bar(aes(y=mean), stat = "identity") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  scale_fill_grey() +
  geom_text(aes(y = mean+(mean/abs(mean))*.5, label = round(mean,2)), position = position_nudge(0,0)) +
  geom_hline(yintercept = 0) +
  coord_cartesian(
    ylim = c(0,7)
  ) +
  theme_minimal() +
  labs(
    y = "Whether China is a Threat to US Security\n(1 = No Threat, 7 = Major Threat)",
    x = "Treatment Group"
  ) +  theme(
    axis.line.y = element_line(size = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.5,.85),
    legend.direction = "horizontal",
    legend.background = element_rect(color = 'black'),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) + ggtitle("Groups 11-12")

pc <- grid.arrange(p1, p2 + theme(axis.title.y = element_blank()), nrow = 1)

# Save individual Panels
ggsave(path=output_dir, filename="Bar Graph AER 2A.png", p1, width=8, height=6)
ggsave(path=output_dir, filename="Bar Graph AER 2B.png", p2, width=8, height=6)
#Save combined
ggsave(path=output_dir, filename="Bar Graph AER 2Combined.png", pc, width=8, height=6)

# Example
# ggsave(path="src/distribution", filename="Bar Graph AER 2A-example.png", p1, width=8, height=6)
# ggsave(path="src/distribution", filename="Bar Graph AER 2B-example.png", p2, width=8, height=6)
# ggsave(path="src/distribution", filename="Bar Graph AER 2Combined-example.png", pc, width=8, height=6)


