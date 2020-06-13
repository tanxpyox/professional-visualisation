# Multiple time series, multiple panels

# Jérôme Adda, Economic Activity and the Spread of Viral Diseases: 
#   Evidence from High Frequency Data , 
#   The Quarterly Journal of Economics, 
#   Volume 131, Issue 2, May 2016, Pages 891–941, 
#   DOI:10.1093/qje/qjw005

# Data: ibid Figure II

library(ggplot2)
library(dplyr)
library(magrittr)
library(gridExtra)

# Plot 1
df <- read.csv("data/adda-2016-flu.csv")

p1 <- ggplot(df, aes(age, infection, linetype= class)) + 
  geom_line(size=1) +
  ggtitle("Flu-like Illnesses")+
  theme_bw() +
  scale_linetype_manual(
    values = c(3,1,4)
  ) +
  labs(
    x = "Cases",
    y = "Infection rate per 100,000"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.box.background = element_rect(color = "black"),
    legend.background = element_rect(fill = NA),
  )

# Plot 2
df <- read.csv("data/adda-2016-diarrhea.csv")

p2 <- ggplot(df, aes(age, infection, linetype= class)) + 
  geom_line(size=1) +
  ggtitle("Acute Diarrhea")+
  theme_bw() +
  scale_linetype_manual(
    values = c(3,1,4)
  ) +
  labs(
    x = "Cases",
    y = "Infection rate per 100,000"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.box.background = element_rect(color = "black"),
    legend.background = element_rect(fill = NA),
  )

# Plot 3
df <- read.csv("data/adda-2016-chickenpox.csv")

p3 <- ggplot(df, aes(age, infection, linetype= class)) + 
  geom_line(size=1) +
  ggtitle("Chickenpox")+
  theme_bw() +
  scale_linetype_manual(
    values = c(3,1,4)
  ) +
  labs(
    x = "Cases",
    y = "Infection rate per 100,000"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.box.background = element_rect(color = "black"),
    legend.background = element_rect(fill = NA),
  )

# combine plots
pc <- ggarrange(p1, p2, p3, 
          ncol=1, nrow=3, 
          labels = "AUTO",
          align = "v",
          common.legend = FALSE, legend="bottom")

# Save individual Panels
ggsave(path=output_dir, filename="Line Graph QJE 13A.png", p1, width=8, height=6)
ggsave(path=output_dir, filename="Line Graph QJE 13B.png", p2, width=8, height=6)
ggsave(path=output_dir, filename="Line Graph QJE 13C.png", p3, width=8, height=6)

# Save combined
ggsave(path=output_dir, filename="Line Graph QJE 13 Combined.png", pc, width=8, height=15)

# Save as Example
# ggsave(path="src/others/time-series", filename="Line Graph QJE 13A-example.png", p1, width=8, height=6)
# ggsave(path="src/others/time-series", filename="Line Graph QJE 13B-example.png", p2, width=8, height=6)
# ggsave(path="src/others/time-series", filename="Line Graph QJE 13C-example.png", p3, width=8, height=6)
# ggsave(path="src/others/time-series", filename="Line Graph QJE 13Combined-example.png", pc, width=8, height=15)
