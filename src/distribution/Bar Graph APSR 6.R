# Bar plot, stacked, multiple panels

# DAVENPORT, L. D. (2016). Beyond black and white:
#   Biracial attitudes in contemporary U.S. politics. The
#   American Political Science Review, 110(1), 58.
#   doi:10.1017/S0003055415000556

# Data: USA 2020 (Protected)

library(ggplot2)
library(dplyr)
library(gtools)
library(ggpubr)
library(magrittr)

load("data/protected/usa-2020-cleaned.Rda")
output_dir <- "output"

df1 <- df %>%
  group_by(pride, hawk) %>%
  summarise(
    n = n(),
    var = "hawk"
  ) %>%
  rename(
    pref = hawk
  )
df2 <- df %>%
  group_by(pride, nativist) %>%
  summarise(
    n = n(),
    var = "nativist"
  ) %>%
  rename(
    pref = nativist
  )
df3 <- df %>%
  group_by(pride, trust) %>%
  summarise(
    n = n(),
    var = "trust"
  ) %>%
  rename(
    pref = trust
  )

pdf <- rbind(df1,df2,df3)
pdf %<>% na.omit() %>% filter(pref>0 & pref<=7)

ggplot(pdf,aes(var, n, fill = factor(pref))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_brewer(
    name = "Preference",
    palette = 3,
    labels = c("1 Greatly disagree", paste(2:6), "7 Greatly Agree")
  ) +
  labs(
    y = "Distribution of Preference",
    x = "Dependent Variable"
  )  +
  theme_minimal() +
  facet_grid(~pride, labeller = labeller(pride = c("0"="Not Proud as American", "1"="Proud as American")))

ggsave(path=output_dir, filename="Bar Graph APSR 6.png", width=8, height=6)

# Example
# ggsave(path="src/distribution", filename="Bar Graph APSR 6-example.png", width=8, height=6)
