# Bar plot, multiple

# Young, L. E. (2019). The psychology of state repression:
#   Fear and dissent decisions in zimbabwe. The
#   American Political Science Review, 113(1), 149.
#   doi:10.1017/S000305541800076X

# Data: USA 2020 (protected)

library(ggplot2)
library(dplyr)
output_dir = "output"
load("data/protected/usa-2020-cleaned.Rda")

### By education
df$uni = df$edu>=5
df1 <- df %>% group_by(uni,except) %>% summarise(.y. = "nativist", mean = mean(nativist, na.rm=T), e = sd(nativist,na.rm=T)/sqrt(n()))
df2 <- df %>% group_by(uni,except) %>% summarise(.y. = "hawk", mean = mean(hawk, na.rm=T), e = sd(hawk,na.rm=T)/sqrt(n()))
df3 <- df %>% group_by(uni,except) %>% summarise(.y. = "trust", mean = mean(trust, na.rm=T), e = sd(trust,na.rm=T)/sqrt(n()))

uni.name <- c("FALSE" = "Primary or Secondary", "TRUE" = "Education: Undegrad+")
dv.name <- c("hawk" = "Hawkish?", "nativist" = "Nativist?", "trust" = "Trust?")

plot_df <- rbind(df1,df2,df3) %>% na.omit()

ggplot(plot_df, aes(factor(except),mean, fill= factor(except))) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymax = mean + e, ymin = mean - e), width = 0.2) +
  facet_wrap(~ uni + .y.,
             labeller = labeller(uni = uni.name, .y. = dv.name)) +
  labs(
    y = "Preference",
    x = "Treatment Group",
    fill = "Groups"
  ) +
  scale_fill_brewer(palette = "Set1", labels  = c("Control", "Peace-loving", "Hardworking", "Unique")) +
  theme_bw() +
  theme(
    axis.text.x = element_blank()
  )

# Save individual Panels
ggsave(path=output_dir, filename="Bar Graph APSR 2.png", width=8, height=6)

# Example
# ggsave(path="src/distribution", filename="Bar Graph APSR 2-example.png", width=8, height=6)

