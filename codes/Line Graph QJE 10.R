# Line Graph (with data points) 

# Amy Finkelstein, Matthew Gentzkow, Heidi Williams, 
#   Sources of Geographic Variation in Health Care: Evidence From Patient Migration,
#   The Quarterly Journal of Economics, Volume 131, Issue 4, November 2016, Pages 1681–1726,
#   https://doi.org/10.1093/qje/qjw023 

# Data: Quek (2017) Rationalist Experiments on War, Fig 2

library(haven)
library(ggplot2)

master <- read_dta("data/quek-rw-2017.dta")
output_dir="output"

master$condition <- ifelse(floor((master$subject-1)/6) %% 2, "Enforcement in Rounds 1-5", "Enforcement in Rounds 6-10")

df <- master %>%
  filter(period<11 & condition=="Enforcement in Rounds 1-5") %>%
  group_by(condition, period) %>%
  summarise(
    mean = mean(war, na.rm=T)
  )

cc <- c("Enforcement in Rounds 1-5" = 'black')

p <- ggplot(df,aes(x=factor(period),y=mean,color=condition)) + 
  geom_point(size=3) + 
  geom_line(aes(x=period,y=mean)) +
  scale_y_continuous(labels = scales::percent, limits=c(0,1)) +
  scale_color_manual(values=cc) +
  theme_bw() + 
  labs(
    x = "Round",
    y = "Incidence of War",
    color = "Legend"
  ) + 
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    legend.title = element_blank(),
    axis.line = element_line(size=0.5),
    panel.border=element_blank(),
  )

print(p)

ggsave(path=output_dir, filename="Line Graph QJE 10.png", width=8, height=6)

# Save as Example
# ggsave(path='src/relationship', filename='Line Graph QJE 10-example.png', width=8, height=6)
