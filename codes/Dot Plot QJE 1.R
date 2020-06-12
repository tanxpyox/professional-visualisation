# Dot Plot

# Shaun Larcom, Ferdinand Rauch, and Tim Willems, 
#   “The Benefits of Forced Experimentation: Striking Evidence from the 
#     London Underground Network,” The Quarterly Journal of Economics 132, 
#    no. 4 (2017): 2032

# Data: Quek (2017) Rationalist Experiments on War, Fig 2

library(haven)
library(ggplot2)

master <- read_dta("data/quek-rw-2017.dta")
output_dir="output"

master$condition <- ifelse(floor((master$subject-1)/6) %% 2, "Enforcement in Rounds 1-5", "Enforcement in Rounds 6-10")

df <- master %>%
  filter(period<11) %>%
  group_by(condition, period) %>%
  summarise(
    mean = mean(war, na.rm=T)
  )

cc <- c("Enforcement in Rounds 1-5" = 'black', 
        "Enforcement in Rounds 6-10" = 'blue')

p <- ggplot(df,aes(x=factor(period),y=mean,color=condition)) + 
      geom_point(size=3) + 
      geom_line(aes(x=period,y=mean)) +
      geom_vline(aes(xintercept=5.5), linetype='dotdash') + 
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
#        panel.grid.major = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.line = element_line(size=0.5),
        panel.border=element_blank(),
      )

ggsave(path=output_dir, filename="Dot Plot QJE 1.png", width=8, height=6)

# Save as Example
# ggsave(path='src/distribution', filename='Dot Plot QJE 1-example.png', width=8, height=6)
