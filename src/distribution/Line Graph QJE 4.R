# Line graph, Multiple curves (real, diff line styles)

# Emmanuel Saez and Gabriel Zucman, 
#   “Wealth Inequality in the United States since 1913: Evidence from Capitalized Income Tax Data,” 
#   The Quarterly Journal of Economics 131, no. 2 (2016): 548,

# Data: emulated from Quek (2017) Rationalist Experiments on War, Fig 2

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


# build plot
p <- ggplot(df,aes(x=factor(period),y=mean,linetype=condition)) + 
  geom_point(size=3, aes(shape=condition)) + 
  geom_line(aes(x=period,y=mean)) +
  geom_vline(aes(xintercept=5.5), linetype='dotted') + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1)) +
  scale_shape_manual(values = c(19,1)) + 
  theme_bw() + 
  labs(
    x = "Round",
    y = "Incidence of War",
    linetype = "Legend",
    shape = "Legend"
  ) + 
  theme(
    panel.grid.minor = element_blank(),
    #        panel.grid.major = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.line = element_line(size=0.5),
    panel.border=element_blank(),
  )

ggsave(path=output_dir, filename="Line Graph QJE 4.png", width=8, height=6)

# Save as Example
ggsave(path='src/distribution', filename='Line Graph QJE 4-example.png', width=8, height=6)
