# Time series 

# Ulrike Malmendier, Stefan Nagel, Learning from Inflation Experiences, 
#   The Quarterly Journal of Economics, 
#   Volume 131, Issue 1, February 2016, Pages 53–87

# Data: Shiller (2005) Available online at http://www.econ.yale.edu/~shiller/data.htm

df <- read.csv("data/shiller-2005.csv")
output_dir <- "output"


df$year <- NA
df$year <- round(df$Date)

df <- df %>% 
  group_by(year) %>%
  summarize(
    y = mean(CPI)
  ) 

df$rate <- NA
for (i in 2:nrow(df)) {
  df$rate[i] = (df$y[i]-df$y[i-1])/df$y[i-1]
}

p <- ggplot(df,aes(year,rate))+
  geom_line(colour = 'blue')+
  theme_bw() + 
  labs(
    y= "Inflation",
    x = "Year"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line = element_line(size=0.5),
    panel.border=element_blank(),
  )

ggsave(path=output_dir, filename="Line Graph QJE 11.png", width=8, height=6)
# Save as Example
ggsave(path="src/others/time-series", filename="Line Graph QJE 11-example.png", width=8, height=6)

