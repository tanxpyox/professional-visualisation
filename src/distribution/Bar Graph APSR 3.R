# Bar plot, multiple

# Pan, J., & Chen, K. (2018). Concealing corruption: How chinese officials
#   distort upward reporting of online
#   grievances. The American Political Science Review, 112(3), 608.
#   doi:10.1017/S0003055418000205

# Data: USA 2020 (protected)

library(ggplot2)
library(dplyr)
library(stringr)
library(magrittr)
library(scales)
output_dir = "output"
load("data/protected/usa-2020-cleaned.Rda")

fq <- data.frame(d = df$StartDate)
fq$d %<>% str_replace(".+ ", "") %>% as.POSIXct("%H:%M", tz = "America/Los_Angeles")

ggplot(fq,aes(d)) +
  geom_histogram(binwidth = 3600, fill = 'grey90', color = 'black') +
  geom_vline(xintercept = as.POSIXct("12:00","%H:%M", tz = "America/Los_Angeles"), linetype = 2) +
  annotate("text", x=c(as.POSIXct("11:00","%H:%M", tz = "America/Los_Angeles"),as.POSIXct("13:00","%H:%M", tz = "America/Los_Angeles")), y = c(300,300), label = c("AM","PM")) +
  scale_x_datetime(labels = date_format("%H:%M", tz = "America/Los_Angeles"), breaks = date_breaks("2 hour"))  +
  ggtitle("Distribution of Responses") +
  theme_minimal() +
  labs(
    y = "Frequency",
    x = "Time of a day"
  ) +
  theme(
    plot.title = element_text(hjust= 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 45)
  )


# Save individual Panels
ggsave(path=output_dir, filename="Bar Graph APSR 3.png", width=8, height=6)

# Example
# ggsave(path="src/distribution", filename="Bar Graph APSR 3-example.png", width=8, height=6)

