# Bar plot, multiple

# Lowande, K. (2018). Who polices the administrative state?
#   The American Political Science Review, 112(4), 879.
#   doi:10.1017/S0003055418000497

# Data: USA 2020 (protected)

library(ggplot2)
library(dplyr)
library(stringr)
library(magrittr)
library(scales)
output_dir = "output"
load("data/protected/usa-2020-cleaned.Rda")

fq <- data.frame(d = df$StartDate, g = df$gender) %>% na.omit()
fq$d %<>% str_replace(".+ ", "") %>% as.POSIXct("%H:%M", tz = "America/Los_Angeles")
fq$g <- ifelse(fq$g==1, "Male", "Female")

ggplot(fq,aes(x=d, fill=g)) +
  geom_histogram(binwidth = 3600, color = 'black', alpha=.6) +
  geom_vline(xintercept = as.POSIXct("12:00","%H:%M", tz = "America/Los_Angeles"), linetype = 2) +
  annotate("text", x=as.POSIXct("11:00","%H:%M", tz = "America/Los_Angeles"), y = 300, label = "AM") +
  annotate("text", x=as.POSIXct("13:00","%H:%M", tz = "America/Los_Angeles"), y = 300, label = "PM") +
  scale_x_datetime(labels = date_format("%H:%M", tz = "America/Los_Angeles"), breaks = date_breaks("2 hour"))  +
  ggtitle("Distribution of Responses") +
  theme_minimal() +
  labs(
    y = "Frequency",
    x = "Time of a day",
    fill = "Gender"
  ) +
  theme(
    plot.title = element_text(hjust= 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 45),
    legend.position = "none"
  ) +
  facet_grid(g ~ .)

# Save individual Panels
ggsave(path=output_dir, filename="Bar Graph APSR 4.png", width=8, height=6)

# Example
# ggsave(path="src/distribution", filename="Bar Graph APSR 4-example.png", width=8, height=6)

