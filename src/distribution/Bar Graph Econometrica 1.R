# Bar Graph with Label

# Eugenio J. Miravete, Katja Seim, and Jeff Thurk,
#   “Market Power and the Laffer Curve,”
#   Econometrica 86, no. 5 (2020): 1665

# Data: USA 2020 (not released)

master <- read.csv("data/protected/usa-2020.csv")
output_dir="output"

library(ggplot2)
library(magrittr)
library(ggpubr)
library(ggsignif)
library(scales)
library(dplyr)

master$tmp <- ifelse(master$pride3 %in% c(1,3,6), "Materialist",
                     ifelse(master$pride3 %in% c(2,4,5), "Ideational",
                            ifelse(master$pride3 == 7, "others", NA)
                     )
)

sum_table <- master %>%
  group_by(tmp) %>%
  summarise(
    sum=n()
  )

df <- master %>%
  filter(!is.na(tmp) & tmp!="others") %>% # filter out 'others'
  group_by(tmp,pride3) %>%
  summarise(
    count = n(),
  )

df$percentage <- NA

for (i in 1:6) {
  df$percentage[i] <- df$count[i] / sum_table[[which(sum_table$tmp == df$tmp[i]),2]]
}

# Define colour gradients
cc <- scales::seq_gradient_pal("white", "darkgrey")(seq(0,1,length.out=6))

percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
}

p <- ggplot(df,aes(x=tmp,y=percentage, fill=factor(pride3) )) +
  geom_bar(stat="identity",colour=1) +
  geom_text(aes(label=count), position=position_stack(vjust=0.5)) +
  # show percentage instead:
  # geom_text(aes(label=percent(percentage)), position=position_stack(vjust=0.5)) +
  scale_fill_manual(values=cc) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Categories",
    y = "Percentage",
    fill = "pride3"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line = element_line(size=0.5),
    panel.border=element_blank()
  )

ggsave(path=output_dir, filename="Bar Graph 3.png",  width=9, height=6)

# Save example to src folder
# ggsave(path='src/distribution', filename='Bar Graph Econometrica 1-example.png', width=9, height = 6)
