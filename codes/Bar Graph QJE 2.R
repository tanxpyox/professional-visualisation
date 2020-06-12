# Bar Graph with Label

# Oriana Bandiera, Robin Burgess, Narayan Das, Selim Gulesci, 
#   Imran Rasul, and Munshi Sulaiman, 
#   “Labor Markets and Poverty in Village Economies,” 
#   The Quarterly Journal of Economics 132, no. 2 (2017): 822

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
cc <- scales::seq_gradient_pal("lightgrey", "black")(seq(0,1,length.out=6))

p <- ggplot(df,aes(x=tmp,y=percentage, fill=factor(pride3) )) + 
     geom_bar(stat="identity",colour=1) + 
     scale_fill_manual(values=cc) + 
     scale_y_continuous(labels = scales::percent) +
     labs(
        x = "Categories",
        y = "Percentage",
        fill = "pride3"
      ) +
     theme_bw() +
     theme(
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(size=0.5),
        panel.border=element_blank(),
     )

ggsave(path=output_dir, filename="Bar Graph 2.png",  width=9, height=6)

# Save example to src folder
# ggsave(path='src/distribution', filename='Bar Graph QJE 2-example.png', width=9, height = 6)
