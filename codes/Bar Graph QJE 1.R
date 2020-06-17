# Bar Graph with Label

# Lars J. Kirkeboen, Edwin Leuven, Magne Mogstad,
#   Field of Study, Earnings, and Self-Selection,
#   The Quarterly Journal of Economics, Volume 131,
#   Issue 3, August 2016, Pages 1057–1111,

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
cc <- rep('lightgrey',6)

labels = c('Economic\nAchievenments',
            'Cultural\nand\nArtistic\nAchieve-\nments',
            'Technological\nand\nScientific\nAchievements',
            'Moral values',
            'Democratic values',
            'Public\nservices'
            )

df$label = NA
for (i in 1:6){
  df$label[i] <- labels[df$pride3[i]]
}


ggplot(df,aes(x=tmp,y=percentage,fill=factor(pride3), label=pride3)) +
  geom_bar(stat="identity", colour = 'black') +
  geom_text(aes(label=label), position=position_stack(vjust=0.5)) +
  scale_fill_manual(values=cc) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Categories",
    y = "Percentage",
    fill = "pride3"
  ) +
  theme_bw() +
  theme(
    #   panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line = element_line(size=0.5),
    panel.border=element_blank(),
    legend.position = "none"
  ) +
  coord_flip()


ggsave(path=output_dir, filename="Bar Graph 1.png",  width=9, height=6)

# Save example to src folder
# ggsave(path='src/distribution', filename='Bar Graph QJE 1-example.png', width=9, height = 6)
