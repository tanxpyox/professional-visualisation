# Dot Plot

# Shaun Larcom, Ferdinand Rauch, and Tim Willems, 
#   “The Benefits of Forced Experimentation: Striking Evidence from the 
#     London Underground Network,” The Quarterly Journal of Economics 132, 
#    no. 4 (2017): 2032

# Data: Ibid,  
#   "Replication Data.zip", https://doi.org/10.7910/DVN/1HUQRM/IKPGHY, Harvard Dataverse, V1

library(haven)
library(ggplot2)

df <- read_dta("data/larcom-2017.dta")
output_dir="output"

p <- ggplot(df,aes(x=day,y=duration_)) + 
      geom_point(size=3) + 
      geom_vline(aes(xintercept=16.5)) + 
      geom_vline(aes(xintercept=18.5)) + 
      theme_bw() + 
      labs(
        x = "Day",
        y = "Mean duration (mins.)"
      ) + 
      theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()
      )

ggsave(path=output_dir, filename="Dot Plot QJE.png", width=8, height=6)

# Save as Example
# ggsave(path='src/central-tendency/mean', filename='Dot Plot QJE-example.png', width=8, height=6)
