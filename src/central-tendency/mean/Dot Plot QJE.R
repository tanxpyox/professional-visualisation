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

p <- plot(df$duration_ ~ df$day, 
     xlab="Day", 
     ylab="Mean duration (mins.)", 
     pch=19) + 
  abline(v=16.5) + 
  abline(v=18.5)

dev.copy(jpeg,"output/Dot Plot.jpg")
dev.off()

# Save as Example
# dev.copy(jpeg,"src/central-tendency/mean/Dot Plot QJE-example.jpg")
# dev.off()
