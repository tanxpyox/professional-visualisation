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

