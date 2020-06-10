# Bar graph with CI

# Arthur Blouin and Sharun W. Mukand, "Erasing Ethnicity? Propaganda, Nation Building, and Identity in Rwanda," 
#   Journal of Political Economy 127, no. 3 (2019): 1046

df <- read.csv("data/JFE")
library(ggplot2)
library(magrittr)
library(ggpubr)
library(ggsignif)
library(scales)

