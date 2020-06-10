# Event Study Plot w/ CI

# Proto, Rustichini, and Sofianos, "Intelligence, Personality, 
#  and Gains from Cooperation in Repeated Interactions",
#  Journal of Political Economy, 1363.

# Data from Quek (2017) "Rationalist Experiments on War"

library(haven)
master <- read_dta("data/quek-rw-2017.dta")

library(dplyr)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(ggsignif)

df <- master %>%
      group_by(period) %>%
      summarize(
        mean = mean(war,na.rm = T)
      )

