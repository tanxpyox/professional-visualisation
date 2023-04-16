# 3D bar plot

# Munshi, K., & Rosenzweig, M. (2016). Networks and misallocation:
#   Insurance, migration, and the rural-urban
#   wage gap. The American Economic Review, 106(1), 52.
#   doi:10.1257/aer.20131365

# Data: USA 2020 (protected)

library(epade)
library(magrittr)
library(DescTools)

# Load data
load("data/protected/usa-2020-cleaned.Rda")
output_dir = "output"
df$uni = (df$edu>=5)
df %<>% group_by(uni,except) %>% summarise(mean = mean(nativist, na.rm=T)) %>% na.omit()

uni.name <- c("FALSE" = "Primary or Secondary", "TRUE" = "Education: Undegrad+")
dv.name <- c("hawk" = "Hawkish?", "nativist" = "Nativist?", "trust" = "Trust?")

xt<- xtabs(mean~uni+except,df)
m <- DescTools::as.matrix.xtabs(xt)

png(paste(output_dir,"Bar Graph AER 1.png",sep="/"), units = "in", width = 8, height= 6, res=300)

bar3d.ade(m, wall=2,
          xlab='Treatment Group', ylab='Preference', zlab='Uni?', alpha=1,
          main = 'Nativist: Subset Analysis')

dev.off()
