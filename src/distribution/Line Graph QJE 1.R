# Line graph, Multiple normal distribution curves

# M. Caridad Araujo, Pedro Carneiro, Yyannú Cruz-Aguayo, and Norbert Schady,
#   “Teacher Quality and Learning Outcomes in Kindergarten,”
#   The Quarterly Journal of Economics 131, no. 3 (2016): 1429

# Data: emulated from ibid, Fig 1

library(ggplot2)
library(dplyr)
library(tidyr)
output_dir="output"

df <- data.frame(line = c("Study Sample", "National Sample", "US Sample"),
                 mean = c(3.7,3.75,4.5),
                 sd = c(0.32,0.33,0.66))

x <- seq(2, 6, by = 0.001)

pdfs <- mapply(dnorm, mean = df$mean, sd = df$sd, MoreArgs = list(x = x),
               SIMPLIFY = FALSE)

# add group names
names(pdfs) <- df$line

# convert list to dataframe
pdfs <- do.call(cbind.data.frame, pdfs)
pdfs$x <- x

# convert dataframe to tall format
tall_df <- gather(pdfs, line, density, -x)
plot_df <- tall_df %>% filter(line!="NA")

# build plot
p <- ggplot(plot_df, aes(linetype = line, x = x, y = density)) +
      geom_line() +
      coord_cartesian(ylim = c(0, 1.2)) +
     theme_bw() +
     labs(
       x = "CLASS Score",
       y = "Density"
     ) +
     scale_linetype_manual(values = c(1,5,4)) +
     theme(
       panel.grid.major.x = element_blank(),
       panel.grid.minor = element_blank(),
       legend.position = "bottom",
       legend.title=element_blank(),
       legend.direction = "vertical",
       legend.background = element_blank(),
       legend.box.background = element_rect(colour = "black")
     )

ggsave(path=output_dir, filename="Line Graph QJE 1.png", width=8, height=6)

# Save as Example
# ggsave(path='src/distribution', filename='Line Graph QJE 1-example.png', width=8, height=6)
