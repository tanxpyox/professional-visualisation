# Theme setup

library(ggplot2)

thm <- theme_bw() +
  theme(axis.text = element_text(colour = "black", size = 8),
        axis.title = element_text(colour = "black", size = 8),
        plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(colour = "black", size = 7, hjust = 0),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank()
  )

thm_legend_title <- theme_bw() +
  theme(axis.text = element_text(colour = "black", size = 8),
        axis.title = element_text(colour = "black", size = 8),
        plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(colour = "black", size = 7, hjust = 0),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_text(colour = "black", size = 7, hjust = 0),
        legend.position = "top",
        legend.key.width = unit(1.25, "cm")
  )
