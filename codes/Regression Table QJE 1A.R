# Graphical Regression Table

# Amy Finkelstein, Matthew Gentzkow, Heidi Williams,
#   Sources of Geographic Variation in Health Care: Evidence From Patient Migration,
#   The Quarterly Journal of Economics, Volume 131, Issue 4, November 2016, Pages 1681–1726,
#   https://doi.org/10.1093/qje/qjw023

# Data: Emulated from Ibid, Fig VIII

library(ggplot2)
output_dir="output"

df <- data.frame(
  order = rev(paste(1:5)),
  label=c("Hospital Compare Score",
          "Specialist Per Capita",
          "PCP Per Capita",
          "Hospital Beds Per Capita",
          "Non-Profit Hospitals"
          ),
  pt = c(0.06,0.01,0.012,-0.02,0.011),
  error = c(.02, .02, .02, .04, .03)
)

p <- ggplot(df, aes(order, pt)) +
  geom_errorbar(aes(ymax = pt + error, ymin = pt - error), width = 0.2, color = 'darkgrey') +
  geom_point() +
  scale_y_continuous(
                     sec.axis = sec_axis(~., "Bivariate OLS")
                     ) +
  annotate(x=5.8,y=-.145,geom="text" , label = "Place Characteristics", size = 3.5) +
  scale_x_discrete(
    labels = df$label
  ) +
  xlab("Place Characteristics") +
  geom_hline(aes(yintercept =0)) +
  coord_flip(ylim = c(-.1,.1), clip = 'off') +
  theme_bw() +
  theme(
    axis.title.y.left = element_blank(),
    axis.title.x.bottom = element_blank(),
    axis.title.x.top = element_text(size=14),
    axis.text.x.top = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# Note: do not change the width/height. Changing them could cause the axis title to shift.
ggsave(path=output_dir, filename="Regression Table QJE 1.png", width=6, height=4)
# Save as Example
ggsave(path="src/relationship/", filename="Regression Table QJE 1A-example.png", width=6, height=4)
