# Scatter plot, multipanel with trendline

# CLINTON, J. D., & SANCES, M. W. (2018). The politics of policy:
#   The initial mass political effects of medicaid
#   expansion in the states. The American Political Science Review, 112(1), 176.
#   doi:10.1017/S0003055417000430

# Data: Generated

library(ggplot2)
library(magrittr)
library(gridExtra)
output_dir = "output"
total = 2000
# Generate dataset
df <- data.frame(
      x = rep(0,total),
      y = rep(0,total),
      g = rep(0,total))

for (i in 1:total){
  df$x[i] = rnorm(1)
  df$y[i] = rnorm(1)
  df$g[i] = ifelse(floor((i-1)/(total/8))%%2, "Expanding", "Not Expanding")
}

titles = c("Registration 2014-2010",
           "Registration 2016-2010",
           "Turnout 2014-2010",
           "Turnout 2016-2010")

# Plot function

plot <- function(df){
  p = list()
  for (i in 1:4) {
    tmp <- ggplot(df[seq((i-1)*total/4+1,(i)*total/4,1),], aes(x,y)) +
      geom_point(alpha=.2) +
      geom_smooth(method = "lm") +
      theme_bw() +
      ggtitle(label = titles[i]) +
      labs(
        y = "Change in Participation",
        x = "Potential Eligibility"
      ) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = NA, color = NA),
        strip.text = element_text(size= 12),
        axis.line = element_line(size=0.5),
        panel.border=element_blank()
      ) +
      facet_wrap(~g, nrow = 1)

    p = c(p,list(tmp))
  }
  return(list(plots=p,num=4))
}
output <- plot(df)

# Save individual panels
for(i in 1:4) {
  ggsave(path=output_dir, filename=paste("Scatter Plot APSR 2", LETTERS[i], ".png", sep=""), output$plots[[i]], width=8, height=6)
}


# Save combined panels
ggsave(path=output_dir, filename="Scatter Plot APSR 2Combined.png", grid.arrange(grobs = output$plots), width=8, height=6)

# Save as example
# ggsave(path="src/relationship", filename="Scatter Plot APSR 2Combined.png", grid.arrange(grobs = output$plots), width=8, height=6)
