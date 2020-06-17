# Bar Graph with t-test bar

# Hobbs, W., & Lajevardi, N. (2019). Effects of divisive political campaigns on
#   the day-to-day segregation of arab and muslim americans.
#   The American Political Science Review, 113(1), 273. doi:10.1017/S0003055418000801

# Data: ibid, fig 1.

output_dir <- "output"

library(xts)
library(forecast)
library(strucchange)
library(dplyr)
library(plyr)
library(ggplot2)
library(rsvg)

# Build dataset
load("data/hobbs-2019.Rdata")
data <- subset(
  data,
  missing_coords=="False"
)

agg <- data %>%
  group_by(date) %>%
  dplyr::summarise(count = n_distinct(userid),
                   tweets = sum(tweets)
  )

## missing/partial data in collection (creates drop artifact)
agg <- subset(agg, !(as.character(as.Date(date)) %in% c(
  "2016-05-23","2016-05-24","2016-05-25",
  "2016-05-26","2016-05-27","2016-05-28","2016-05-29",
  "2016-05-30","2016-05-31"
)))

agg$avg.tweets <- with(agg, (tweets / count))

aggts.noscale <- xts(agg$count, order.by=agg$date)
df <- data.frame(date=index(aggts.noscale), val = coredata(aggts.noscale))

## This smoother dampens weekly seasonality --
## without it, breakpoints will identify the ends of weekends before events as
## the starts of the drops
fit.noscale <- tbats(
  aggts.noscale, use.box.cox=FALSE, use.trend=F,
  use.damped.trend=T, seasonal.periods=7
)

## For appendix tweet per day comparison
aggts <- xts(scale(agg$count), order.by=agg$date)
aggts.tweets <- xts(scale(agg$avg.tweets), order.by=agg$date)

fit <- tbats(
  aggts, use.box.cox=FALSE, use.trend=F,
  use.damped.trend=T, seasonal.periods=7
)
fit.tweets <- tbats(
  aggts.tweets, use.box.cox=FALSE, use.trend=F,
  use.damped.trend=T, seasonal.periods=7
)

## Red Trend: muslim ban mentions
trend <- read.csv(
  "data/hobbs-2019-2.csv"
)
trend$Date <- as.Date(trend$Date, "%m/%d/%y")
trend$log.posts <- with(trend, log(Total.Posts+1))

## Smoth Mean
smooth.mean <- fit.noscale$x[1,]

## Regression lines
prex <- mean(aggts.noscale[index(aggts.noscale) < "2015-12-07"])
## pre speech
prex <- predict(
  lm(smooth.mean[index(aggts.noscale) < "2015-12-07"] ~
       as.numeric(index(aggts.noscale)[index(aggts.noscale) < "2015-12-07"])
  )
)

# Save as example
svg(paste(output_dir,"Line Graph APSR 1.svg",sep = "/"), width = 8, height = 6)

plot(
  index(aggts.noscale),
  aggts.noscale,
  type="l", bty="n",
  xlab="Date", ylab="Number of geo-tagged users",
  col="purple"
)
lines(index(aggts.noscale), smooth.mean, lwd=3, col="purple")

## breakpoints
abline(
  ## 2 breaks
  v=index(aggts.noscale)[breakpoints(smooth.mean~1,breaks=2)$breakpoints],
  lwd=3, lty=2
)

mtext(
  text=as.Date(
    index(aggts.noscale)[breakpoints(smooth.mean~1,breaks=2)$breakpoints]
  ),
  at=index(aggts.noscale)[breakpoints(smooth.mean~1, breaks=2)$breakpoints])

segments(
  x0=as.POSIXct("2015-09-01"), x1=as.POSIXct("2015-12-05"),
  y0=prex[1], y1=rev(prex)[1],
  lwd=3
)
## speech to election
midx <- predict(
  lm(smooth.mean[index(aggts.noscale) >= "2015-12-07"
                 & index(aggts.noscale) < "2016-11-08"] ~
       as.numeric(
         index(aggts.noscale)[index(aggts.noscale) >= "2015-12-07"
                              & index(aggts.noscale) < "2016-11-08"]
       )
  )
)
segments(
  x0=as.POSIXct("2015-12-07"), x1=as.POSIXct("2016-11-06"),
  y0=midx[1], y1=rev(midx)[1],
  lwd=3
)
## post election
postx <- predict(
  lm(smooth.mean[index(aggts.noscale) >= "2016-11-08"] ~
       as.numeric(
         index(aggts.noscale)[index(aggts.noscale) >= "2016-11-08"]
       )
  )
)
segments(
  x0=as.POSIXct("2016-11-08"), x1=as.POSIXct("2017-02-01"),
  y0=postx[1], y1=rev(postx)[1], lwd=3
)

## muslim ban mentions
with(trend, lines(as.POSIXct(Date), scale(log.posts)*20 + 100, col="red"))

## annotation
abline(v=as.POSIXct("2015-12-07"), col="red", lwd=3)
abline(v=as.POSIXct("2016-11-08"), col="red", lwd=3)
abline(v=as.POSIXct("2017-01-27"), col="red", lwd=3)
legend(
  as.POSIXct("2016-01-01"), 170,
  legend=c(
    "'muslim' AND 'ban' (log-scaled, all Twitter mentions)",
    "geo-tagged users with Arabic names"
  ),
  bty="n", col=c("red","purple"), lwd=2, cex=0.8
)

while (!is.null(dev.list()))  dev.off()

# Save as example
# rsvg_png(svg = "output/Line Graph APSR 1.svg",
#      file = "src/distribution/Line Graph APSR 1-example.png",
#      width = 2400,
#      height = 1800)
