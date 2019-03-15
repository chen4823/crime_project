## data manipulation

require(tidyverse)
require(lubridate)

X <- read.csv("Categorized crime data.csv", as.is=TRUE)

## use functions from lubridate package
X$dttime <- mdy_hm(X$datetime)
X$dayofweek <- wday(X$dttime, label=TRUE)
X$month <- month(X$dttime, label=TRUE)

## there are three observations where the year is < 2017.
## just change it to 2017.
X$year <- year(X$dttime)
X$year[ X$year < 2017 ] <- 2017
X$year <- ordered(X$year)   # turn year into a factor

## character to factor
X$offensetype <- ordered(X$Offensetype, levels=c("normal","emergency"))
X$region <- factor(X$Region)

determine.shift <- function(dt) {
    ## 7am to 5pm is day
    ## 3pm to 1am is afternoon
    ## 9:30pm to 7:30am is overnight
    ##
    ## there is overlap but make the assignment as
    ## 7am to 4pm day
    ## 4pm to 11pm afternoon
    ## 11pm to 7am night
    shift <- "afternoon"
    hr <- hour(dt)
    if (hr < 7) {
        shift <- "night"
    } else if (hr < 16) {
        shift <- "day"
    } else if (hr >= 23) {
        shift <- "night"
    }
    shift
}
X$shift <- ordered(sapply(X$dttime, determine.shift),
                   levels=c("day","afternoon","night"))

X$doy <- yday(X$dttime)  # day number of the year

## count the number of crimes per shift (per day).
## there are 2 years of data (2017, 2108) so divide
## by 2 to get the average
crime.counts <- with(X, tapply(shift, list(doy, shift),
                               function(x) length(x)/2))
crime.counts[ is.na(crime.counts) ] <- 0
CC <- data.frame(doy=rep(1:365, 3),
                 shift=rep(levels(X$shift), each=365))
CC$num.crimes <- as.vector(t(crime.counts))
