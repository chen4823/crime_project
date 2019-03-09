## data exploration and visualization 

require(tidyverse)
require(lubridate)

X <- read.csv("Categorized crime data.csv", as.is=TRUE)

## use functions from lubridate package
X$dttime <- mdy_hm(X$datetime)
X$dayofweek <- wday(X$dttime, label=TRUE)
X$month <- month(X$dttime, label=TRUE)
X$year <- ordered(year(X$dttime))

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
        shift <- "night")
    }
    shift
}
X$shift <- ordered(sapply(X$dttime, determine.shift),
                   levels=c("day","afternoon","night"))

X$doy <- yday(X$dttime)  # day number of the year

## count the number of crimes per shift (per day)
crime.counts <- tapply(X$shift, list(X$shift, X$doy), length)
crime.counts[ is.na(crime.counts) ] <-  0
CC <- data.frame(doy=rep(1:365, 3),
                 shift=rep(levels(X$shift), each=365))
CC$num.crimes <- as.vector(t(crime.counts))

## plots using ggplot2

ggplot(CC) +
    geom_histogram(aes(x=num.crimes, fill=shift), position="dodge") +
    labs(title="Number of crimes per shift")

ggplot(CC) +
    geom_violin(aes(x=shift, y=num.crimes), draw_quantiles=c(.5),
                trim=FALSE) +
    labs(title="Distribubtion of number of crimes per shift")


ggplot(X) +
    geom_bar(aes(x=dayofweek, fill=offensetype), position="dodge") +
    facet_wrap(~ region)

grouped <- filter(X, year %in% c("2017","2018")) %>%
    group_by(month, year) %>%
    summarise(
        count = n(),
        )

ggplot(grouped, aes(x=month, y=count, color=year)) +
    geom_point() +
    geom_smooth()

