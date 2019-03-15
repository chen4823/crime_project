## plots using ggplot2

source("data.R")  # run code to clean up data

ggplot(CC) +
    geom_histogram(aes(x=num.crimes, fill=shift), position="dodge") +
    labs(title="Number of crimes per shift")

ggplot(CC) +
    geom_violin(aes(x=shift, y=num.crimes), draw_quantiles=c(.5),
                trim=FALSE) +
    labs(title="Distribubtion of number of crimes per shift")

ggplot(X) +
    geom_bar(aes(x=dayofweek, fill=offensetype), position="dodge") +
    facet_wrap(~ region) +
    labs(title="Crime count by day of week", subtitle="Total for 2017 and 2018")

grouped <- filter(X, year %in% c("2017","2018")) %>%
    group_by(month, year) %>%
    summarise(
        count = n(),
        )

ggplot(grouped, aes(x=month, y=count, color=year)) +
    geom_point() +
    geom_smooth() +
    labs(title="Crime count by month and year")


