library(tidyverse)
library(dslabs)
data("polls_us_election_2016")

polls_us_election_2016$startdate %>% head
as.numeric(polls_us_election_2016$startdate)

polls_us_election_2016 %>% 
  filter(pollster == "Ipsos" & state =="U.S.") %>%
  ggplot(aes(startdate, rawpoll_trump)) +
  geom_line()

library(lubridate)
set.seed(2)
dates <- sample(polls_us_election_2016$startdate,10) %>% sort
dates

dates_df <- data.frame(date = days(dates),
                       month =month(dates),
                       day =day(dates),
                       year =year(dates))
dates_df
x <- c(20090101, "2009-01-02", "2009 01 03", "2009-1-4",
       "2009-1, 5", "Created on 2009 1 6", "200901 !!! 07")
x <- "09/01/02"
ymd(x)
myd(x)
dym(x)
now()
now("GMT")

x <- c("12:34:56")
hms(x)

x <- "Nov/2/2012 12:34:56"
mdy_hms(x)
mdy_hms(x,tz= "Asia/Tokyo")