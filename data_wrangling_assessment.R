library(rvest)
library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[5]] %>% html_table(fill = TRUE)

polls <- polls %>% setNames(c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes"))
n(polls$remain)
sum(str_detect(polls$remain,"\\d%" ))

as.numeric(polls$remain)
parse_number(polls$remain)/100
as.numeric(str_replace(polls$remain, "%", ""))/100
str_remove(polls$remain, "%")   

parse_number(polls$undecided, "N/A", "0")

temp <- str_extract_all(polls$dates,"\\d+\\s[a-zA-Z]+")