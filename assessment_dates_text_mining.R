Sys.setenv(LANG = "en")

library(dslabs)
library(lubridate)
options(digits = 3)  

dates <- c("09-01-02", "01-12-07", "02-03-04")

data(brexit_polls)
x <-brexit_polls$startdate
sum(month(x)==4)

y <-brexit_polls$enddate
sum(round_date(y,unit = "week")=="2016-06-12")

library(lubridate)
table(weekdays(brexit_polls$enddate))

data(movielens)
head(movielens$timestamp)
x <- as_datetime(movielens$timestamp)
y <- table(year(x))
z <- table(hour(x))

names(which.max(z))
names(which.max(y))

#text_mining
install.packages("gutenbergr")
library(tidyverse)
library(gutenbergr)
library(tidytext)
options(digits = 3)

names(gutenberg_metadata)
gutenberg_metadata %>% filter(str_detect(title,"Pride and Prejudice"))
x <- gutenberg_metadata %>% filter(str_detect(title,"Pride and Prejudice"))

gutenberg_metadata %>% 
gutenberg_works(title=="Pride and Prejudice")$gutenberg_id

P_P_text <- gutenberg_download(gutenberg_id = 1342)
library(tidytext)
words <-P_P_text %>% unnest_tokens(word,text)

#point to a different library for the right version of the book
library(janeaustenr)
#load "book" with the ojbect austen_books with just Pride & Prejudice
book <-  austen_books() %>% filter(book == "Pride & Prejudice")

words <- book %>% unnest_tokens(word,text)
count(words)

words %>% filter(!word %in% stop_words$word ) %>%count

stop_words <- words %>% anti_join(stop_words)
sum(!str_detect(stop_words$word,"\\d+"))

no_digit_stop_words <- stop_words %>% filter(!str_detect(stop_words$word,"\\d+"))
no_digit_stop_words %>% 
  count(word) %>%
  top_n(30, n) %>%
  mutate(word = reorder(word, n)) %>%
  arrange(desc(n))

x <- data.frame(table(no_digit_stop_words$word))
sum(x$Freq>100)

no_digit_stop_words %>%
  count(word) %>%
  filter(n>100) %>%
  nrow
words <-no_digit_stop_words
afinn <- get_sentiments("afinn")

afinn_sentiments <- words %>%
  left_join(afinn, by="word")

affin_sentiments <-words %>%
  inner_join(afinn, by="word")
count(affin_sentiments)

sum(affin_sentiments$value>0)/count(affin_sentiments)

afinn_sentiments <- affin_sentiments
sum(afinn_sentiments==4)