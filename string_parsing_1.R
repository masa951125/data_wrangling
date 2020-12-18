
url <- "https://en.wikipedia.org/w/index.php?title=Gun_violence_in_the_United_States_by_state&direction=prev&oldid=810166167"

murders_raw <- read_html(url) %>% 
  html_nodes("table")%>%
  html_table() %>%
  .[[1]] %>%
  setNames(c("state", "population", "total", "murder_rate"))

head(murders_raw)
class(murders_raw$population)
class(murders_raw$total)