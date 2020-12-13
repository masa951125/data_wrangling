library(tidyverse)
library(ggrepel)
library(dslabs)
library(dplyr)

ds_theme_set()
data(murders)
head(murders)

data(polls_us_election_2016)
head(results_us_election_2016)
identical(results_us_election_2016$state, murders$state)

tab1 <- slice(murders, 1:6) %>% select(state, population)
tab2 <- slice(results_us_election_2016, c(1:3, 5, 7:8)) %>% select(state, electoral_votes)

results_us_election_2016<- results_us_election_2016[order(results_us_election_2016$state),]
rownames(results_us_election_2016) <- 1:nrow(results_us_election_2016)

left_join(tab1, tab2)
right_join(tab1,tab2)
full_join(tab1,tab2)
inner_join(tab1,tab2)

semi_join(tab1,tab2)
anti_join(tab1,tab2)

bind_cols(a=1:3,b=4:6)
bind_rows(a=1:3,b=4:6)

tab <- left_join(murders, results_us_election_2016, by = "state") %>%
  select(-others) %>% 
  rename(ev = electoral_votes)

tab1 <- tab[, 1:3]
tab2 <- tab[, 4:6]
tab3 <- tab[, 7:8]

new_tab <- bind_cols(tab1,tab2,tab3)

intersect(tab1,tab2)

library(Lahman)
install.packages("Lahman")
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()

Master %>% as_tibble()

LahmanData

top_names <- top %>% left_join(Master) %>%
  select(playerID, nameFirst, nameLast, HR)

Salaries %>% as_tibble()

top_salaries <- Salaries %>% filter(yearID == 2016) %>%
  right_join(top_names)%>%
  select(nameFirst, nameLast, teamID, HR, salary)

award <- AwardsPlayers %>% filter(yearID ==2016)
top_names_ID <- top_names %>% select(playerID)
award_ID <- award %>% select(playerID)

intersect(top_names_ID, award_ID)
setdiff(award_ID, top_names_ID)
length(setdiff(award_ID, top_names_ID))

length(intersect(award$playerID, top_names$playerID))
length(setdiff(award$playerID, top_names$playerID))
