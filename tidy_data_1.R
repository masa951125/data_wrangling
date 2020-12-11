library(dslabs)
library(dplyr)
library(tidyverse)
library(ggplot2)

data("gapminder")
view(gapminder)

system.file("extdata", package = "dslabs")
list.files("C:/Users/masay/Documents/R/win-library/4.0/dslabs/extdata")

filename <- file.path(path,"fertility-two-countries-example.csv")
wide_data <- read_csv(filename)
select(wide_data, country, '1960':'1967')

new_tidy_data <- wide_data %>% gather(year, fertility, '1960':'2015')
new_tidy_data

new_tidy_data %>% ggplot(aes(year, fertility, col=country))+
  geom_point()

fertility_data <- read_csv("fertility_data.csv")
new_fertility_data <- fertility_data %>% gather(year, fertility,'1960':'2015' )
Japan <- new_fertility_data %>% filter(`Country Name`=="Japan")
World <- new_fertility_data %>%filter(`Country Name`=="World")

ggplot(data=World,aes(x=year, y=fertility))+geom_point()