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
ggplot(data=Japan,aes(x=year,y=fertility))+geom_point()

Japan_World <- new_fertility_data %>% 
  filter(`Country Name`%in% c("Japan", "World"))
Japan_World$

ggplot(data=Japan_World, aes(x=year,y=fertility,col= `Country Name`))+
  geom_point() +
  ggtitle("Fertility Comparison")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# how to use "separate","unite" function
path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, "life-expectancy-and-fertility-two-countries-example.csv")
raw_dat <-read_csv(filename)

dat <- raw_dat %>% gather(key, value, -country)
dat %>% separate(key, c("year", "variable_name"))
new_dat <- dat %>% separate(key, c("year", "variable_name"), 
                 sep = "_", extra="merge") %>%
  spread(variable_name, value)

ggplot(data=new_dat)+
  geom_point(aes(year, fertility,col=country))+
  geom_point(aes(year, life_expectancy,col=country))+
  theme(axis.text.x = element_text(angle=90, hjust=1))

#Q
co2
co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))

CO2_tidy <- gather(co2_wide,month,co2,-year)
CO2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()

library(dslabs)
data(admissions)
dat <- admissions %>% select(-applicants)

dat %>% spread(gender, admitted)

tmp <- gather(admissions, key, value, admitted:applicants)
tmp

tmp2 <- unite(tmp, column_name, c(key, gender))
new_tmp2 <- spread(tmp2, column_name, value)



