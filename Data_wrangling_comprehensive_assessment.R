library(tidyverse)
library(pdftools)
options(digits = 3)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
system("cmd.exe", input = paste("start", fn))
txt <- pdf_text(fn)
head(txt)
summary(txt)

x <- txt[9] %>% str_split("\n")
class(x)
length(x)
s <-x[[1]]
class(s)
length(s)
s <-str_trim(s)

str_which(s, "2015")
s[2]
s[24]

header_index <- str_which(s, "2015")[1]

tmp <-str_split(s[2],"\\s+", simplify = T)
month <- tmp[1]
header <-tmp[-1]

tail_index <-str_which(s,"Total")
s[35]

n <-str_count(s,"\\d+")


#header_index 2, tail_index 35 35-2-1-2=30

out <-c(1:header_index, tail_index:length(s), which(n==1))
length(s[-out])
s <-s[-out]
s <-str_remove_all(s,"[^\\d\\s]")
s <- str_split_fixed(s, "\\s+", n = 6)[,1:5]
tab <- as.numeric(s) 

colnames(s) <-c("day","2015","2016","2017","2018")

tab <- s %>% mutate_if(is.character, as.numeric)

mean(as.numeric(s$X2015))
mean(as.numeric(s$X2016))
mean(as.numeric(s$X2017[1:19]))
mean(as.numeric(s$X2017[20:30]))

tab <- s %>% 
  as_tibble() %>% 
  setNames(c("day", header)) %>%
  mutate_all(as.numeric)

tab <- tab %>% gather(year, deaths, -day) %>%
  mutate(deaths = as.numeric(deaths))

tab %>% ggplot(aes(x=day,y=deaths,col=year))+
  geom_point()+ geom_line()
  geom_vline(xintercept = 20)
fn

