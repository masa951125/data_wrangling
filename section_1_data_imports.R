#show working directory
getwd()

path <- system.file("extdata", package="dslabs")
path


list.files(path)

filename <- "murders.csv"
fullpath <- file.path(path, filename)
file.copy(fullpath, getwd())

file.exists(filename)

library(readr)
library(dslabs)
library(tidyverse)    # includes readr
library(readxl)

read_lines("murders.csv", n_max =3)

dat <- read_csv(filename)
dat <- read_csv(fullpath)

path <- system.file("extdata", package ="dslabs")
files <-list.files(path)
files

dat2 <- read.csv(filename)
class(dat2)

url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
dat <- read_csv(url)
download.file(url,"murder2.csv")

tmp_filename <-tempfile()
download.file(url, tmp_filename)
dat <-read_csv(tmp_filename)
file.remove(tmp_filename)

#problem
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data" 
dat <-read_csv(url, col_names=F)
