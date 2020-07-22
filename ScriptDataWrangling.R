library(dslabs)
library(tidyverse)
path <- system.file('extdata', package = 'dslabs')
list.files(path = path)
filename <- 'murders.csv'
fullpath <- file.path(path, filename)
file.copy(fullpath, getwd())


# Paths and the Working Directory -----------------------------------------

# see working directory
getwd()
# change your working directory
setwd()
# set path to the location for raw data files in the dslabs package and list files
path <- system.file("extdata", package="dslabs")
list.files(path)
# generate a full path to a file
filename <- "murders.csv"
fullpath <- file.path(path, filename)
fullpath
# copy file from dslabs package to your working directory
file.copy(fullpath, getwd())
# check if the file exists
file.exists(filename)
# The readr and readxl Packages -------------------------------------------

library(dslabs)
library(tidyverse)    # includes readr
library(readxl)
# inspect the first 3 lines
read_lines("murders.csv", n_max = 3)
# read file in CSV format
dat <- read_csv(filename)
#read using full path
dat <- read_csv(fullpath)
head(dat)
#Ex:
path <- system.file("extdata", package = "dslabs")
files <- list.files(path)
files
filename <- "murders.csv"
filename1 <- "life-expectancy-and-fertility-two-countries-example.csv"
filename2 <- "fertility-two-countries-example.csv"
dat=read_csv(file.path(path, filename))
dat1=read_csv(file.path(path, filename1))
dat2=read_csv(file.path(path, filename2))
# Bajar archivos de internet ----------------------------------------------


url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
dat <- read_csv(url)
download.file(url, "murders.csv")
tempfile()
tmp_filename <- tempfile()
download.file(url, tmp_filename)
dat <- read_csv(tmp_filename)
file.remove(tmp_filename)


# ASSESSMENT --------------------------------------------------------------

url <- "http://mlr.cs.umass.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
data <- read_csv(url, col_names = FALSE)
head(data)
dim(data)

url2 <- "ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_annmean_mlo.txt"
download.file(url2, 'co2_mauna_loa.txt')
data17 <- read_table(url2,skip = 56)
head(data17)
dim(data17)
nrow(data17)
read_lines(url2, n_max = 60)

#trabajo del 12 de diciembre
d <- read_csv('times.csv')
tidy_data <- d %>%
  gather(key = 'key', value = 'value', -age_group)# %>%
tidy_data2 <- tidy_data%>% separate(col = key, into = c('year', 'variable_name'), sep = '_')# %>% 
tidy_data3 <- tidy_data2%>%spread(key = variable_name, value = value)

tidy_data <- d %>%
  gather(key = 'key', value = 'value', -age_group) %>%
  separate(col = key, into = 'year', sep = '_') %>% 
  spread(key = year, value = value)

stats <- read_table2('stats.txt')
tidy_data <- stats %>%
  separate(col = key, into = c("player", "variable_name"), sep = "_", extra = "merge")%>%
  spread(key = variable_name, value = value)
  
  tidy_data2 <- stats %>%
    separate(col = key, into = c("player", "variable_name1", "variable_name2"), sep = "_", fill = "right") %>% 
    unite(col = variable_name, variable_name1, variable_name2, sep = "_") %>% 
    spread(key = variable_name, value = value)

    tidy_data <- stats %>%
    separate(col = key, into = c("player", "variable_name"), sep = "_") %>% 
    spread(key = variable_name, value = value)
#RESHAPING DATA ASSESSMENT 2
    
co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))
co2_tidy <- gather(co2_wide,month,co2,-year)    

co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()

#q12
library(dslabs)
data(admissions)
dat <- admissions %>% select(-applicants)

#q13
dat_tidy <- spread(dat, gender, admitted)

tmp <- gather(admissions, key, value, admitted:applicants)
tmp
tmp2 <- unite(tmp, column_name, c(key, gender))
spread(tmp2, key = column_name, value = value)

# ASSESSMENT: COMBINING TABLES --------------------------------------------

library(tidyverse)
library(Lahman)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()
Master %>% as_tibble()

top_names <- top %>%left_join(Master)  %>%
  select(playerID, nameFirst, nameLast, HR)
top_names%>% as.tibble()

Salaries%>%as_tibble()

top_salary <- Salaries %>% filter(yearID == 2016) %>%
  right_join(top_names) %>%
  select(nameFirst, nameLast, teamID, HR, salary)
top_salary%>%as_tibble()
AwardsPlayers%>%as_tibble()
aw216 <- AwardsPlayers%>%
  filter(yearID == 2016)%>%
  select(playerID)
top%>%select(playerID)%>%
  intersect(aw216)
topides <- top%>%select(playerID)
setdiff(aw216, topides)

install.packages('rvest')
library(rvest)


# INTENTOS PARA BAJAR LISTA DE AUTOSERVICIOS ------------------


# import a webpage into R
library(rvest)
library(tidyverse)
#urlas <- "http://webserver1.siiaa.siu.buap.mx:81/autoservicios/bwlkfcwl.P_FacClaListSum"    # no sirve
autserv <- read_html('2019_lista.html') #se selecciona todo el archivo y se pega en el bloc de notas.
class(autserv)
nodesal <- html_nodes(autserv,"table")
length(nodesal)
#Seleccioné los correos y los guardé como .csv. Lo abrí con la utilería de Rstudio en la variable correos
head(correos)
t(correos)
emails <- gather(correos, key = 'email') #ahora hay que usar pivot_longer
lista <- html_table(nodesal[[9]])
lista <- lista[-1, 2:3]
lista <- separate(lista, col = X2, into = c('lastname', 'firstname'), sep = ',')
encab <- c('username',	'password',	'firstname',	'lastname',	'email',	'course1',	'group1')
listafin <- lista%>%
  rename(username = X3)%>%
  mutate(password = username, email = emails$value, course1 = rep('MCP-415', length.out = nrow),
         group1 = rep('LVL', length.out = nrow))%>%
  select(username, password, firstname, lastname, email, course1, group1)
#Guardar listafin
write.csv(listafin, '2020_1_Subirusuarios_LVL.csv', row.names = FALSE)
library(rvest)
url <- "https://en.wikipedia.org/w/index.php?title=Gun_violence_in_the_United_States_by_state&oldid=919733895"    # permalink
h <- read_html(url)
class(h)
h
tab <- h %>% html_nodes("table")
tab <- tab[[2]]
tab <- tab %>% html_table
class(tab)


# Assessment: Web Scraping ------------------------------------------------

library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
nodes <- html_nodes(h, "table")
html_text(nodes[[8]])
html_table(nodes[[8]])
html_table(nodes[[21]])

tab1 <- html_table(nodes[[10]])
tab2 <- html_table(nodes[[19]])
library(dplyr)
titcols <- tab1[1,2:4]
titcols <- as.character(titcols)
tab1 <- select(tab1, -X1)
tab1 <- tab1[-1,]
tab2 <- tab2[-1,]
colnames(tab1) <- titcols
colnames(tab2) <- titcols
dim(full_join(tab1,tab2, by = 'Team'))

#question 4 and 5
library(rvest)
library(tidyverse)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
h2 <- read_html(url)
tab <- html_nodes(h2, 'table')
length(tab)
# Find the first table that has 9 columns with the first column named "Date(s) conducted"
dim(html_table(tab[[5]], fill = TRUE))
colnames(html_table(tab[[5]], fill = TRUE))


# STRING PARSING ----------------------------------------------------------

library(rvest)
# read in raw murders data from Wikipedia
url <- "https://en.wikipedia.org/w/index.php?title=Gun_violence_in_the_United_States_by_state&direction=prev&oldid=810166167"
murders_raw <- read_html(url) %>%
  html_node("table") %>%
  html_table()%>%
  setNames(c("state", "population", "total", "murder_rate"))
# inspect data and column classes
head(murders_raw)
class(murders_raw$population)
class(murders_raw$total)
library(tidyverse)

# detect whether there are commas
commas <- function(x) any(str_detect(x, ","))
murders_raw %>% summarize_all(funs(commas))
# replace commas with the empty string and convert to numeric
test_1 <- str_replace_all(murders_raw$population, ",", "")
test_1 <- as.numeric(test_1)

# parse_number also removes commas and converts to numeric
test_2 <- parse_number(murders_raw$population)
identical(test_1, test_2)
murders_new <- murders_raw %>% mutate_at(2:3, parse_number)
murders_new %>% head

cat(" LeBron James is 6'8\" ")

murders_new <- murders_raw %>% mutate_all( parse_number)


# Case Study 2: Reported Heights ------------------------------------------

library(tidyverse)
# load raw heights data and inspect
library(dslabs)
data(reported_heights)
class(reported_heights$height)
# convert to numeric, inspect, count NAs
x <- as.numeric(reported_heights$height)
head(x)
sum(is.na(x))
# keep only entries that result in NAs
reported_heights %>% mutate(new_height = as.numeric(height)) %>%
  filter(is.na(new_height)) %>% 
  head(n=10)
# calculate cutoffs that cover 99.999% of human population.
alpha <- 1/10^6
qnorm(1-alpha/2, 69.1, 2.9)
qnorm(alpha/2, 63.7, 2.7)
# keep only entries that either result in NAs or are outside the plausible range of heights
not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}
# number of problematic entries
problems <- reported_heights %>% 
  filter(not_inches(height)) %>%
  .$height
length(problems)
# 10 examples of x'y or x'y" or x'y\"
pattern <- "^\\d\\s*'\\s*\\d{1,2}\\.*\\d*'*\"*$"
str_subset(problems, pattern) %>% head(n=10) %>% cat
# 10 examples of x.y or x,y
pattern <- "^[4-6]\\s*[\\.|,]\\s*([0-9]|10|11)$"
str_subset(problems, pattern) %>% head(n=10) %>% cat
# 10 examples of entries in cm rather than inches
ind <- which(between(suppressWarnings(as.numeric(problems))/2.54, 54, 81) )
ind <- ind[!is.na(ind)]
problems[ind] %>% head(n=10) %>% cat

#Regex
# load stringr through tidyverse
library(tidyverse)
# detect whether a comma is present
pattern <- ","
str_detect(murders_raw$total, pattern) 
# show the subset of strings including "cm"
str_subset(reported_heights$height, "cm")
# use the "or" symbol inside a regex (|)
yes <- c("180 cm", "70 inches")
no <- c("180", "70''")
s <- c(yes, no)
str_detect(s, "cm") | str_detect(s, "inches")
str_detect(s, "cm|inches")
# highlight the first occurrence of a pattern
str_view(s, "\\d")
# highlight all instances of a pattern
str_view_all(s, "\\d")

# Character Classes, Anchors and Quantifiers ------------------------------

library(tidyverse)
# s was defined in the previous video
yes <- c("5", "6", "5'10", "5 feet", "4'11")
no <- c("", ".", "Five", "six")
s <- c(yes, no)
pattern <- "\\d"
# [56] means 5 or 6
str_view(s, "[56]")
# [4-7] means 4, 5, 6 or 7
yes <- as.character(4:7)
no <- as.character(1:3)
s <- c(yes, no)
str_detect(s, "[4-7]")

# ^ means start of string, $ means end of string (anchors)
pattern <- "^\\d$" #is read as start of the string followed by one digit followed by the end of the string.
yes <- c("1", "5", "9")
no <- c("12", "123", " 1", "a4", "b")
s <- c(yes, no)
str_view(s, pattern)

# curly braces define quantifiers: 1 or 2 digits 
pattern <- "^\\d{1,2}$"
yes <- c("1", "5", "9", "12")
no <- c("123", "a4", "b")
str_view(c(yes, no), pattern)
# combining character class, anchors and quantifier
pattern <- "^[4-7]'\\d{1,2}\"$"
yes <- c("5'7\"", "6'2\"",  "5'12\"")
no <- c("6,2\"", "6.2\"","I am 5'11\"", "3'2\"", "64")
str_detect(yes, pattern)
str_detect(no, pattern)


# Search and Replace with Regex -------------------------------------------

library(tidyverse)

# number of entries matching our desired pattern
pattern <- "^[4-7]'\\d{1,2}\"$"
sum(str_detect(problems, pattern))
# inspect examples of entries with problems
problems[c(2, 10, 11, 12, 15)] %>% str_view(pattern)
str_subset(problems, "inches")
str_subset(problems, "''")
# replace or remove feet/inches words before matching
pattern <- "^[4-7]'\\d{1,2}$"
problems %>% 
  str_replace("feet|ft|foot", "'") %>% # replace feet, ft, foot with ' 
  str_replace("inches|in|''|\"", "") %>% # remove all inches symbols
  str_detect(pattern) %>% 
  sum()

# R does not ignore whitespace
identical("Hi", "Hi ")

# \\s represents whitespace
pattern_2 <- "^[4-7]'\\s\\d{1,2}\"$"
str_subset(problems, pattern_2)

# * means 0 or more instances of a character
yes <- c("AB", "A1B", "A11B", "A111B", "A1111B")
no <- c("A2B", "A21B")
str_detect(yes, "A1*B")
str_detect(no, "A1*B")

# test how *, ? and + differ
data.frame(string = c("AB", "A1B", "A11B", "A111B", "A1111B"),
           none_or_more = str_detect(yes, "A1*B"),
           nore_or_once = str_detect(yes, "A1?B"),
           once_or_more = str_detect(yes, "A1+B"))

# update pattern by adding optional spaces before and after feet symbol
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
problems %>% 
  str_replace("feet|ft|foot", "'") %>% # replace feet, ft, foot with ' 
  str_replace("inches|in|''|\"", "") %>% # remove all inches symbols
  str_detect(pattern) %>% 
  sum()

# Groups with Regex -------------------------------------------------------

library(tidyverse)
# define regex with and without groups
pattern_without_groups <- "^[4-7],\\d*$"
pattern_with_groups <-  "^([4-7]),(\\d*)$"
# create examples
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
# demonstrate the effect of groups
s
str_detect(s, pattern_without_groups)
str_detect(s, pattern_with_groups)
# demonstrate difference between str_match and str_extract
str_match(s, pattern_with_groups)
str_extract(s, pattern_with_groups)
# improve the pattern to recognize more events
pattern_with_groups <-  "^([4-7]),(\\d*)$"
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
str_replace(s, pattern_with_groups, "\\1'\\2")
#final pattern
pattern_with_groups <-"^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"
# combine stringr commands with the pipe
str_subset(problems, pattern_with_groups) %>% head
str_subset(problems, pattern_with_groups) %>% 
  str_replace(pattern_with_groups, "\\1'\\2") %>% head

# Testing and Improving ---------------------------------------------------

library(tidyverse)
# function to detect entries with problems
not_inches_or_cm <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- !is.na(inches) &
    ((inches >= smallest & inches <= tallest) |
       (inches/2.54 >= smallest & inches/2.54 <= tallest))
  !ind
}
# identify entries with problems
problems <- reported_heights %>% 
  filter(not_inches_or_cm(height)) %>%
  .$height
length(problems)
converted <- problems %>% 
  str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
  str_replace("inches|in|''|\"", "") %>%  #remove inches symbols
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") ##change format
# find proportion of entries that fit the pattern after reformatting
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
mean(index)

converted[!index]    # show problems

# Assessment: String Processing Part 2
#question 4
s <- c("70", "5 ft", "4'11", "", ".", "Six feet")
pattern <- "\\d|ft"
str_view_all(s, pattern)

#question 6
animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[A-Z]$"
str_detect(animals, pattern)

#question 8
animals <- c("moose", "monkey", "meerkat", "mountain lion")
str_detect(animals, 'mo*')
str_detect(animals, 'mo?')

#question 10
problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")

problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.\\s](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")

#question 13

yes <- c("5 feet 7inches", "5 7")
no <- c("5ft 9 inches", "5 ft 9 inches")
s <- c(yes, no)

converted <- s %>% 
  str_replace("\\s*(feet|foot|ft)\\s*", "'") %>% 
  str_replace("\\s*(inches|in|''|\")\\s*", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
str_detect(converted, pattern)


# Separate with Regex -----------------------------------------------------

library(tidyverse)
# first example - normally formatted heights
s <- c("5'10", "6'1")
tab <- data.frame(x = s)
# the separate and extract functions behave similarly
tab %>% separate(x, c("feet", "inches"), sep = "'")
tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")
# second example - some heights with unusual formats
s <- c("5'10", "6'1\"","5'8inches")
tab <- data.frame(x = s)
# separate fails because it leaves in extra characters, but extract keeps only the digits because of regex groups
tab %>% separate(x, c("feet","inches"), sep = "'", fill = "right")
tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")

# String Splitting --------------------------------------------------------

library(tidyverse)
# read raw murders data line by line
filename <- system.file("extdata/murders.csv", package = "dslabs")
lines <- readLines(filename)
lines %>% head()
# split at commas with str_split function, remove row of column names
x <- str_split(lines, ",") 
x %>% head()
col_names <- x[[1]]
x <- x[-1]
# extract first element of each list entry
library(purrr)
map(x, function(y) y[1]) %>% head()
map(x, 1) %>% head()
# extract columns 1-5 as characters, then convert to proper format - NOTE: DIFFERENT FROM VIDEO
dat <- data.frame(parse_guess(map_chr(x, 1)),
                  parse_guess(map_chr(x, 2)),
                  parse_guess(map_chr(x, 3)),
                  parse_guess(map_chr(x, 4)),
                  parse_guess(map_chr(x, 5))) %>%
  setNames(col_names)
dat %>% head
# more efficient code for the same thing
dat <- x %>%
  transpose() %>%
  map( ~ parse_guess(unlist(.))) %>%
  setNames(col_names) %>% 
  as.data.frame() 
# the simplify argument makes str_split return a matrix instead of a list
x <- str_split(lines, ",", simplify = TRUE) 
col_names <- x[1,]
x <- x[-1,]
x %>% as_data_frame() %>%
  setNames(col_names) %>%
  mutate_all(parse_guess)

# Case Study: Extracting a Table from a PDF -------------------------------

# https://rafalab.github.io/dsbook/string-processing.html#case-study-3-extracting-tables-from-a-pdf

library("pdftools")
temp_file <- tempfile()
url <- "http://www.pnas.org/content/suppl/2015/09/16/1510159112.DCSupplemental/pnas.201510159SI.pdf"
download.file(url, temp_file)
txt <- pdf_text(temp_file)
file.remove(temp_file)

# Recoding ----------------------------------------------------------------

# https://rafalab.github.io/dsbook/string-processing.html#recode
# life expectancy time series for Caribbean countries
library(dslabs)
data("gapminder")
gapminder %>% 
  filter(region=="Caribbean") %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()
# display long country names
gapminder %>% 
  filter(region=="Caribbean") %>%
  filter(str_length(country) >= 12) %>%
  distinct(country) 
# recode long country names and remake plot
gapminder %>% filter(region=="Caribbean") %>%
  mutate(country = recode(country, 
                          'Antigua and Barbuda'="Barbuda",
                          'Dominican Republic' = "DR",
                          'St. Vincent and the Grenadines' = "St. Vincent",
                          'Trinidad and Tobago' = "Trinidad")) %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()


# Assessment Part 1: String Processing Part 3 -----------------------------

#Question 1
library(tidyverse)
s <- c("5'10", "6'1\"", "5'8inches", "5'7.5")
tab <- data.frame(x = s)

extract(data = tab, col = x, into = c("feet", "inches", "decimal"), 
        regex = "(\\d)'(\\d{1,2})(\\.\\d+)?")

#Question 2
schedule <- tibble(day = c('monday', 'tuesday'), staff = c('Mandy, Chris and Laura', 'Steve, Ruth and Frank'))

#opt1 (correcta)
tidy <- schedule %>% 
  mutate(staff = str_split(staff, ", | and ")) %>% 
  unnest(cols = c(staff))
#opt2
tidy <- separate(schedule, staff, into = c("s1","s2","s3"), sep = ',') %>% 
  gather(key = s, value = staff, s1:s3)
#opt3
tidy <- schedule %>% 
  mutate(staff = str_split(staff, ", | and ", simplify = TRUE)) %>% 
  unnest()

# Assessment Part 2: String Processing Part 3 -----------------------------

# Import raw Brexit referendum polling data from Wikipedia:
  
library(rvest)
library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[5]] %>% html_table(fill = TRUE)
# You will use a variety of string processing techniques learned in this section to reformat these data.

titpols <- c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes")
colnames(polls) <- titpols
dim(polls)
str_detect(polls$remain, '%$')
polls <- polls%>%
  filter(str_detect(remain, '%$'))
dim(polls)

#q 2
parse_number(polls$remain)/100
as.numeric(str_replace(polls$remain, "%", ""))/100

#q3
str_replace(polls$undecided, 'N/A','0')

#q4

temp <- str_extract_all(polls$dates, "\\d+\\s[a-zA-Z]{3,5}")
temp2 <- str_extract_all(polls$dates, "\\d{1,2}\\s[a-zA-Z]+")
temp3 <- str_extract_all(polls$dates, "[0-9]+\\s[a-zA-Z]+")
end_date <- sapply(temp, function(x) x[length(x)]) # take last element (handles polls that cross month boundaries)

# Dates and Times ---------------------------------------------------------

# https://rafalab.github.io/dsbook/parsing-dates-and-times.html

# inspect the startdate column of 2016 polls data, a Date type
library(tidyverse)
library(dslabs)
data("polls_us_election_2016")
polls_us_election_2016$startdate %>% head
class(polls_us_election_2016$startdate)
as.numeric(polls_us_election_2016$startdate) %>% head
# ggplot is aware of dates
polls_us_election_2016 %>% filter(pollster == "Ipsos" & state =="U.S.") %>%
  ggplot(aes(startdate, rawpoll_trump)) +
  geom_line()
# lubridate: the tidyverse date package
library(lubridate)
# select some random dates from polls
set.seed(2)
dates <- sample(polls_us_election_2016$startdate, 10) %>% sort
dates
# extract month, day, year from date strings
data.frame(date = dates, 
           month = month(dates),
           day = day(dates),
           year = year(dates))
month(dates, label = TRUE)    # extract month label
# ymd works on mixed date styles
x <- c(20090101, "2009-01-02", "2009 01 03", "2009-1-4",
       "2009-1, 5", "Created on 2009 1 6", "200901 !!! 07")
ymd(x)
# different parsers extract year, month and day in different orders
x <- "09/01/02"
ymd(x)
mdy(x)
ydm(x)
myd(x)
dmy(x)
dym(x)
now()    # current time in your time zone
now("GMT")    # current time in GMT
now() %>% hour()    # current hour
now() %>% minute()    # current minute
now() %>% second()    # current second
# parse time
x <- c("12:34:56")
hms(x)
#parse datetime
x <- "Nov/2/2012 12:34:56"
mdy_hms(x)

# Assessment Part 1: Dates, Times, and Text Mining ------------------------

#q 3
library(tidyverse)
library(lubridate)
data("brexit_polls")
names(brexit_polls)
brexit_polls %>%
  count(month(startdate) == 4)

brexit_polls %>%
count(round_date(enddate, unit = 'week') == '2016-06-12')

brexit_polls%>%
  mutate(dias = weekdays(enddate))%>%
  group_by(dias)%>%
  summarise(n = n())%>%
  arrange(n)%>%
  select(dias, n)
#Mejor:
table(weekdays(brexit_polls$enddate))  

#Q 5
data("movielens")
names(movielens)
head(movielens)
poranio <- table(year(as_datetime(movielens$timestamp)))
names(which.max(poranio))
porhora <- table(hour(as_datetime(movielens$timestamp)))
names(which.max(porhora))

# Assessment Part 2: Dates, Times, and Text Mining ------------------------

library(tidyverse)
library(gutenbergr)
library(tidytext)
options(digits = 3)
gutenberg_metadata
sum(str_detect(gutenberg_metadata$title, 'Pride and Prejudice'), na.rm = TRUE)

gutenberg_metadata%>%
  filter(str_detect(title, 'Pride and Prejudice'))
idpride <- gutenberg_works(title == 'Pride and Prejudice')$gutenberg_id
book <- gutenberg_download(idpride)
words <- book %>% unnest_tokens(word, text)
nrow(words)
words <- words%>%
  filter(!word %in% stop_words$word)
nrow(words)
#También:
words <- words %>% anti_join(stop_words)

#q 10
s <- c('1', '2', 'aa')
words <- words%>%
  filter(!str_detect(word, '\\d'))
nrow(words)

#q 11
words%>%
  count(word)%>%
  filter(n > 100)%>%
  nrow

# q 11b
words%>%
  count(word)%>%
  top_n(10, n)%>%
  mutate(word = reorder(word, n))%>%
  arrange(desc(n))
#También:
words%>%
  count(word)%>%
  top_n(1, n)%>%
  pull(word)

words%>%
  count(word)%>%
  top_n(1, n)%>%
  pull(n)

#q 12
afinn <- get_sentiments('afinn')
afinn_sentiments <- words%>%
  inner_join(afinn, by = 'word')%>%
  select(word, value)
nrow(afinn_sentiments)

sum(afinn_sentiments$value >= 1)/nrow(afinn_sentiments)
#más fácil:
mean(afinn_sentiments$value > 0)

afinn_sentiments%>%
  filter(value == 4)%>%
  nrow
#más fácil:
sum(afinn_sentiments$value == 4)

# Comprehensive Assessment: Puerto Rico Hurricane Mortality ---------------

library(tidyverse)
library(pdftools)
options(digits = 3)    # report 3 significant digits

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
#Ls siguiente función abre el acrobat reader
system("cmd.exe", input = paste("start", fn))
txt <- pdf_text(fn)

x <- str_split(txt[9],'\n' )
class(x)
length(x)

s <- x[[1]]
class(s)
length(s)

s <- str_trim(s, side = 'both')
s[1]

#str_which(s, 'SEP\\s+2015')

header_index <- str_which(s, '2015')[1]
header_index

tmp <- str_split(s[header_index], pattern = '\\s+', simplify = TRUE)
month <- tmp[1]
header <- tmp[-1]
month
header[3]
s

#q8
tail_index <- str_which(s, 'Total')
tail_index

#q9
n <- str_count(s, pattern = '\\d+')
n
sum(n == 1)

# q 10
s1 <- s[(header_index+1):(tail_index-1)]
s2 <- s1[-which(n == 1)]
length(s2)

#Mejor:
out <- c(1:header_index, which(n==1), tail_index:length(s))
s <- s[-out]
length(s)

#q 11
s <- str_remove_all(s, "[^\\d\\s]")
s

# q 12
s <- str_split_fixed(s, "\\s+", n = 6)[,1:5]
s
# s <- as.numeric(s)
# s <- matrix(s, 30, 5)
# colnames(s) <- c('day', as.character(header))
# tab <- as.tibble(s)
#Así debió haber sido:
tab <- s %>% 
  as_data_frame() %>% 
  setNames(c("day", header)) %>%
  mutate_all(as.numeric)

# tab <- tab%>%
#   mutate(month = rep(month, length.out = nrow(tab)))
mean(tab$`2015`)
mean(tab$`2016`)
tab%>%filter(day <= 19)%>%
    summarise(prom = mean(`2017`))%>%
  pull(prom)

tab%>%filter(day > 19)%>%
  summarise(prom = mean(`2017`))%>%
  pull(prom)

tab <- tab %>% gather(year, deaths, -day) %>%
  mutate(deaths = as.numeric(deaths))
tab

# q 14

tab%>%
  filter(year != 2018)%>%
  ggplot(aes(x = day, y = deaths, color = year ))+
  geom_line() +
  geom_vline(xintercept = 20)+
  geom_point()
