library(tidyverse)
library(readxl)

#----Functions----
fix_year <- function(year, x){
  
  fall <- c("Aug", "Sep", "Oct", "Nov", "Dec", "08", "09", "10", "11", "12")
  winter <- c("Jan", "Feb", "Mar", "Apr", "May", "01", "02", "03", "04", "05")
  
  if(month(x) %in% winter){paste(year+1)}else{paste(year)}
  
}

#----Load data----
yr14_15 <- read_excel("data/micro_immuno/2014-2015 Seminar check list .xlsx", range = "A2:F38") #spring year incorrect

yr15_16 <- read_excel("data/micro_immuno/`2015-2016 Seminar check list.xlsx", range = "A3:G42") #years correct

yr16_17 <- read_excel("data/micro_immuno/2016-2017_M+I_seminar_schedule.xlsx") #add years

yr17_18 <- read_excel("data/micro_immuno/2017-2018 M&I SEMINARS.xlsx", skip = 2) #add years

yr18_19 <- read_excel("data/micro_immuno/2018-2019 M&I SEMINARS.xlsx", col_types = c("date", "guess", "guess", "guess", "guess", "guess"))

test_14<- yr14_15 %>% 
  mutate(Lectureship = str_extract(Speaker, "^.*(?=:)")) %>% 
  mutate(Speaker = str_replace(Speaker, "^.*:", "")) %>% 
  select(-Institution, -URL, -Affiliation)
