library(tidyverse)
library(readxl)
library(lubridate)

#----Functions----
fix_year <- function(year, x){
  
  fall <- c("Aug", "Sep", "Oct", "Nov", "Dec", "8", "9", "10", "11", "12")
  winter <- c("Jan", "Feb", "Mar", "Apr", "May", "1", "2", "3", "4", "5")
  
  if_else(month(x) %in% winter,
    paste0(year+1, "-", month(x), "-", day(x)),
      paste0(year, "-", month(x), "-", day(x)))
}

get_lectureship <- function(x){
    
    case_when(
      
      str_detect(x, "neidhardt|freter") == TRUE ~ "Neidhardt/Freter",
      str_detect(x, "brockman") == TRUE ~ "Brockman",
      str_detect(x, "willison") == TRUE ~ "Willison",
      str_detect(x, "mmmp") == TRUE ~ "MMMP-invited",
      str_detect(x, "heritage") == TRUE ~ "Heritage",
      str_detect(x, "student") == TRUE ~ "Student-invited",
      str_detect(x, "postdoc") == TRUE ~ "Postdoc-invited"
  )
}


drop_words <- c("LECTURE|INVITE|STUDENT|INVITED|NEIDHARDT/FRETER|BROCKMAN|WILLISON|POSTDOC|MMMP|HERITAGE|GRAD|D\\sSPEAKER|SYMPOSIUM|NEIDHARDT-FRETER:|CELEBRATION|MICROBIOME|MILESTONES|M&I")

#----Load data----
yr14_15 <- read_excel("data/micro_immuno/2014-2015 Seminar check list .xlsx", range = "A2:F38") %>% 
  select(-Institution, -URL, -Affiliation) %>% #drop unnessecary data
  mutate(Date = map(Date, function(x){fix_year(2014, x)})) %>%  #fix dates
  #mutate(Lectureship = "empty") %>% 
  mutate(Date = ymd(unlist(Date)))

yr15_16 <- read_excel("data/micro_immuno/`2015-2016 Seminar check list.xlsx", range = "A3:G42") %>% 
  select(-INSTITUTION, -AFFILIATION, -`SEMINAR TITLE`, -URL) %>% 
  mutate(DATE = ymd(DATE)) %>% 
  rename(Date = DATE, Speaker = SPEAKER, Host = HOST) #%>% 
  #mutate(Lectureship = "empty")
  
yr16_17 <- read_excel("data/micro_immuno/2016-2017_M+I_seminar_schedule.xlsx") %>%  
  mutate(Date = map(Date, function(x){paste0(x, "-2016") %>% dmy() %>% fix_year(2016, .)})) %>% 
  mutate(Date = ymd(unlist(Date))) %>% 
  rename(Speaker = SPEAKER, Host = HOST)

yr17_18 <- read_excel("data/micro_immuno/2017-2018 M&I SEMINARS.xlsx", skip = 2) %>%
  mutate(Date = map(Date, function(x){paste0(x, "-2017") %>% dmy() %>% fix_year(2017, .)})) %>% 
  mutate(Date = ymd(unlist(Date))) %>% 
  select(-Affiliation) %>% 
  rename(Speaker = `Speaker/Contact`)

yr18_19 <- read_excel("data/micro_immuno/2018-2019 M&I SEMINARS.xlsx", col_types = c("date", "guess", "guess", "guess", "guess", "guess")) %>% 
  mutate(Date = map(`2018`, function(x){fix_year(2018, x)})) %>% #fix dates
  mutate(Date = ymd(unlist(Date))) %>% 
  select(-`2018`, -URL, -Affiliation, -`Seminar title`) %>% 
  rename(Speaker = `Speaker/Contact`) #%>% 
  #mutate(Lectureship = "empty")

all_yr <- bind_rows(yr14_15, yr15_16, yr16_17, yr17_18, yr18_19) %>% 
  mutate(Lectureship = if_else(is.na(Lectureship) == TRUE, 
                               str_to_lower(Speaker) %>% get_lectureship(),
              paste(Lectureship))) %>% #dropping yr17/18
  mutate(Speaker = map(Speaker, function(x){str_to_upper(x) %>% str_replace_all(., drop_words, "") %>% 
      str_to_title()})) %>% 
  mutate(Speaker = unlist(Speaker)) %>% 
  mutate(Speaker = map(Speaker, function(x){str_replace(x, ",\\s.*", "")})) %>% 
  mutate(Speaker = unlist(Speaker))

all_yr_clean <- all_yr %>% 
  filter(!is.na(Speaker)) %>% 
  mutate(Speaker = str_replace(Speaker, "Neidhardt-Freter", "")) %>% 
  mutate(Speaker = str_replace_all(Speaker, ":", "")) %>% 
  mutate(Speaker = str_replace(Speaker, "Uga Found.*$", "")) %>% 
  mutate(Speaker = trimws(Speaker)) %>% 
  mutate(Lectureship = case_when(
    str_detect(Host, "Molecular Mechanisms") ~ "MMMP",
    str_detect(Host, "DEI") ~ "DEI",
    TRUE ~ paste(Lectureship)
    )) %>% 
  mutate(Host = case_when(
    str_detect(Host, "Molecular Mechanisms") ~ "Vic DiRita",
    str_detect(Host, "Mobley [:alnum:]") ~ "Harry Mobley",
    TRUE ~ paste(Host)
    )) %>% 
  mutate(Host = trimws(Host))

write_csv(all_yr_clean, "data/micro_immuno/data_14-19.csv")

hosts <- enframe(name = NULL, all_yr_clean$Host) %>% 
  rename(Host = value) %>% 
  distinct()

speakers <- all_yr_clean$Speaker %>% unique()

speakers <- speakers[!speakers %in% hosts$Host] %>% enframe(name = NULL, .) %>% 
  distinct()

write_csv(hosts, "data/micro_immuno/hosts_14-19.csv")

write_csv(speakers, "data/micro_immuno/speakers_14-19.csv")
