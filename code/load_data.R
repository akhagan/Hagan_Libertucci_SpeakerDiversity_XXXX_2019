library(tidyverse)
library(lubridate)
library(hexbin)

get_diversity_value <- function(x, y){
  case_when(
    x == "man" & y == "y" ~ paste("Cauc M"),
    x == "man" & y == "n" ~ paste("MOC"),
    x == "woman" & y == "y" ~ paste("Cauc W"),
    x == "woman" & y == "n" ~ paste("WOC"))
}

#calculate two decimal percentage given num & denom
get_percent <- function(x, y){
  z <- (as.numeric(x)/as.numeric(y))*100
  percent <- round(z, digits = 2)
  return(percent)
}

#data setup----
speaker_data <- read_csv("data/speaker_dataset_14-19.csv") %>% 
  mutate(Speaker_id = rownames(.)) %>% 
  rename(Speaker_Caucasian = Speaker_caucasian, 
         Speaker_Gender = Speaker_gender)

tidy_speaker <- speaker_data %>% 
  select(contains("Speaker")) %>% 
  gather(Speaker_Gender:Speaker_Internatl, 
         key = demographic, value = value) %>% 
  separate(demographic, into = c("role", "demographic"), sep = "_") %>% 
  rename(id = Speaker_id) %>% 
  mutate()

trainee_data <- read_csv("data/trainee_data_19.csv") %>% 
  mutate(id = rownames(.))

tidy_trainee <- trainee_data %>% 
  gather(Gender:Internatl, key = demographic, value = value) %>% 
  rename(role = Trainee_type) %>% 
  mutate(role = str_to_title(role))
