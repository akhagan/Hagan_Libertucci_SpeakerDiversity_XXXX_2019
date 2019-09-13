library(tidyverse)

source("../code/get_plot_options.R")

get_NCNH_group <- function(x, y){
  if_else(x == "n" & y == "n", "y", "n")
}

#calculate two decimal percentage given num & denom
get_percent <- function(x, y){
  z <- (as.numeric(x)/as.numeric(y))*100
  percent <- round(z, digits = 2)
  return(percent)
}

#data setup----
speaker_data <- read_csv("../data/speaker_dataset_14-19.csv") %>% 
  mutate(Speaker_id = rownames(.),
         Host_id = rownames(.)) %>% 
  rename(Speaker_Caucasian = Speaker_caucasian, 
         Speaker_Gender = Speaker_gender) %>% 
  mutate(Speaker_NCNH = get_NCNH_group(Speaker_Caucasian, Speaker_HURM),
         Host_NCNH = get_NCNH_group(Host_caucasian, Host_HURM)) %>% 
  filter(!is.na(Speaker_Gender))

tidy_speaker <- speaker_data %>% 
  select(contains("Speaker")) %>%
  gather(Speaker_Caucasian:Speaker_NCNH, -Speaker_id, 
         key = demographic, value = value) %>% 
  separate(demographic, into = c("role", "demographic"), sep = "_") %>% 
  rename(id = Speaker_id, Gender = Speaker_Gender)
  
tidy_host <- speaker_data %>% 
  select(contains("Host")) %>%
  gather(Host_caucasian:Host_NCNH, -Host_id, 
         key = demographic, value = value) %>% 
  separate(demographic, into = c("role", "demographic"), sep = "_") %>% 
  mutate(demographic = if_else(demographic == "caucasian", 
                               "Caucasian", demographic)) %>% 
  select(-role) %>% 
  rename(role = Host, id = Host_id, Gender = Host_gender) %>% 
  filter(!is.na(Gender))
  
trainee_data <- read_csv("../data/trainee_data_19.csv") %>% 
  mutate(id = rownames(.))

tidy_trainee <- trainee_data %>% 
  mutate(NCNH = get_NCNH_group(Caucasian, HURM)) %>% 
  gather(Caucasian:HURM, NCNH, key = demographic, value = value) %>% 
  rename(role = Trainee_type) %>% 
  mutate(role = str_to_title(role)) 
