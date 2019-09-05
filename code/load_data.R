library(tidyverse)

source("../code/get_plot_options.R")

get_diversity_value <- function(x, y){
  case_when(
    x == "man" & y == "y" ~ paste("Cauc M"),
    x == "woman" & y == "y" ~ paste("Cauc W")
  )
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
         Speaker_Gender = Speaker_gender)

tidy_speaker <- speaker_data %>% 
  select(contains("Speaker")) %>% 
  gather(Speaker_Gender:Speaker_Internatl, 
         key = demographic, value = value) %>% 
  separate(demographic, into = c("role", "demographic"), sep = "_") %>% 
  rename(id = Speaker_id) %>% 
  mutate(demographic = if_else(demographic == "Internatl", 
                               "International", demographic))
tidy_host <- speaker_data %>% 
  select(contains("Host")) %>% 
  gather(Host_gender:Host_Internatl, 
         key = demographic, value = value) %>% 
  separate(demographic, into = c("role", "demographic"), sep = "_") %>% 
  rename(id = Host_id) %>% 
  mutate(demographic = case_when(
    demographic == "Internatl" ~ "International",
    demographic == "gender" ~ "Gender",
    demographic == "caucasian" ~ "Caucasian",
    TRUE ~ demographic
  )) %>% 
  select(-role) %>% 
  rename(role = Host) 
  
trainee_data <- read_csv("../data/trainee_data_19.csv") %>% 
  mutate(id = rownames(.))

tidy_trainee <- trainee_data %>% 
  gather(Gender:Internatl, key = demographic, value = value) %>% 
  rename(role = Trainee_type) %>% 
  mutate(role = str_to_title(role)) %>% 
  mutate(demographic = if_else(demographic == "Internatl", 
                               "International", demographic))
