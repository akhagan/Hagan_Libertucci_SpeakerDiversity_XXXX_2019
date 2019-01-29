library(tidyverse)

raw_data <- read_csv("data/micro_immuno/data_14-19.csv")

host_data <- read_csv("data/micro_immuno/hosts_14-19.csv")

speaker_data <- read_csv("data/micro_immuno/speakers_14-19.csv")

compiled_data <- raw_data %>% left_join(., host_data, by = "Host") %>% 
  left_join(., speaker_data, by = "Speaker") %>% 
  filter(!is.na(Speaker_gender)) %>% 
  mutate(Host_gender = ifelse(str_detect(Host, "Harry")==TRUE, "man", paste(Host_gender))) %>% 
  mutate(Host_caucasian = ifelse(str_detect(Host, "Harry")==TRUE, "y", paste(Host_caucasian))) %>% 
  gather(Host_gender, Speaker_gender, key = "Role", value = "gender") %>% 
  gather(Host_caucasian, Speaker_caucasian, key = "Role_c", value = "Caucasian") %>% 
  distinct()

write_csv(compiled_data, "data/micro_immuno/complete_dataset_14-19.csv")  

