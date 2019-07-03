library(tidyverse)

#Functions----
get_diversity_value <- function(x, y){
  case_when(
    x == "man" & y == "y" ~ paste("Cauc M"),
    x == "man" & y == "n" ~ paste("MOC"),
    x == "woman" & y == "y" ~ paste("Cauc W"),
    x == "woman" & y == "n" ~ paste("WOC"))
}

get_host_type <- function(x){
  
  student <- c("Daniel Goldstein", "Grad Student Invite", "Jessica Beauchamp & Jay Lubow", "Olivia McGovern and JT McCrone", "students, Matt Jenior", "Sukhmani Bedi")
    
  postdoc <- c("POSTDOCTORAL FELLOWS           Josie Libertucci", "Postdoc-Michael Taveirne", "Postdoc host-Chelsie Armbruster", "Laura Mike", "Kayla Peck", "Chelsie Armbruster", "Anna Seekatz")
  
  host_type <- case_when(
    x %in% student ~ "Student",
    x %in% postdoc ~ "Postdoc",
    TRUE ~ "Faculty"
  )
  
  return(host_type)
}

raw_data <- read_csv("data/micro_immuno/data_14-19.csv")

host_data <- read_csv("data/micro_immuno/hosts_14-19.csv")

speaker_data <- read_csv("data/micro_immuno/speakers_14-19.csv")

compiled_data <- raw_data %>% left_join(., host_data, by = "Host") %>% 
  left_join(., speaker_data, by = "Speaker") %>% 
  filter(!is.na(Speaker_gender)) %>% 
  mutate(Host_gender = ifelse(str_detect(Host, "Harry")==TRUE, "man", paste(Host_gender))) %>% 
  mutate(Host_caucasian = ifelse(str_detect(Host, "Harry")==TRUE, "y", paste(Host_caucasian))) %>% 
#  gather(Host_gender, Speaker_gender, key = "Role", value = "gender") %>% 
#  gather(Host_caucasian, Speaker_caucasian, key = "Role_c", value = "Caucasian") %>% 
  distinct()


MI_diversify <- compiled_data %>% 
  mutate(speaker_div = get_diversity_value(Speaker_gender, Speaker_caucasian), 
         host_div = get_diversity_value(Host_gender, Host_caucasian),
         Host = get_host_type(Host), 
         Lectureship = fct_collapse(Lectureship,
                                    "Brockman" = c("Brockman", "BROCKMAN SPEAKER"),
                                    "Heritage" = c("Heritage", "HERITAGE LECTURE"),
                                    "MMMP" = c("MMMP", "MMMP-invited"),
                                    "Neidhardt/Freter" = c("Neidhardt/Freter", "NEIDHARDT/FRETER SYMPOSIUM", "Neidhart-Freter"),
                                    "Willison" = c("Willison", "WILLISON SPEAKER"))) %>% 
  select(-Speaker)

write_csv(MI_diversify, "data/micro_immuno/complete_dataset_14-19.csv")  

