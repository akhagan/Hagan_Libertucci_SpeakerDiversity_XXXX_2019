library(tidyverse)
library(lubridate)
library(hexbin)

get_diversity_value <- function(x, y){
  case_when(
  x == "man" & y == "y" ~ paste("Cauc M"),
  x == "man" & y == "n" ~ paste("POC M"),
  x == "woman" & y == "y" ~ paste("Cauc W"),
  x == "woman" & y == "n" ~ paste("POC W"))
}

MI_data <- read_csv("data/micro_immuno/complete_dataset_14-19.csv") %>% 
  select(-Speaker, -Host) %>% distinct()

MI_diversify <- MI_data %>% 
  mutate(speaker_div = get_diversity_value(Speaker_gender, Speaker_caucasian)) %>% 
  mutate(host_div = get_diversity_value(Host_gender, Host_caucasian))

MI_diversify %>% 
  ggplot()+
  geom_bar(aes(x = year(Date), fill = as.factor(speaker_div)), position = "dodge")

MI_diversify %>% 
  filter(!is.na(host_div)) %>% 
  filter(is.na(Lectureship)) %>% 
  ggplot()+
  geom_dotplot(aes(x = year(Date), fill = host_div), position = "jitter")
  

MI_data %>% 
  filter(Role == "Speaker_gender") %>% 
  group_by(year(Date), gender) %>% 
  summarise(n = n())

MI_data %>% 
  filter(is.na(Lectureship)) %>% 
  ggplot()+
  geom_count(aes(x = gender, y = ))