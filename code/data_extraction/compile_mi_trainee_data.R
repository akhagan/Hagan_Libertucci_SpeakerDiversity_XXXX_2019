library(tidyverse)

postdoc_data <- read_csv("data/micro_immuno/postdocs_19.csv") %>% 
  mutate(Trainee_type = "postdoc")

student_data <- read_csv("data/micro_immuno/students_19.csv") %>% 
  mutate(Trainee_type = "student")

trainee_data <- bind_rows(postdoc_data, student_data) %>% 
  distinct() %>% 
  #mutate(train_div = get_diversity_value(Gender, Caucasian)) %>% 
  select(-Name)

write_csv(trainee_data, "data/trainee_data_19.csv")
