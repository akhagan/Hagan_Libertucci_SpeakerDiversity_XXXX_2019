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

#data setup----
MI_data <- read_csv("data/micro_immuno/complete_dataset_14-19.csv") 

postdoc_data <- read_csv("data/micro_immuno/postdocs_19.csv") %>% 
  mutate(Trainee_type = "postdoc")

student_data <- read_csv("data/micro_immuno/students_19.csv") %>% 
  mutate(Trainee_type = "student")

trainee_data <- bind_rows(postdoc_data, student_data) %>% 
  distinct() %>% 
  mutate(train_div = get_diversity_value(Gender, Caucasian))
