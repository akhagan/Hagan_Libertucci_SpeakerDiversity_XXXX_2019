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
speaker_data <- read_csv("data/speaker_dataset_14-19.csv") 

trainee_data <- read_csv("data/trainee_data_19.csv")
