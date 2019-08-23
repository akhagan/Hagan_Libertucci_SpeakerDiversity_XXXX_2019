library(tidyverse)
library(pdftools)

text <- pdf_text("data/micro_immuno/2016-2017_M+I_seminar_schedule.pdf")

text_split <- str_split(text, "  ") %>% unlist()

empty <- ""

text_split <- text_split[!text_split %in% empty]

dates <- map_chr(text_split, function(x) str_extract(string = x, pattern = "(?<=\\b)[:digit:]+-[:alpha:]{3}"))

                 