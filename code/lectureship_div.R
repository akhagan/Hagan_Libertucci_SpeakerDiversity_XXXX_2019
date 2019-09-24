#What are the proportions of each demographic for lectureships ----
lectureship <- speaker_data %>% 
  filter(!is.na(Lectureship)) %>% 
  filter(Host != "Student") %>% 
  filter(Host != "Postdoc") %>% 
  select(contains("Speaker")) %>%
  gather(Speaker_Caucasian:Speaker_NCNH, -Speaker_id, 
         key = demographic, value = value) %>% 
  separate(demographic, into = c("role", "demographic"), sep = "_") 

lectureship_demo_plot <- lectureship %>% 
  select(-Speaker_Gender) %>% 
  filter(value == "y") %>% 
  ggplot(aes(x = value))+
  geom_bar(aes(y = (..count..)/sum(..count..), fill = value))+
  facet_grid(~demographic)+
  scale_y_continuous(labels = multiply, limits = c(0, 1))+
  #geom_text(aes(y = ((..count..)/sum(..count..)), label = ..count..), stat = "count", vjust = -0.25)+
  labs(y = "Lectureships (%)", x = "Speaker Demographics")+
  scale_fill_manual(values = "grey17")+
  my_theme_horiz+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

lectureship_gend_plot <- lectureship %>% 
  filter(value == "y") %>% 
  ggplot(aes(x = Speaker_Gender))+
  geom_bar(aes(y = (..count..)/sum(..count..), 
               fill = Speaker_Gender), position = "dodge")+
  facet_grid(~demographic)+
  scale_y_continuous(labels = multiply, limits = c(0, 1))+
  labs(y = "\nLectureships (%)", x = "Speaker Gender")+
  scale_fill_manual(values = c("grey17", "dark gray"))+
  my_theme_horiz

