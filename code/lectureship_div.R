#What are the proportions of each demographic for lectureships ----
lectureship <- speaker_data %>% 
  filter(!is.na(Lectureship)) %>% 
  filter(Host != "Student") %>% 
  filter(Host != "Postdoc") %>% 
  select(contains("Speaker")) %>% 
  gather(Speaker_Gender:Speaker_Internatl, 
         key = demographic, value = value) %>% 
  separate(demographic, into = c("role", "demographic"), sep = "_") %>% 
  mutate(demographic = if_else(demographic == "Internatl", 
                               "International", demographic))

lectureship_demo_plot <- lectureship %>% 
  filter(demographic != "Gender") %>% 
  filter(value == "y") %>% 
  ggplot(aes(x = value))+
  geom_bar(aes(y = (..count..)/sum(..count..), fill = value))+
  facet_grid(~demographic)+
  scale_y_continuous(labels = scales::percent)+
  #geom_text(aes(y = ((..count..)/sum(..count..)), label = ..count..), stat = "count", vjust = -0.25)+
  labs(y = "Percent of Lectureships", x = "Speaker Demographics")+
  scale_fill_manual(values = "grey17")+
  my_theme_horiz+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

lectureship_gend_plot <- speaker_data %>% 
  filter(!is.na(Lectureship)) %>% 
  filter(Host != "Student") %>% 
  filter(Host != "Postdoc") %>% 
  select(Speaker_Gender, Speaker_Caucasian) %>% 
  ggplot(aes(x = Speaker_Gender))+
  geom_bar(aes(y = (..count..)/sum(..count..), 
               fill = Speaker_Gender))+
  facet_wrap(~if_else(Speaker_Caucasian == "y", "Caucasian", "Non-Caucasian"))+
  scale_y_continuous(labels = scales::percent)+
  #geom_text(aes(y = ((..count..)/sum(..count..)), label = ..count..), stat = "count", vjust = -0.25)+
  labs(y = "\nPercent of Lectureships", x = "Speaker Gender")+
  scale_fill_manual(values = c("grey17", "dark gray"))+
  my_theme_horiz
