#What are the proportions of M/F, W/POC for lectureships ----
MI_diversify %>% 
  filter(!is.na(Lectureship)) %>% 
  filter(Host != "Student" | Host != "Postdoc") %>% 
  group_by(speaker_div) %>% summarise(n = n()) %>% 
  add_row(., speaker_div = "WOC", n = 0) %>% 
  mutate(percent = (n/sum(n))*100) %>% 
  ggplot(aes(x = speaker_div))+
  geom_col(aes(y = percent))+
  #scale_y_continuous(labels = scales::percent)+
  #geom_text(aes(y = ((..count..)/sum(..count..)), label = ..count..), stat = "count", vjust = -0.25)+
  labs(y = "Percent of Lectureships", x = "Speaker Demographics")+
  theme_classic() + 
  theme(legend.position = "none", axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"))

MI_diversify %>% 
  filter(!is.na(Lectureship)) %>% 
  filter(Host != "Student" | Host != "Postdoc") %>% 
  ggplot(aes(x = Speaker_caucasian))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(y = ((..count..)/sum(..count..)), label = ..count..), stat = "count", vjust = -0.25)+
  labs(y = "Percent of Speakers", x = "Is Speaker Caucasian?")+
  theme_classic() + 
  theme(legend.position = "none", axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"))

MI_diversify %>% 
  filter(!is.na(Lectureship)) %>% 
  filter(Host != "Student") %>% 
  filter(Host != "Postdoc") %>%
  ggplot(aes(x = factor(speaker_div, levels = c("Cauc M", "Cauc W", "MOC", "WOC"))))+
  geom_bar(aes(y = (..count..)/sum(..count..), fill = Lectureship))+
  scale_x_discrete(drop=FALSE)+
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(y = ((..count..)/sum(..count..)), label = ..count..), stat = "count", vjust = -0.25)+
  labs(y = "Percent of Lectureships", x = "Speaker Demographics")+
  scale_fill_brewer(palette = "Set2")+
  theme_classic() + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"))

ggsave("figures/lectureship_demog.png")
