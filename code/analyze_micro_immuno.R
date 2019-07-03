#what is the frequency of hosting over the past several years -- weighted % of hosting by demographic----
plot_hosting_freq <- MI_diversify %>% 
  group_by(Host, host_div) %>% summarise(n = n()) %>% 
  ggplot()+
  geom_col(aes(x = reorder(Host, n), y = n, fill = host_div))+
  theme_classic() + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1))

#What are the proportions of M/F, W/POC for invited speakers----
MI_diversify %>% #over time
  group_by(year(Date), speaker_div) %>% summarise(n = n()) %>% 
  ggplot()+
  geom_col(aes(x = `year(Date)`, y = n, fill = speaker_div), position = "dodge")+
  scale_fill_brewer(palette = "Dark2")+
  labs(x = "Year", y = "Number Invited")+
  theme_classic() + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"), 
        axis.text.x = element_text(angle = 0, hjust = 1))
  

MI_diversify %>% 
  ggplot(aes(x = Speaker_gender))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(y = ((..count..)/sum(..count..)), label = ..count..), stat = "count", vjust = -0.25)+
  labs(y = "Percent of Speakers", x = "Speaker Gender")+
  theme_classic() + 
  theme(legend.position = "none", axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"))

ggsave("figures/speaker_gender.png")

MI_diversify %>% 
  ggplot(aes(x = Speaker_caucasian))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(y = ((..count..)/sum(..count..)), label = ..count..), stat = "count", vjust = -0.25)+
  labs(y = "Percent of Speakers", x = "Caucasian Speaker")+
  theme_classic() + 
  theme(legend.position = "none", axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"))

ggsave("figures/speaker_cauc.png")

MI_diversify %>% 
  #filter(!is.na(Lectureship)) %>% 
  #filter(Host != "Student") %>% 
  #filter(Host != "Postdoc") %>%
  ggplot(aes(x = speaker_div))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(y = ((..count..)/sum(..count..)), label = ..count..), stat = "count", vjust = -0.25)+
  labs(y = "Percent of Invited Speakers", x = "Speaker Demographics")+
  scale_fill_brewer(palette = "Set2")+
  theme_classic() + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"))
ggsave("figures/speaker_div.png")

#What are the proportions of M/F, W/POC for lectureships ----
MI_diversify %>% 
  filter(!is.na(Lectureship)) %>% 
  filter(Host != "Student" | Host != "Postdoc") %>% 
  group_by(speaker_div) %>% summarise(n = n()) %>% 
  add_row(., speaker_div = "WOC", n = 0) %>% 
  mutate(percent = (n/sum(n))*100) %>% 
  ggplot(aes(x = speaker_div))+
  geom_col(aes(y = percent,))+
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

#what are the trainee demographics?----

trainee_data %>% 
  ggplot(aes(x = train_div))+
  geom_bar(aes(y = (..count..)/sum(..count..), fill = Trainee_type))+
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(y = ((..count..)/sum(..count..)), label = ..count..), stat = "count", vjust = -0.25)+
  labs(y = "Percent of Trainees", x = "Demographics", 
       fill = "Trainee Type")+
  scale_fill_brewer(palette = "Set2")+
  theme_classic() + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"))

ggsave("figures/trainee_demog_fill.png")
