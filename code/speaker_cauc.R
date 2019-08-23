#speaker caucasian vs not

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
  ggplot(aes(x = Speaker_caucasian))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(y = ((..count..)/sum(..count..)), label = ..count..), stat = "count", vjust = -0.25)+
  labs(y = "Percent of Speakers", x = "Caucasian Speaker")+
  theme_classic() + 
  theme(legend.position = "none", axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"))

ggsave("figures/speaker_cauc.png")