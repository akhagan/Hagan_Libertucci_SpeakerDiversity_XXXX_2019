#speaker gender demographics

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