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
