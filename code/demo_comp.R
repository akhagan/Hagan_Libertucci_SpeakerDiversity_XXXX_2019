#what are the trainee demographics?----

speaker_demo <- tidy_speaker %>% 
  filter(demographic != "Gender")

trainee_demo <- tidy_trainee %>% 
  filter(demographic != "Gender") 
  
demo_combined <- rbind(trainee_demo, speaker_demo) %>% 
  group_by(demographic, role, value) %>% summarise(n = n()) %>% 
  spread(key = value, value = n) %>% 
  mutate(prop_y = get_percent(y, (y + n)))
  
demo_combined %>% 
  ggplot()+
  geom_col(aes(x = role, y = prop_y))+
  facet_wrap(~demographic)
