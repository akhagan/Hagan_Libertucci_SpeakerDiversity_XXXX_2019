
trainee_gender <- tidy_trainee %>% 
  filter(demographic == "Gender")  

speaker_gender <- tidy_speaker %>% 
  filter(demographic == "Gender")

gender_combined <- rbind(trainee_gender, speaker_gender) %>% 
  group_by(role, value) %>% summarise(n = n()) %>% 
  spread(key = value, value = n) %>% 
  mutate(prop_W = get_percent(woman, (man + woman)))

gender_combined %>% 
  ggplot()+
  geom_col(aes(x = role, y = prop_W))
