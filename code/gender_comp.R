#compare gender compositions for trainees and speakers
trainee_gender <- tidy_trainee %>% 
  filter(demographic == "Gender")  

speaker_gender <- tidy_speaker %>% 
  filter(demographic == "Gender")

gender_combined <- rbind(trainee_gender, speaker_gender) %>% 
  group_by(role, value) %>% summarise(n = n()) %>% 
  spread(key = value, value = n) %>% 
  mutate(prop_W = get_percent(woman, (man + woman)))

gender_combined$role <- fct_relevel(gender_combined$role, dat_levels)

gender_fig <- gender_combined %>% 
  ggplot()+
  geom_col(aes(x = role, y = prop_W, fill = role), alpha = 0.75)+
  labs(x = "Academic Role", y = "Proportion of Women")+
  geom_text()+
  set_role_colors +
  my_theme_horiz
