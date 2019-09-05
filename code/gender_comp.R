#compare gender compositions for trainees and speakers

gender_combined <- tidy_host %>% 
  filter(role == "Faculty") %>% 
  rbind(., tidy_trainee, tidy_speaker) %>% 
  filter(demographic == "Gender") %>% 
  group_by(role, value) %>% summarise(n = n()) %>% 
  spread(key = value, value = n) %>% 
  mutate(prop_W = get_percent(woman, (man + woman)))

gender_combined$role <- fct_relevel(gender_combined$role, dat_levels)

gender_fig <- gender_combined %>% 
  ggplot()+
  geom_col(aes(x = role, y = prop_W, fill = role), alpha = 0.75)+
  labs(x = "Academic Role", y = "Proportion of Women")+
  #geom_text(prop_W)+
  set_role_colors +
  my_theme_horiz

#add host demo