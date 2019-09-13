#compare gender compositions for trainees and speakers

gender_combined <- tidy_host %>% 
  filter(role == "Host Faculty") %>% 
  rbind(., tidy_trainee, tidy_speaker) %>% 
  select(Gender, role, id) %>% 
  group_by(role, Gender) %>% summarise(n = n()) %>% 
  spread(key = Gender, value = n) %>% 
  mutate(prop_W = get_percent(woman, (man + woman)))

gender_combined$role <- fct_relevel(gender_combined$role, dat_levels)

gender_plot_leg <- gender_combined %>% 
  ggplot()+
  geom_col(aes(x = role, y = prop_W, fill = role), alpha = 0.75)+
  labs(x = "Academic Role\n", y = "Proportion of Women\n in the Population", 
       fill = "Academic Role")+
  set_role_colors

gender_plot <- gender_combined %>% 
  ggplot()+
  geom_col(aes(x = role, y = prop_W, fill = role), alpha = 0.75)+
  labs(x = "Academic Role\n", 
       y = "Proportion of Women\n in the Population", 
       fill = "Academic Role")+
  set_role_colors +
  my_theme_horiz+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#add host demo