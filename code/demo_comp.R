#what are the trainee demographics?----

demo_combined <- tidy_host %>% 
  filter(role == "Faculty") %>% 
  rbind(., tidy_trainee, tidy_speaker) %>% 
  filter(demographic != "Gender") %>% 
  group_by(demographic, role, value) %>% summarise(n = n()) %>% 
  spread(key = value, value = n) %>% 
  mutate(prop_y = get_percent(y, (y + n)))
  
demo_combined$role <- fct_relevel(demo_combined$role, dat_levels)

demo_plot <- demo_combined %>% 
  ggplot()+
  geom_col(aes(x = role, y = prop_y, fill = role), alpha = 0.75)+
  facet_wrap(~demographic)+
  labs(x = "Academic Role\n", y = "Proportion of Population")+
  set_role_colors +
  my_theme_horiz +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#add host demo