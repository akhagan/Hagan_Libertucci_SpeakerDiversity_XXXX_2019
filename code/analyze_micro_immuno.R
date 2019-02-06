library(tidyverse)
library(lubridate)
library(hexbin)

get_diversity_value <- function(x, y){
  case_when(
  x == "man" & y == "y" ~ paste("Cauc M"),
  x == "man" & y == "n" ~ paste("MOC"),
  x == "woman" & y == "y" ~ paste("Cauc W"),
  x == "woman" & y == "n" ~ paste("WOC"))
}

#data setup----
MI_data <- read_csv("data/micro_immuno/complete_dataset_14-19.csv") 

MI_diversify <- MI_data %>% 
  mutate(speaker_div = get_diversity_value(Speaker_gender, Speaker_caucasian)) %>% 
  mutate(host_div = get_diversity_value(Host_gender, Host_caucasian)) %>% 
  mutate(Host = fct_collapse(Host,
                             "Student" = c("Daniel Goldstein", "Grad Student Invite", "Jessica Beauchamp & Jay Lubow", "Olivia McGovern and JT McCrone", "students, Matt Jenior", "Sukhmani Bedi"),
                             "Postdoc" = c("POSTDOCTORAL FELLOWS           Josie Libertucci", "Postdoc-Michael Taveirne", "Postdoc host-Chelsie Armbruster", "Laura Mike", "Kayla Peck", "Chelsie Armbruster", "Anna Seekatz"),
                             "Harry Mobley" = c("Harry Mobley", "Harry Mobley & Chris Alteri"),
                             "Christiane Wobus" = c("Christiane Wobus", "Christine Wobus"))) %>% 
  mutate(Lectureship = fct_collapse(Lectureship,
                                    "Brockman" = c("Brockman", "BROCKMAN SPEAKER"),
                                    "Heritage" = c("Heritage", "HERITAGE LECTURE"),
                                    "MMMP" = c("MMMP", "MMMP-invited"),
                                    "Neidhardt/Freter" = c("Neidhardt/Freter", "NEIDHARDT/FRETER SYMPOSIUM", "Neidhart-Freter"),
                                    "Willison" = c("Willison", "WILLISON SPEAKER")))

#what is the frequency of hosting over the past several years -- weighted % of hosting by demographic----
plot_hosting_freq <- MI_diversify %>% 
  group_by(Host, host_div) %>% summarise(n = n()) %>% 
  ggplot()+
  geom_col(aes(x = reorder(Host, n), y = n, fill = host_div))+
  theme_classic() + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1))

fac_div <- MI_diversify %>% 
  filter(is.na(Lectureship)) %>% 
  filter(Host != "Student") %>% 
  filter(Host != "Postdoc") %>% 
  select(Host, host_div) %>% distinct() %>% group_by(host_div) %>% summarise(n_fac =n())

MI_diversify %>% 
  filter(is.na(Lectureship)) %>% 
  filter(Host != "Student") %>% 
  filter(Host != "Postdoc") %>% 
  group_by(host_div) %>% summarise(n_hosted = n()) %>% 
  mutate(total = sum(n_hosted)) %>% 
  mutate(percent = n_hosted/total) %>% 
  left_join(., fac_div, by = "host_div") %>% 
  mutate(impact = round((percent/n_fac), digits = 4)) %>% 
  ggplot()+
  geom_col(aes(x = host_div, y = impact))+
  coord_flip(ylim = c(0.03, 0.045))+
  labs(y = "Relative Contribution", x = "M&I Host Demographic",
       caption = "Does not include hosts for lectureships, student-invited, or postdoc-invited speakers")+
  theme_classic() + 
  theme(legend.position = "none", axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"))

ggsave("figures/hosting_contribution.png")

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

#What are the proportions of M/F, W/POC for lectureships ----
MI_diversify %>% 
  filter(!is.na(Lectureship)) %>% 
  filter(Host != "Student" | Host != "Postdoc") %>% 
  ggplot(aes(x = Speaker_gender))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(y = ((..count..)/sum(..count..)), label = ..count..), stat = "count", vjust = -0.25)+
  labs(y = "Percent of Speakers", x = "Speaker Gender")+
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
  ggplot(aes(x = speaker_div))+
  geom_bar(aes(y = (..count..)/sum(..count..), fill = Lectureship))+
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(y = ((..count..)/sum(..count..)), label = ..count..), stat = "count", vjust = -0.25)+
  labs(y = "Percent of Lectureships", x = "Speaker Demographics")+
  scale_fill_brewer(palette = "Set2")+
  theme_classic() + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"))

ggsave("figures/lectureship_demog.png")

#what are the trainee demographics?----
postdoc_data <- read_csv("data/micro_immuno/postdocs_19.csv") %>% 
  mutate(Trainee_type = "postdoc")
student_data <- read_csv("data/micro_immuno/students_19.csv") %>% 
  mutate(Trainee_type = "student")

trainee_data <- bind_rows(postdoc_data, student_data) %>% 
  distinct() %>% 
  mutate(train_div = get_diversity_value(Gender, Caucasian))

trainee_data %>% 
  ggplot(aes(x = train_div))+
  geom_bar(aes(y = (..count..)/sum(..count..), fill = Trainee_type))+
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(y = ((..count..)/sum(..count..)), label = ..count..), stat = "count", vjust = -0.25)+
  labs(y = "Percent of Trainees", x = "Demographics")+
  scale_fill_brewer(palette = "Set2")+
  theme_classic() + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"))

ggsave("figures/trainee_demog.png")
