# Author:       Johannes Brachem
# Last Update:  05.08.2019
# Purpose:      Create a plot, showing the study progress of participants
# Output:       "plots/german/participants_semester_both.png"

# packages
library(tidyverse)
library(ggpubr)
# function import 
source("code/helpers/helper_functions.R")

# data import
wide <- read_csv("data/wide.csv")

wide_descriptives_full<- wide %>% 
  filter(field_of_study_mc == "Psychology" | study_degree_field == "Psychology") %>% 
  select(age, sex, semester, study_stage_mc, study_stage_open, 
         uni_bachelor, uni_current, study_degree, study_degree_field, 
         study_degree_year, study_degree_yesno, comments, exp_specific)

wide_descriptives <- wide %>% 
  filter(field_of_study_mc == "Psychology" | study_degree_field == "Psychology") %>% 
  filter(emp_experience == "Yes") %>% 
  select(age, sex, semester, study_stage_mc, study_stage_open, 
         uni_bachelor, uni_current, study_degree, study_degree_field, 
         study_degree_year, study_degree_yesno, comments, exp_specific)



# progress of all students
sems_plot_full <- wide_descriptives_full %>% 
  # light wrangling
  filter(study_stage_mc != "Other") %>% 
  filter(semester < 11) %>% 
  mutate(semester = ifelse(semester %% 2 == 0, semester - 1, semester)) %>% 
  mutate(semester = factor(semester, 
                           labels = c("1/2", "3/4", "5/6", "7/8", "9/10"))) %>% 
  group_by(semester, study_stage_mc) %>% 
  summarise(n = n()) %>% 
  
  # plot
  ggplot(aes(x = factor(semester), y = n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~study_stage_mc, strip.position = "bottom") +
  scale_y_continuous(limits = c(0, 325)) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.placement = "outside")

# Anzahl Teilnehmende nach Fachsemester und Studienabschnitt
sems_plot_full_ger.out <- sems_plot_full + 
  labs(x = "Semester", y = "Anzahl", title = "Volle Stichprobe")

# Number of participants, split by semester and study stage
sems_plot_full_en.out <- sems_plot_full + 
  labs(x = "Semester", y = "Count")

# print plot
sems_plot_full_en.out


# progress of students with empirical experience
sems_plot <- wide_descriptives %>% 
  # light wrangling
  filter(study_stage_mc != "Other") %>% 
  filter(semester < 11) %>% 
  mutate(semester = ifelse(semester %% 2 == 0, semester - 1, semester)) %>% 
  mutate(semester = factor(semester, 
                           labels = c("1/2", "3/4", "5/6", "7/8", "9/10"))) %>% 
  group_by(semester, study_stage_mc) %>% 
  summarise(n = n()) %>% 
  
  # plot
  ggplot(aes(x = factor(semester), y = n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~study_stage_mc, strip.position = "bottom") +
  scale_y_continuous(limits = c(0, 325)) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.placement = "outside")

# Anzahl Teilnehmende nach Fachsemester und Studienabschnitt
sems_plot_ger.out <- sems_plot + 
  labs(x = "Semester", y = "Anzahl", title = "EE-Teilstichprobe")

# Number of participants, split by semester and study stage
sems_plot_en.out <- sems_plot + 
  labs(x = "Semester", y = "Count")

# print plot
sems_plot_en.out


# combining both semester plots
(sems_plot_both.ger <- ggarrange(sems_plot_full_ger.out, sems_plot_ger.out,
                                ncol = 2, nrow = 1, labels = "auto"))

(sems_plot_both.en <- ggarrange(sems_plot_full_en.out, sems_plot_en.out,
                                 ncol = 1, nrow = 2, labels = "auto"))



# Both semesters together
ggsave("plots/german/participants_semester_both_ger.png", sems_plot_both.ger,
       width = 200, height = 50, units = "mm")

ggsave("plots/english/participants_semester_both_en.png", sems_plot_both.en,
       width = 100, height = 100, units = "mm")
