# Author:       Johannes Brachem
# Last Update:  05.08.2019
# Purpose:      Create a plot, showing the 
# Output:       "plots/german/data_per_uni_ger.png"
#               "plots/english/data_per_uni_en.png"

# packages
library(tidyverse)

# data import
n_obs_uni <- read_csv("data/n_observations_uni.csv")

## Plot: Data points per university
# These numbers concern only the subset of participants who indicated that 
# they have conducted at least one empirical project in the course of their 
# studies. We count psychology students and former psychology students who 
# graduated 2017 or later. Subjects who studied at two different universities 
# for their bachelor's and master's degree are counted as a data point for each 
# of the universities.

uni_plot <- n_obs_uni %>% 
  # light wrangling
  filter(!is.na(uni)) %>% 
  mutate(uni = factor(uni)) %>%
  
  # plot
  ggplot(aes(x = fct_reorder(uni, n), y = n)) +
  geom_bar(stat = "identity") + 
  geom_hline(yintercept = 30, color = "red") +
  geom_hline(yintercept = 100, color = "green") +
  theme_minimal() +
  coord_flip()

uni_plot_ger.out <- uni_plot + 
  labs(x = "Universität", y = "Anzahl Datenpunkte", title = "Datenpunkte pro Universität")

uni_plot_en.out <- uni_plot + 
  labs(x = "University", y = "Number of Observations", title = "Observations per University")

# print plot
uni_plot_en.out


# Data points per university (EMP EXP SUBSET)
ggsave("plots/german/data_per_uni_ger.png", uni_plot_ger.out, height = 9)
ggsave("plots/english/data_per_uni_en.png", uni_plot_en.out, height = 9)