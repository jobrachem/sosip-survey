# Author:       Johannes Brachem
# Last Update:  17.08.2019
# Purpose:      Create a plot, showing knowledge about open science initiatives
# Output:       "plots/german/nosi_plot_ger.png"
#               "plots/english/nosi_plot_en.png"


# packages and imports
library(tidyverse)
main <- read_csv("data/main.csv") %>% 
  filter(field_of_study_mc == "Psychology" | study_degree_field == "Psychology")
unis_to_include <- readRDS("data/unis_to_include.rds")


# Two warnings are shown. These are no problem

nosi_plot_en <- main %>% 
  # data wrangling for likert plot
  filter(uni_current %in% (unis_to_include %>% pull(uni))) %>% 
  mutate(nosi = factor(nosi, levels = c("No", "Not Sure", "Yes"))) %>% 
  select(nosi, uni_current, nosi_at_uni) %>% 
  filter(nosi_at_uni == "Yes") %>% 
  select(uni_current, nosi) %>% 
  mutate(row = 1:nrow(.)) %>% 
  spread(1:2, key = uni_current, value = nosi) %>% 
  select(-row) %>% 
  as.data.frame() %>% 
  likert::likert(.) %>% 
  
  # plot
  plot() +
  labs(title = "Awareness of Open Science Initiatives",
       subtitle = "Only universities with more than 30 participants are displayed."
    )
  NULL

nosi_plot_ger <- main %>% 
  # data wrangling for likert plot
  filter(uni_current %in% (unis_to_include %>% pull(uni))) %>% 
  mutate(nosi = factor(nosi, levels = c("No", "Not Sure", "Yes"),
                       labels = c("Nein", "Nicht sicher", "Ja"))) %>% 
  select(nosi, uni_current, nosi_at_uni) %>% 
  filter(nosi_at_uni == "Yes") %>% 
  select(uni_current, nosi) %>% 
  mutate(row = 1:nrow(.)) %>% 
  spread(1:2, key = uni_current, value = nosi) %>% 
  select(-row) %>% 
  as.data.frame() %>% 
  likert::likert(.) %>% 
  
  # plot
  plot() +
  labs(y = "Prozent", title = "Kennst du eine Open Science Initiative an deiner Universität?") +
  guides(fill=guide_legend(title="Antwort")) +
  NULL


# print plots
nosi_plot_ger
nosi_plot_en


ggsave("plots/german/nosi_plot_ger.png", nosi_plot_ger, height = 4)
ggsave("plots/english/nosi_plot_en.png", nosi_plot_en, height = 4)
