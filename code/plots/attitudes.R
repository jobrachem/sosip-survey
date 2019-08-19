# Author:       Johannes Brachem
# Last Update:  17.08.2019
# Purpose:      Create a plot, visualizing participants' answers to the questions
#               1) How important are the topics replication crisis / open science
#                   in your opinion?
#               2) How interested are you in the topics replication crisis / open
#                   science?
#               3) How well informed do you feel about the topics replication crisis /
#                   open science?
# Output:       "plots/english/attitudes_plot_en.png"
#               "plots/german/attitudes_plot_ger.png"


# packages and imports
library(tidyverse)

main <- read_csv("data/main.csv") %>% 
  filter(field_of_study_mc == "Psychology" | study_degree_field == "Psychology")


# plot
likert_labs_ger <- c("gar nicht", "2", "3", "4", "sehr")
likert_labs_en <- c("not at all", "2", "3", "4", "very much")

attitudes_plot <- function(likert_labs) {
  attitudes_plot <- main %>% 
    select(interest, importance, felt_information) %>% 
    mutate(interest = factor(interest, levels = 1:5, labels = likert_labs)) %>%
    mutate(importance = factor(importance, levels = 1:5, labels = likert_labs)) %>% 
    mutate(felt_information = factor(felt_information, 
                                     levels = 1:5, labels = likert_labs)) %>% 
    as.data.frame() %>% 
    likert::likert() %>% 
    plot() + 
    NULL
  
  return(attitudes_plot)
}

attitudes_plot_ger <- attitudes_plot(likert_labs_ger) +
  scale_x_discrete(labels = c("Eindruck von Informiertheit", "Interesse", "Wichtigkeit")) +
  labs(y = "\nProzent", color = "Antwort") +
  guides(fill=guide_legend(title="Antwort"))

attitudes_plot_en <- attitudes_plot(likert_labs_en) +
  scale_x_discrete(labels = c("Felt Information", "Interest", "Importance")) +
  # labs(title = "Attitudes towards Open Science and Replication Crisis") +
  NULL

# print plot
attitudes_plot_en
attitudes_plot_ger


ggsave("plots/english/attitudes_plot_en.png", 
       attitudes_plot_en, width = 170, height = 60, units = "mm")
ggsave("plots/german/attitudes_plot_ger.png", 
       attitudes_plot_ger, width = 170, height = 60, units = "mm")