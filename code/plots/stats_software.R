# Author:       Johannes Brachem
# Last Update:  17.08.2019
# Purpose:      Create a plot, showing the use of statistical software by our participants
# Output:       "plots/german/stats_plot_ger.png"
#               "plots/english/stats_plot_en.png"

# packages and imports
library(tidyverse)
stats <- readRDS("data/stats_software.rds")
main <- read_csv("data/main.csv") %>% 
  filter(field_of_study_mc == "Psychology" | study_degree_field == "Psychology")


n_participants <- nrow(main_all)

# plot
stats_plot <- stats %>% 
  # wrangling
  mutate(software = ifelse(n < 10, "other", software)) %>% 
  group_by(software) %>%
  summarise(n = sum(n)) %>%
  mutate(share = n / n_participants * 100) %>% 
  
  # plot
  ggplot(aes(x = fct_reorder(software, share), y = share, fill = software)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_brewer(palette = "Set1") +
  coord_flip() +
  theme_classic()

stats_plot_en.out <- stats_plot +
  labs(#title = "Use of statistical software amongst German Psychology Students",
    #subtitle = glue("N = {n_participants}"),
    x = "Software",
    y = "Share (%)")

stats_plot_ger.out <- stats_plot +
  labs(x = "Software",
       y = "Anteil (%)") +
  theme(legend.position = "none")

# print plot
stats_plot_en.out
stats_plot_ger.out


ggsave("plots/english/stats_plot_en.png", stats_plot_en.out, width = 6, height = 3)
ggsave("plots/german/stats_plot_ger.png", stats_plot_ger.out, width = 6, height = 3)