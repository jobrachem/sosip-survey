# Author:       Johannes Brachem
# Last Update:  12.08.2019
# Purpose:      Create plots to display detailed results on rp usage
# Output:       "plots/german/rp_shares_ger.png"
#               "plots/german/rp_grid_ger.png"
#               "plots/german/rp_detailed_ger.png"  <- combined display
#               "plots/english/rp_shares_en.png"
#               "plots/english/rp_grid_en.png"
#               "plots/english/rp_detailed_en.png"  <- combined display


# packages and imports
library(tidyverse)
library(ggpubr)
source("code/co.r")
block2 <- readRDS("data/block2_results.rds")

# part one: bar plots, aggregated share of qrp usage

## preliminary wrangling

### getting the german names right
practices_ger <- c("Rounding p-Values" = "Runden von p-Werten",
                   "Flexible Sample Size" = "Adaptives Nacherheben",
                   "HARKing" = "HARKing",
                   "SR of Hypotheses" = "SB von Hypothesen",
                   "SR of Conditions" = "SB von Bedingungen",
                   "Flexible Exclusion" = "Flexibler Ausschluss von Beob.",
                   "Flexible Analysis" = "Flexible Datenanalyse",
                   "SR of Variables" = "SB von Variablen",
                   "Preregistration" = "Präregistrierung",
                   "No Sample Planning" = "Keine Stichprobenplanung",
                   "Power Analysis" = "Power-Analyse")

practices_en <- names(practices_ger) %>% factor()

practices_ger_sorted <- tibble(practices_ger, practices_en) %>% 
  arrange(practices_en) %>% pull(practices_ger)

### combining data for combined display
q03 <- block2$q03 %>%  # q03: share of subjects who applied a practice at least once
  rename(n_applied_once = n, once = share) %>% arrange(practice)

q05 <- block2$q05 %>% # q05: share of projects in which a practice was applied
  rename(projects = share) %>% ungroup() %>% arrange(practice)

combined_data <- bind_cols(q03, q05 %>% select(-c(1,2))) %>% 
  mutate(projects2 = projects) %>% # duplicate column for sorting
  mutate(once2 = once) %>% # duplicate column for sorting
  gather(once2, projects2, key = "type_of_share", value = "share") %>% 
  mutate(type_of_share = type_of_share %>% factor(levels = c("projects2", "once2"),
                                                  labels = c("projects", "once"))) %>% 
  ungroup() %>% mutate(practice = factor(practice)) %>% 
  mutate(practice_ger = factor(practice, labels = practices_ger_sorted))

## function for plotting
### this function allows for easy switching between german and english output
### choose lang == "en" for english output
### it also allows for switching between a focus on "applied at least once"
### and "share of projects in which the practice was applied".
### this affects the arrangement (the focal plot is shown lefthand) and
### the sorting (the practices are sorted with respect to the focal variable)
### choose sort_by = "once" for focus on "applied at least once"
create_rp_share_plot <- function(data, lang = "ger", sort_by = "projects") {
  
  # langugae switch
  if (lang == "ger") {
    d <- data %>% mutate(y = practice_ger)
    x_lab <- "Forschungspraktik\n"
    y_lab <- "\nAnteil Studierender, mind. einmaliger Einsatz (%)"
    fill_title <- "Art der Forschungspraktik"
    fill_lab <- c("Positiv", "Fragwürdig")
    facet_names <- c("once" = "2) Anteil TN, mind. einmaliger Einsatz (%)",
                     "projects" = "1) Anteil von Proj. mit jeweiliger Praktik (%)")
    ger <- T
    
  } else if(lang == "en") {
    d <- data %>% mutate(y = practice)
    x_lab <- "Research Practice"
    y_lab <- "Share (%)"
    fill_title <- "Type of Practice"
    fill_lab <- c("Positive", "Questionable")
    facet_names <- c("once" = "2) Share of subj., at least one application (%)",
                     "projects" = "1) Share of proj. with respective practice (%)")
    ger <- F
  }
  
  # focus switch
  if (sort_by == "projects") {
    d <- d %>% mutate(sorting_var = projects)
  } else if (sort_by == "once") {
    d <- d %>% mutate(sorting_var = once) %>% 
      mutate(type_of_share = factor(type_of_share, levels = c("once", "projects")))
    facet_names <- facet_names %>% rev()
    str_sub(facet_names[1], 1, 1) <- "2"
    str_sub(facet_names[2], 1, 1) <- "1"
    
  }
  
  # plot
  out <- d %>% ggplot(aes(x = fct_reorder(y, sorting_var),
                          y = share, fill = type_of_practice)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(limits = c(0,60), breaks = c(0, 10, 20, 30, 40, 50, 60)) +
    facet_wrap(~type_of_share,
               labeller = as_labeller(facet_names),
               strip.position = "bottom") +
    coord_flip() +
    geom_text(aes(label = share %>% co(nround = 1, nsmall = 1, ger = ger)), size = 3, nudge_y = 5.5) +
    scale_fill_grey(labels = fill_lab) +
    labs(x = x_lab, y = y_lab, fill = fill_title) +
    theme_classic() +
    theme(legend.position = "top", 
          strip.background = element_blank(),
          strip.placement = "outside",
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    NULL
  
  return(out)
}




# part two: grid plot, rp usage per project
## preliminary data wrangling
practice_ordered <- block2$q05 %>% ungroup() %>% 
  mutate(practice = factor(practice)) %>% 
  mutate(practice_ger = factor(practice, labels = practices_ger_sorted)) %>%
  arrange(share) %>% select(practice_en = practice, practice_ger)

project_order <- c("emp.intern", "project", "thesis.bsc", "thesis.msc", "other")
projects_en <- c("Emp. Internship", "Project", "Bachelor's Thesis",
                 "Master's Thesis", "Other")
projects_ger <- c("Expra", "Projekt", "Bachelorarbeit", "Masterarbeit", "Anderes")

### give appropriate labels and levels to the factors
grid_data <- block2$q04 %>% ungroup() %>% 
  mutate(practice_en = factor(practice, levels = practice_ordered$practice_en)) %>% 
  mutate(practice_ger = factor(practice, levels = practice_ordered$practice_en,
                               labels = practice_ordered$practice_ger)) %>% 
  mutate(project_en = factor(project, levels = project_order, labels = projects_en)) %>% 
  mutate(project_ger = factor(project, levels = project_order, labels = projects_ger)) %>% 
  select(-practice, -project) %>% 
  mutate(type_of_practice_ger = type_of_practice %>% 
           factor(levels = c("Questionable", "Open"),
                  labels = c("Fragwürdig", "Positiv")))

## create function to easily switch between plotting an english and a german version
### select lang = "en" for english version
### select colored = TRUE for a colored version
create_grid_plot <- function(data, lang = "ger", colored = FALSE) {
  
  # language switch
  if (lang == "ger") {
    d <- data %>% mutate(y = practice_ger, x = project_ger, facets = type_of_practice_ger)
    fill_title <- "Anteil (%)"
    x_lab <- "Art des Forschungsprojekts\n"
    ger <- T
  } else if (lang == "en") {
    d <- data %>% mutate(y = practice_en, x = project_en, facets = type_of_practice)
    fill_title <- "Share (%)"
    x_lab <- "Type of Empirical Project\n"
    ger <- F
  }
  
  # color switch
  if (colored) {
    low_color <- "#ffffcc"
    high_color <- "#bd0026"
  } else {
    low_color <- "#DCDCDC"
    high_color <- "#696969"
  }
  
  # plot
  out <- d %>% ggplot(aes(x = x, y = y)) +
    geom_raster(aes(fill = share)) +
    geom_text(aes(label = share %>% co(nround = 1, nsmall = 1, ger = ger))) +
    scale_fill_gradient(low = low_color, high = high_color, limits = c(0,50)) +
    theme_classic()+
    facet_grid(rows = vars(facets), 
               scales = "free_y", space = "free_y") +
    theme(#axis.title = element_blank(), 
      axis.title.y = element_blank(),
      strip.background = element_blank(),
      legend.position = "bottom") +
    scale_x_discrete(position = "top", 
                     labels = ) +
    labs(fill = fill_title, x = x_lab) +
    theme(plot.margin = unit(c(1,0,0,0), "cm"))
  
  return(out)
}


# create plots
# german
(rp_shares_ger <- create_rp_share_plot(combined_data))
(grid_ger <- create_grid_plot(grid_data))
(rp_detailed_ger <- ggarrange(rp_shares_ger, grid_ger, nrow = 2, ncol = 1, 
                                 labels = "auto", heights = c(1, 1.2)))


# english
(rp_shares_en <- create_rp_share_plot(combined_data, lang = "en"))
(grid_en <- create_grid_plot(grid_data, lang = "en"))
(rp_detailed_en <- ggarrange(rp_shares_en, grid_en, nrow = 2, ncol = 1, 
                                 labels = "auto", heights = c(1, 1.2)))


# save output
# german
ggsave("plots/german/rp_shares_ger.png", rp_shares_ger, width = 210, height = 90, units = "mm")
ggsave("plots/german/rp_grid_ger.png", grid_ger, width = 210, height = 130, units = "mm")
ggsave("plots/german/rp_detailed_ger.png", rp_detailed_ger, width = 180, height = 170, units = "mm")

# egnlish
ggsave("plots/english/rp_shares_en.png", rp_shares_en, width = 210, height = 90, units = "mm")
ggsave("plots/english/rp_grid_en.png", grid_en, width = 210, height = 130, units = "mm")
ggsave("plots/english/rp_detailed_en.png", rp_detailed_en, width = 180, height = 170, units = "mm")


