# Author:       Johannes Brachem
# Last Update:  12.08.2019
# Purpose:      Create a plot, showing the number of QRPs applied per project
# Output:       "plots/german/n_qrps_per_project_ger.png"
#               "plots/german/n_qrps_per_project_en.png"

# packages and imports
library(tidyverse)
library(glue)
df_practices <- read_csv("data/df_practices.csv")%>% 
  filter(field_of_study_mc == "Psychology" | study_degree_field == "Psychology") %>% 
  filter(emp_experience == "Yes")


# preliminary data wrangling
n_projects <- df_practices %>% 
  filter(project_conducted) %>% 
  filter(type_of_practice == "Questionable") %>% 
  distinct(project, id) %>% 
  group_by(project) %>% 
  summarise(n = n())

project_labels_ger <- c(glue("Anderes \nn = {n_projects[[2,2]]}"), 
                     glue("Masterarbeit \nn = {n_projects[[5,2]]}"),
                     glue("Bachelorarbeit \nn = {n_projects[[4,2]]}"), 
                     glue("Projekt \nn = {n_projects[[3,2]]}"), 
                     glue("Expra \nn = {n_projects[[1,2]]}")) %>% rev()

project_labels_en <- c(glue("Other \nn = {n_projects[[2,2]]}"), 
                        glue("Master's Thesis \nn = {n_projects[[5,2]]}"),
                        glue("Bachelor's Thesis \nn = {n_projects[[4,2]]}"), 
                        glue("Project \nn = {n_projects[[3,2]]}"), 
                        glue("Empirical Internship \nn = {n_projects[[1,2]]}")) %>% rev()

project_order <- c("emp.intern", "project", "thesis.bsc", "thesis.msc", "other")

plot_df <- df_practices %>% ungroup() %>% 
  # create factors with appropriate labels for plotting
  mutate(project_ger = factor(project, levels = project_order, labels = project_labels_ger)) %>% 
  mutate(project_en = factor(project, levels = project_order, labels = project_labels_en)) %>% 
  
  # keep only observations for qrps and projects that have been conducted
  filter(type_of_practice == "Questionable") %>% 
  filter(project_conducted) %>% 
  filter(!is.na(rp_applied)) %>% 
  
  # per project and person: how many qrps has subject i applied in project x?
  group_by(id, project) %>% 
  mutate(n = sum(rp_applied, na.rm = TRUE)) %>% ungroup() %>% 
  
  # to be able to order by the share of projects with zero qrps, we need to calculate this number
  ## first: number of projects (for each type of project)
  group_by(project) %>% 
  mutate(n_project = n_distinct(project, id)) %>% ungroup() %>% 
  ## second: how often have we seen k qrps be applied to project x?
  group_by(rp_applied, project, n) %>% 
  mutate(n_practice = n_distinct(id, project)) %>% ungroup() %>% 
  ## third: what is the share of projects with k qrps?
  mutate(share_this_n_qrps = n_practice / n_project) %>% 
  ## fourth: what is the share of projects with zero qrps?
  mutate(share_zero_qrps = ifelse(n == 0, share_this_n_qrps, NA)) %>%
  
  # make n a factor
  mutate(n_f = factor(n, levels = 0:9, labels = as.character(0:9))) %>%
  # drop redundant observations
  distinct(id, project, .keep_all = TRUE) %>%
  # filler variable for plotting
  mutate(x.helper = 1)

# create function for plotting, allowing for a switch between german and english,
# colored and greyscale and ordering of the projects by hand as defined above
# or by the share of projects with zero qrps
# choose lang = "en" for english
# choose colored = TRUE for colored
# choose order_by_zero_share = T for ordering by share of projects with zero qrps
create_plot <- function(data, lang = "ger", colored = F, order_by_zero_share = F) {
  
  # language switch
  if (lang == "ger") {
    y_lab <- "Anteil\n"
    x_lab <- "\nArt der Forschungsarbeit"
    fill_lab <- "Anzahl QRPs"
    d <- data %>% rename(proj = project_ger)
    
  } else if (lang == "en") {
    y_lab <- "Share\n"
    x_lab <- "\nType of Empirical Project"
    fill_lab <- "Number of QRPs"
    d <- data %>% rename(proj = project_en)
  }
  
  # color switch
  if (colored) {
    col_values <- c("#a1d99b", "#ffffcc", "#ffeda0", "#fed976", "#feb24c",
                    "#fd8d3c", "#fc4e2a", "#e31a1c", "#bd0026", "#800026")
    col_values <- rev(col_values)
  } else {
    col_values <- c("#000000", "#1a1a1a", "#333333", "#4d4d4d", "#666666",
                    "#7f7f7f", "#999999", "#b3b3b3", "#cccccc", "#e5e5e5")
  }
  
  # order switch
  if (order_by_zero_share) {
    d <- d %>% mutate(proj = proj %>% fct_reorder(share_zero_qrps, .fun = max, na.rm = T))
  }
  
  # plot
  out <- d %>% ggplot(aes(x = x.helper)) +
    geom_bar(aes(fill = n_f %>% fct_rev()),
             position = "fill", stat = "count",width = 0.8) +
    facet_wrap(~proj, ncol = 5, scales = "free_y", strip.position = "bottom") +
    labs(y = y_lab,
         x = x_lab,
         fill = fill_lab) +
    scale_x_continuous(labels = "", breaks = 0) + 
    scale_y_continuous(labels = c("0 %","25 %","50 %","75 %","100 %")) + 
    scale_fill_manual(values = col_values) +
    # scale_fill_manual(values = color_values) +
    theme_classic() +
    theme(strip.background = element_blank()) +
    # theme(legend.position = c(0.8, 0.4)) +
    # theme(legend.position = "top") +
    NULL
  
  return(out)
    
}


# create plot
(n_qrps_per_project_ger <- create_plot(plot_df))
(n_qrps_per_project_en <- create_plot(plot_df, lang = "en"))

# save plot
ggsave("plots/german/n_qrps_per_project_ger.png", n_qrps_per_project_ger, width = 210, height = 90, units = "mm")
ggsave("plots/german/n_qrps_per_project_en.png", n_qrps_per_project_en, width = 210, height = 90, units = "mm")
