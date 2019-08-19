# Author:       Johannes Brachem
# Last Update:  21.05.2019
# Purpose:      Replace the value of "uni_current" and "uni_bachelor" with NA,
#               if there are fewer than 30 data points from that university.
#               We do this to 1) protect the identity of our participants and
#               2) to protect individual universities from unsolicited 
#               shaming based on survey results.
# Output:       main_start.csv


library(tidyverse)

# import data
unis_to_include <- readRDS("data/unis_to_include.rds")
main_raw <- read_csv("data/main_raw.csv")

main_raw <- main_raw %>% 
  filter(!(uni_current %contains% "Amsterdam") | is.na(uni_current)) %>% 
  filter(!(uni_current %contains% "[Gg]roningen") | is.na(uni_current))

# do replacement
main_start <- main_raw %>% 
  mutate(uni_current = ifelse(uni_current %in% unis_to_include$uni,
                              uni_current, NA)) %>% 
  mutate(uni_bachelor = ifelse(uni_bachelor %in% unis_to_include$uni,
                               uni_bachelor, NA))

# look at difference in NAs
main_raw %>% select(uni_bachelor, uni_current) %>% is.na() %>%  sum()
main_start %>% select(uni_bachelor, uni_current) %>% is.na() %>%  sum()

# save result
write_csv(main_start, "data/main_start.csv")
