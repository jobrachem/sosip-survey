# Author:       Johannes Brachem
# Last Update:  21.05.2019
# Purpose:      Replace the value of "uni_current" and "uni_bachelor" with neutral ids,
#               if there are fewer than 30 data points from that university.
#               We do this to 1) protect the identity of our participants and
#               2) to protect individual universities from unsolicited 
#               shaming based on survey results.
#               Also, the basic exclusions are processed here and data about 
#               the exclusions is saved.
# Output:       start.csv




# import data
unis_to_include <- readRDS("data/unis_to_include.rds")
raw <- read_csv("data/raw.csv")

excl <- raw %>% basic_exclusions() 
d1 <- excl$data %>% fix_typos()

uni <- d1 %>% filter(!(uni_current %in% unis_to_include$uni)) %>% 
  pull(uni_current) %>% unique()

ids <- tibble(uni, id = 1:length(uni)) %>% 
  replace_na(replace = list(uni = "na"))

x <- ids %>% pull(id)
names(x) <- ids %>% pull(uni)

# filter netherlands universities
d2 <- d1 %>% german_unis()

# do replacement
d3 <- d2 %>% 
  replace_na(replace = list(uni_current = "na")) %>% 
  mutate(uni_current = ifelse(uni_current %in% unis_to_include$uni,
                              uni_current, x[uni_current])) %>% 
  mutate(uni_bachelor = ifelse(uni_bachelor %in% unis_to_include$uni,
                               uni_bachelor, x[uni_current]))

start <- d3

excl$exclusions$ger <- nrow(d2)

# save result
write_csv(start, "data/start.csv")
saveRDS(excl$exclusions, "data/exclusions.rds")
