# Author:       Johannes Brachem
# Last Update:  05.03.2020
# Purpose:      Remove participant age from public data set to ensure data protection.
# Output:       start.csv, wide.csv, long.csv

library(dplyr)
library(readr)

start <- read_csv("data/start.csv")
wide <- read_csv("data/wide.csv")
long <- read_csv("data/long.csv")

start <- start %>% mutate(age = NA)
wide <- wide %>% mutate(age = NA)
long <- long %>% mutate(age = NA)

write_csv(start, "data/start.csv")
write_csv(wide, "data/wide.csv")
write_csv(long, "data/long.csv")

