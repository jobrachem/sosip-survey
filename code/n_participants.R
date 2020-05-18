# Author:         Johannes Brachem
# Last Update:    07.05.2020
# Purpose:        Extract total number of participants per university and number of 
#                 observations per university. Save those as .rds objects.
#                 Save a vector of universities with more than 30 observations.
# Output:         n_observations_uni.csv (no. of participants per university)
#                 n_observations_detailed.csv (like above, but including add. info
#                 on Bachelors, Masters, and raw numbers)
#                 unis_to_include.rds    (vector of universities with >30 obs.)
#                 switchers.rds (no. of participants who changed university)


excl <- read_csv("data/raw.csv") %>% 
  basic_exclusions()

main <- excl$data %>% fix_typos()

## Number of Participants per University

### Raw Number of Participants per University
# This number counts every participant once for their current/last university.

n_participants_raw <- main %>% 
  group_by(uni_current) %>% 
  summarise(n = n()) %>% arrange(desc(n)) %>% 
  print(n = nrow(.))

n_participants_raw$n %>% sum()

### Adjusted Number of Participants per University
# This Number takes the fact into account that for one participant who did 
# their B.Sc. at university A and is currently enrollend in a M.Sc. programme at 
# university B, we actually have a participant for university A, *and* a 
# participant for university *B*, even though it's the same person.

# It's straightforward to count B.Sc. students.
n_subj_bsc <- main %>% filter(study_stage_mc == 1) %>% 
  group_by(uni_current) %>% summarise(n = n()) %>% ungroup() %>% 
  rename(uni = uni_current)

# Non-B.Sc. students who switched uni: 
# For these participants, we also count an observation for their bachelor university.
n_subj_bsc2 <- main %>% 
  filter(study_stage_mc != 1 & uni_current != uni_bachelor) %>% 
  group_by(uni_bachelor) %>% summarise(n = n()) %>% ungroup() %>% 
  rename(uni = uni_bachelor) %>% 
  filter(uni != "iran")

# total number of extra observations
switchers <- n_subj_bsc2 %>% pull(n) %>% sum()

# Non-B.Sc. students are counted towards their current university 
# (university of graduation for alumni),
n_subj_beyond.bsc <- main %>% 
  filter(study_stage_mc != 1) %>% 
  group_by(uni_current) %>% summarise(n = n()) %>% ungroup() %>% 
  rename(uni = uni_current)

# aggregation
n_observations_uni <- bind_rows(n_subj_bsc, n_subj_bsc2, n_subj_beyond.bsc) %>% 
  group_by(uni) %>% summarise(n = sum(n, na.rm = TRUE)) %>% ungroup() %>% arrange(uni)

n_observations_uni %>% summarise(n = sum(n))

# detailed aggregation

n1 <- n_subj_bsc
n1$stage = "Bachelor"

n2 <- n_subj_bsc2
n2$stage = "Bachelor"

n3 <- n_subj_beyond.bsc
n3$stage = "Master+"

n <- bind_rows(n1, n2, n3) %>% 
  group_by(uni, stage) %>% 
  summarise(n = sum(n)) %>% spread(key="stage", value="n") %>% arrange(uni) %>% 
  ungroup() %>% 
  mutate(total = n_observations_uni$n) %>% 
  arrange(desc(total))

n$raw <- NA

for (uni in n_participants_raw$uni_current) {
  if (is.na(uni)){
    row <- which(is.na(n$uni))
    count <- n_participants_raw %>% filter(is.na(uni_current)) %>% pull(n)
  } else {
    row <- which(n$uni == uni)
    count <- n_participants_raw %>% filter(uni_current == uni) %>% pull(n)
  }

  n[row, "raw"] <- count
}

# total number of university-level observations
n_observations_uni %>% pull(n) %>% sum()

### Take a look
n_observations_uni %>% arrange(n %>% desc()) %>% print(n = nrow(.))

### Subset of Universities to include in further analysis
unis_to_include <- n_observations_uni %>% filter(n >= 30)

# save data
write_csv(n_observations_uni, "data/n_observations_uni.csv")
write_csv(n, "data/n_observations_uni_detailed.csv")
saveRDS(unis_to_include, "data/unis_to_include.rds")
saveRDS(switchers, "data/switchers.rds")
