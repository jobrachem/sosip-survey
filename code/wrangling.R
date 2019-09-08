# Author:       Johannes Brachem
# Last Update:  27.08.2019
# Purpose:      Prepare data
# Output:       

#### 1.2 Data import from disk ####
d <- read_csv("data/start.csv")

#### 2 Exclusion of participants ####
# apply basic exclusion criteria
excl <- d %>% basic_exclusions()

d <- excl$data %>% german_unis()

excl$exclusions$ger <- nrow(d)

# --- participants with surprising age --- #
# deal with participants who are surprisingly young
# Some people are surprisingly young. Maybe typos in the age field? Otherwise, 
# they seem to be normal students and have not left any comments. 
# For us, it seems most likely that participants who indicate an age of 14 or 15
# while studying e.g. in their 5th master's term either paid little attention 
# or did not answer seriously.
# We therefore exclude participants who indicated an age of 14 or 15 (9 cases).
# The two 17-year-old participants in their first bachelor's term seem plausible
# however. We keep those in the data.
d %>% pull(age) %>% table()
d %>% filter(age < 18) %>% select(study_stage_mc, semester, age)
d <- d %>% filter(age >16) 

d <- d %>%  mutate(id = 1:nrow(.))

#### 2.1 Exclusion of items ####
# --- illogical responses --- #
# This code chunk allows to check for illogical responses. 
# An example of what we are looking for: 
# A participant responds that she has never preregistered a study (coded `"6"`),
# but also that she has preregistered her bachelor's thesis 
# (coded `"3"`, full answer: `"6, 3"`). Such an answer was technically possible, 
# but would be illogical and indicate problems with the answer. 

# Thus, with the code below, we look for observations that contain a `"6"` 
# but have more than one character, indicating that apart from `"6"`, 
# another answer was given. If we find such observations, they are excluded 
# from further analysis.

# create marker variable 
# data frame is turned into long format in the process
d_illog <- d %>% 
  gather(matches("qrp[0-9]"), matches("orp"), 
         key = "practice", value = "answer") %>% 
  mutate(illogical_answer = ifelse(answer %contains% "6" & nchar(answer) > 1, 
                                   TRUE, FALSE)) 

# look at how many problemativc observations there are
n_prob <- d_illog %>% select(illogical_answer) %>% 
  filter(illogical_answer == TRUE) %>% nrow()

# share of problematic items
n_prob / (d_illog %>% nrow())

# remove problematic observations
# data frame is turned back to wide format
# NAs are introduced in place of the problematic observations
d <- d_illog %>% 
  group_by(id) %>% 
  mutate(illog_answ_subj = ifelse(any(illogical_answer == TRUE), TRUE, FALSE)) %>% 
  ungroup() %>% 
  filter(is.na(illogical_answer) | illogical_answer == FALSE) %>% 
  spread(key = "practice", value = "answer") %>% 
  select(-illogical_answer)

#### 3 Wrangle categorical data ####
d <- d %>% mutate(
  study_stage_mc = factor(study_stage_mc, labels = lab.stage),
  field_of_study_mc = factor(field_of_study_mc, labels = lab.field),
  emp_experience = factor(emp_experience, labels = lab.yn1),
  study_degree = factor(study_degree, levels = 1:5, labels = lab.degree),
  study_degree_field = factor(study_degree_field, labels = lab.field),
  sex = factor(sex, labels = lab.sex),
  rc_teaching = factor(rc_teaching, labels = lab.rc_lectures),
  nosi = factor(nosi, labels = lab.yn2),
  nosi_at_uni = ifelse(uni_current %in% nosi_unis, "Yes", "No"),
  nosi_events_mc = factor(nosi_events_mc, labels = lab.yn2))

#### 4 Add variables ####
d <- d %>% add_n_projects()


#### 5 Create long data frame ####
# add indicators: has practice i been applied to project k?
practices_aggregation <- d %>% 
  select(matches("qrp[0-9]"), matches("orp")) %>%
  map2_dfc(names(.), spread_practices) # map2 iterates over 2 vectors

# gather
d.l <- bind_cols(d, practices_aggregation) %>% 
  gather(qrp01_emp.intern:orp02_other, key = "practice_project", value = "rp_applied") %>% 
  separate(practice_project, into = c("practice", "project"), sep = "_")

#### 5.1 Add variables to long DF ####
# indicator: was project conducted?
d.l <- d.l %>% 
  mutate(project = factor(project, levels = lab.project)) %>% 
  mutate(project_conducted = ifelse(exp_specific %contains% as.numeric(project), TRUE, FALSE)) %>% 
  mutate(project_conducted = ifelse(is.na(project_conducted), FALSE, project_conducted))

# most likely university associated with a project
d.l <- d.l %>% 
  mutate(uni_project = ifelse(study_stage_mc != "Bachelor" & (project == "thesis.bsc" | project == "emp.intern"),
                              uni_bachelor, as.character(uni_current))) %>%
  mutate(uni_project = ifelse(study_stage_mc != "Bachelor" & uni_current != uni_bachelor & (project == "project" | project == "other"),
                              "_unclear", uni_project)) %>% 
  mutate(uni_project = ifelse(project == "thesis.msc", uni_current, uni_project)) %>% 
  replace_na(list(uni_project = "_unclear"))

# idicator: was practice taught in a lecure?
d.l <- d.l %>% 
  mutate(q.practice = str_replace(practice, "qrp0", "")) %>% 
  mutate(practice_taught = ifelse(str_detect(as.character(specific_qrps_teaching), q.practice), "yes", "no")) %>% 
  select(-q.practice)

# indicator: is practice questionable or open?
d.l <- d.l %>% 
  mutate(rp_applied = ifelse(project_conducted, rp_applied, NA)) %>% 
  mutate(practice = factor(practice, labels = c(orp_names, qrp_names)))

d.l <- d.l %>% 
  mutate(type_of_practice = ifelse(practice %in% qrp_names, "Questionable", "Open"))

#### 6 Remove Variables ####
# d <- d %>% select(-c(1:6),-c(orp01:qrp09), -c(n_project:n_other))
# d.l <- d.l %>% select(-c(1:6),-c(orp01:qrp09), -c(n_project:n_other))

#### 7 Save results ####
write_csv(d, "data/wide.csv")
write_csv(d.l, "data/long.csv")

