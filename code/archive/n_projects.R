# Author:         Johannes Brachem
# Last Update:    27.05.2019
# Purpose:        Count number of project data points per university
# Output:         n_projects_uni.csv



library(tidyverse)
library(formr)
source("code/helper_functions.R")
main <- read_csv("data/main_raw.csv") %>% basic_exclusions() %>% 
  fix_typos() %>% add_n_projects()

## Number of Projects conducted per University and Project Type

### Current B.Sc. Students
# For B.Sc. students, it's straightfoward. 
# We simply count the number of projects that were conducted by our 
# participants and sum them up for each university and project.

# number of total project data per uni per project for current bsc students
n_proj_bsc_students <- main %>% 
  filter(study_stage_mc == 1) %>% 
  group_by(uni_current) %>% 
  summarise(emp.intern = sum(n_emp.intern, na.rm = TRUE),
            project = sum(n_project, na.rm = TRUE),
            thesis.bsc = sum(n_thesis.bsc, na.rm = TRUE),
            thesis.msc = sum (n_thesis.msc, na.rm = TRUE),
            other = sum(n_other, na.rm = TRUE)) %>% 
  gather(emp.intern:other, key = "project", value = "n_applied") %>% 
  rename(uni = uni_current)

# Some people are currently enrolled in a bachelor's program, 
# but indicated that they have already written a master's thesis.
# Currently: n = 3
# All 3 of those are old enough to possibly be studying for a second time.
# One of them told us so in the free text comment.
main %>% 
  filter(study_stage_mc == 1) %>% 
  filter(exp_specific %contains% "4") %>% 
  select(age, comments) %>% 
  identity()

### Other participants
# Participants who are enrolled in an M.Sc. program or have finished their 
# studies need to be dealt with.

#### Master's theses
# Counting master's theses for the current uni of students enrolled 
# in master's programs and alumni.

n_proj_thesis.msc <- main %>% 
  filter(study_stage_mc != 1) %>% 
  group_by(uni_current) %>% 
  summarise(thesis.msc = sum(n_thesis.msc, na.rm = TRUE)) %>% 
  gather(thesis.msc, key = "project", value = "n_applied") %>% 
  rename(uni = uni_current)

#### Empirical internship and Bachelor's thesis
# These projects need to be counted towards the participants' bachelor uni,
# not towards the students' master uni.

n_proj_bsc.specific <- main %>% 
  filter(study_stage_mc != 1) %>% 
  group_by(uni_bachelor) %>% 
  summarise(thesis.bsc = sum(n_thesis.bsc, na.rm = TRUE),
            emp.intern = sum(n_emp.intern, na.rm = TRUE)) %>% 
  gather(thesis.bsc:emp.intern, key = "project", value = "n_applied") %>% 
  rename(uni = uni_bachelor)

#### "Project"s and "other"
# Counting "project" and "other" towards uni_current is not a problem, 
# if `uni_current == uni_bachelor`.
n_proj_uni.notswitched <- main %>% 
  filter(study_stage_mc != 1) %>% 
  filter(uni_current == uni_bachelor) %>% 
  group_by(uni_current) %>% 
  summarise(project = sum(n_project, na.rm = TRUE),
            other = sum(n_other, na.rm = TRUE)) %>% 
  gather(project:other, key = "project", value = "n_applied") %>% 
  rename(uni = uni_current)

# If `uni_current != uni_bachelor`, it's hard to identify which university a 
# project belongs to, except for the already mentioned empirical internship, 
# bachelor's thesis and master's thesis.
# Maybe we can count "project" and "other" towards `uni_bachelor` 
# if `semester < 2`, assuming that data about a project from a student this 
# early in his/her master's program most likely refers to a project conducted 
# during his/her B.Sc.

n_proj_bsc.assumed <- main %>% 
  filter(study_stage_mc != 1) %>%
  filter(uni_current != uni_bachelor & semester < 2) %>%
  group_by(uni_bachelor) %>% 
  summarise(project = sum(n_project, na.rm = TRUE),
            other = sum(n_other, na.rm = TRUE)) %>% 
  gather(project:other, key = "project", value = "n_applied") %>% 
  rename(uni = uni_bachelor)

# We can also look into the free text comments to see if the participant 
# left information that helps us to decide, how to count his/her projects.
# look at the comments of students who switched unis
comments_uni_changers <- main %>%
  filter(study_stage_mc != 1) %>% 
  filter(uni_current != uni_bachelor) %>% 
  #filter(semester > 2) %>% 
  select(comments)

### Joining all partsums
n_projects_uni <- bind_rows(n_proj_bsc_students, 
                            n_proj_thesis.msc,
                            n_proj_bsc.specific,
                            n_proj_uni.notswitched,
                            n_proj_bsc.assumed) %>% 
  group_by(uni, project) %>% 
  summarise(n_applied = sum(n_applied)) %>% 
  mutate(n_total = sum(n_applied)) %>% 
  arrange(n_total %>% desc())


### Take a look
n_projects_uni %>% 
  distinct(uni, n_total) %>% 
  print(n = nrow(.))


# save
write_csv(n_projects_uni, "data/n_projects_uni.csv")