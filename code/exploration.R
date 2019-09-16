# Author:       Johannes Brachem
# Last Update:  31.08.2019
# Purpose:      Exploratory analyses
# Output:       "data/exploration.rds" R object, containing all results

#### Imports ####

data <- read_csv("data/long.csv", 
                 col_types = cols(study_stage_open = col_character()))

#### Wrangling ####
# wrangle factors
data <- data %>% 
  mutate_at(c("age", "interest", "importance", "felt_information"),  # , "n_projects"
            scale) %>% 
  mutate(sex = sex %>% factor(levels = lab.sex)) %>% 
  mutate(sex = sex %>% relevel("female")) %>% 
  mutate(practice = practice %>% factor() %>% relevel("No Sample Planning")) %>% 
  mutate(rc_teaching = rc_teaching %>% factor() %>% relevel("no"))

# filter data
d_filtered <- data %>% 
  only_psych() %>% filter(emp_experience == "Yes") %>% 
  filter(project_conducted) 

d <- d_filtered

#### Descriptives for exploration ####

d %>% summarise(n = n_distinct(id))
d %>% pull(uni_project) %>% is.na() %>% sum()
d %>% pull(uni_project) %>% is.na() %>% sum()

# number of projects, for which the university is clear/unclear
# The uni for 'project' and 'other' is considered clear, if the participant
# is currently doing his/her bachelor's degree. It is considered unclear,
# if the participant is currently doing his/her master's degree, because these
# projects could have then been done either at the participants' bachelor-university
# or at the participants' master-university
(proj <- d %>% distinct(id, project, .keep_all = TRUE) %>% 
    mutate(uni = ifelse(uni_project == "_unclear", "_unclear", "clear")) %>% 
    group_by(uni, project) %>% summarise(n = n()) %>% ungroup() )

# total number of clear and unclear project-unis
(proj.aggregated <- proj %>% group_by(uni) %>% summarise(n = sum(n)))

# closer look at participants with unclear project-unis
d %>% distinct(id, uni_project, .keep_all = TRUE) %>% 
  filter(uni_project == "_unclear") %>% 
  distinct(id, .keep_all = TRUE) %>% 
  select(id, study_stage_mc, uni_current, uni_bachelor, uni_project) %>% 
  print(n = nrow(.))

# closer look at participants who switched universities
equal_unis <- d %>% distinct(id, .keep_all = TRUE) %>% 
  mutate(unis_equal = ifelse(study_stage_mc != "Bachelor" & uni_current != uni_bachelor, "no", "yes")) %>% 
  mutate(unis_equal = ifelse(study_stage_mc != "Bachelor" & study_stage_mc != "Master", "unclear", unis_equal))

# number of participants who switched uni, who didn't switch uni or for whom we don't know it
equal_unis %>% group_by(unis_equal)  %>% summarise(n = n())

# printout of all participants who switched uni or for whom we don't know it
equal_unis %>% filter(unis_equal == "no" | unis_equal == "unclear") %>% 
  select(id, study_stage_mc, uni_current, uni_bachelor) %>% 
  print(n = nrow(.))

# number of projects per uni
d %>% distinct(id, project, .keep_all = TRUE) %>% 
  group_by(uni_project) %>% 
  summarise(n = n()) %>% print(n = nrow(.))


#### Models for QRPs ####

d_clear_qrp <- d %>% 
  filter(uni_project != "_unclear") %>% 
  filter(type_of_practice == "Questionable")

# baseline without random effects
qrp.m0 <- glm(rp_applied ~ +1, data = d_clear_qrp, family = binomial(link = "logit"))

# add random intercept for participant
qrp.m1a <- glmer(rp_applied ~ (1|id), data = d_clear_qrp,
                 family = binomial(link = "logit"),
                 control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)))
qrp.m1b <- update(qrp.m1a, .~. + (1|uni_project)) # randint for university
qrp.m1c <- update(qrp.m1b, .~. + (1|practice)) # randint for practice in project
# qrp.m1b <- update(qrp.m1a, .~. + (1|project/practice)) # randint for practice in project
# qrp.m1c <- update(qrp.m1b, .~. + (1|uni_project)) # randint for university

# likelihood-ratio-tests
qrp.an1 <- anova(qrp.m1a, qrp.m0) # test for variation between participants
qrp.an2 <- anova(qrp.m1a, qrp.m1b, qrp.m1c) # test for variation between universities

# add fixed effects
qrp.m2 <- update(qrp.m1c, .~. + age + sex + study_stage_mc + n_projects +
                   interest + importance + felt_information +
                   nosi_at_uni + nosi +
                   rc_teaching + practice_taught)

# qrp.m2.all <- allFit(qrp.m2)

# add fixed effetcs part 2
# qrp.m3 <- update(qrp.m2, .~. + practice)  # fixef for practice
qrp.m3 <- update(qrp.m2, .~. + project)   # fixef for project
# qrp.m4b <- update(qrp.m3, .~. + project,
#                   control = glmerControl(optimizer = "Nelder_Mead",
#                                          optCtrl=list(maxfun=4e6), 
#                                          calc.derivs = F))   # fixef for project
# qrp.m4c <- update(qrp.m3, .~. + project,
#                   control = glmerControl(optimizer = "nloptwrap",
#                                          optCtrl=list(maxfun=4e6), 
#                                          calc.derivs = F))   # fixef for project
# qrp.m4d <- update(qrp.m3, .~. + project,
#                   control = glmerControl(optCtrl=list(maxfun=4e6), 
#                                          calc.derivs = F))   # fixef for project
# 
qrp.m3 %>% summary()
# # likelihood-ratio-test
qrp.an3 <- anova(qrp.m2, qrp.m3)

# save to list
qrp <- list(models = list(m0 = qrp.m0, m1a = qrp.m1a, m1b = qrp.m1b, m1c = qrp.m1c,
                          m2 = qrp.m2, m3 = qrp.m3),#, m3 = qrp.m3, m4 = qrp.m4a),
            lr_tests = list(an1 = qrp.an1, an2 = qrp.an2, an3 = qrp.an3))
#m4_models = list(a = qrp.m4a, b = qrp.m4b, c = qrp.m4c, d = qrp.m4d))

#### Models for positive RPs ####

d_clear_orp <- d %>% 
  filter(uni_project != "_unclear") %>% 
  filter(type_of_practice != "Questionable") %>% 
  mutate(practice = practice %>% factor() %>% relevel("Power Analysis"))

# baseline without random effects
orp.m0 <- glm(rp_applied ~ +1, data = d_clear_orp, family = binomial(link = "logit"))

# add random intercept for participant
orp.m1a <- glmer(rp_applied ~ (1|id), data = d_clear_orp,
                 family = binomial(link = "logit"),
                 control = glmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=4e6), calc.derivs = F))
orp.m1b <- update(orp.m1a, .~. + (1|uni_project)) # randint for university
orp.m1c <- update(orp.m1b, .~. + (1|practice)) # randint for practice

# likelihood-ratio-tests
orp.an1 <- anova(orp.m1a, orp.m0) # test for variation between participants
orp.an2 <- anova(orp.m1a, orp.m1b, orp.m1c) # test for variation between universities

# add fixed effects part 1
orp.m2 <- update(orp.m1b, .~. + age + sex + study_stage_mc + n_projects +
                   interest + importance + felt_information +
                   nosi_at_uni + nosi +
                   rc_teaching)

orp.m2 %>% summary()

# # add fixed effetcs part 2
# orp.m3 <- update(orp.m2, .~. + practice)  # fixef for practice
orp.m3 <- update(orp.m2, .~. + project)   # fixef for project
# 
# # likelihood-ratio-test
orp.an3 <- anova(orp.m2, orp.m3)

# save to list
orp <- list(models = list(m0 = orp.m0, m1a = orp.m1a, m1b = orp.m1b, m1c = orp.m1c,
                          m2 = orp.m2, m3 = orp.m3),#, m3 = orp.m3, m4 = orp.m4),
            lr_tests = list(an1 = orp.an1, an2 = orp.an2))#, an3 = orp.an3))


#### Linear trend of project type ####
# --- QRPs --- #
proj <- c("emp.intern", "thesis.bsc", "thesis.msc")
d_linear.qrp <- d_clear_qrp %>% 
  filter(project %in% proj) %>% 
  mutate(project = factor(project, levels = proj, ordered = TRUE))

lin.qrp.m1 <- glmer(rp_applied ~ (1|id) + (1|uni_current) + (1|practice), data = d_linear.qrp,
                    family = binomial(link = "logit"),
                    control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=4e6)))

lin.qrp.m2 <- update(lin.qrp.m1, .~. + age + sex + study_stage_mc + n_projects +
                       interest + importance + felt_information +
                       nosi_at_uni + nosi +
                       rc_teaching + practice_taught)
lin.qrp.m3 <- update(lin.qrp.m2, .~. + project)
lin.qrp.m2 %>% summary()
lin.qrp.m3 %>% summary()

lin.qrp <- list(models = list(m1 = lin.qrp.m1, m2 = lin.qrp.m2, m3 = lin.qrp.m3))

# --- ORPs --- #
proj <- c("emp.intern", "thesis.bsc", "thesis.msc")
d_linear.orp <- d_clear_orp %>% 
  filter(project %in% proj) %>% 
  mutate(project = factor(project, levels = proj, ordered = TRUE))

lin.orp.m1 <- glmer(rp_applied ~ (1|id) + (1|uni_current) + (1|practice), data = d_linear.orp,
                    family = binomial(link = "logit"),
                    control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=4e6)))

lin.orp.m2 <- update(lin.orp.m1, .~. + age + sex + study_stage_mc + n_projects +
                       interest + importance + felt_information +
                       nosi_at_uni + nosi +
                       rc_teaching)
lin.orp.m3 <- update(lin.orp.m2, .~. + project)
lin.orp.m2 %>% summary()
lin.orp.m3 %>% summary()

lin.orp <- list(models = list(m1 = lin.orp.m1, m2 = lin.orp.m2, m3 = lin.orp.m3))


#### Models for recent projects ####
# --- Are qrps less prevalent in more recent expras and bachelor's theses?
d_qrp_recent <- d %>% 
  filter(project == "emp.intern" | project == "thesis.bsc") %>% 
  filter(type_of_practice == "Questionable") %>% 
  filter(uni_project != "_unclear")

recent.q.m0 <- glm(rp_applied ~ +1, data = d_qrp_recent, family = binomial(link = "logit"))
recent.q.m1 <- glmer(rp_applied ~ (1|id) + (1|uni_project), data = d_qrp_recent,
                     family = binomial(link = "logit"),
                     control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)))

recent.q.an1 <- anova(recent.q.m1, recent.q.m0)

recent.q.m2 <- update(recent.q.m1, .~. + study_stage_mc + project)

recent.q.m2 %>% summary()

recent.qrp <- list(models = list(m0 = recent.q.m0, m1 = recent.q.m1, m2 = recent.q.m2),
                   lr_tests = list(an1 = recent.q.an1))

# --- Are positive rps more prevalent in more recent expras and bachelor's theses?
d_orp_recent <- d %>% 
  filter(project == "emp.intern" | project == "thesis.bsc") %>% 
  filter(type_of_practice == "Open") %>% 
  filter(uni_project != "_unclear") 

recent.o.m0 <- glm(rp_applied ~ +1, data = d_orp_recent, family = binomial(link = "logit"))
recent.o.m1 <- glmer(rp_applied ~ (1|id) + (1|uni_project), data = d_orp_recent,
                     family = binomial(link = "logit"),
                     control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)))

recent.o.an1 <- anova(recent.o.m1, recent.o.m0)

recent.o.m2 <- update(recent.o.m1, .~. + study_stage_mc + project)

recent.o.m2 %>% summary()

recent.orp <- list(models = list(m0 = recent.o.m0, m1 = recent.o.m1, m2 = recent.o.m2),
                   lr_tests = list(an1 = recent.o.an1))

#### Models for specific questions ####

# --- Is it more likely that students heard of the rc, if there is an OSI at their university?
d_specific <- data %>% only_psych() %>% 
  distinct(id, uni_current, rc_teaching, nosi_at_uni, nosi, study_stage_mc, 
           interest, importance, felt_information, semester, n_projects, age, sex) %>% 
  mutate(rc_teaching = ifelse(rc_teaching == "yes", 1, 0))


specific.m0 <- glm(rc_teaching ~ +1, data = d_specific, family = binomial(link = "logit"))
specific.m1 <- glmer(rc_teaching ~ (1|uni_current), data = d_specific,
                     family = binomial(link = "logit"),
                     control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)))

specific.an1 <- anova(specific.m1, specific.m0)

specific.m2 <- update(specific.m1, .~. + age + sex + study_stage_mc + n_projects +
                        interest + importance + felt_information + nosi_at_uni + nosi)


specific <- list(models = list(m0 = specific.m0, m1 = specific.m1, 
                               m2 = specific.m2),
                 lr_tests = list(an1 = specific.an1))

# --- Which factors influence students' impression of being informed?
d_att <- data %>% only_psych() %>% 
  distinct(id, uni_current, rc_teaching, nosi_at_uni, nosi, study_stage_mc, 
           interest, importance, felt_information, semester, n_projects, age, sex)

inf.m0 <- lm(felt_information ~ +1, data = d_att)
inf.m1 <- lmer(felt_information ~ (1|uni_current), data = d_att, 
               control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))

inf.an1 <- anova(inf.m1, inf.m0)

inf.m2 <- update(inf.m1, .~. + age + sex + study_stage_mc + n_projects +
                   interest + importance + nosi_at_uni + nosi + rc_teaching)


inf <- list(models = list(m0 = inf.m0, m1 = inf.m1, m2 = inf.m2),
            lr_tests = list(an1 = inf.an1))

# --- Which factors influence students' opinion that the rc and open science are important?
imp.m0 <- lm(importance ~ +1, data = d_att)
imp.m1 <- lmer(importance ~ (1|uni_current), data = d_att,
               control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))

imp.an1 <- anova(imp.m1, imp.m0)

imp.m2 <- update(imp.m1, .~. + age + sex + study_stage_mc + n_projects +
                   interest + felt_information + nosi_at_uni + nosi + rc_teaching)

imp <- list(models = list(m0 = imp.m0, m1 = imp.m1, m2 = imp.m2),
            lr_tests = list(an1 = imp.an1))

#### save results ####

data_list <- list(general = d, qrp = d_clear_qrp, orp = d_clear_orp,
                  recent.qrp = d_qrp_recent, recent.orp = d_orp_recent,
                  specific = d_specific, linear.qrp = d_linear.qrp, linear.orp = d_linear.orp,
                  att = d_att)

results <- list(qrp = qrp, orp = orp, recent.qrp = recent.qrp,
                recent.orp = recent.orp, specific = specific, 
                inf = inf, imp = imp, lin.qrp = lin.qrp, lin.orp = lin.orp, data = data_list)

saveRDS(results, "data/exploration.rds")
