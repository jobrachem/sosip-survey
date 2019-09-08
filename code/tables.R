#### Imports ####
source("code/packages.R")
source("code/helper_functions.R")
source("code/static.R")

d <- read_csv("data/long.csv")
ex <- readRDS("data/exploration.rds")

font <- "Times"

#### Correlation Table ####
c <- d %>% distinct(id, .keep_all = TRUE) %>% 
  select(age, semester, importance, felt_information, interest, n_projects) %>% 
  as.matrix() %>% rcorr()

cor_table <- format_cor(c, names = n.cor)$table %>% 
  flextable() %>% 
  align(j = 2:7, align = "center", part = "header") %>% 
  align(j = 2:7, align = "left", part = "body") %>% autofit()


#### LMER Table ####
ex$inf$lr_tests$an2 %>% prep_anova()

ex$inf$models$m7 %>% summary()
