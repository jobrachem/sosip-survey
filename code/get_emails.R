# Author: Johannes Brachem
# Last Update: 20.03.2019
# Purpose: Download E-Mail Adresses from end of survey for newsletter.


library(tidyverse)
library(formr)
library(writexl)

# First: Log in to our formr.org account
formr_connect("-", 
              password = "-")

# Second: Download data
email_raw <- formr_raw_results("email_opensci") %>% 
  as_tibble() # turn data frame into tibble (easier processing)

email <- email_raw %>% 
  filter(!is.na(mail_adress)) %>% 
  filter(mail_adress != "formr_monkey@example.org")

writexl::write_xlsx(email, path = "emails_opensci_newsletter.xlsx")
