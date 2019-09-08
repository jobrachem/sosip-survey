# Author:       Johannes Brachem
# Last Update:  20.03.2019
# Purpose:      Download survey data from formr
# Output:       main_raw.csv


# Download Data
library(readr)
library(formr)

# First: Log in to our formr.org account
formr_connect("openscience@psyfako.org", password = "2YEIqeKHr3dRJTnUxDag")

# Second: Download data
main_raw <- formr_raw_results("osci_umfrage_v3") 

# Third: Save Raw Data
write_csv(main_raw, "data/main_raw.csv")
