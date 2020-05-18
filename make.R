# install.packages("renv")
renv::init()
source("code/helpers/packages.R")
source("code/helpers/helper_functions.R")
source("code/helpers/static.R")

# data preparation
# source("code/donwload_data.R")
source("code/n_participants.R")
source("code/prepare_data.R")
source("code/remove_age.R")
source("code/wrangling.R")

# analysis files
rmarkdown::render("code/sample_descriptives.Rmd", knit_root_dir = "..", 
                  output_file = "../docs/sample_descriptives.html")

rmarkdown::render("code/analysis.Rmd", knit_root_dir = "..", 
                  output_file = "../docs/analysis.html")

# plots
source("code/plots/attitudes.R")
source("code/plots/n_qrps_per_project.R")
source("code/plots/os_initiatives.R")
source("code/plots/obs_per_university.R")
source("code/plots/rp_usage_detailed.R")
source("code/plots/stats_software.R")
source("code/plots/study_progress.R")




# source("code/exploration.R")

# render report
# knit directly from file
# rmarkdown::render("manuscript/manuscript.Rmd", knit_root_dir = "..", 
#                   output_file = "../manuscript/manuscript.docx")

# render online supplements
# knit directly from file
rmarkdown::render("manuscript/supplements.Rmd", knit_root_dir = "..", 
                  output_file = "../manuscript/supplements.docx")

