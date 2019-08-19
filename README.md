# PsyFaKo Replication Crisis and Open Science Survey 2019

This repository contains the data, code and manuscript for the *PsyFaKo Replication Crisis and Open Science Survey 2019*.

## Project on the Open Science Framework

You can find our project on the Open Science Framework: [https://osf.io/t3mak/](https://osf.io/t3mak/)

## How to reproduce our analyses

1. Download or clone the repository from GitHub.
1. Open the file `main.Rproj` in RStudio. This will automatically ensure, that your working directory is set correctly and the relative paths in our script work.
1. You should now be able to run our R-scripts.
1. Your can refer to the `misc/codebook_xxx.xlsx` files for detailed information about the data.

#### Preliminaries

1. `code/wrangling/n_participants.R`: Based on the data set `main_allunis.csv`, this script calculates the number of participants and number of observations per university. It also creates a list of universities with att least 30 observations. `main_allunis.csv` is created by applying `code/wrangling/wrangling.Rmd` to the raw data `main_raw.csv` instead of the further anonymised `main_start.csv`: It contains all available information about participants' universities, and is therefore (like `main_raw.csv`) not included in this public repository.
1. `code/wrangling/n_projects.R`: Based on the data set `main_allunis.csv`, this script calculates the number of projects per university and saves the resul in `data/n_projects_uni.csv`
1. `code/wrangling/main_start.R`: This short script creates a dataset `main_start.csv` from the raw data `main_raw.csv`. It's purpose is to remove information about participants' university, if there are fewer than 30 observations from that university. This is a step to protect the identity of our participants. Therefore, only the output `main_start.csv` and the R-script that produces it are available here, but not the original dataset `main_raw.csv`

#### Data wrangling
1. `code/wrangling/wrangling.Rmd`: This is our main wrangling script, where we apply exclusions and prepare data for further analyses. Input: `main_start.csv` Output: `data/main.csv` (main wide dataset), `data/main_descriptives.csv` (main wide dataset with only sample descriptives), `data/stats.rds` (cleaned data about use of statistical software)
1. `code/wrangling/df_practices.Rmd`: This script creates a data set in long format, which allows for a detailed analysis of data about individual projects and individual research practices. Output: `data/df_practices.csv` (long dataset)

#### Analysis
1. `code/sample_descriptives.Rmd`: Provides sample descriptives.
1. `code/analysis.Rmd`: Provides a descriptive analysis of our questions about research practices in empirical projects.
1. `code/analysis2.Rmd`: Provides a descriptive analysis of our questions about replication crisis and QRP coverage in lectures, knowledge about open science initiatives, and usage of stats softwares.

#### Plots

The directory `code/plots/` contains individual .R files for each plot. The .R files should run by themselves.