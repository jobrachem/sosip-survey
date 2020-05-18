# PsyFaKo Replication Crisis and Open Science Survey 2019

This repository contains the data and code for the *PsyFaKo Replication Crisis and Open Science Survey 2019*.

## Project on the Open Science Framework

You can find our project on the Open Science Framework: [https://osf.io/t3mak/](https://osf.io/t3mak/)

## How to reproduce the complete analysis

1. Download or clone the repository.
1. Open the file `main.Rproj` in RStudio. This will automatically ensure, that your working directory is set correctly and the relative paths in our script work.
1. Install the dependency management package `renv` via `install.packages("renv")`
1. Follow the structure of the `make.R` file, starting with `renv::init()`, which will prepare the R environment.
1. Your can refer to the `misc/codebook_xxx.xlsx` files for detailed information about the data.

*Note: For the purpose of data protection, we removed participants' age from the public data.*