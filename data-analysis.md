data analysis
================
Yiying Wu
2024-02-20

## R packages

``` r
# INSTALL PACKAGES
packages <- c("tidyverse", "dplyr", "knitr", "haven",
              "survey", "mice")

# Install missing packages
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages], dependencies = TRUE)
}

# Load packages invisibly
invisible(lapply(packages, library, character.only = TRUE))

# # Remove variables associated with package installation
# rm(packages, installed_packages)
```

## data preparation

``` r
# input demo data
dat_demo<-read_xpt("./data/P_DEMO.XPT")%>%
  janitor::clean_names()

# input food insecurity data
dat_fsq<-read_xpt("./data/P_FSQ.XPT")%>%
  janitor::clean_names()%>%
  select(seqn,fsdad)

# combine datasets
dat <- dat_demo %>% left_join(dat_fsq, by = "seqn")
```
