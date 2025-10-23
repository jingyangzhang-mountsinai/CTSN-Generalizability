# Author: Jingyang (Judy) Zhang
# Date: Oct 23th, 2025
# Purpose: explore the MMR dataset.
## 1. Explore relationship between NUMERICAL potential baseline predictors and two endpoints (first_mace_day and mace).



library(haven)
library(tidyverse)
library(skimr)
library(kableExtra)
library(janitor)
library(DescTools)
library(knitr)
## Read the SAS dataset

mmr_num <- read_csv("data/mmr_num.csv")





## Explore relationships between numeric potential baseline predictors and endpoints

### Remove columns with sd = 0

mmr_num_varOnly <- mmr_num[, sapply(mmr_num, function(x) sd(x, na.rm = TRUE) == 0)]
 

corr_num <- lapply(mmr_num_varOnly, function(x) cor(x, mmr_num_varOnly$first_mace_day, use = "complete.obs"))



