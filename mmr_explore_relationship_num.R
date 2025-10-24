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



## Read the dataset

mmr_num <- read_csv("data/mmr_num.csv")

## Check standard deviations of these numeric predictors.
### Some of them have very small sds -> little variation in the data. 

sd_num <- data.frame(variable = names(mmr_num), sd = sapply(mmr_num, sd, na.rm = TRUE))




##### FIRST_MACE_DAY #####


## Note: proc_day column because there is little variation: all 0 except 1 value equals to 4. The value that equals to 4 corresponding to a NA in first_mace_day.
proc_day4 <- mmr_num %>% 
  filter(proc_day == 4) %>% 
  select(first_mace_day, mace)


corr_num_fmd <- data.frame(lapply(mmr_num, function(x) cor(x, mmr_num$first_mace_day, use = "complete.obs"))) %>% 
  pivot_longer(
                cols = everything(),
                names_to = "predictor",
               values_to = "corr_num_fmd") %>% 
  ## Remove endpoints
  filter(predictor != "first_mace_day", predictor != "mace") %>% 
  mutate(corr_num_fmd = round(corr_num_fmd, 3)) %>% 
  mutate(
    corr_num_label_fmd = paste0("corr = ", corr_num_fmd)
  ) 



write.csv(corr_num_fmd, "data/corr_num_fmd.csv", row.names = FALSE)







##### MACE #####

## Note:
## After removing NAs in death_day, there is no variation in mace.
death_day <- mmr_num %>% select(death_day, mace) %>% na.omit()


## After removing NAs in first_stroke_day, there is no variation in mace.
first_stroke_day <- mmr_num %>% select(first_stroke_day, mace) %>% na.omit()


## After removing NAs in first_chf_day, there is no variation in mace.
first_chf_day <- mmr_num %>% select(first_chf_day, mace) %>% na.omit()

## After removing NAs in first_mv_op_day, there is no variation in mace.
fist_mv_op_day <- mmr_num %>% select(first_mv_op_day, mace) %>% na.omit()

## After removing NAs in first_whf_day, there is no variation in mace.
fist_whf_day <- mmr_num %>% select(first_whf_day, mace) %>% na.omit()


## For columns with outliers, we will calculate the Spearman correlation. Otherwise, we calculate Pearson correlation. 


corr_num_mace <- data.frame(lapply(mmr_num, function(x) cor(x, mmr_num$mace, use = "complete.obs"))) %>% 
  pivot_longer(
    cols = everything(),
    names_to = "predictor",
    values_to = "corr_num_mace") %>% 
  filter(predictor != "first_mace_day", predictor != "mace") %>% 
  mutate(corr_num_mace = round(corr_num_mace, 3)) %>% 
  mutate(
    corr_num_label_mace = paste0("corr = ", corr_num_mace)
  ) 







write.csv(corr_num_mace, "data/corr_num_mace.csv", row.names = FALSE)
