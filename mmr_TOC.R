# Author: Jingyang (Judy) Zhang
# Date: Oct 31th, 2025
# Purpose: Plot Target Operator Characteristic (TOC) and calculate the Area Under the Curve (AUTOC)

library(tidyverse)




library(grf)
library(survival)

library(timeROC)


## Read the prepared dataset

mmr_dat <- read_csv("data/results/mmr_dat_complete.csv") 


### Separate covariate, treatment, survival time, and event indicator.
mmr_cov <- mmr_dat %>% select(-first_mace_day, -mace, -randomization_assignment)
mmr_event_ind <- as.vector(mmr_dat$mace)
mmr_trt <- as.vector(mmr_dat$randomization_assignment)
mmr_survival_time <- as.vector(mmr_dat$first_mace_day)

## Read in the trained CSF model
mmr_csf_params <- readRDS("results/trained_csf_params.rds")


mmr_csf <- mmr_csf_params$model
mmr_csf_params <- mmr_csf_params$params




## Predict treatment effects on test set
#### tau(X) = P[T(1) > horizon | X = x] - P[T(0) > horizon | X = x]
tau_hat <- predict(mmr_csf, mmr_cov)$predictions

## Rank predicted treatment effects
### q has to be in (0, 1]
### q starting from 0.01 will result in the last value being slightly greater than 1 (e.g. 1.000000002), thus gives error. 
rate <- rank_average_treatment_effect(mmr_csf, tau_hat, q = seq(0.05, 1, by = 0.05))





## Find columns that have no variation (i.e. var = 0)
no_var_cols <- sapply(mmr_cov, function(x) length(unique(x)) == 1)
which(no_var_cols)
### Aneurysmectomy, cardiac_transplant, and liver_disease have no variation in data (i.e. all three indicators are 0 for every one).


mmr_cov <- mmr_cov %>% 
  select(-aneurysmectomy, -cardiac_transplant, -liver_disease) %>% 
  drop_na()


blp <- best_linear_projection(mmr_csf,
                              mmr_cov)



