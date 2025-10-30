# Author: Jingyang (Judy) Zhang
# Date: Oct 30th, 2025
# Purpose: plot time-dependent ROC curve 

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



## Calculate time-dependent ROC

### Predict treatment effects on test set
#### tau(X) = P[T(1) > horizon | X = x] - P[T(0) > horizon | X = x]
tau_hat <- predict(mmr_csf, mmr_cov)$predictions


### Compute time-dependent ROC/AUC
roc_result <- timeROC(
  T = mmr_survival_time,
  delta = mmr_event_ind,
  marker = tau_hat,
  cause = 1,
  
  #### weighting = "marginal": uses the Kaplan-Meier estimator of the censoring distribution.
  #### weighting = "cox": models the censoring by the Cox model.
  #### weighting = "aalen": models the censoring by the additive Aalen model. 
  weighting = "marginal",
  times = quantile(mmr_survival_time, probs = seq(0.1, 0.9, by = 0.05))
)

AUTOC_overall <- mean(roc_result$AUC, na.rm = TRUE)


# Plot 1: AUC against time
plot(
  roc_result$times,
  roc_result$AUC,
  type = "b",
  pch = 19,
  xlab = "Time",
  ylab = "AUC(t)",
  main = "Time-dependent AUC (AUTOC) for Causal Survival Forest"
)
abline(h =0.5, col = "red", lty = 2)




# Plot 2: ROC Curve at time 0
time_point <- 730
### Compute time-dependent ROC/AUC
roc_result_t730 <- timeROC(
  T = mmr_survival_time,
  delta = mmr_event_ind,
  marker = tau_hat,
  cause = 1,
  
  #### weighting = "marginal": uses the Kaplan-Meier estimator of the censoring distribution.
  #### weighting = "cox": models the censoring by the Cox model.
  #### weighting = "aalen": models the censoring by the additive Aalen model. 
  weighting = "marginal",
  times = time_point
)


plot(roc_result$FP[,1], roc_result$TP[,1],
     type = "l",
     xlab = "1 - Specificity",
     ylab = "Sensitivity",
     main = paste0("Time-dependent ROC at t = ", 730)
)
abline(0, 1, col = "red", lty = 2)  # diagonal line


roc_result$AUC[1]
