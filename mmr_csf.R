# Author: Jingyang (Judy) Zhang
# Date: Oct 27th, 2025
# Purpose: try causal survival forest on MMR data. 

library(tidyverse)




library(grf)
library(survival)
library(rsample)


## Read the SAS dataset

mmr_dat <- read_csv("data/results/mmr_dat_preped.csv") 

## Drop all missing values except missing values in the outcomes

mmr_dat <- mmr_dat[!apply(mmr_dat[, !names(mmr_dat) %in% c("first_mace_day")], 1, function(x) any(is.na(x))), ]

## Set NAs in first_mace_day to 0
mmr_dat$first_mace_day[is.na(mmr_dat$first_mace_day)] <- 0


## Split data into training and testing
set.seed(123)
training_ratio <- 0.8
split <- initial_split(mmr_dat, prop = training_ratio)
mmr_train <- training(split)
mmr_test <- testing(split)



## Separate covariates, treatment, survival time, and event indicator. 

mmr_train_cov <- mmr_train %>% select(-first_mace_day, -mace, -randomization_assignment)
mmr_train_trt <- as.vector(mmr_train$randomization_assignment)
mmr_train_survival_time <- as.vector(mmr_train$first_mace_day)
mmr_train_event_ind <- as.vector(mmr_train$mace)


## Manual k-fold splitting for cross-validation

k <- 5
folds <- sample(rep(1:k, length.out = nrow(mmr_train)))

cv_results <- numeric(k)

mmr_csf_params <- tibble(
  W.hat = 0.5,
  horizon = 730, # 730 days -> 2 years
)


for(i in 1:k){
  print(i)
  # Split training data into train and validat
  train_idx <- which(folds != i)
  val_idx <- which(folds == i)
  
  ## Get training set
  cov_train <- mmr_train_cov[train_idx,]
  survival_time_train <- mmr_train_survival_time[train_idx]
  trt_train <- mmr_train_trt[train_idx]
  event_train <- mmr_train_event_ind[train_idx]
  
  ## Get validation set
  cov_val <- mmr_train_cov[val_idx,]
  survival_time_val <- mmr_train_survival_time[val_idx]
  trt_val <- mmr_train_trt[val_idx]
  event_val <- mmr_train_event_ind[val_idx]
  
  
  ## Fit causal survival forest
  mmr_csf <- causal_survival_forest(
    X = cov_train,
    Y = survival_time_train,
    W = trt_train,
    D = event_train,
    #W.hat = mmr_csf_params$W.hat,
    horizon = mmr_csf_params$horizon,
    target = "survival.probability",
    num.trees = 1500,
    mtry = 30,
    min.node.size = 5,
    sample.fraction = 0.5,
    honesty = FALSE,
    alpha = 0.05,
    imbalance.penalty = 0,
    ci.group.size = 2,
    honesty.fraction = 0.5,
    honesty.prune.leaves = TRUE,
    compute.oob.predictions = TRUE,
    stabilize.splits = TRUE
  )
  
  ## Predict treatment effects on test set
  tau_hat <- predict(mmr_csf,cov_val)$predictions
  
  ## Survival object
  surv_obj <- Surv(survival_time_val, event_val)
  
  # Evaluate performance
  c_index <- concordance(surv_obj ~ tau_hat)$concordance
  cv_results[i] <- c_index
  
}













