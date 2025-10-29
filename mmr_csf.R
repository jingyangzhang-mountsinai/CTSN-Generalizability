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


########## Training Data ##########

## Separate covariates, treatment, survival time, and event indicator. 

mmr_train_cov <- mmr_train %>% select(-first_mace_day, -mace, -randomization_assignment)
mmr_train_trt <- as.vector(mmr_train$randomization_assignment)
mmr_train_survival_time <- as.vector(mmr_train$first_mace_day)
mmr_train_event_ind <- as.vector(mmr_train$mace)


########## Testing Data ##########

## Separate covariates, treatment, survival time, and event indicator. 

mmr_test_cov <- mmr_test %>% select(-first_mace_day, -mace, -randomization_assignment)
mmr_test_trt <- as.vector(mmr_test$randomization_assignment)
mmr_test_survival_time <- as.vector(mmr_test$first_mace_day)
mmr_test_event_ind <- as.vector(mmr_test$mace)



####### Run on all training data using automatic tunning in causal survival forest function ###################
mmr_csf_auto <- causal_survival_forest(
  mmr_train_cov,
  mmr_train_survival_time,
  mmr_train_trt,
  mmr_train_event_ind,
  horizon = 730
)

## Evaluate on Test Data

### Predict treatment effects on test set
tau_hat_auto <- predict(mmr_csf_auto, mmr_test_cov)$predictions

### Survival object
surv_obj_auto <- Surv(mmr_test_survival_time, mmr_test_event_ind)

### Calculate concordance index
c_index_auto <- concordance(surv_obj_auto ~ tau_hat_auto)$concordance


  
####### Manual Tuning Using Grid Search ###################

## Define hyperparameter grid
num_trees <- 2000

min_node_grid <- c(3, 5, 7)
mtry_grid <- c(floor(sqrt(ncol(mmr_train_cov))) - 1, floor(sqrt(ncol(mmr_train_cov))), floor(sqrt(ncol(mmr_train_cov)))+1)
honesty_grid <- c(0.4, 0.5, 0.6)

grid <- expand.grid(
  min.node.size = min_node_grid,
  mtry = mtry_grid,
  honesty.fraction = honesty_grid
)

## Manual k-fold splitting for cross-validation

k <- 5

### This assign each observation a fold number from 1 to k=5.
folds <- sample(rep(1:k, length.out = nrow(mmr_train)))

cv_cindex <- numeric(nrow(grid))


for(i in 1:nrow(grid)){
  print(i)
  params <- grid[i, ]
  fold_cindices <- numeric(5)
  
  for(k in 1:5){
    ### Split training data into train and validat
    train_idx <- which(folds != k)
    val_idx <- which(folds == k)
    
    #### Get training set
    cov_train <- mmr_train_cov[train_idx,]
    survival_time_train <- mmr_train_survival_time[train_idx]
    trt_train <- mmr_train_trt[train_idx]
    event_train <- mmr_train_event_ind[train_idx]
    
    #### Get validation set
    cov_val <- mmr_train_cov[val_idx,]
    survival_time_val <- mmr_train_survival_time[val_idx]
    trt_val <- mmr_train_trt[val_idx]
    event_val <- mmr_train_event_ind[val_idx]
    
    
    ### Fit causal survival forest
    mmr_csf <- causal_survival_forest(
      X = cov_train,
      Y = survival_time_train,
      W = trt_train,
      D = event_train,
   
      horizon = 730, # Number of days in 2 years
      target = "survival.probability",
      num.trees = num_trees,
      min.node.size = params$min.node.size,
      mtry = params$mtry,
      honesty.fraction = params$honesty.fraction
      
    )
    
    ### Predict treatment effects on validation set
    tau_hat <- predict(mmr_csf,cov_val)$predictions
    
    ### Evaluate c-index
    surv_obj <- Surv(survival_time_val, event_val)
    fold_cindices[k] <- concordance(surv_obj ~ tau_hat)$concordance
    
    
  }
  
  # Average c-index across 5 folds
  cv_cindex[i] <- mean(fold_cindices)
  
  cat("Grid", i, "/", nrow(grid), ": min.node.size=", params$min.node.size,
      ", mtry=", params$mtry, ", honesty=", params$honesty.fraction,
      ", CV C-index=", round(cv_cindex[i], 3), "\n")
  
  
}




### Find the best hyperparameters
best_idx <- which.max(cv_cindex)
best_params <- grid[best_idx, ]
cat("Best hyperparameters found:\n")

print(best_params)
print(paste0("Corresponding validation c-index is ", round(cv_cindex[best_idx], digits = 3)))


## Manual tuning perform worse than auto tuning



