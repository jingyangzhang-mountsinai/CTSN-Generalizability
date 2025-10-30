# Author: Jingyang (Judy) Zhang
# Date: Oct 27th, 2025
# Purpose: try causal survival forest on MMR data. 

library(tidyverse)




library(grf)
library(survival)
library(rsample)
library(timeROC)
library(mice)


set.seed(123)

####################### DATA WRANGLING ############################

## Read the prepared dataset

mmr_dat <- read_csv("data/results/mmr_dat_preped.csv") 

## Drop all missing values except missing values in the outcomes

mmr_dat <- mmr_dat[!apply(mmr_dat[, !names(mmr_dat) %in% c("first_mace_day")], 1, function(x) any(is.na(x))), ]

mmr_dat <- mmr_dat %>% mutate(
  ### Define outcomes
  
  Y = first_mace_day,
  delta = mace
)


## Handle missingness in survival time.
### Because the completion rate of first_mace_day is only 31.23%, there are lots of missingness in first_mace_day. 
### Assume first_mace_day is missing at random conditional on covariates and treatment. 
### Perform multiple imputation using the observed event indicator and other covariates. 

### Specify imputation methods:
#### numeric/integer variables: use predictive mean matching (pmm).
#### factors:
##### 1. 2-level factors: use binary logistic regression (logreg).
##### 2. Unordered factors: use polytomous logistic regression (polyreg).
##### 3. Ordered factors: use proportional odds model (polr).
#### logical variables: treated as binary factor, use logreg.

#### Note: using imputation means observations are no longer independent. 

mmr_dat_imputed <- mice(mmr_dat, m = 5)

mmr_complete <- complete(mmr_dat_imputed, 1)

mmr_complete <- mmr_complete %>% select(-Y, -delta)


#### Save the imputed MMR data.
write.csv(mmr_complete, file = "data/results/mmr_dat_complete.csv", row.names = FALSE)

## causal_survival_forest function() requires all variables in the covariate matrix to be numeric.
mmr_complete_num <- mmr_complete %>% 
  mutate(across(everything(), ~ as.numeric(as.character(.))))
  

## Separate covariates, treatment, survival time, and event indicator. 
mmr_cov <- mmr_complete_num %>% select(-first_mace_day, -mace, -randomization_assignment)
mmr_trt <- as.vector(mmr_complete_num$randomization_assignment)
mmr_survival_time <- as.vector(mmr_complete_num$first_mace_day)
mmr_event_ind <- as.vector(mmr_complete_num$mace)


## Split data into training and testing

training_ratio <- 0.8
split <- initial_split(mmr_complete_num, prop = training_ratio)
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



##################### CSF Using Automatic Tunning ##################



####### Run on all training data using automatic tunning in causal survival forest function ###################
mmr_csf_auto <- causal_survival_forest(
  mmr_train_cov,
  mmr_train_survival_time,
  mmr_train_trt,
  mmr_train_event_ind,
  horizon = 730,
  target = "survival.probability"
)

## Evaluate on Test Data

### Predict treatment effects on test set
#### tau(X) = P[T(1) > horizon | X = x] - P[T(0) > horizon | X = x]
tau_hat_auto <- predict(mmr_csf_auto, mmr_test_cov)$predictions

### Survival object
surv_obj_auto <- Surv(mmr_test_survival_time, mmr_test_event_ind)

### Calculate concordance index
c_index_auto <- concordance(surv_obj_auto ~ tau_hat_auto)$concordance

### Compute time-dependent ROC/AUC
roc_results_auto <- timeROC(
  T = mmr_test_survival_time,
  delta = mmr_test_event_ind,
  marker = tau_hat_auto,
  cause = 1,
  weighting = "marginal",
  times = quantile(mmr_test_survival_time, probs = seq(0.1, 0.9, by = 0.05))
)



plot(
  roc_results_auto$times,
  roc_results_auto$AUC,
  type = "b",
  pch = 19,
  xlab = "Time",
  ylab = "AUC(t)",
  main = "Time-dependent AUC (AUTOC) for Causal Survival Forest"
)
abline(h =0.5, col = "red", lty = 2)

AUTOC_overall_auto <- mean(roc_results_auto$AUC, na.rm = TRUE)

  
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
cv_auc <- numeric(nrow(grid))

for(i in 1:nrow(grid)){
  print(i)
  params <- grid[i, ]
  fold_cindices <- numeric(5)
  fold_auc <- numeric(5)
  
  for(j in 1:k){
    ### Split training data into train and validat
    train_idx <- which(folds != j)
    val_idx <- which(folds == j)
    
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
    fold_cindices[j] <- concordance(surv_obj ~ tau_hat)$concordance
   
    roc_results <- timeROC(
      T = survival_time_val,
      delta = event_val,
      marker = tau_hat,
      cause = 1,
      weighting = "marginal",
      times = quantile(survival_time_val, probs = seq(0.1, 0.9, by = 0.005)),
      iid = FALSE
    )
    fold_auc[j] <- mean(roc_results$AUC, na.rm = TRUE)
    
  }
  
  # Average c-index across 5 folds
  cv_cindex[i] <- mean(fold_cindices)
  cv_auc[i] <- mean(fold_auc)
  
  cat("Grid", i, "/", nrow(grid), ": min.node.size=", params$min.node.size,
      ", mtry=", params$mtry, ", honesty=", params$honesty.fraction,
      ", CV C-index=", round(cv_cindex[i], 3), "\n")
  
  cat("Grid", i, "/", nrow(grid), ": min.node.size=", params$min.node.size,
      ", mtry=", params$mtry, ", honesty=", params$honesty.fraction,
      ", CV AUC =", round(cv_auc[i], 3), "\n")
  
  
}




### Find the best hyperparameters
best_idx <- which.max(cv_cindex[cv_auc > 0.5])
best_params <- grid[best_idx, ]
cat("Best hyperparameters found:\n")

print(best_params)
print(paste0("Corresponding validation c-index is ", round(cv_cindex[best_idx], digits = 3)))


### Train final model

mmr_csf_final <- causal_survival_forest(
  X = mmr_train_cov,
  Y = mmr_train_survival_time,
  W = mmr_train_trt,
  D = mmr_train_event_ind,
  horizon = 730, # Number of days in 2 years
  target = "survival.probability",
  num.trees = num_trees,
  min.node.size = best_params$min.node.size,
  mtry = best_params$mtry,
  honesty.fraction =best_params$honesty.fraction
)



####### Evaluate on Test Data ###################

### Predict treatment effect on test set
tau_hat_best <- predict(mmr_csf_final, mmr_test_cov)$predictions

### Survival object
surv_obj_best <- Surv(mmr_test_survival_time, mmr_test_event_ind)

### Calculate concordance index
c_index_best <- concordance(surv_obj_best ~ tau_hat_best)$concordance

### Calculate AUC
roc_results <- timeROC(
  T = mmr_test_survival_time,
  delta = mmr_test_event_ind,
  marker = tau_hat_best,
  cause = 1,
  weighting = "marginal",
  times = quantile(survival_time_val, probs = seq(0.1, 0.9, by = 0.005)),
  iid = FALSE
)

AUTOC_overall_best <- mean(roc_results$AUC, na.rm = TRUE)


### Save the model and the parameters
### Train the model on the entire dataset

mmr_csf_final <- causal_survival_forest(
  X = mmr_cov,
  Y = mmr_survival_time,
  W = mmr_trt,
  D = mmr_event_ind,
  horizon = 730, # Number of days in 2 years
  target = "survival.probability",
  num.trees = num_trees,
  min.node.size = best_params$min.node.size,
  mtry = best_params$mtry,
  honesty.fraction =best_params$honesty.fraction
)

params_best <- list(
  horizon = 730, # Number of days in 2 years
  target = "survival.probability",
  num.trees = num_trees,
  min.node.size = best_params$min.node.size,
  mtry = best_params$mtry,
  honesty.fraction =best_params$honesty.fraction
)

final_csf_params <- list(
  model = mmr_csf_final,
  params = params_best
)

saveRDS(final_csf_params, file = "results/trained_csf_params.rds")


