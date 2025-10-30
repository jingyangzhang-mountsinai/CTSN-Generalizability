# Author: Jingyang (Judy) Zhang
# Date: Oct 27th, 2025
# Purpose: try survival forest on MMR data. 

library(tidyverse)




library(randomForestSRC)
library(survival)
library(rsample)


set.seed(123)

####################### DATA WRANGLING ############################

mmr_dat <- read_csv("data/results/mmr_dat_complete.csv")


## Separate covariates + treatment, survival time, and event indicator. 
mmr_cov <- mmr_dat %>% select(-first_mace_day, -mace)

mmr_survival_time <- as.vector(mmr_dat$first_mace_day)
mmr_event_ind <- as.vector(mmr_dat$mace)


## Split data into training and testing

training_ratio <- 0.8
split <- initial_split(mmr_dat, prop = training_ratio)
mmr_train <- training(split)
mmr_test <- testing(split)


########## Training Data ##########

## Separate covariates+treatment, survival time, and event indicator. 

mmr_train_cov <- mmr_train %>% select(-first_mace_day, -mace)
mmr_train_survival_time <- as.vector(mmr_train$first_mace_day)
mmr_train_event_ind <- as.vector(mmr_train$mace)


########## Testing Data ##########

## Separate covariates + treatment, survival time, and event indicator. 

mmr_test_cov <- mmr_test %>% select(-first_mace_day, -mace)
mmr_test_survival_time <- as.vector(mmr_test$first_mace_day)
mmr_test_event_ind <- as.vector(mmr_test$mace)

### Separate treatment and control
mmr_test_cov0 <- mmr_test_cov 

mmr_test_cov0$randomization_assignment <- 0

mmr_test_cov1 <- mmr_test_cov 

mmr_test_cov1$randomization_assignment <- 1

##################### CSF Using Automatic Tunning ##################



####### Run on all training data using automatic tunning in survival forest function ###################
rsf_auto <- 
  rfsrc(Surv(first_mace_day, mace) ~ ., data = mmr_train)


## Evaluate on Test Data

### Predict risk scores on test set

#### Control:


pred0 <- predict(rsf_auto, mmr_test_cov0)



#### Treatment:


pred1 <- predict(rsf_auto, mmr_test_cov1)


times <- pred0$time.interest

# Average survival over all individuals in each group
avg_surv0 <- colMeans(pred0$survival)
avg_surv1 <- colMeans(pred1$survival)

plot(
  times, avg_surv0, type = "l", col = "red", lwd = 2,
  xlab = "Time", ylab = "Survival Probability",
  main = "Survival Curves by Treatment Group"
)
lines(times, avg_surv1, col = "blue", lwd = 2)
legend("topright", legend = c("Control", "Treatment"), col = c("red", "blue"), lwd = 2)







####### Manual Tuning Using Grid Search ###################


## Define hyperparameter grid


grid <- expand.grid(
  ntree = c(100, 500),
  mtry = c(2, 3, 4),
  nodesize = c(3, 5, 10)
)

## Manual k-fold splitting for cross-validation

k <- 5

### This assign each observation a fold number from 1 to k=5.
folds <- sample(rep(1:k, length.out = nrow(mmr_train)))


cv_results <- data.frame()


for(i in 1:nrow(grid)){
  ntree_i <- grid$ntree[i]
  mtry_i <- grid$mtry[i]
  nodesize_i <- grid$nodesize[i]
  
  fold_cindex <- c()
  for(j in 1:k){
    ### Split training data into train and validat
    train_idx <- which(folds != j)
    val_idx <- which(folds == j)
    
    fold_train <- mmr_train[train_idx,]
    fold_val <- mmr_train[val_idx,]
    
    rsf <- rfsrc(Surv(first_mace_day, mace) ~ .,
                 data = fold_train,
                 ntree = ntree_i,
                 mtry = mtry_i,
                 nodesize = nodesize_i,
                 nsplit = 10)
    
    ### Predict on validation fold
    pred <- predict(rsf, newdata = fold_val)
    
    ### Calculate c-index
  

    surv_obj <- Surv(fold_val$first_mace_day, fold_val$mace)
    cindex <- concordance(surv_obj ~ pred$predicted)$concordance
    fold_cindex <- c(fold_cindex, cindex)
    
  }
  
  cv_results <- rbind(cv_results,
                      data.frame(ntree = ntree_i,
                                 mtry = mtry_i,
                                 nodesize = nodesize_i,
                                 mean_cindex = mean(fold_cindex)))
  
  cat("Grid", i, "/", nrow(grid), ": ntree =", ntree_i ,
      ", mtry=", mtry_i, ", nodesize=", nodesize_i,
      ", CV C-index=", round(mean(fold_cindex), 3), "\n")
}


best_params <- cv_results[which.max(cv_results$mean_cindex), ]
best_params


## Train final model

final_model <- rfsrc(Surv(first_mace_day, mace) ~ .,
                     data = mmr_train,
                     ntree = best_params$ntree,
                     mtry = best_params$mtry,
                     nodesize = best_params$nodesize,
                     nsplit = 10)



pred_best <- predict(final_model, newdata = mmr_test)


surv_obj <- Surv(mmr_test$first_mace_day, mmr_test$mace)
cindex_best <- concordance(surv_obj ~ pred_best$predicted)$concordance
