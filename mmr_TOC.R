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


mmr_dat <- mmr_dat %>% mutate(tau_hat = tau_hat)

## Decide number of bins (quantiles)

n_bins <- length(rate)
mmr_dat$bin <- cut(rank(-mmr_dat$tau_hat), breaks = n_bins, labels = FALSE)



## Calculate the mean outcomes by bin and treatment group
mmr_dat_summary <- mmr_dat %>% 
  group_by(bin, randomization_assignment) %>% 
  summarize(rate = mean(first_mace_day), .group = "drop")

## Estimate Kaplan-Meier survival per bin and group
# Function to get KM survival probability at t0
km_survival <- function(time, status, t0) {
  fit <- survfit(Surv(time, status) ~ 1)
  surv_prob <- summary(fit, times = t0)$surv
  if(length(surv_prob) == 0) surv_prob <- NA
  return(surv_prob)
}

t0 <- 730  # fixed time horizon
mmr_dat_summary <- mmr_dat %>%
  group_by(bin, randomization_assignment) %>%
  summarise(
    surv_prob = km_survival(first_mace_day, mace, t0),
    .groups = "drop"
  )

ggplot(mmr_dat_summary, aes(x = bin / n_bins, y = surv_prob, color = factor(randomization_assignment))) +
  geom_line(size = 1.2) +
  geom_point() +
  scale_color_manual(values = c("red", "blue"), labels = c("Control", "Treatment")) +
  labs(
    x = "Fraction of population ranked by predicted survival benefit",
    y = paste0("Estimated survival probability at t=", t0),
    color = "Group",
    title = "Treatment vs Control by Predicted Survival Benefit"
  ) +
  theme_minimal()
