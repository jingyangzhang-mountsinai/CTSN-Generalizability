# Author: Jingyang (Judy) Zhang
# Date: Oct 24th, 2025
# Purpose: explore the MMR dataset.
## 1. Explore relationship between CATEGORICAL potential baseline predictors and two endpoints (first_mace_day and mace).



library(haven)
library(tidyverse)
library(skimr)
library(kableExtra)
library(janitor)
library(DescTools)
library(knitr)


## Read the dataset

mmr_categ <- read_csv("data/mmr_categ.csv")

## Check standard deviations of these numeric predictors.




## RANDOMIZATION_ASSIGNMENT ##

corr_rand_first_mace_day = cor(mmr$randomization_assignment, mmr$first_mace_day, use = "complete.obs")
corr_rand_first_mace_day

corr_rand_mace = cor(mmr$randomization_assignment, mmr$mace, use = "complete.obs")
corr_rand_mace

## SEX ##

corr_sex_first_mace_day = cor(mmr$sex, mmr$first_mace_day, use = "complete.obs")
corr_sex_first_mace_day

corr_sex_mace = cor(mmr$sex, mmr$mace, use = "complete.obs")
corr_sex_mace



## AGE ##

corr_age_first_mace_day = cor(mmr$age, mmr$first_mace_day, use = "complete.obs")
corr_age_first_mace_day

corr_age_mace = cor(mmr$age, mmr$mace, use = "complete.obs")
corr_age_mace



## ETHNICITY ##
corr_ethn_first_mace_day = cor(mmr$ethnicity, mmr$first_mace_day, use = "complete.obs")
corr_ethn_first_mace_day

corr_ethn_mace = cor(mmr$ethnicity, mmr$mace, use = "complete.obs")
corr_ethn_mace



## RACIAL_CATEGORY ##

prop.table(table(mmr$racial_category)) * 100

race <- factor(mmr$racial_category)
kw_race <- kruskal.test(mmr$first_mace_day ~ race)
H_race <- kw_race$statistic
n_first_mace_day <- length(mmr$first_mace_day)


# Effect size from kruskal-wallis
eta_sqr <- (H_race - length(levels(race)) + 1) / (n_first_mace_day - 1)

# Proportion of variance in first_mace_day explained by race. 
## In this case, the negative means very little difference in first_mace_day between racial categories. 
eta_sqr

fisher.test(table(race, factor(mmr$mace)))
CramerV(table(race, factor(mmr$mace)))



## ANEURYSMECTOMY ##
mmr_ANEU <- mmr %>% filter(aneurysmectomy == 0)
mmr_ANEU_NA <- mmr %>% filter(is.na(aneurysmectomy) == TRUE)


mean(mmr_ANEU$first_mace_day, na.rm = TRUE)
median(mmr_ANEU$first_mace_day, na.rm = TRUE)

mean(mmr_ANEU_NA$first_mace_day, na.rm = TRUE)
median(mmr_ANEU_NA$first_mace_day, na.rm = TRUE)



mean(mmr_ANEU$mace, na.rm = TRUE)
median(mmr_ANEU$mace, na.rm = TRUE)

mean(mmr_ANEU_NA$mace, na.rm = TRUE)
median(mmr_ANEU_NA$mace, na.rm = TRUE)





## ATRIAL_FIBRILLATION ##

corr_af_first_mace_day = cor(mmr$atrial_fibrillation, mmr$first_mace_day, use = "complete.obs")
corr_af_first_mace_day

corr_af_mace = cor(mmr$atrial_fibrillation, mmr$mace, use = "complete.obs")
corr_af_mace



## CABG ##

corr_cabg_first_mace_day = cor(mmr$cabg, mmr$first_mace_day, use = "complete.obs")
corr_cabg_first_mace_day

corr_cabg_mace = cor(mmr$cabg, mmr$mace, use = "complete.obs")
corr_cabg_mace






## CARDIAC_TRANSPLANT ##
mmr_CT <- mmr %>% filter(cardiac_transplant == 0)
mmr_CT_NA <- mmr %>% filter(is.na(cardiac_transplant) == TRUE)


mean(mmr_CT$first_mace_day, na.rm = TRUE)
median(mmr_CT$first_mace_day, na.rm = TRUE)

mean(mmr_CT_NA$first_mace_day, na.rm = TRUE)
median(mmr_CT_NA$first_mace_day, na.rm = TRUE)



mean(mmr_CT$mace, na.rm = TRUE)
median(mmr_CT$mace, na.rm = TRUE)

mean(mmr_CT_NA$mace, na.rm = TRUE)
median(mmr_CT_NA$mace, na.rm = TRUE)


############ VERIFICATION #####################################
#
#mmr_CT_NA <- mmr_CT_NA %>% select(first_mace_day, mace)      #
#mmr_ANEU_NA <- mmr_ANEU_NA %>% select(first_mace_day, mace)  #
#
#
#mmr_CT <- mmr_CT %>% select(first_mace_day, mace)            #
#mmr_ANEU <- mmr_ANEU %>% select(first_mace_day, mace)        #
#
#identical(mmr_CT_NA, mmr_ANEU_NA)                            #
#identical(mmr_CT, mmr_ANEU)                                  #
#
###############################################################



## CARDIOMYOPLASTY ##

mmr_CARD0 <- mmr %>% filter(cardiomyoplasty == 0)
mmr_CARD1 <- mmr %>% filter(cardiomyoplasty == 1)
mmr_CARD_NA <- mmr %>% filter(is.na(cardiomyoplasty) == TRUE)

mean(mmr_CARD0$first_mace_day, na.rm = TRUE)
median(mmr_CARD0$first_mace_day, na.rm = TRUE)

mean(mmr_CARD0$mace, na.rm = TRUE)
median(mmr_CARD0$mace, na.rm = TRUE)

# mmr_CARD1 has first_mace_day = NA
mmr_CARD1$mace



mean(mmr_CARD_NA$first_mace_day, na.rm = TRUE)
median(mmr_CARD_NA$first_mace_day, na.rm = TRUE)

mean(mmr_CARD_NA$mace, na.rm = TRUE)
median(mmr_CARD_NA$mace, na.rm = TRUE)


## CAROTID_STENOSIS ##
corr_caro_first_mace_day = cor(mmr$carotid_stenosis, mmr$first_mace_day, use = "complete.obs")
corr_caro_first_mace_day

corr_caro_mace = cor(mmr$carotid_stenosis, mmr$mace, use = "complete.obs")
corr_caro_mace





## CERENRPVASCULAR_DISEASE ##
corr_cere_first_mace_day = cor(mmr$cerebrovascular_disease, mmr$first_mace_day, use = "complete.obs")
corr_cere_first_mace_day

corr_cere_mace = cor(mmr$cerebrovascular_disease, mmr$mace, use = "complete.obs")
corr_cere_mace






## CHRONIC_LUNG_DISEASE ##

prop.table(table(mmr$chronic_lung_disease)) * 100

cl <- factor(mmr$chronic_lung_disease)
kw_cl <- kruskal.test(mmr$first_mace_day ~ cl)
H_cl <- kw_race$statistic
n_first_mace_day <- length(mmr$first_mace_day)


# Effect size from kruskal-wallis
eta_sqr <- (H_cl - length(levels(cl)) + 1) / (n_first_mace_day - 1)

# Proportion of variance in first_mace_day explained by race. 
## In this case, the negative means very little difference in first_mace_day between racial categories. 
eta_sqr

fisher.test(table(cl, factor(mmr$mace)))
CramerV(table(cl, factor(mmr$mace)))

