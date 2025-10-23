# Author: Jingyang (Judy) Zhang
# Date: Oct 23th, 2025
# Purpose: explore the MMR dataset.
## 1. Explore relationship between potential baseline predictors and two endpoints (first_mace_day and mace).



library(haven)
library(tidyverse)
library(skimr)
library(kableExtra)
library(janitor)
library(DescTools)
library(knitr)
## Read the SAS dataset

mmr <- read_sas("data/mmr_primary.sas7bdat") %>% 
  clean_names()




## Recode some of the variables for easier analysis


mmr <- mmr %>% mutate(
  
  
  ### Original: cabg alone = 1 cabg + mvrepair = 2
  ### Recoded: cabg alone = 0 cabg + mvrepair = 1
  randomization_assignment = ifelse(randomization_assignment == 1, 0, 1),
  
  
  ### Original: male = 1 female = 2
  ### Recoded: male = 0 female = 1
  sex = ifelse(sex == 1, 0, 1),
  
  ### Original: hispanic/latino = 1 non-hispanic/latino = 2
  ### Recoded: non-hispanic/latino = 0 hispanic/latino = 1
  ethnicity = ifelse(ethnicity == 1, 1, 0),
  
  ### Original: american indian/native = 1 asian = 2 black = 3 hawaiian/pacific = 4 white = 5 other = 98
  ### Recoded: white = 0 american indian/native = 1 asian = 2 black = 3 hawaiian/pacific = 4 other = 98
  racial_category = ifelse(racial_category == 5, 0, racial_category),
  
  
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  aneurysmectomy = ifelse(aneurysmectomy == 1, 0, 1),
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  atrial_fibrillation = ifelse(atrial_fibrillation == 1, 0, 1),
  
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  cabg = ifelse(cabg == 1, 0, 1),
  
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  cardiac_transplant = ifelse(cardiac_transplant == 1, 0, 1),
  
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  cardiomyoplasty = ifelse(cardiomyoplasty == 1, 0, 1),
  
  
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  carotid_stenosis = ifelse(carotid_stenosis == 1, 0, 1),
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  cerebrovascular_disease = ifelse(cerebrovascular_disease == 1, 0, 1),
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  diabetes = ifelse(diabetes == 1, 0, 1),
  
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  dyslipidemia = ifelse(dyslipidemia == 1, 0, 1),
  
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  family_history_coronary = ifelse(family_history_coronary == 1, 0, 1),
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  gastrointestinal_bleeding = ifelse(gastrointestinal_bleeding == 1, 0, 1),
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  heart_failure = ifelse(heart_failure == 1, 0, 1),
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  heart_surgery = ifelse(heart_surgery == 1, 0, 1),
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  hypertension = ifelse(hypertension == 1, 0, 1),
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  icd = ifelse(icd == 1, 0, 1),
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  infectious_endocarditis = ifelse(infectious_endocarditis == 1, 0, 1),
  
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  liver_disease = ifelse(liver_disease == 1, 0, 1),
  
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  malignancy = ifelse(malignancy == 1, 0, 1),
  
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  myocardial_infarction = ifelse(myocardial_infarction == 1, 0, 1),
  
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  on_iabp = ifelse(on_iabp == 1, 0, 1),
  
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  pacemaker = ifelse(pacemaker == 1, 0, 1),
  
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  pci = ifelse(pci == 1, 0, 1),
  
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  peripheral_arterial_disease = ifelse(peripheral_arterial_disease == 1, 0, 1),
  
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  peripheral_vascular_disease = ifelse(peripheral_vascular_disease == 1, 0, 1),
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  prior_surgery = ifelse(prior_surgery == 1, 0, 1),
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  psychiatric_disorder = ifelse(psychiatric_disorder == 1, 0, 1),
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  renal_insufficiency = ifelse(renal_insufficiency == 1, 0, 1),
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  stroke = ifelse(stroke == 1, 0, 1),
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  thyroid_disease = ifelse(thyroid_disease == 1, 0, 1),
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  tia = ifelse(tia == 1, 0, 1),
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  tobacco = ifelse(tobacco == 1, 0, 1),
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  ventricular_arrhythmia = ifelse(ventricular_arrhythmia == 1, 0, 1),
  
  
  ### Original: none = 1 mild = 2 moderate = 3 severe = 4 unknown/undocumented = 9
  ### Recoded: none = 0 mild = 1 moderate = 2 severe = 3 unknon/undocument = 9
  chronic_lung_disease = ifelse(chronic_lung_disease == 9, chronic_lung_disease, chronic_lung_disease - 1)
  
)



## Explore relationships between potential baseline predictors and endpoints



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

