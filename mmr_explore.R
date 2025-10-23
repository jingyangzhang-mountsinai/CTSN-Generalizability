# Author: Jingyang (Judy) Zhang
# Date: Oct 22th, 2025
# Purpose: explore the MMR dataset.

library(haven)
library(tidyverse)
library(skimr)
library(kableExtra)
library(janitor)
library(DescTools)

## Read the SAS dataset

mmr <- read_sas("data/mmr_primary.sas7bdat") %>% 
  clean_names()




## Recode some of the variables for easier analysis


mmr <- mmr %>% mutate(
  ### Original: male = 1 female = 2
  ### Recoded: male = 0 female = 1
  sex = ifelse(sex == 1, 0, 1),
  
  ### Original: hispanic/latino = 1 non-hispanic/latino = 2
  ### Recoded: non-hispanic/latino = 0 hispanic/latino = 1
  ethnicity = ifelse(ethnicity == 1, 1, 0),
  
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
  ventricular_arrhythmia = ifelse(ventricular_arrhythmia == 1, 0, 1)
  
)

mmr_skim_summary <- skim(mmr)



kable(mmr_skim_summary) %>% 
  kable_styling(full_width = FALSE) %>% 
  save_kable("results/mmr_skim_summary.html")





## Explore relationships between baseline predictors and endpoints

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
mmr_ANEU <- mmr %>% filter(aneurysmectomy == 1)
mmr_ANEU_NA <- mmr %>% filter(is.na(aneurysmectomy) == TRUE)


mean(mmr_ANEU$first_mace_day, na.rm = TRUE)
median(mmr_ANEU$first_mace_day, na.rm = TRUE)

mean(mmr_ANEU_NA$first_mace_day, na.rm = TRUE)
median(mmr_ANEU_NA$first_mace_day, na.rm = TRUE)





mean(mmr_ANEU$mace, na.rm = TRUE)
median(mmr_ANEU$mace, na.rm = TRUE)

mean(mmr_ANEU_NA$mace, na.rm = TRUE)
median(mmr_ANEU_NA$mace, na.rm = TRUE)
