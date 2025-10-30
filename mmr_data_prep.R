# Author: Jingyang (Judy) Zhang
# Date: Oct 27th, 2025
# Purpose: select and prepare data used for CTSN generalizability project. 

library(tidyverse)
library(janitor)
library(haven)


## Read the SAS dataset

mmr <- read_sas("data/mmr_primary.sas7bdat") %>% 
  clean_names()



#### Data Wrangling ####

## Recode some of the variables for easier analysis


mmr <- mmr %>% mutate(
  
  
  ### Original: cabg alone = 1 cabg + mvrepair = 2
  ### Recoded: cabg alone = 0 cabg + mvrepair = 1
  randomization_assignment = factor(ifelse(randomization_assignment == 1, 0, 1)),
  
  
  ### Original: male = 1 female = 2
  ### Recoded: male = 0 female = 1
  sex = factor(ifelse(sex == 1, 0, 1)),
  
  ### Original: hispanic/latino = 1 non-hispanic/latino = 2
  ### Recoded: non-hispanic/latino = 0 hispanic/latino = 1
  ethnicity = factor(ifelse(ethnicity == 1, 1, 0)),
  
  ### Original: american indian/native = 1 asian = 2 black = 3 hawaiian/pacific = 4 white = 5 other = 98
  ### Recoded: white = 0 american indian/native = 1 asian = 2 black = 3 hawaiian/pacific = 4 other = 98
  racial_category = factor(ifelse(racial_category == 5, 0, racial_category)),
  
  
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  aneurysmectomy = factor(ifelse(aneurysmectomy == 1, 0, 1)),
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  atrial_fibrillation = factor(ifelse(atrial_fibrillation == 1, 0, 1)),
  
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  cabg = factor(ifelse(cabg == 1, 0, 1)),
  
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  cardiac_transplant = factor(ifelse(cardiac_transplant == 1, 0, 1)),
  
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  cardiomyoplasty = factor(ifelse(cardiomyoplasty == 1, 0, 1)),
  
  
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  carotid_stenosis = factor(ifelse(carotid_stenosis == 1, 0, 1)),
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  cerebrovascular_disease = factor(ifelse(cerebrovascular_disease == 1, 0, 1)),
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  diabetes = factor(ifelse(diabetes == 1, 0, 1)),
  
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  dyslipidemia = factor(ifelse(dyslipidemia == 1, 0, 1)),
  
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  family_history_coronary = factor(ifelse(family_history_coronary == 1, 0, 1)),
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  gastrointestinal_bleeding = factor(ifelse(gastrointestinal_bleeding == 1, 0, 1)),
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  heart_failure = factor(ifelse(heart_failure == 1, 0, 1)),
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  heart_surgery = factor(ifelse(heart_surgery == 1, 0, 1)),
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  hypertension = factor(ifelse(hypertension == 1, 0, 1)),
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  icd = factor(ifelse(icd == 1, 0, 1)),
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  infectious_endocarditis = factor(ifelse(infectious_endocarditis == 1, 0, 1)),
  
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  liver_disease = factor(ifelse(liver_disease == 1, 0, 1)),
  
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  malignancy = factor(ifelse(malignancy == 1, 0, 1)),
  
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  myocardial_infarction = factor(ifelse(myocardial_infarction == 1, 0, 1)),
  
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  on_iabp = factor(ifelse(on_iabp == 1, 0, 1)),
  
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  pacemaker = factor(ifelse(pacemaker == 1, 0, 1)),
  
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  pci = factor(ifelse(pci == 1, 0, 1)),
  
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  peripheral_arterial_disease = factor(ifelse(peripheral_arterial_disease == 1, 0, 1)),
  
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  peripheral_vascular_disease = factor(ifelse(peripheral_vascular_disease == 1, 0, 1)),
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  prior_surgery = factor(ifelse(prior_surgery == 1, 0, 1)),
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  psychiatric_disorder = factor(ifelse(psychiatric_disorder == 1, 0, 1)),
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  renal_insufficiency = factor(ifelse(renal_insufficiency == 1, 0, 1)),
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  stroke = factor(ifelse(stroke == 1, 0, 1)),
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  thyroid_disease = factor(ifelse(thyroid_disease == 1, 0, 1)),
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  tia = factor(ifelse(tia == 1, 0, 1)),
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  tobacco = factor(ifelse(tobacco == 1, 0, 1)),
  
  ### Original: no = 1 yes = 2
  ### Recoded: no = 0 yes = 1
  ventricular_arrhythmia = factor(ifelse(ventricular_arrhythmia == 1, 0, 1)),
  
  
  ### Original: none = 1 mild = 2 moderate = 3 severe = 4 unknown/undocumented = 9
  ### Recoded: none = 0 mild = 1 moderate = 2 severe = 3 unknon/undocument = 9
  chronic_lung_disease = factor(ifelse(chronic_lung_disease == 9, chronic_lung_disease, chronic_lung_disease - 1)),
  
  
  
  ## Original: none = 1 trace = 2 mild = 3 moderate = 4 severe = 5
  ### Recoded: none = 0 trace = 1 mild = 2 moderate = 3 severe = 4
  mr0 = factor(mr0 - 1),
  
  ## Original: none = 1 trace = 2 mild = 3 moderate = 4 severe = 5
  ### Recoded: none = 0 trace = 1 mild = 2 moderate = 3 severe = 4
  mr6 = factor(mr6 - 1),
  
  ## Original: none = 1 trace = 2 mild = 3 moderate = 4 severe = 5
  ### Recoded: none = 0 trace = 1 mild = 2 moderate = 3 severe = 4
  mr12 = factor(mr12 - 1),
  
  ## Original: none = 1 trace = 2 mild = 3 moderate = 4 severe = 5
  ### Recoded: none = 0 trace = 1 mild = 2 moderate = 3 severe = 4
  mr24 = factor(mr24 - 1),
  
  
  ## Original: class i = 1 class ii = 2 class iii = 3 class iv = 4
  ### Recoded: class i = 0 class ii = 1 class iii = 2 class iv = 3 
  nyha0 = factor(nyha0 - 1),
  
  ## Original: class i = 1 class ii = 2 class iii = 3 class iv = 4
  ### Recoded: class i = 0 class ii = 1 class iii = 2 class iv = 3 
  nyha6 = factor(nyha6 - 1),
  
  
  ## Original: class i = 1 class ii = 2 class iii = 3 class iv = 4
  ### Recoded: class i = 0 class ii = 1 class iii = 2 class iv = 3 
  nyha12 = factor(nyha12 - 1),
  
  
  
  ## Original: class i = 1 class ii = 2 class iii = 3 class iv = 4
  ### Recoded: class i = 0 class ii = 1 class iii = 2 class iv = 3 
  nyha24 = factor(nyha24 - 1),
  
  ccsc0 = factor(ccsc0),
  
  mace = factor(mace)
  
) 



## Select baseline predictors and outcomes of interest.

mmr_dat <- mmr %>% select(randomization_assignment,
                          sex,
                          age, ethnicity, racial_category, 
                          aneurysmectomy, atrial_fibrillation, 
                          cabg, cardiac_transplant, cardiomyoplasty, carotid_stenosis, cerebrovascular_disease, chronic_lung_disease,
                          diabetes, dyslipidemia, 
                          family_history_coronary,
                          gastrointestinal_bleeding,
                          heart_failure, heart_surgery,
                          icd, infectious_endocarditis, 
                          liver_disease,
                          malignancy, myocardial_infarction,
                          on_iabp, 
                          pacemaker, pci, peripheral_arterial_disease, peripheral_vascular_disease, prior_sternotomies_num, prior_surgery, psychiatric_disorder,
                          renal_insufficiency, 
                          stroke, 
                          thyroid_disease, tia, tobacco, 
                          ventricular_arrhythmia, 
                          echo0_day, lvesvi0, ero0, lvef0, lvsphere0, vc0, lvid_ed0, lvid_es0, mr0, nyha0, ccsc0, first_mace_day, mace)




write.csv(mmr_dat, file = "data/results/mmr_dat_preped.csv", row.names = FALSE)
