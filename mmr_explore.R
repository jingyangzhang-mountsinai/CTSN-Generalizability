# Author: Jingyang (Judy) Zhang
# Date: Oct 22th, 2025
# Purpose: explore the MMR dataset.
## 1. Recode some of the categorical variables.
## 2. Look at values, missingness, distribution, outliers.

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
  chronic_lung_disease = ifelse(chronic_lung_disease == 9, chronic_lung_disease, chronic_lung_disease - 1),
  
  
  
  ## Original: none = 1 trace = 2 mild = 3 moderate = 4 severe = 5
  ### Recoded: none = 0 trace = 1 mild = 2 moderate = 3 severe = 4
  mr0 = mr0 - 1,
  
  ## Original: none = 1 trace = 2 mild = 3 moderate = 4 severe = 5
  ### Recoded: none = 0 trace = 1 mild = 2 moderate = 3 severe = 4
  mr6 = mr6 - 1,
  
  ## Original: none = 1 trace = 2 mild = 3 moderate = 4 severe = 5
  ### Recoded: none = 0 trace = 1 mild = 2 moderate = 3 severe = 4
  mr12 = mr12 - 1,
  
  ## Original: none = 1 trace = 2 mild = 3 moderate = 4 severe = 5
  ### Recoded: none = 0 trace = 1 mild = 2 moderate = 3 severe = 4
  mr24 = mr24 - 1,
  
  
  ## Original: class i = 1 class ii = 2 class iii = 3 class iv = 4
  ### Recoded: class i = 0 class ii = 1 class iii = 2 class iv = 3 
  nyha0 = nyha0 - 1,
  
  ## Original: class i = 1 class ii = 2 class iii = 3 class iv = 4
  ### Recoded: class i = 0 class ii = 1 class iii = 2 class iv = 3 
  nyha6 = nyha6 - 1,
  
  
  ## Original: class i = 1 class ii = 2 class iii = 3 class iv = 4
  ### Recoded: class i = 0 class ii = 1 class iii = 2 class iv = 3 
  nyha12 = nyha12 - 1,
  
  
  
  ## Original: class i = 1 class ii = 2 class iii = 3 class iv = 4
  ### Recoded: class i = 0 class ii = 1 class iii = 2 class iv = 3 
  nyha24 = nyha24 - 1
  

  
)



mmr_skim_summary <- skim(mmr)



kable(mmr_skim_summary) %>% 
  kable_styling(full_width = FALSE) %>% 
  save_kable("results/mmr_skim_summary.html")




## Convert all 0/1 variables into factors

is_binary <- function(x) {
  vals <- unique(x[!is.na(x)])
  all(vals %in% c(0, 1, TRUE, FALSE, "0", "1"))
}


mmr <- mmr %>%
  mutate(across(
    .cols = everything(),
    .fns = ~ {
      if (is_binary(.x)) {
        as.factor(.x)  # safely convert to factor
      } else {
        .x             # leave as-is
      }
    }
  )) %>% 
  ## Manually convert categorical variables with more than 2 levels
  mutate(
    racial_category = as.factor(racial_category),
    chronic_lung_disease = as.factor(chronic_lung_disease),
    across(starts_with("mr"), as.factor),
    across(starts_with("nyha"), as.factor),
    across(starts_with("ccsc"), as.factor),
    mace = as.factor(mace)
    
  )



## Assess numerical and categorical variables separately
mmr_num <- mmr %>% select(where(is.numeric))

write.csv(mmr_num, file = "data/mmr_num.csv")

mmr_skim_num_summary <- skim(mmr_num)



kable(mmr_skim_num_summary) %>% 
  kable_styling(full_width = FALSE) %>% 
  save_kable("results/mmr_skim_num_summary.html")


mmr_categ <- mmr %>% select(where(is.factor))

write.csv(mmr_categ, file = "data/mmr_categ.csv")



## Assess outliers


summary_with_outliers <- function(df) {
  detect_outliers <- function(x) {
    if(!is.numeric(x)) return(NA)
    ### Remove NAs
    x <- na.omit(x)
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    outliers <- x[x < (Q1 - 1.5 * IQR) | x > (Q3 + 1.5 * IQR)]
    if(length(outliers) == 0) return(NA)
    return(unique(sort(outliers)))
  }
  
  data.frame(
    variable = names(df),
    type = sapply(df, class),
    n_outliers = sapply(df, function(x) 
      if(is.numeric(x)) sum(!is.na(detect_outliers(x))) else NA),
    outliers = I(sapply(df, detect_outliers))
  )
}


mmr_outliers <- summary_with_outliers(mmr)

mmr_outliers <- mmr_outliers %>% filter(type != "factor", n_outliers != 0, is.na(n_outliers) == FALSE)

mmr_outliers

write.csv(mmr_outliers, file = "data/results/mmr_outliers.csv")
