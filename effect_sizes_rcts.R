# Author: Jingyang (Judy) Zhang
# Purpose: calculate effect sizes (Cohen's d) for the four RCTs: 
## Goldstein et al., 2016 Two-Year Outcomes of Surgical Treatment of Severe Ischemic Mitral Regurgitation
### ctrl: MV Repair trt: MV Replacement

## Michler et al., 2016 Two-Year Outcomes of Surgical Treatment of Moderate Ischemic Mitral Regurgitation. 
### ctrl: CABG Alone trt: CABG + MV Replacement

## Gammie et al., 2012 The Role of Tricuspid Repair in Patients Undergoing Surgery for Primary Mitral Regurgitation
### ctrl: MVS alone trt: MVS + TA


## Gillinov et al., 2015 Surgical Ablation of Atrial Fibrillation During Mitral-Valve Surgery
### ctrl: MVS alone trt: MVS + AF Ablation


######### Load Libraries #########

library(tidyverse)


#### Outcome LVESVI ####
dat_lvesvi <- read_csv("data/rct_LVESVI.csv")

dat_lvesvi <- dat_lvesvi %>% mutate(
  sd_pooled = sqrt(((n_trt - 1)*sd_trt^2 + (n_ctrl - 1)*sd_ctrl^2) / (n_trt + n_ctrl - 2)),
  d = (y_trt - y_ctrl) / sd_pooled,
  #y_ro1 = (y_trt + y_ctrl) / 2,
  #sd_ro1 = (sd_trt + sd_ctrl)/2
)

write_csv(dat_lvesvi,"data/results/rct_LVESVI_es.csv")



#### Outcome LVEF ####
dat_lvef <- read_csv("data/rct_LVEF.csv")

dat_lvef <- dat_lvef %>% mutate(
  sd_pooled = sqrt(((n_trt - 1)*sd_trt^2 + (n_ctrl - 1)*sd_ctrl^2) / (n_trt + n_ctrl - 2)),
  d = (y_trt - y_ctrl) / sd_pooled,
  #y_ro1 = (y_trt + y_ctrl) / 2,
  #sd_ro1 = (sd_trt + sd_ctrl)/2
)

write_csv(dat_lvef,"data/results/rct_LVEF_es.csv")
