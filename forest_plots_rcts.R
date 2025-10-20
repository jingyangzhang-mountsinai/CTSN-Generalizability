# Author: Jingyang (Judy) Zhang
# Purpose: create forest plots for the four RCTs: 
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



######### Data Processing #########

dat <- tibble(
  study = c("Goldstein et al., 2016", "Michler et al., 2016", "Gammie et al., 2022", "Gillnov et al., 2015"),
  n = c(251, 301, 401, 260),
  n_ctrl = c(126, 151, 203, NA),
  n_trt = c(125, 150, 198, NA),

  # Outcome 1: LVESVI (ml/m^2)
  twoyrLVESVI_estimate = c(63.4, 57.2, 59.6, NA),
  twoyrLVESVI_estimate_se = c(26.8, 25.3, 26.6, NA),
 
  
  # Outcome 2: LVEF (%)
  twoyrLVEF_estimate = c(40.1, 41.1, 64.2, 55.8),
  twoyrLVEF_estimate_se = c(10.8, 10.8, 7.3, 7.7),

  
) %>%
  # Calculate CIs
  mutate(
    # Outcome 1: LVESVI (ml/m^2)
    LVESVI_lower = twoyrLVESVI_estimate - twoyrLVESVI_estimate_se,
    LVESVI_upper = twoyrLVESVI_estimate + twoyrLVESVI_estimate_se,

    # Outcome 2: LVEF (%) (values mismatch)
    LVEF_lower = twoyrLVEF_estimate - twoyrLVEF_estimate_se,
    LVEF_upper = twoyrLVEF_estimate + twoyrLVEF_estimate_se,
    
  )


# Separate By Outcome


LVESVI_dat <- dat %>% 
  select(-contains("LVEF")) %>% 
  mutate(
    n = as.numeric(n),
    outcome = "LVESVI")
LVEF_dat <- dat %>% 
  select(-contains("LVESVI")) %>% 
  mutate(
    n = as.numeric(n),
    outcome = "LVEF")


######### Data Visualization #########


# Outcome 1: LVESVI (ml/m^2)

ggplot(LVESVI_dat, aes(x = twoyrLVESVI_estimate, y = study)) + 
  geom_point(aes(size = n)) +
  geom_errorbar(aes(xmin = LVESVI_lower, xmax = LVESVI_upper), width = 0.2) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") + 
  scale_size_continuous(name = "Total Sample Size") + 
  theme_minimal(base_size = 14) + 
  scale_x_continuous(name = expression("Esimated LVESVI ("*mL/m^2*")"), limits = c(-10, 120)) + 
  labs(y = "Study",
       title = "Forest Plot of Estimated Left Ventricular End-Systolic Volume Index (LVESVI)") + 
  theme(
    plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),  # top, right, bottom, left
  )





# Outcome 2: LVEF (%)

ggplot(LVEF_dat, aes(x = twoyrLVEF_estimate, y = study)) + 
  geom_point(aes(size = n)) +
  geom_errorbar(aes(xmin = LVEF_lower, xmax = LVEF_upper), width = 0.2) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") + 
  scale_size_continuous(name = "Total Sample Size") + 
  theme_minimal(base_size = 14) + 
  scale_x_continuous(name = "Estimated LVEF (%)", limits = c(-10, 120)) + 
  labs(y = "Study",
       title = "Forest Plot of Estimated Left Ventricular Ejection Fraction (LVEF)") + 
  theme(
    plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),  # top, right, bottom, left
  )

