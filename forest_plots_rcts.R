# Author: Jingyang (Judy) Zhang
# Purpose: create forest plots for the effect sizes of the four RCTs: 
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
library(meta)

dat_lvesvi_es <- read_csv("data/results/rct_LVESVI_es.csv") %>% mutate(
  d_lower = d - 1.96 * se_d,
  d_upper = d + 1.96 * se_d
) %>% drop_na()


dat_lvef_es <- read_csv("data/results/rct_LVEF_es.csv")%>% mutate(
  d_lower = d - 1.96 * se_d,
  d_upper = d + 1.96 * se_d
)



### Method 1: Using Meta-Analysis ###
# Not Recommend: since the four RCTs are not "similar" in the sense that they are comparing the same pair of treatment arms, meta-analysis of the four RCTs is not recommended. 



m_lvesvi <- metagen(d, se_d, studlab = study, data = dat_lvesvi_es)
forest(m_lvesvi, xlab = "Effect Size (Cohen's d")

m_lvef <- metagen(d, se_d, studlab = study, data = dat_lvef_es)
forest(m_lvef, xlab = "Effect Size (Cohen's d")


### Method 2: Manually Plot Effect Sizes ###
base_size <- 5

forest_lvesvi <- ggplot(dat_lvesvi_es, aes(x = d, y = paste0(study, ": \n", ctrl, " vs. ", trt))) + 
  geom_point(aes(size = n)) +
  geom_errorbar(aes(xmin = d_lower, xmax = d_upper), width = 0.2) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") + 
  scale_size_continuous(name = "Total Sample Size") + 
  scale_x_continuous(name = expression("Standardized Mean Difference In LVESVI ("*mL/m^2*") Between Treatmetn Arms"), 
                     limits = c(-0.5, 0.5),
                     expand = c(0, 0)) + 
  scale_y_discrete(name = "Study:\n control vs. treatment",
                   expand = c(0.1, 0.1)) + 
  coord_fixed(ratio = 0.1) + 
  theme_minimal(base_size = 5) + 
  theme(
    axis.title = element_text(size = rel(2)),
    axis.text = element_text(size = rel(1.5)),
    legend.title = element_text(size = rel(1.5)),
    legend.text = element_text(size = rel(1)),
    panel.grid.major = element_line(linewidth = rel(1)),
    panel.grid.minor = element_line(linewidth = rel(1.5)),
    plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),  # top, right, bottom, left
  )


ggsave("results/forest_lvesvi.pdf", forest_lvesvi, height = 2, width = 10)

forest_lvef <- ggplot(dat_lvef_es, aes(x = d, y = paste0(study, ": \n", ctrl, " vs. ", trt))) + 
  geom_point(aes(size = n)) +
  geom_errorbar(aes(xmin = d_lower, xmax = d_upper), width = 0.2) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") + 
  scale_size_continuous(name = "Total Sample Size",
                    range = c(1, 4)) + 

  scale_x_continuous(name = "Standardized Mean Difference in LVEF (%) Between Treatmetn Arms", 
                     limits = c(-0.75, 0.25),
                     expand = c(0, 0)) + 
  scale_y_discrete(name = "Study:\n control vs. treatment",
                   expand = c(0.05, 0)) + 
  coord_fixed(ratio = 0.1) + 
  theme_minimal(base_size = 5) + 
  theme(
    axis.title = element_text(size = rel(2)),
    axis.text = element_text(size = rel(1.5)),
    legend.title = element_text(size = rel(1.5)),
    legend.text = element_text(size = rel(1)),
    plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),  # top, right, bottom, left
  )


ggsave("results/forest_lvef.pdf", forest_lvef, height = 2, width = 10)
