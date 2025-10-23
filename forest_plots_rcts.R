# Author: Jingyang (Judy) Zhang
# Date: Oct 20th, 2025
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
library(ggpubr)
library(patchwork)

#### Outcome: LVESVI (mL/m2) ####

dat_lvesvi_es <- read_csv("data/results/rct_LVESVI_es.csv") %>% mutate(
  d_lower = d - 1.96 * se_d,
  d_upper = d + 1.96 * se_d,
  label = factor(paste0(study, ": \n", ctrl, " vs. ", trt))
) %>% drop_na()

dat_lvesvi_es$label <- factor(dat_lvesvi_es$label, levels = c("CTSN Severe MR: \nmv repair vs. mv replacement",
                                                              "CTSN Moderate MR: \nCABG alone vs. CABG + mv repair",
                                                              "CTSN TR Trial: \nMVS alone vs. MVS + TA",
                                                              "CTSN AF Trial: \nMVS alone vs. MVS + ablation"))

dat_lvesvi_es$label <- factor(dat_lvesvi_es$label, levels = rev(unique(dat_lvesvi_es$label)))


#### Outcome: LVEF (%) ####
dat_lvef_es <- read_csv("data/results/rct_LVEF_es.csv")%>% mutate(
  d_lower = d - 1.96 * se_d,
  d_upper = d + 1.96 * se_d,
  label = factor(paste0(study, ": \n", ctrl, " vs. ", trt))
)  



dat_lvef_es$label <- factor(dat_lvef_es$label, levels = c("CTSN Severe MR: \nmv repair vs. mv replacement",
                                                              "CTSN Moderate MR: \nCABG alone vs. CABG + mv repair",
                                                              "CTSN TR Trial: \nMVS alone vs. MVS + TA",
                                                              "CTSN AF Trial: \nMVS alone vs. MVS + ablation"))

dat_lvef_es$label <- factor(dat_lvef_es$label, levels = rev(unique(dat_lvef_es$label)))




#### Outcome: MACE ####

dat_mace_hr <- read_csv("data/rct_MACE_hr.csv")%>% mutate(
  label = factor(paste0(study, ": \n", ctrl, " vs. ", trt)),
  se_HR = (log(upper) - log(lower)) / 3.92
)  



dat_mace_hr$label <- factor(dat_mace_hr$label, levels = c("CTSN Severe MR: \nmv repair vs. mv replacement",
                                                          "CTSN Moderate MR: \nCABG alone vs. CABG + mv repair",
                                                          "CTSN TR Trial: \nMVS alone vs. MVS + TA",
                                                          "CTSN AF Trial: \nMVS alone vs. MVS + ablation"))

dat_mace_hr$label <- factor(dat_mace_hr$label, levels = rev(unique(dat_mace_hr$label)))




### Method 1: Using Meta-Analysis ###
# Not Recommend: since the four RCTs are not "similar" in the sense that they are comparing the same pair of treatment arms, meta-analysis of the four RCTs is not recommended. 



m_lvesvi <- metagen(d, se_d, studlab = study, data = dat_lvesvi_es)
forest(m_lvesvi, xlab = "Effect Size (Cohen's d)")

m_lvef <- metagen(d, se_d, studlab = study, data = dat_lvef_es)
forest(m_lvef, xlab = "Effect Size (Cohen's d)")

m_mace <- metagen(HR, se_HR, studlab = study, data = dat_mace_hr)
forest(m_mace, xlab = "Effect Size (hazard ratio)")





### Method 2: Manually Plot Effect Sizes ###
base_size <- 1



#### Outcome: LVESVI (mL/m2) ####

forest_lvesvi <- ggplot(dat_lvesvi_es, aes(x = d, y =label)) + 
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") + 
  geom_point(aes(size = n)) +
  geom_errorbar(aes(xmin = d_lower, xmax = d_upper), width = 0.2) + 
  scale_size_continuous(name = "Total Sample Size", range = c(1,4)) + 
  scale_x_continuous(name = expression("Standardized Mean Difference In LVESVI ("*mL/m^2*") Between Treatment Arms"), 
                     limits = c(-0.75, 0.5),
                     expand = c(0, 0)) + 
  scale_y_discrete(name = "Study:\n control vs. treatment",
                   expand = c(0.1, 0.1)) + 
  coord_fixed(ratio = 0.1) + 
  theme_minimal(base_size = 5) + 
  theme(
    axis.title = element_text(size = rel(1)),
    axis.text = element_text(size = rel(1)),
    legend.title = element_text(size = rel(1)),
    legend.text = element_text(size = rel(1)),
    panel.grid.major = element_line(linewidth = rel(1)),
    panel.grid.minor = element_line(linewidth = rel(1)),
    plot.margin = unit(c(0, 0, 0, 0), "cm") # top, right, bottom, left
  )


#ggsave("results/forest_lvesvi.pdf", forest_lvesvi, height = 2, width = 10)




#### Outcome: LVEF (%) ####

forest_lvef <- ggplot(dat_lvef_es, aes(x = d, y = label)) + 
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") + 
  geom_point(aes(size = n)) +
  geom_errorbar(aes(xmin = d_lower, xmax = d_upper), width = 0.2) + 
  scale_size_continuous(name ="Total Sample Size", range = c(1,4)) + 

  scale_x_continuous(name = "Standardized Mean Difference in LVEF (%) Between Treatment Arms", 
                     limits = c(-0.75, 0.5),
                     expand = c(0, 0)) + 
  scale_y_discrete(name = "Study:\n control vs. treatment",
                   expand = c(0.05, 0)) + 
  coord_fixed(ratio = 0.1) + 
  theme_minimal(base_size = 5) + 
  theme(
    axis.title = element_text(size = rel(1)),
    axis.text = element_text(size = rel(1)),
    legend.title = element_text(size = rel(1)),
    legend.text = element_text(size = rel(1)),
    panel.grid.major = element_line(linewidth = rel(1)),
    panel.grid.minor = element_line(linewidth = rel(1)),
    plot.margin = unit(c(0, 0, 0, 0), "cm") # top, right, bottom, left
  )


ggarrange(forest_lvef, forest_lvesvi, ncol = 1, 
          heights = c(1,1), common.legend = TRUE,
          legend = "bottom",
          align = "h")



forest_lvef_lvesvi <- forest_lvef / forest_lvesvi + plot_layout(guides = "collect")

ggsave("results/forest_lvef_lvesvi.pdf", 
       forest_lvef_lvesvi, 
       height = 2, width = 10, 
       limitsize = FALSE, bg = "transparent",
       dpi = 600)






#### Outcome: MACE (HR) ####

forest_mace <- ggplot(dat_mace_hr, aes(x = HR, y = label)) + 
  geom_vline(xintercept = 1, color = "red", linetype = "dashed") + 
  geom_point(aes(size = n)) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.2) + 
  scale_size_continuous(name ="Total Sample Size", range = c(1,4)) + 
  
  scale_x_continuous(name = "Hazard Ratio Between Treatment Arms", 
                     limits = c(0.2, 2),
                     expand = c(0, 0)) + 
  scale_y_discrete(name = "Study:\n control vs. treatment",
                   expand = c(0.05, 0)) + 
  coord_fixed(ratio = 0.1) + 
  theme_minimal(base_size = 5) + 
  theme(
    axis.title = element_text(size = rel(1)),
    axis.text = element_text(size = rel(1)),
    legend.title = element_text(size = rel(1)),
    legend.text = element_text(size = rel(1)),
    panel.grid.major = element_line(linewidth = rel(1)),
    panel.grid.minor = element_line(linewidth = rel(1)),
    plot.margin = unit(c(0, 0, 0, 0), "cm") # top, right, bottom, left
  )



ggsave("results/forest_mace.pdf", 
       forest_mace, 
       height = 2, width = 10, 
       limitsize = FALSE, bg = "transparent",
       dpi = 600)
