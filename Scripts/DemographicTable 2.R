#######################################################
# Number of samples in each Subset #####################
#######################################################

# data
{
  rm(list = ls())
  try(setwd("~/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/Volumes/ehudda/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/vol/sci/bio/data/moran.yassour/lab/Projects/GMAP/Docs/GMAP_Paper/"), silent = T)
  getwd()
  metadata <- read.delim("Results/TSVs/metadataPerInfant.tsv", header=T)
}

library(gtsummary)
library(dplyr)

# order variables as correct order
metadata$gestational_age <- factor(metadata$gestational_age, levels = c(">37WGA",   "33-37WGA", "25-32WGA"), ordered = T)
metadata$race_final <- factor(metadata$race_final, levels = c("Asian","Black", "White", "Multiple Race", "Other"), ordered = T)
metadata$diet0mo <- factor(metadata$diet0mo, levels = c("Exclusively BF", "Partially BF", "Formula" ), labels = c("Breastmilk",  "Mixed", "Formula"), ordered = T)
metadata$first_child <- factor(metadata$first_child, levels = c("First child", "Not first child"), labels = c("Yes", "No"), ordered = T)
metadata$ethnicity_final <- factor(metadata$ethnicity_final, levels = c("Hispanic or Latino", "Not Hispanic or Latino"), labels = c("Yes", "No"), ordered = T)
metadata$Vaginal <- factor(metadata$mode_of_delivery, levels = c("Vaginal", "C-section"), labels = c("Yes", "No"), ordered = T)
metadata$Female <- factor(metadata$gender, levels = c("Female", "Male"), labels = c("Yes", "No"), ordered = T)


tab <- metadata %>% 
  # select(-c(record_id,
  #        perinatal_antibiotic_exposure,
  #        antibiotics_during_delivery,
  #        gestational_age,
  #        gender,
  #        mode_of_delivery,
  #        eczema,
  #        first_child)) %>% 
  tbl_summary(
    include = c(Female, race_final, ethnicity_final, Vaginal, any_antibiotics, diet0mo),
    by = case_id, # split table by group
    label = list(# mode_of_delivery ~ "Delivery mode", 
                 # gestational_age ~ "Gestational age",
                 # gender ~ "Gender",
                 race_final ~ "Race",
                 Vaginal ~ "Vaginal Delivery",
                 # perinatal_antibiotic_exposure ~ "Perinatal antibiotic exposure",
                 # antibiotics_during_delivery ~ "Antibiotics during delivery",
                 ethnicity_final ~ "Hispanic or Latino",
                 diet0mo ~ "Initial diet",
                 any_antibiotics ~ "Any Perinatal antibiotics"),
    missing = "no",digits = all_categorical() ~ 0
  ) %>% 
  modify_header(list(label ~ "**Variable**", all_stat_cols() ~ "**{level}**")) %>%
  add_overall() %>%
  # modify_header(all_stat_cols() ~ "**{level}**<br>N =  {n} ({style_percent(p)}%)") %>%
  # add_n() %>% # add column with total number of non-missing observations
  # add_p() %>% # test for a difference between groups
  modify_spanning_header(c(stat_1, stat_2) ~ "**Allergy status**") %>%  
  modify_header(list(label ~ '**Characteristic**', 
                stat_0 ~ '**Overall**<br> {N} (100%)',
                stat_1 ~ '**FPIAP**<br>{n} ({style_percent(p)}%)',
                stat_2 ~ '**Control**<br>{n} ({style_percent(p)}%)')) %>% 
  
  modify_footnote(all_stat_cols() ~ NA) %>% 
  bold_labels() 

tab

if (1){
  tab %>% as_gt() %>% gt::gtsave(filename = "Figures/revision/PDFs/DemographicTable.pdf", zoom = 1)
  # tab %>% as_gt() %>% gt::gtsave(filename = "DemographicTable.pdf", zoom = 1)
  
}
