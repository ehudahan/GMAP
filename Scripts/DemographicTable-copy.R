#######################################################
# Number of samples in each Subset #####################
#######################################################

# data
{
  rm(list = ls())
  try(setwd("~/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/Volumes/ehudda/GMAP/Docs/GMAP_Paper/"), silent = T)
  # try(setwd("/Volumes/morani/GMAP/Docs/GMAP_Paper/"), silent = T)
  
  try(setwd("/vol/sci/bio/data/moran.yassour/lab/Projects/GMAP/Docs/GMAP_Paper/"), silent = T)
  getwd()
  metadata <- read.delim("Results/TSVs/metadataPerInfant.tsv", header=T)
}


library(gtsummary)


# order variables as correct order
metadata$gestational_age <- factor(metadata$gestational_age, levels = c(">37WGA",   "33-37WGA", "25-32WGA"), ordered = T)
metadata$race_final <- factor(metadata$race_final, levels = c("Asian","Black", "White", "Multiple Race", "Other"), ordered = T)
metadata$initial_diet <- factor(metadata$initial_diet, levels = c("BM only", "BM + F", "F only" ), labels = c("Breastfed",  "Mix", "Formula"), ordered = T)
metadata$first_child <- factor(metadata$first_child, levels = c("First child", "Not first child"), labels = c("Yes", "No"), ordered = T)
metadata$ethnicity_final <- factor(metadata$ethnicity_final, levels = c("Hispanic or Latino", "Not Hispanic or Latino"), labels = c("Yes", "No"), ordered = T)



tab <- metadata %>% 
  select(-record_id) %>% 
  tbl_summary(
    by = case_id, # split table by group
    label = list(mode_of_delivery ~ "Delivery mode", 
                 gestational_age ~ "Gestational age",
                 gender ~ "Gender",
                 race_final ~ "Race",
                 perinatal_antibiotic_exposure ~ "Perinatal antibiotic exposure",
                 antibiotics_during_delivery ~ "Antibiotics during delivery",
                 ethnicity_final ~ "Hispanic or Latino",
                 initial_diet ~ "Initial diet",
                 any_antibiotics ~ "Any antibiotics"),
    missing = "no"
  ) %>% 
  # modify_header(all_stat_cols() ~ "**{level}**<br>N =  {n} ({style_percent(p)}%)") %>% 
  add_n() %>% # add column with total number of non-missing observations
  add_p() %>% # test for a difference between groups
  modify_header(list(label ~ '**Characteristic**', 
                n ~ '**Cohort<br>** N(%)', 
                stat_1 ~ '**FPIAP**<br>{n} ({style_percent(p)}%)',
                stat_2 ~ '**Control**<br>{n} ({style_percent(p)}%)')) %>% 
  bold_labels() 

# check that each column has only expected variables and not too many in one variable
lapply(names(metadata), function(x) table(metadata[[x]]))

tab

if (1){
  tab %>% as_gt() %>% gt::gtsave(filename = "Figures/revision/PDFs/DemographicTable.pdf", zoom = 1)
}
