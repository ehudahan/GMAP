###################################################
############### EasyMap input ###################
###################################################
###################################################
###################################################
library(dplyr)

# data
{
  rm(list = ls())
  try(setwd("/Volumes/ehudda/lab/Projects/GMAP/Docs/GMAP_Paper/"), silent = T)
  getwd()
  data <- read.delim("/Volumes/ehudda/lab/Projects/GMAP/Docs/GMAP_Paper/Data/Microbiome_revision/grossAP_easymap_format.tsv", sep = "\t", as.is = T)
}

tab <- data %>% filter(visit_age_mo < 3)

write.table(tab, "Data/Microbiome_revision/grossAP_0-2_easymap_format.tsv", sep = "\t", row.names = F)