# create metadata per infant (and not per sample)
{
  rm(list = ls())
  try(setwd("~/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/Volumes/ehudda/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/vol/sci/bio/data/moran.yassour/lab/Projects/GMAP/Docs/GMAP_Paper/"), silent = T)
  metadata <- read.delim("Results/TSVs/metadata.tsv", header=T)
}

library(dplyr)

vars <- c("case_id", 
       "gender", 
       "gestational_age",
       "race_final",
       "ethnicity_final",
       "mode_of_delivery",
       "perinatal_antibiotic_exposure", 
       "antibiotics_during_delivery",
       "any_antibiotics",
       "initial_diet", 
       "eczema", 
       "first_child")

res <- metadata %>% 
  group_by(record_id) %>% 
  summarise_at(vars, .funs = function(x) {unique(x)})

res  
write.table(res, file = "Results/TSVs/metadataPerInfant.tsv", quote = F, sep = "\t", row.names = F, qmethod =  "escape")
