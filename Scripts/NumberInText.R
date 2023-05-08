############################################################
#################### Filters ###############################
############################################################


{
  library(dplyr)
}
# data
{
  try(setwd("~/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/Volumes/ehudda/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/vol/sci/bio/data/moran.yassour/lab/Projects/GMAP/Docs/GMAP_Paper/"), silent = T)
  getwd()
  metadata <- read.table("Results/TSVs/metadata.tsv")
}


{
  n_final_set <- nrow(metadata)
  final_metadata <- metadata
  final_set_kids <- final_metadata %>% pull(record_id) %>% unique()
}


{
  n_cases <- final_metadata %>% filter(case_id == "AP Case") %>% pull(record_id) %>% unique() %>% length()
  n_controls <- final_metadata %>% filter(case_id == "No AP") %>% pull(record_id) %>% unique() %>% length()
  med_n_samples_per_kid <- final_metadata %>% group_by(record_id) %>% summarise(n=n()) %>% pull(n) %>% median()
  max_n_samples_per_kid <- final_metadata %>% group_by(record_id) %>% summarise(n=n()) %>% pull(n) %>% max()
  min_n_samples_per_kid <- final_metadata %>% group_by(record_id) %>% summarise(n=n()) %>% pull(n) %>% min()
  female_proportion <- round(sum(final_metadata$gender == "Female") / nrow(final_metadata) * 100, 0)
  delivered_proportion <- round(sum(final_metadata$mode_of_delivery == "Vaginal") / nrow(final_metadata) * 100, 0)
  init_breast_fed_proportion <- round(sum(final_metadata$initial_diet == "BM only",na.rm = T) / nrow(final_metadata) * 100, 0)
  antbio_proportion <- round(sum(final_metadata$antacid_firstyr == 1,na.rm = T) / nrow(final_metadata) * 100, 0)
  
  n_samples <- n_samples
  n_final_set
}



text <- paste0("We selected ", n_cases ," infants diagnosed with food protein-induced
allergic proctocolitis (FPIAP) and ", n_controls, " matched controls from the 
GMAP healthy infant cohort with robust fecal microbiome sampling across their 
first year of life (Supp Figure 1).  The median number of samples in the first 
year per child was ", med_n_samples_per_kid, " [", min_n_samples_per_kid, ", ", 
               max_n_samples_per_kid, "]. ", female_proportion, "% were female, ", 
               delivered_proportion, "% were delivered vaginally, ",init_breast_fed_proportion, 
               "% were initially exclusively breastfed, and ", antbio_proportion,"% were 
perinatally exposed to antibiotics (Figure 1).  No significant differences among
any of these factors were noted between infants with FPIAP and controls, with 
the exception of initial diet which was more commonly formula in the infants 
with FPIAP (Martin, JACI-IP, 2020). \n
A final set of ", n_final_set, " samples were sequenced succesfully using 16S ribosomal gene 
sequencing.", "Sequencing data were analyzed using QIIME2 (see Methods), generating a 
genus-level composition map for each sample.")

message(text)
