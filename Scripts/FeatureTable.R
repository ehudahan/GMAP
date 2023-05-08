#######################################################
#################### Feature Table ####################
#######################################################

# Create final feature table with the finak set of samples (after filtering lower and other filters detalied in wfinal set)
# data
{
  rm(list = ls())
  try(setwd("~/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/Volumes/ehudda/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/vol/sci/bio/data/moran.yassour/lab/Projects/GMAP/Docs/GMAP_Paper/"), silent = T)
  tab <- read.table("Results/TSVs/FeatureTable_not_filtered.tsv")
  final_set <- read.table("Results/TSVs/FinalSetOfSamples.tsv")[,1]
}

# remove duplicated rows CHECK WHERE THIS SAMPLES DISAPEAR
{
  dup_cols <- which(colnames(tab) %in% c("sub.790.3.twomonth.1", "sub.790.3.twomonth.2"))
  dup_cols
  new_tab <- tab[,-dup_cols[1]]
}

# fixed unmatche names between seq data and metadata
{
  tab_samples <- colnames(new_tab)
  samples_with_unmatch_seq_data <- tab_samples[-which(tab_samples %in% final_set)]
  fixed_names <- sapply(strsplit(samples_with_unmatch_seq_data, "\\."), function(x) paste(x[1:4], collapse = "."))
  fixed_names
  tab_samples[-which(tab_samples %in% final_set)] <- fixed_names
}

# filter features with very low maximal abundance value
{
  feature_max_thr <- 0.03
  low_features_rows_inx <- which(apply(new_tab, 1, FUN = max) < feature_max_thr)
}


final_tab <- new_tab[-low_features_rows_inx,final_set]
# write.table(final_tab, file = "Results/TSVs/FeatureTable.tsv")
if (0){
  write.table(final_tab, file = "Results/TSVs/FeatureTable.tsv", quote = F, sep = "\t", col.names = NA, row.names = T)
}
