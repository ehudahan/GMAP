
# Assumptions
# run the script "Scripts/metadata_not_filtered.R" before
# run the script "Scripts/FeatureTable_not_filtered.R" before

# data
{
  rm(list = ls())
  try(setwd("~/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/Volumes/ehudda/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/vol/sci/bio/data/moran.yassour/lab/Projects/GMAP/Docs/GMAP_Paper/"), silent = T)
  metadata <- read.delim("Results/TSVs/metadata_not_filtered.tsv",header = T, sep = "\t", as.is = T)
  tab <- read.table("Results/TSVs/FeatureTable_not_filtered.tsv")
  tab.abs <- read.table("Results/TSVs/FeatureTable_not_filtered_abs.tsv")
}
head(tab)

# library
{
  library(dplyr)
}


{
  # list the samples
  tab_samples_unmatch <- colnames(tab)
  metadata_samples <- paste("sub",metadata$record_id,gsub("_",".",metadata$sample), sep = ".")
}

# choose one of duplicated samples (Choose the one with higher reads)
{
  samples_with_unmatch_seq_data <- tab_samples_unmatch[-which(tab_samples_unmatch %in% metadata_samples)]
  fixed_names <- sapply(strsplit(samples_with_unmatch_seq_data, "\\."), function(x) paste(x[1:4], collapse = "."))
  fixed_names
  tab_samples <- tab_samples_unmatch
  tab_samples[-which(tab_samples_unmatch %in% metadata_samples)] <- fixed_names
  tab_samples[-which(tab_samples %in% tab_samples_unmatch)]
  tab_samples_unmatch[-which(tab_samples_unmatch %in% tab_samples)]
  sort(tab_samples)
  sort(tab_samples_unmatch)
  length(unique(tab_samples))
  length(unique(tab_samples_unmatch))

}

# ATTENTION - there is 1088 rows in metadata but only 1087 unique samples! 
# (that because "sub.790.3.twomonth have two sequence data. we choose the sample with highest reads number)

# filtering
{
  ##### Samples to exclude #####
  # filter samples with unvalid case information
  samples_with_na_case <- metadata_samples[which(is.na(metadata$case_id))]
  # # filter samples with unvalid probiotica information
  # samples_with_na_pro <- metadata_samples[which(is.na(metadata$probiotics_firstyr))]
  # samples_without_sequencing_data
  samples_without_sequencing_data <- metadata_samples[-which(metadata_samples %in% tab_samples)]
  # filter according to lower reads
  lower.reads.samples <- names(colSums(tab.abs[,c(which(colSums(tab.abs[,-c(1)]) < 3000) + 1)]))

  total_samples_to_exclude <- unique(c(samples_with_na_case,
                                       # samples_with_na_pro,
                                       samples_without_sequencing_data, 
                                       lower.reads.samples))
  length(unique(total_samples_to_exclude))
  
  ##### Samples to include #####
  # filter samples with valid case information
  samples_with_valid_case <- metadata_samples[which(!is.na(metadata$case_id))]
  # # filter samples with valid probiotica information
  # samples_with_valid_pro <- metadata_samples[which(!is.na(metadata$probiotics_firstyr))]
  # samples_without_sequencing_data
  samples_with_sequencing_data <- metadata_samples[which(metadata_samples %in% tab_samples)]
  # filter according to lower reads
  higher.reads.samples <- names(colSums(tab.abs[,-c(which(colSums(tab.abs[,]) < 3000))]))
  #samples without metadata
  samples_with_metadata <- tab_samples[which(tab_samples %in% metadata_samples)]
  
  total_samples_to_include <- Reduce(intersect, list(samples_with_valid_case, 
                                                     # samples_with_valid_pro,
                                                     samples_with_sequencing_data, 
                                                     higher.reads.samples, 
                                                     samples_with_metadata))
  
  length(unique(total_samples_to_include))
  
}


# check that all samples is in the two groups
if (length(c(total_samples_to_exclude,total_samples_to_include)) != 1088) {
  message("Did you miss a sample? I expect to 1088 sample in boith groups")
} else {
  message("Nice, you have exactly 1088 samples")
}



final_metadata <- metadata %>% filter(sampleID %in% total_samples_to_include)

# check number of samples per kid after filtering
{
  min <- min(table(final_metadata$record_id))
  count_min <- sum(min == table(final_metadata$record_id))
  quantile(table(final_metadata$record_id))
  
  message("Mininum number of samples per kid in final set 
  after all filtering proccess is: ", min," (", count_min, " kid with this low number)")
}

{
  # check symptoms is valid
  if (length(unique(final_metadata$symptoms)) != 4){
    message("Check your symptoms information")
  }
}

write.table(x = list(total_samples_to_include), file = "Results/TSVs/FinalSetOfSamples.tsv", quote = F, sep = "\t")





# print a text that explain why we drop each sample



