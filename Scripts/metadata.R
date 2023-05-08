#######################################################
#################### Final Metadata ####################
#######################################################

# Create final Metadata with the finak set of samples (after filtering lower and other filters detalied in wfinal set)
# data
{
  rm(list = ls())
  try(setwd("~/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/Volumes/ehudda/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/vol/sci/bio/data/moran.yassour/lab/Projects/GMAP/Docs/GMAP_Paper/"), silent = T)
  metadata <- read.delim("Results/TSVs/metadata_not_filtered.tsv",header = T, sep = "\t", as.is = T)
  final_set <- read.table("Results/TSVs/FinalSetOfSamples.tsv")[,1]
}

# remove duplicated rows
{
  dup_rows <- which(metadata$sampleID == "sub.790.3.twomonth")
  no_dup_metadata <- metadata[-dup_rows[1],]
}

final_metadata <- no_dup_metadata[which(no_dup_metadata$sampleID %in% final_set),]
write.table(final_metadata, file = "Results/TSVs/metadata.tsv", quote = F, sep = "\t", row.names = F, qmethod =  "escape")

