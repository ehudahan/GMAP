# Assumptions
{
  # Please run filter scripts to get the final_set of sample
}
# data
{
  rm(list = ls())
  
  # copy the file from our data folder to paper data folder
  try(setwd("~/GMAP/"), silent = T)
  try(setwd("/Volumes/ehudda/GMAP/"), silent = T)
  try(setwd("/vol/sci/bio/data/moran.yassour/lab/Projects/GMAP/"), silent = T)
  file.copy("QIIME2/Data/output/table_norm_l6.tsv", "Docs/GMAP_Paper/Data/feature-table-norm-not-filtered-l6-v2.tsv", overwrite = T)
  file.copy("QIIME2/Data/output/table_abs_l6.tsv", "Docs/GMAP_Paper/Data/feature-table-abs-not-filtered-l6-v2.tsv", overwrite = T)
  
  try(setwd("~/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/Volumes/ehudda/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/vol/sci/bio/data/moran.yassour/lab/Projects/GMAP/Docs/GMAP_Paper/"), silent = T)
  source("Scripts/Functions/get_lowest_taxname.R")
  AbsFeatureTable <- read.delim("Data/feature-table-abs-not-filtered-l6-v2.tsv", sep = "\t", as.is = T, header = T, skip = 1)
  NormFeatureTable <- read.delim("Data/feature-table-norm-not-filtered-l6-v2.tsv", sep = "\t", as.is = T, header = T, skip = 1)
  # final_set <- read.table("Results/TSVs/FinalSetOfSamples.tsv")[,1]
}

# shorten the taxa names
{
  NormFeatureTable[1:4,1:3]
  taxanames <- NormFeatureTable$X.OTU.ID
  
  if (length(grep("g__Clostridium", taxanames)) > 1) {
    message("Warning! Check Clostridium trees is valid")
    taxanames[grep("g__Clostridium", taxanames)]
  }
  
  
  level <- 6
  short.names <- get_lowest_taxaname(NormFeatureTable$X.OTU.ID, start_lvl = level)
  
  if (nrow(NormFeatureTable) != length(short.names)){
    message("Warning! check your shorten name proccess")
  }
  sort(table(short.names))
  length(unique(short.names))
}

# convert to matrix
{
  NormFeatureTableAsMatrix <- as.matrix(NormFeatureTable[,-c(1)])
  rownames(NormFeatureTableAsMatrix) <- short.names
  NormFeatureTableAsMatrix[1:4,1:4]
  
  AbsFeatureTableAsMatrix <- as.matrix(AbsFeatureTable[,-c(1)])
  rownames(AbsFeatureTableAsMatrix) <- short.names
  AbsFeatureTableAsMatrix[1:4,1:4]

  NormFeatureTableAsMatrixFullNames <- as.matrix(NormFeatureTable[,-c(1)])
  rownames(NormFeatureTableAsMatrixFullNames) <- taxanames
  NormFeatureTableAsMatrixFullNames[1:4,1:4]
  
  
}


write.table(NormFeatureTableAsMatrix, file = "Results/TSVs/FeatureTable_not_filtered.tsv", append = F)
write.table(AbsFeatureTableAsMatrix, file = "Results/TSVs/FeatureTable_not_filtered_abs.tsv", append = F)
write.table(NormFeatureTableAsMatrixFullNames, file = "Results/TSVs/FeatureTable_not_filtered_full_names.tsv", append = F)
