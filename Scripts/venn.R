###########################################################
########## dropped samples explanation ####################
###########################################################

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
  library(ggVennDiagram)
  library(ggpubr)
  
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
  
metadata_explain <- metadata
metadata_explain$valid_case <- metadata_explain$sampleID %in% metadata_samples[which(!is.na(metadata$case_id))]
metadata_explain$pass_seq <- metadata_explain$sampleID %in% metadata_samples[which(metadata_samples %in% tab_samples)]
metadata_explain$higher_reads <- metadata_explain$sampleID %in% names(colSums(tab.abs[,-c(which(colSums(tab.abs[,]) < 3000))]))
metadata_explain$valid_metadata <- metadata_explain$sampleID %in% tab_samples[which(tab_samples %in% metadata_samples)]


m <- ncol(metadata_explain)
df <- colSums(metadata_explain[,(m-3):m])
total <- paste(names(df), " = ",df, collapse = " | ")
set1 <- which(metadata_explain$valid_case)
set2 <- which(metadata_explain$pass_seq)
set3 <- which(metadata_explain$higher_reads)
set4 <- which(metadata_explain$valid_metadata)

# Chart
g <- ggVennDiagram(
  x = list(set1, set2, set3, set4),
  label = "count",
  label_alpha = 0,
  category.names = c("Valid cases" , "seq data" , "high reads","valid metadata"),
  stroke_size = 0.5, set_name_size = 2
) + ggplot2::scale_fill_gradient(low="white",high = "blue")+
  ggplot2::labs(title = "GMAP samples", caption = total)
g


gg <- g
gg
# Save
{
  path <- "Figures/"
  fileName <- "venn"
  if (1) {
    ggsave(gg,filename = paste0(path, "PDFs/",fileName, ".pdf"), "pdf", useDingbats=FALSE,
           width = 6, height = 3)
    ggsave(gg,filename = paste0(path, "PNGs/",fileName, ".png"), "png",
           width = 6, height = 3)
  }
  
}

