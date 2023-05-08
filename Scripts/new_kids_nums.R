####### new numbers for kids ########

if (1) {
  rm(list = ls())
  try(setwd("~/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/Volumes/ehudda/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/vol/sci/bio/data/moran.yassour/lab/Projects/GMAP/Docs/GMAP_Paper/"), silent = T)
  getwd()
  metadata <- read.delim("Results/TSVs/metadata.tsv", header=T, sep = '\t')
}


df <- data.frame("orig" = sort(unique(metadata$record_id[which(metadata$case_id == "AP Case")])))
df$new <- 1:nrow(df)
df$case_id = "AP"

df2 <- data.frame("orig" = sort(unique(metadata$record_id[which(metadata$case_id == "No AP")])))
df2$new <- (max(df$new) + 1):((max(df$new) + 1) + nrow(df2) - 1)
df2$case_id <- "Control"

head(df)
head(df2)
tab <- rbind(df, df2)
tab

write.table(x = tab, file = "Results/TSVs/new_kids_nums.tsv", quote = F, sep = "\t", col.names = NA, row.names = T)
