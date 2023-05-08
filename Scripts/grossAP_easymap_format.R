###################################################
############### EasyMap input ###################
###################################################
###################################################
###################################################
library(dplyr)

# data
{
  rm(list = ls())
  try(setwd("/Volumes/ehudda/GMAP/Docs/GMAP_Paper/"), silent = T)
  metadata <- read.table("Results/TSVs/metadata.tsv", header=T, sep = '\t')
  tab.n <- read.table("Results/TSVs/FeatureTable.tsv",header = T)
  GrossAPFilter_data <- read.delim("/Volumes/ehudda/lab/Projects/GMAP/Docs/GMAP_Paper/Data/Microbiome_revision/GMAP_Broad_case_id7.csv", sep = ",", as.is = T)
}

# join with GrossAP data
colSums(table(GrossAPFilter_data))

in_analysis <- GrossAPFilter_data[which(GrossAPFilter_data$case_id7 %in% c("Gross AP Case", "No AP")),]
in_analysis
out <- GrossAPFilter_data[-which(GrossAPFilter_data$case_id7 %in% c("Gross AP Case", "No AP")),]
out

f_metadata <- metadata %>% 
  left_join(GrossAPFilter_data) %>% 
  filter(case_id7 %in% c("Gross AP Case", "No AP"))

table(f_metadata[,c("record_id", "case_id7")])



# join with features data
df <- data.frame(t(tab.n))
colnames(df) <- rownames(tab.n)
df <- df %>% mutate("sampleID" = rownames(df))
dim(df)
head(df)

tab <- f_metadata %>% 
  left_join(df)
dim(tab)
head(tab)
sort(names(tab))
tab %>% select(c("sampleID", "record_id", "mode_of_delivery", "visit_age_mo", "lastDiet", "case_id7", "probiotics_firstyr", "symptoms", rownames(tab.n)))
"f__[Barnesiellaceae]_unclsfd" %in% names(tab)

# remove non relevant variales
tab2 <- tab[,c("sampleID", "record_id", "mode_of_delivery", "visit_age_mo", "lastDiet", "case_id7", "probiotics_firstyr", "symptoms", rownames(tab.n))]
head(tab2)
sort(colnames(tab2))

if(0){
  write.table(tab2, "Data/Microbiome_revision/grossAP_easymap_format.tsv", sep = "\t")
}

