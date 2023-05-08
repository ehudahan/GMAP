library(tidyverse)
library(dplyr)
if(1){
  rm(list=ls())
  try(setwd("~/GMAP/QIIME2/Data/output/UnknownVsUnclsf/"), silent = T)
  try(setwd("/Volumes/ehudda/GMAP/QIIME2/Data/output/UnknownVsUnclsf/"), silent = T)
  try(setwd("/vol/sci/bio/data/moran.yassour/lab/Projects/GMAP/QIIME2/Data/output/UnknownVsUnclsf/"), silent = T)
  getwd()
  source("../../../../Docs/GMAP_Paper/Scripts/Functions/get_lowest_taxname.R")
  dna_seqs_raw <- read_lines("dna-sequences.fasta")
  tax <- read.table("taxonomy.tsv", header = T, sep = "\t")
  tab_raw <- read.delim("table_abs_l6.tsv", sep = "\t", as.is = T, header = T, skip = 1)
  tab_raw[1:4,1:4]
  tab.n <- read.table("FeatureTable.tsv",header = T)
  biom <- read.table("table.from_biom.txt", row.names = 1)
  
}

# edit dna seq
dna_seqs <- dna_seqs_raw[seq(2, length(dna_seqs_raw), 2)]
names(dna_seqs) <- substr(dna_seqs_raw[seq(2, length(dna_seqs_raw), 2) - 1], 2, 65)
table(lengths(dna_seqs_raw[seq(2, length(dna_seqs_raw), 2) - 1]))
dna_seqs

rownames(biom)
biom[1:4,1:4]



# AbsFeatureTable <- read.delim("Data/feature-table-abs-not-filtered-l6-v2.tsv", sep = "\t", as.is = T, header = T, skip = 1)
NormFeatureTable <- read.delim("feature_table_biom_l6.tsv", sep = "\t", as.is = T, header = T, skip = 1)
# final_set <- read.table("Results/TSVs/FinalSetOfSamples.tsv")[,1]

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


bacs <- c("o__Clostridiales_unknwn", "f__Enterobacteriaceae_unknwn", "f__Enterobacteriaceae_unclsfd")

df <- data.frame("short_name" = short.names,
              "ftable_name" = taxanames)
df[which(df$short_name %in% bacs),]

sort(grep("f__; g__; s__", x = tax$Taxon, value = T))
sort(tax$Taxon[grep(";__", x = tax$Taxon, value = T)])
tax$ftable_name <- gsub(pattern = "; ",replacement = ";",x = tax$Taxon)

tax$ftable_name[grep("f__;g__;s__", tax$ftable_name)]

df %>% filter(short.names %in% bacs)
tax %>% filter(str_detect(ftable_name, "k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__Enterobacteriales;f__Enterobacteriaceae")) %>% arrange(ftable_name)
# tax$ftable_name <- gsub("k__Bacteria;p__Firmicutes;c__Clostridia;o__Clostridiales;f__;g__;s__",
#                         replacement = "k__Bacteria;p__Firmicutes;c__Clostridia;o__Clostridiales;__;__", x = tax$ftable_name)

tax$ftable_name <- sapply(tax$ftable_name, function(x) ifelse(x == "k__Bacteria;p__Firmicutes;c__Clostridia;o__Clostridiales;f__;g__;s__", 
                                                              "k__Bacteria;p__Firmicutes;c__Clostridia;o__Clostridiales;__;__", x))

# tax$ftable_name <- gsub(pattern = "k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__Enterobacteriales;f__Enterobacteriaceae;g__;s__",
#                         replacement = "k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__Enterobacteriales;f__Enterobacteriaceae;g__", x = tax$ftable_name)

tax$ftable_name <- sapply(tax$ftable_name, function(x) ifelse(x == "k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__Enterobacteriales;f__Enterobacteriaceae;g__;s__", 
                                                              "k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__Enterobacteriales;f__Enterobacteriaceae;g__", x))

tax$ftable_name <- sapply(tax$ftable_name, function(x) ifelse(x == "k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__Enterobacteriales;f__Enterobacteriaceae", 
                        "k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__Enterobacteriales;f__Enterobacteriaceae;__", x))

# tax$ftable_name <- gsub(pattern = "k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__Enterobacteriales;f__Enterobacteriaceae",
#                         replacement = "k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__Enterobacteriales;f__Enterobacteriaceae;__", x = tax$ftable_name)

# tax$ftable_name <- gsub("f__;g__;s__",replacement = "__;__", x = tax$ftable_name)  # add clos
tax %>% filter(str_detect(pattern = "k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__Enterobacteriales;f__Enterobacteriaceae",string = ftable_name)) %>% arrange(ftable_name)
head(tax)
head(df)
df2 <- left_join(tax, df)
df2[which(df2$short_name %in% bacs),]

df3 <- left_join(df2, data.frame("seq" = dna_seqs, "Feature.ID" = names(dna_seqs)))
df3[which(df3$short_name %in% bacs),]

num_of_samples_df <- data.frame("samples" = rowSums(biom[df3$Feature.ID,]))
num_of_samples_df$Feature.ID <- rownames(num_of_samples_df)
df4 <- left_join(df3, num_of_samples_df)


df5 <- df4 %>% filter(short_name %in% bacs) %>% arrange(desc(samples))
head(df5)
sum(df5$samples)


write.table(x = df5,file =  "alignmet_table.tsv", quote = F, sep = "\t", col.names = NA, row.names = T)
