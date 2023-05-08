library(tidyverse)
library(dplyr)
library(ggpubr)
library(stringr)
if(1){
  rm(list=ls())
  try(setwd("~/GMAP/QIIME2/Data/output/UnknownVsUnclsf/"), silent = T)
  try(setwd("/Volumes/ehudda/GMAP/QIIME2/Data/output/UnknownVsUnclsf/"), silent = T)
  try(setwd("/vol/sci/bio/data/moran.yassour/lab/Projects/GMAP/QIIME2/Data/output/UnknownVsUnclsf/"), silent = T)
  getwd()
  tab <- read.delim("top.unkwn.uncls.blast.out", header = F)
  df_reads <- read.delim("alignmet_table.tsv", header = T)
  names(tab) <- c("qaccver", "saccver", "pident", "length", "mismatch", "gapopen", "qstart", "qend", "sstart", "send", "evalue", "bitscore", "qlen", "slen", "sscinames")
}



# Filtering for only Entero unknown
plot_it <- function(bac){

tab <- tab %>% filter(stringr::str_detect(string = qaccver,pattern = bac))

subtitle <- paste0(bac," (num of reads > 0)")


tab <- tab %>% 
  mutate(first2_name = stringr::word(sscinames, 1, 2, " ")) %>% 
  mutate(first1_name = stringr::word(sscinames, 1, 1, " "))

tab_sum1 <- tab %>% 
  group_by("otu" = qaccver, "bac" = first1_name) %>% 
  summarise(n = n())
tab_sum1

df_reads$reads <- df_reads$samples
reads <- df_reads %>% 
  mutate("otu" = paste0(short_name, "_", Feature.ID)) %>% 
  filter(otu %in% tab_sum1$otu) %>% 
  select(otu, reads)

head(reads)
# Add num of reads info
tab_sum1 <- left_join(tab_sum1, reads)

tab_sum1$hexa_name <- gsub("f__Enterobacteriaceae_unknwn_", "", tab_sum1$otu)



gg <- ggplot(tab_sum1, aes(x=hexa_name, y=bac, size=n, color = reads))+
  geom_point()+
  labs(title = "Blast results by sequences of interest",
       subtitle = bac)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
gg
ggsave(filename = paste0("Figures/PNGs/blast.per.seq",bac , ".png"), 
       plot = gg, device = "png", width = 10, height = 8)
ggsave(filename = paste0("Figures/PDFs/blast.per.seq",bac , ".pdf"), 
       plot = gg, device = "pdf", width = 10, height = 8)


}
bac <- "f__Enterobacteriaceae_unknwn"
plot_it(bac)

bac <- "f__Enterobacteriaceae_unclsfd"
plot_it(bac)



