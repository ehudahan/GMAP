library(tidyverse)
library(dplyr)
library(stringr)
library(ggpubr)
if(1){
  rm(list=ls())
  try(setwd("~/GMAP/QIIME2/Data/output/UnknownVsUnclsf/"), silent = T)
  try(setwd("/Volumes/ehudda/GMAP/QIIME2/Data/output/UnknownVsUnclsf/"), silent = T)
  try(setwd("/vol/sci/bio/data/moran.yassour/lab/Projects/GMAP/QIIME2/Data/output/UnknownVsUnclsf/"), silent = T)
  getwd()
  tab <- read.delim("top.unkwn.uncls.blast.out", header = F)
  names(tab) <- c("qaccver", "saccver", "pident", "length", "mismatch", "gapopen", "qstart", "qend", "sstart", "send", "evalue", "bitscore", "qlen", "slen", "sscinames")
}


hist_tab <- function(tab, file.name, subtitle = ""){
  gg_list <- list()
  for (i in 1:ncol(tab)) {
    if(is.numeric(tab[[i]]))
    {
      gg <- qplot(tab[[i]], geom="histogram") + 
        labs(title = names(tab)[i])
      
      gg_list[names(tab)[i]] <- gg
      if(0){
        ggsave(filename = paste0("Figures/PNGs/hist.", as.character(names(tab)[i]), ".png"),
               plot = gg, device = "png")
      }
    }
  }
  
  quantile(tab[,c("slen")], c(0,0.5,0.8,0.9))
  sort(table(tab[,c("length")]))
  sort(table(tab[,c("evalue")]))
  
  tab <- tab %>% 
    mutate(first2_name = stringr::word(sscinames, 1, 2, " ")) %>% 
    mutate(first1_name = stringr::word(sscinames, 1, 1, " "))
  
  
  
  count_names2_df <- data.frame(sort(table(tab$first2_name), decreasing = T))
  count_names1_df <- data.frame(sort(table(tab$first1_name), decreasing = T))
  
  g2 <- ggplot(count_names2_df[1:15,], aes(x=Var1, y=Freq))+
    geom_col() +
    labs(title = "Group and Count by first 2 names",
         subtitle = subtitle)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  g1 <- ggplot(count_names1_df[1:15,], aes(x=Var1, y=Freq))+
    geom_col() +
    labs(title = "Group and Count by first 1 names",
         subtitle = subtitle)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  gg <- ggarrange(g1, g2, ncol = 1)
  gg
  

	   ggsave(filename = paste0("Figures/PNGs/hist.",file.name , ".pdf"), 
         plot = gg, device = "png", width = 7, height = 5)
}


# hist_tab(tab, "Bacs.Count")
# 
# # Filtering by Not Entero
# tab.NOT.Entro <- tab %>% filter(!str_detect(string = qaccver,pattern = "f__Enterobacteriaceae"))
# hist_tab(tab.NOT.Entero, "tab.NOT.Entero")

# Filtering for only Entero unknown
bac <- "f__Enterobacteriaceae_unknwn"
tab.Entero.unknown <- tab %>% filter(stringr::str_detect(string = qaccver,pattern = bac))
# only Entero unknown
hist_tab(tab.Entero.unknown, "Bacs.Count.Entero.Unknown", bac)

# Filtering for only Entero unclsd
bac <- "f__Enterobacteriaceae_unclsfd"
tab.Entero.unclsfd <- tab %>% filter(str_detect(string = qaccver,pattern = bac))
hist_tab(tab.Entero.unclsfd, "Bacs.Count.Entero.unclsfd", bac)


# filtering by blast parameters
ftab <- tab %>% 
  filter(pident > 99) %>% 
  filter(length > 130) %>% 
  filter(mismatch < 1) %>% 
  filter(gapopen < 1) %>% 
  filter(qstart < 10) %>% 
  filter(qend > 100) %>% 
  filter(qlen > 10) %>% 
  filter(slen > 10)

hist_tab(ftab, "Bacs.Count.filtered", "All but filtered")
