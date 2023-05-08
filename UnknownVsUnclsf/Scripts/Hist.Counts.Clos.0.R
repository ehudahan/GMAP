library(tidyverse)
library(dplyr)
if(1){
  rm(list=ls())
  try(setwd("~/GMAP/QIIME2/Data/output/UnknownVsUnclsf/"), silent = T)
  try(setwd("/Volumes/ehudda/GMAP/QIIME2/Data/output/UnknownVsUnclsf/"), silent = T)
  try(setwd("/vol/sci/bio/data/moran.yassour/lab/Projects/GMAP/QIIME2/Data/output/UnknownVsUnclsf/"), silent = T)
  getwd()
  tab <- read.delim("top.clos.0.unkwn.uncls.blast.out", header = F)
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
  
  ggsave(filename = paste0("Figures/PNGs/hist.",file.name , ".png"), 
         plot = gg, device = "png", width = 7, height = 5)
}


# Filtering for only Entero unclsd
bac <- "o__Clostridiales_unknwn"
tab.clos <- tab %>% filter(str_detect(string = qaccver,pattern = bac))
hist_tab(tab.clos, "Bacs.Count.clos.0", paste0(bac," (num of reads > 0)"))

# drop uncultured
tab.clos.NOT.uncultured <- tab %>% 
  filter(str_detect(string = qaccver,pattern = bac)) %>% 
  filter(!str_detect(string = sscinames, pattern = "uncultured"))
hist_tab(tab.clos.NOT.uncultured, "Bacs.Count.clos.0.WO.uncultured", paste0(bac," (num of reads > 0)"))
