##########################################################
###################### Flag Post 0.05 Symp ##############
##########################################################

# data
{
  rm(list = ls())
  try(setwd("~/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/Volumes/ehudda/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/vol/sci/bio/data/moran.yassour/lab/Projects/GMAP/Docs/GMAP_Paper/"), silent = T)
  getwd()
  tab <- read.delim("Results/TSVs/SuppTable1.tsv",header = T)
}

library(ggplot2)
library(tidyr)
library(RColorBrewer)
library(ggpubr)


# edit
{
  tab$name <- paste0(tab$Subset,".",tab$Test.Variable,".",tab$Feature)
  
  tab$Subset <- factor(as.character(tab$Subset),
                       levels=unique(sort(as.character(tab$Subset),decreasing = F)))
  tab$name <- factor(as.character(tab$name),
                     levels=unique(sort(as.character(tab$name),decreasing = T)))
}

# filtering
{
  symp.tab <- tab[tab$Measured.Variable=="symptoms",]
  unique(symp.tab$Model.Name)
  symp_models <- c("samples 0-2 Model", "last Resolved", "samples 9-12 Model")
  thr <- 0.01
  symp.tab.filter <- symp.tab %>% 
    filter(abs(coef) > thr) %>% 
    filter(Subset %in% symp_models) %>% 
    filter(Model.Name == "symptoms lastDiet pro (Control as reference)")
  head(symp.tab.filter)
  # symp.tab.filter$coef <- -symp.tab.filter$coef
  
  # cases.tab.filter$Subset <- factor(cases.tab.filter$Subset, levels = c("samples 0-2 Model","last Pre-symptoms","first Symptomatic" ,"first Resolved"))
  source("Scripts/Functions/GMAPPlotsColors.R")
  colors <- colorGMAPDataFunction("higherTaxa")
  
  # colors <- c(brewer.pal(9, "Set1"))
  sort(unique(tab$Feature))
  paper_bacs <- c("f__Clostridiaceae_unclsfd",
                  "f__Enterobacteriaceae_unclsfd",
                  "o__Clostridiales_unknwn", 
                  "g__Bacteroides",
                  "g__Lactobacillus",
                  "g__Streptococcus")
  else_bacs <- sort(unique(symp.tab.filter$Feature)[!unique(symp.tab.filter$Feature) %in% paper_bacs])
  bacs <- c(paper_bacs, else_bacs)
  names(bacs) <- colors[1:length(bacs)]
  
}



gg <- ggplot(symp.tab.filter, aes(x = coef, y = Feature, fill = Feature))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = names(bacs), breaks = bacs)+
  facet_grid(Subset+Test.Variable~.,scales = "free",space = "free")+
  labs(title = "Control vs Symptoms stage",
              subtitle = paste0(paste0(unique(symp.tab.filter$Subset),collapse = ", "), "\n",
                                "Enriched in Control <-------> Enriched in Symptoms"),
       caption = paste0("coef > ", thr, " | qval < 0.25"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_line(size=0.2))

gg
# Save
{
  path <- "Figures/"
  fileName <- paste0("FlagPostSymp", thr)
  width <- 8
  height <- 4
  if (1) {
    ggsave(gg,filename = paste0(path, "PDFs/",fileName, ".pdf"), "pdf", useDingbats=FALSE,
           width = width, height = height)
    ggsave(gg,filename = paste0(path, "PNGs/",fileName, ".png"), "png",
           width = width, height = height)
  }
  
}
