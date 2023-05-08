##########################################################
###################### Flag Post 0.01 cases ##############
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

tab$name <- paste0(tab$Subset,".",tab$Test.Variable,".",tab$Feature)

tab$Subset <- factor(as.character(tab$Subset),
                     levels=unique(sort(as.character(tab$Subset),decreasing = F)))
tab$name <- factor(as.character(tab$name),
                   levels=unique(sort(as.character(tab$name),decreasing = T)))

cases.tab <- tab[tab$Measured.Variable=="case_id",]
cases_models <- c("samples 0-2 Model","last Pre-symptoms", "first Symptomatic","first Resolved", "last Resolved", "samples 9-12 Model")
thr <- 0.2
cases.tab.filter <- cases.tab %>% 
  filter(abs(coef) > thr) %>% 
  filter(Subset %in% cases_models) %>% 
  filter(Model.Name == "case lastDiet pro")
head(cases.tab.filter)
# cases.tab.filter$coef <- -cases.tab.filter$coef
cases.tab.filter$Subset <- factor(cases.tab.filter$Subset, levels = c("samples 0-2 Model","last Pre-symptoms","first Symptomatic" ,"first Resolved", "last Resolved", "samples 9-12 Model"))

source("Scripts/Functions/GMAPPlotsColors.R")
colors <- colorGMAPDataFunction("higherTaxa")
sort(unique(tab$Feature))
paper_bacs <- c("f__Clostridiaceae_unclsfd",
                #"f__Clostridiaceae_unclsfd",
                "f__Enterobacteriaceae_unclsfd",
                "o__Clostridiales_unknwn", 
                
                "g__Bacteroides",
                "g__Lactobacillus",
                "g__Streptococcus")
# CHECK !!!!!!!!!!
# bacs <- c("g__Lactobacillus",
#           "o__Clostridiales_unknwn", 
#           "f__Peptostreptococcaceae_unclsfd", 
#           "f__Enterobacteriaceae_unclsfd",
#           "f__Enterobacteriaceae_unknwn", 
#           "f__Clostridiaceae_unclsfd",
#           "g__Blautia")

else_bacs <- sort(unique(cases.tab.filter$Feature)[!unique(cases.tab.filter$Feature) %in% paper_bacs])
bacs <- c(paper_bacs, else_bacs)
names(bacs) <- colors[1:length(bacs)]

gg <- ggplot(cases.tab.filter, aes(x = coef, y = Feature, fill = Feature))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = names(bacs), breaks = bacs)+
  facet_grid(Subset~.,scales = "free",space = "free")+
  labs(title = "Control vs Cases",
       subtitle = "Enriched in Control <-------> Enriched in Cases",
    caption = paste0("coef > ", thr, " | qval < 0.25"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_line(size=0.2))

gg

# Save
{
  path <- "Figures/"
  fileName <- paste0("FlagPostCases", thr)
  width <- 8
  height <- 3
  if (1) {
    ggsave(gg,filename = paste0(path, "PDFs/",fileName, ".pdf"), "pdf", useDingbats=FALSE,
           width = width, height = height)
    ggsave(gg,filename = paste0(path, "PNGs/",fileName, ".png"), "png",
           width = width, height = height)
  }
  
}
