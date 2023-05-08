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
  tab <- read.delim("Results/TSVs/SuppTable1_revision.tsv",header = T)
}

library(ggplot2)
library(tidyr)
library(RColorBrewer)
library(ggpubr)

thr <- 0.025
q_val <- 0.2

tab$name <- paste0(tab$Subset,".",tab$Test.Variable,".",tab$Feature)

tab$Subset <- factor(as.character(tab$Subset),
                     levels=unique(sort(as.character(tab$Subset),decreasing = F)))
tab$name <- factor(as.character(tab$name),
                   levels=unique(sort(as.character(tab$name),decreasing = T)))

cases.tab <- tab[tab$Measured.Variable=="case_id",]
cases_models <- c("samples 0-2 Model","last Pre-symptoms", "first Symptomatic","first Resolved","samples 9-12 Model")
cases.tab.filter <- cases.tab %>% 
  filter(abs(coef) > thr) %>% 
  filter(Subset %in% cases_models) %>% 
  filter(Model.Name == "case lastDiet pro")
head(cases.tab.filter)
# cases.tab.filter$coef <- -cases.tab.filter$coef
cases.tab.filter$Subset <- factor(cases.tab.filter$Subset, levels = c("samples 0-2 Model","last Pre-symptoms","first Symptomatic" ,"first Resolved","samples 9-12 Model"))



######### Plot COLORS #################################################################
# Copy paste this section to consistent colors
source("Scripts/Functions/GMAPPlotsColors.R")
colors <- GMAPPaperColors()
# add this line to your ggplot
# scale_fill_manual(values = colors, breaks = names(colors))+
#######################################################################################
colors <- colors[c(cases.tab.filter$Feature)]


gg <- ggplot(cases.tab.filter, aes(x = coef, y = Feature, fill = Feature))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = colors, breaks = names(colors))+
  facet_grid(Subset~.,scales = "free",space = "free")+
  labs(title = "Control vs Cases",
       x = "Coefficient",
       subtitle = "Enriched in Control <-------> Enriched in Cases",
    caption = paste0("coef > ", thr, " | qval < ", q_val))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.minor=element_blank(),
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text.y = element_text(angle = 0),
        panel.grid.major=element_line(size=0.2))

gg

# Save
{
  path <- "Figures/revision/"
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
