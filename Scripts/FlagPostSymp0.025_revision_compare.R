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
  tab <- read.delim("Results/TSVs/SuppTable1_revision.tsv",header = T)
  fixed_raw <- readLines("Results/MaAsLin2/input/ModelsLists.csv")
}

# fixed effects
{
  fixed_effects <- list()
  splited <- strsplit(fixed_raw, ",")
  for (i in 2:length(splited)) {
    # first argument is the subset name
    fixed_effects[[splited[[i]][1]]] <- splited[[i]][-1]
  }
}



library(ggplot2)
library(tidyr)
library(RColorBrewer)
library(ggpubr)
library(dplyr)

thr <- 0.025
q_val <- 0.2


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
  as.character(unique(symp.tab$Subset))
  symp_models <- c("0-2 months")
  
  symp.tab.filter <- symp.tab %>% 
    filter(abs(coef) > thr) %>%
    filter(Subset %in% symp_models) %>%
    filter(Model.Name %in% c("symptoms lastDiet pro anti (Control as reference)","symptoms lastDiet pro (Control as reference)"))
  head(symp.tab.filter)

  
  ######### Plot COLORS #################################################################
  # Copy paste this section to consistent colors
  source("Scripts/Functions/GMAPPlotsColors.R")
  colors <- GMAPPaperColors()
  # add this line to your ggplot
  # scale_fill_manual(values = colors, breaks = names(colors))+
  #######################################################################################
}
colors <- colors[c(symp.tab.filter$Feature)]
  

gg <- ggplot(symp.tab.filter, aes(x = coef, y = Feature, fill = Feature))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = colors, breaks = names(colors))+
  facet_grid(Test.Variable~Model.Name,scales = "free",space = "free")+
  labs(title = "Control vs Symptoms stage",
       x = "Coefficient",
              subtitle = paste0(unique(symp.tab.filter$Subset), "\n",
                                "Enriched in Control <-------> Enriched in Symptoms"),
       caption = paste0("coef > ", thr, " | qval < ", q_val, "\n",
                        "Variables that taken into account:\n", 
                        paste0(fixed_effects[[unique(symp.tab.filter$Model.Name)[1]]],collapse = ", ")))+
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
  fileName <- paste0("FlagPostSympCompare", thr)
  width <- 8
  height <- 4
  if (1) {
    ggsave(gg,filename = paste0(path, "PDFs/",fileName, ".pdf"), "pdf", useDingbats=FALSE,
           width = width, height = height)
    ggsave(gg,filename = paste0(path, "PNGs/",fileName, ".png"), "png",
           width = width, height = height)
  }
  
}
