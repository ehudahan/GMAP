#####################
# TIME LINE PLOT    #
# Color by Symptoms #
#####################

# This version need to clarify wich sample was choosen for each subset

# libraries
{
    library(ggplot2)
    library(dplyr)
    library(tidyverse)
    library(scales)
    library(glue)
    library(RColorBrewer)
    library(ggtext)
}

# Input Data
# Data
{
    rm(list=ls())
    try(setwd("~/GMAP/Docs/GMAP_Paper/"), silent = T)
    try(setwd("/Volumes/ehudda/GMAP/Docs/GMAP_Paper/"), silent = T)
    try(setwd("/vol/sci/bio/data/moran.yassour/lab/Projects/GMAP/Docs/GMAP_Paper/"), silent = T)
    metadata <- read.delim("Results/TSVs/metadata.tsv", header=T, sep = '\t')
    tab.n <- read.table("Results/TSVs/FeatureTable.tsv",header = T)
    samples_raw <- readLines("Results/CSVs/SubsetLists.csv")
    {
        # skip 1 row because this description
        samples <- list()
        splited <- strsplit(samples_raw, ",")
        for (i in 2:length(splited)) {
            # first argument is the subset name
            samples[[splited[[i]][1]]] <- splited[[i]][-1]
        }
    }
    
}

    
    # Order variables
    metadata$symptoms <- factor(metadata$symptoms,levels = c("Control","Pre-symptoms", "Symptomatic","Resolved"),ordered = T)
    metadata$lastDiet <- factor(metadata$lastDiet, levels = c("Exclusively BF", "Partially BF", "Formula"), ordered = T)
    metadata$case_id <- factor(metadata$case_id, levels = c("No AP", "AP Case"),ordered = T)
    
    # update variables in days to be in months
    DAYS_IN_MONTHS <- 30.4
    metadata$age_diag_ap_months <- metadata$age_diag_ap_day / DAYS_IN_MONTHS
    metadata$age_ap_resolution_months <- metadata$age_ap_resolution_day / DAYS_IN_MONTHS
    
    
    # color legend
    colored_legend <- paste(
        paste0("<span style='color:",  
               c("#757575",brewer.pal(5,"Set1")[c(3,5,1,2)]), 
               "'>", 
               c("-_---symptomatic period-----       ",levels(metadata$symptoms)), 
               "</span>"), 
        collapse = " ")
    
    metadata$Subsets <- sapply(metadata$sampleID, FUN = function(x) case_when(x %in% samples$`last Pre-symptoms` ~ "Last_Pre_Symptomatic",
                                                                              x %in% samples$`first Symptomatic` ~ "First_Symptomatic",
                                                                              x %in% samples$`first Resolved` ~ "First_Resolved",
                                                                              TRUE ~ "Else"))
    
    gg <- ggplot(metadata) +
        geom_segment(aes(x = age_diag_ap_months,
                         xend = age_ap_resolution_months, 
                         y = reorder(record_id, age_diag_ap_months),
                         yend = reorder(record_id, age_diag_ap_months)), alpha = 5, color = "grey", size = 1.5)+
        geom_point(aes(y = reorder(record_id, age_diag_ap_months),
                       x = visit_age_mo,
                       color = symptoms), size = 1, alpha = .5, shape = 3) +
        geom_point(data = metadata %>% filter(Subsets != "Else"),
                   aes(y = reorder(record_id, age_diag_ap_months),
                       x = visit_age_mo,
                       color = symptoms,
                       shape = Subsets), size = 1.5, alpha = 1) +
        scale_shape_manual(values = c(15,17,18))+
        scale_color_manual(values = brewer.pal(5,"Set1")[c(3,5,1,2)]) +
        scale_x_continuous(breaks = c(0,0.5,1,2,4,6,9,12))+
        theme_light() +
        # theme_minimal() +
        labs(y = "Subject",
             x = "Age (Months)",
             title = "Samples Collection",
             caption = colored_legend)+
        theme(panel.grid = element_blank(),
              # panel.grid.major.y = element_blank(),
              panel.grid.major.x = element_line(color = "gray", size = 0.3),
              axis.line.x.bottom = element_line(color = "black"),
              axis.line.y.left = element_blank(),
              # axis.ticks.x.bottom = element_line(colour = "black"),
              axis.ticks.x = element_line(color = "black"),
              axis.ticks.y = element_blank(),
              # axis.text.y = element_text(size = 2),
              axis.text.y = element_blank(),
              # legend.position = "none",
              plot.caption = element_markdown()
        )
    
    gg
    
    # Save
    {
        path <- "Figures/"
        fileName <- "SampleMapSubsets"
        if (1) {
            ggsave(gg,filename = paste0(path, "PDFs/",fileName, ".pdf"), "pdf", useDingbats=FALSE,
                   width = 10, height = 8)
            ggsave(gg,filename = paste0(path, "PNGs/",fileName, ".png"), "png",
                   width = 10, height = 8)
        }
        
    }
    
    