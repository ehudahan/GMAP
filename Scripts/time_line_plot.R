#####################
# TIME LINE PLOT V5 #
# Color by Symptoms #
#####################

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
{
    rm(list=ls())
    try(setwd("~/GMAP/Docs/GMAP_Paper/"), silent = T)
    try(setwd("/Volumes/ehudda/GMAP/Docs/GMAP_Paper/"), silent = T)
    try(setwd("/vol/sci/bio/data/moran.yassour/lab/Projects/GMAP/Docs/GMAP_Paper/"), silent = T)
    metadata <- read.table("Results/TSVs/metadata.tsv",as.is=T, header=T)
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

gg <- ggplot(metadata) +
    geom_segment(aes(x = age_diag_ap_months,
                     xend = age_ap_resolution_months, 
                     y = reorder(record_id, age_diag_ap_months),
                     yend = reorder(record_id, age_diag_ap_months)), alpha = 5, color = "grey", size = 1.5)+
    geom_point(aes(y = reorder(record_id, age_diag_ap_months),
                   x = visit_age_mo,
                   color = symptoms), size = 1.5) +
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
          legend.position = "none",
          plot.caption = element_markdown()
    )

gg

if (1) {
    ggsave(plot = gg, filename = "Figures/PDFs/time_line_plot.pdf",device = "pdf",width = 8,height = 7, useDingbats=FALSE,)
    ggsave(plot = gg, filename = "Figures/PNGs/time_line_plot.png",device = "png",width = 8,height = 7)
}

