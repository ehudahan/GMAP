###########################################################
##################### Density count of samples ##########
###########################################################

# Input Data
{
  rm(list=ls())
  try(setwd("~/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/Volumes/ehudda/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/vol/sci/bio/data/moran.yassour/lab/Projects/GMAP/Docs/GMAP_Paper/"), silent = T)
  metadata <- read.table("Results/TSVs/metadata.tsv",as.is=T, header=T)
}

# libraries
{ 
  library(dplyr) # for %>% 
  library(ggplot2)
  library(ggpubr)
  library(RColorBrewer)
  library(ggtext)
}

# arrange data
{
  metadata$symptoms <- factor(metadata$symptoms, levels = c("Control","Pre-symptoms", "Symptomatic","Resolved"))
}


# color legend
{
  colored_legend <- paste(
    paste0("<span style='color:",  
           c(brewer.pal(5,"Set1")[c(3,5,1,2)]), 
           "'>", 
           c(levels(metadata$symptoms)), 
           "</span>"), 
    collapse = " ")
  
}


g <- ggplot(metadata, aes(x = visit_age_mo, fill = symptoms)) +
  # geom_histogram(aes(y=..ndensity..)) + 
  # geom_density(aes(colour = id, y = ..scaled..)) +
  geom_histogram(alpha = 1, bins = 50) +
  scale_fill_manual(values = brewer.pal(5, "Set1")[c(3,5,1,2)]) +
  scale_color_manual(values = brewer.pal(5, "Set1")[c(3,5,1,2)]) +
  scale_x_continuous(breaks = c(0,0.5,1,2,4,6,9,12))+
  labs(#title = "Number of samples by age",
       subtitle = "All Samples",
       caption = colored_legend,
       x = "Age (months)")+
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.caption = element_markdown())
g





gg <- g
# Save
{
  path <- "Figures/"
  fileName <- "HistogramSamplesCounts"
  if (1) {
    ggsave(gg,filename = paste0(path, "PDFs/",fileName, ".pdf"), "pdf", useDingbats=FALSE,
           width = 10, height = 5)
    ggsave(gg,filename = paste0(path, "PNGs/",fileName, ".png"), "png",
           width = 10, height = 5)
  }
  
}
