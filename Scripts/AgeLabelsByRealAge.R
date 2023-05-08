######################################################################################################################
##################### Age districution over Subset One Sample per kid per time ##########
######################################################################################################################

# data
{
  rm(list=ls())
  try(setwd("~/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/Volumes/ehudda/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/vol/sci/bio/data/moran.yassour/lab/Projects/GMAP/Docs/GMAP_Paper/"), silent = T)
  metadata <- read.table("Results/TSVs/metadata.tsv", header=T)
  samples_list <- read.csv("Results/CSVs/OneSamplePerKikPerTimePoint.csv", header = T)$x
}

{
  TITLE = "Age Labels By Real Age"
  SUBTITLE = "One Sample Per Kid Per Time Point"
}

# libraries
{ 
  library(dplyr) # for %>% 
  library(ggplot2)
  library(RColorBrewer)
}

# filtering and prepare data
{
  data <- metadata %>% filter(sampleID %in% samples_list)
  data$sampleTimeLabels <-
    factor(
      data$numericSampleTimeWithSick,
      labels = c(
        "initial",
        "two week",
        "one month",
        "two month",
        "four month",
        "six month",
        "nine month",
        "one year"
      )
    )
  
}

colors <- brewer.pal(9, "Set1")



g <- ggplot(data, color="gray", aes(x = visit_age_mo , y = sampleTimeLabels, color = sickVisits)) + 
  geom_point(alpha = .5) +
  scale_x_continuous(breaks = unique(data$numericSampleTimeWithSick))+
  labs(title = TITLE,
       subtitle = SUBTITLE)+
  theme_bw()
g



gg <- g
# Save
{
  path <- "Figures/"
  fileName <- "AgeLabelsByRealAge"
  if (1) {
    ggsave(gg,filename = paste0(path, "PDFs/",fileName, ".pdf"), "pdf", useDingbats=FALSE,
           width = 6, height = 4)
    ggsave(gg,filename = paste0(path, "PNGs/",fileName, ".png"), "png",
           width = 6, height = 4)
  }
  
}

