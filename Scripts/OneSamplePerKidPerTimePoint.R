## list of ONE sample per kid for each periodic time point (0, 0.5, 1, 2, 4, 6, 9, 12)
library(tidyverse)
# Data
if (1) {
    rm(list = ls())
    try(setwd("~/GMAP/Docs/GMAP_Paper/"), silent = T)
    try(setwd("/Volumes/ehudda/GMAP/Docs/GMAP_Paper/"), silent = T)
    try(setwd("/vol/sci/bio/data/moran.yassour/lab/Projects/GMAP/Docs/GMAP_Paper/"), silent = T)
    getwd()
    metadata <- read.delim("Results/TSVs/metadata.tsv", header=T, sep = '\t')
    sort(unique(metadata$sample))
    time_points <- c(0, 0.5, 1, 2, 4, 6, 9, 12)
    kids_list <- unique(metadata$record_id)
}

res <- c()
for (time in time_points) {
    for (kid in kids_list) {
        dat_sub <- metadata %>% 
            filter(numericSampleTimeWithSick == time) %>% 
            filter(record_id == kid)
        dist <- min(abs(dat_sub$visit_age_mo - time))
        if(dist < 2) {res <- c(res, dat_sub$sampleID[which.min(abs(dat_sub$visit_age_mo - time))])}
    }
}
head(res)

# Sanity check
head(metadata)
#this is the check you were missing:
metadata %>% filter(sampleID %in% res ) %>% group_by(numericSampleTimeWithSick,family_id) %>% count() %>% filter(n>1) %>% nrow
metadata %>% filter(sampleID %in% res) %>% filter(numericSampleTimeWithSick == 0) %>% nrow()

d <- metadata %>%
    group_by(numericSampleTimeWithSick) %>%
    summarise(n = n(), 
              uniqID = length(unique(sampleID)), 
              cases = sum(case_id == "AP Case"),
              control = sum(case_id == "No AP"))
d
write.csv(res, file = "Results/CSVs/OneSamplePerKikPerTimePoint.csv")
head(metadata)
ggplot(metadata %>% filter(sampleID %in% res),aes(x=visit_age_mo,y=numericSampleTimeWithSick,color=sickVisits))+
    geom_point(alpha=.25)
    # facet_wrap(~sickVisits,nrow = 2)


metadata %>% filter(sampleID %in% res & numericSampleTimeWithSick==0 & visit_age_mo>2)

