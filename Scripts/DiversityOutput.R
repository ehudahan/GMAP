#### Supp Table 2 - Alpha and Beta diversity results ###

#### Data
if(1){
    rm(list=ls())
    try(setwd("~/GMAP/Docs/GMAP_Paper/"), silent = T)
    try(setwd("/Volumes/ehudda/GMAP/Docs/GMAP_Paper/"), silent = T)
    try(setwd("/vol/sci/bio/data/moran.yassour/lab/Projects/GMAP/Docs/GMAP_Paper/"), silent = T)
    metadata <- read.delim("Results/TSVs/metadata.tsv", header=T, sep = '\t')
    tab.n <- read.table("Results/TSVs/FeatureTable.tsv",header = T)
    dis.mat.bray <- read.delim("../../Data/distance-matrix-braycurtis.tsv",sep = "\t", as.is = T, header = T, row.names = 1)
    dis.mat.jacc <- read.delim("../../Data/distance-matrix-jaccard.tsv",sep = " ", as.is = T, header = T, row.names = 1)
}

{
    library(dplyr) # for %>% 
    library(fossil)
    library(vegan) # for diversity()
}



metadata$shannon <- sapply(metadata$sampleID, function(sampleID) {
    if (sampleID %in% names(tab.n)){
        return(diversity(x = tab.n[,sampleID], index = "shannon", base = 2))
    } 
    else
    {
        print(paste0(sampleID, " not found in names of tab.n"))
        return(NA)
    }
})
metadata$chao1 <- sapply(metadata$sampleID, function(sampleID) {
    if (sampleID %in% names(tab.n)){
        return(chao1(tab.n[,sampleID]))
    } 
    else
    {
        print(paste0(sampleID, " not found in names of tab.n"))
        return(NA)
    }
})
metadata$consequtive <- sapply(1:nrow(metadata), function(i){
    # consequtive sample is sample that come from same kid with minimal older age distance
    kid_age <- metadata$visit_age_mo[i]
    kid_id <- metadata$record_id[i]
    kid_indexes <- which(metadata$record_id == kid_id)
    kid_indexes_drop_i <- kid_indexes[kid_indexes != i]
    kid_indexes_oldest <- kid_indexes_drop_i[which(metadata$visit_age_mo[kid_indexes_drop_i] > kid_age)]
    if (length(kid_indexes_oldest) == 0) {return(NA)}
    j <- kid_indexes_oldest[which.min(metadata$visit_age_mo[kid_indexes_oldest] - kid_age)]
    return(metadata$sampleID[j])
})
metadata$bray <- unlist(lapply(1:nrow(metadata), function(i){
    if(is.na(metadata$consequtive[i])){
        return(NA)
    }
    res <- dis.mat.bray[metadata$sampleID[i], metadata$consequtive[i]]
    return(ifelse(is.null(res), NA, res))
    
}))
metadata$jacc <- unlist(lapply(1:nrow(metadata), function(i){
    if(is.na(metadata$consequtive[i])){
        return(NA)
    }
    res <- dis.mat.jacc[metadata$sampleID[i], metadata$consequtive[i]]
    return(ifelse(is.null(res), NA, res))
    
}))
outputTable <- metadata %>% select(sampleID, shannon, chao1, consequtive, bray, jacc)
write.table(x = outputTable, file = "Results/TSVs/DiversityOutput.tsv")
