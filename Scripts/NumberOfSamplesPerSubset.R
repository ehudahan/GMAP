#######################################################
# Number of samples in each Subset #####################
#######################################################

# data
{
  rm(list = ls())
  try(setwd("~/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/Volumes/ehudda/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/vol/sci/bio/data/moran.yassour/lab/Projects/GMAP/Docs/GMAP_Paper/"), silent = T)
  metadata <- read.delim("Results/TSVs/metadata.tsv", header=T)
  tab.n <- read.table("Results/TSVs/FeatureTable.tsv",header = T)
  
  ## I want to use these rows instide of the code genarate these files
  samples_raw <- readLines("Results/MaAsLin2/input/SubsetLists.csv")
}

# samples
{
  # skip 1 row because this description
  samples <- list()
  splited <- strsplit(samples_raw, ",")
  for (i in 2:length(splited)) {
    # first argument is the subset name
    samples[[splited[[i]][1]]] <- splited[[i]][-1]
  }
}

message(paste0(names(samples), " (n=", lengths(samples), ")\n"))


tab <- data.frame(subset = names(samples), total = lengths(samples), row.names = NULL)
head(tab)

tab$AP <- sapply(seq_along(samples), function(i){
  metadata %>% 
    filter(sampleID %in% samples[[i]]) %>% 
             filter(case_id == "AP Case") %>% nrow()
           })
tab$control <- sapply(seq_along(samples), function(i){
  metadata %>% 
    filter(sampleID %in% samples[[i]]) %>% 
    filter(case_id != "AP Case") %>% nrow()
})

for (i in seq_along(samples)) {
  metadata.filtered <- metadata %>% filter(sampleID %in% samples[[i]])
  AP_n <- metadata.filtered %>% filter(case_id == "AP Case")%>% nrow()
  Control_n <- metadata.filtered %>% filter(case_id != "AP Case")%>% nrow()
  total <- metadata.filtered %>% nrow()
  message(names(samples)[i], "FPIAP: n=",AP_n, " Controls: n=",Control_n, "total=", total)
}

if(0){
  write.table(tab, file = "Results/TSVs/NumberOfSamplesPerSubset.tsv")
}
tab
