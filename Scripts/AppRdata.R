#### Create Rdata for GMAP App #####
# Ehud Dahan

if (1) {
  rm(list = ls())
  try(setwd("~/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/Volumes/ehudda/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/vol/sci/bio/data/moran.yassour/lab/Projects/GMAP/Docs/GMAP_Paper/"), silent = T)
  getwd()
  metadata <- read.delim("Results/TSVs/metadata.tsv", header=T, sep = '\t')
  tab.n <- read.table("Results/TSVs/FeatureTable.tsv",header = T)
  maaslin_results <- read.delim("Results/TSVs/SuppTable1.tsv", header = T)
  samples_raw <- base::readLines("Results/CSVs/SubsetLists.csv")
  new_kids_nums <- read.table("Results/TSVs/new_kids_nums.tsv")
  source("Scripts/Functions/Arc-Sine_Square_Root_Transformation.R")
  # source("Scripts/Functions/GMAPPlotsColors.R")
}


# Samples list
{
  # skip 1 row because this description
  samples <- list()
  splited <- strsplit(samples_raw, ",")
  for (i in 2:length(splited)) {
    # first argument is the subset name
    samples[[splited[[i]][1]]] <- splited[[i]][-1]
  }
}


tab.ast <- AST(tab.n)


metadata <-
  metadata[, c(
    "sampleID",
    "record_id",
    "visit_age_mo",
    "numericSampleTimeWithSick",
    "case_id",
    "symptoms",
    "lastDiet",
    "mode_of_delivery",
    "probiotics_firstyr"
  )]

metadata$symptoms <- factor(metadata$symptoms,levels = c("Control","Pre-symptoms", "Symptomatic","Resolved"))
metadata$lastDiet <- factor(metadata$lastDiet, levels = c("Exclusively BF","Partially BF", "Formula"))
metadata$probiotics_firstyr <- factor(metadata$probiotics_firstyr, labels = c("Pro -", "Pro +"))
metadata$numericSampleTimeWithSick <- factor(metadata$numericSampleTimeWithSick, levels = sort(unique(metadata$numericSampleTimeWithSick)))

metadata$record_id <- sapply(metadata$record_id, function(x) new_kids_nums$new[which(new_kids_nums$orig == x)])
# new numbers for kids


DATA_LAST_UPDATE <- file.info("Scripts/AppRdata.R")$mtime



colorGMAPDataFunction <- function(varTitle) {
  colors <- RColorBrewer::brewer.pal(9, "Set1")
  
  colors_groups <- list("case_id" = c("AP Case" = colors[1],
                                      "No AP" = colors[2]),
                        "probiotics_firstyr" = c("Pro +" = colors[1],
                                      "Pro -" = colors[2]),
                        "symptoms" = c("Control" = "#7c817e",
                                       "Pre-symptoms" = "#fbd68e",
                                       "Resolved" = "#e36a4c",
                                       "Symptomatic" = "#ed985e"),
                        "lastDiet" = c("Formula" = colors[7],
                                       "Partially BF" = colors[8],
                                       "Exclusively BF" = colors[9])
                        )
  if (varTitle %in% names(colors_groups)){
    return(colors_groups[varTitle][[1]])
  } else{
    return(colors)
  }
}


save(metadata, tab.ast, maaslin_results, samples, DATA_LAST_UPDATE, colorGMAPDataFunction , file = "Data/App.Rdata")

