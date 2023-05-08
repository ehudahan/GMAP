##########################################################
###################### All Significat Results ############
##########################################################



# data
{
  rm(list = ls())
  try(setwd("~/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/Volumes/ehudda/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/vol/sci/bio/data/moran.yassour/lab/Projects/GMAP/Docs/GMAP_Paper/"), silent = T)
  getwd()
}

library(dplyr)

QVAL_THR <- 0.2
subsets_in_paper <- c("All Samples", "first Resolved", "first Symptomatic", "last Pre-symptoms", "samples 0-2 Model", "samples 9-12 Model")

# path - where all folder with results (each sub-folder considered as new feature column in final table)
path <- "Results/MaAsLin2/output"
files <- list.files(path,pattern="significant_results.tsv", full.name=TRUE, recursive=T)
files

# each subfolder name match to specific subject
tableTitles <- c("Subset","fixed_effects_group")

for (file_name in files) {
  # file_name <- files[1]
  # read file
  tab <- read.delim(file = file_name)
  if (nrow(tab) != 0){
    #delete head of path to enable take the relevant names for titiles to table
    shortFileName <- gsub(path,"", file_name)
    
    # separate folders names
    separatedFoldersNames <- strsplit(shortFileName,split = "/")[[1]][1:length(tableTitles)+1]
    
    # create 4 columns in tha table contain the value as it write in the folders names
    for (i in 1:length(separatedFoldersNames)) {tab[[tableTitles[i]]] <- as.character(separatedFoldersNames[i])}
    
    # when long_tab still not exist I creat it (In the first iteration )
    if (!exists("long_tab")) {long_tab <- tab
    } else {
      # bind the new tab to the long tab
      long_tab <- bind_rows(long_tab, tab)
    }
    
  }
}

# Edit
head(long_tab)

  reference_list <- list("case_id" = "case_id;AP Case",
                         "symptoms" = "symptoms;Control",
                         "symptoms2" = "symptoms;Symptomatic",
                         "mode_of_delivery" = "mode_of_delivery;C-section",
                         "lastDiet" = "lastDiet;Exclusively BF",
                         "probiotics_firstyr" = "probiotics_firstyr;Pro -")
  
  
  long_tab$reference <- sapply(1:nrow(long_tab), function(i){
    case_when(long_tab$metadata[i] == "lastDiet" ~ "Exclusively BF",
                    long_tab$metadata[i] == "symptoms" ~ "temp",
                    long_tab$metadata[i] == "case_id" ~ "AP Case",
                    long_tab$metadata[i] == "mode_of_delivery" ~ "C-section",
                    long_tab$metadata[i] == "probiotics_firstyr" ~ "Pro -",
                    TRUE ~ long_tab$metadata[i])})
  # Old code - It is not clear to me why the medata variable is 'case_id' instead 'symptoms'. 
  # long_tab$reference <- sapply(1:nrow(long_tab), function(i){
  #   case_when(stringr::str_detect(long_tab$fixed_effects_group[i], "Symptomatic") & long_tab$metadata[i] == "case_id" ~ "Symptomatic",
  #             stringr::str_detect(long_tab$fixed_effects_group[i], "Control") & long_tab$metadata[i] == "case_id" ~ "Control",
  #             TRUE ~ long_tab$reference[i])
  # })
  long_tab$reference <- sapply(1:nrow(long_tab), function(i){
    case_when(stringr::str_detect(long_tab$fixed_effects_group[i], "Symptomatic") & long_tab$metadata[i] == "symptoms" ~ "Symptomatic",
              stringr::str_detect(long_tab$fixed_effects_group[i], "Control") & long_tab$metadata[i] == "symptoms" ~ "Control",
              TRUE ~ long_tab$reference[i])
  })
  
  
  # replace directions of coef in AP cases vs No AP
  i <- 1
  for (i in 1:nrow(long_tab)) {
    if(long_tab$metadata[i] == "case_id"){
      long_tab$value[i] <- "AP Case"
      long_tab$reference[i] <- "No AP"
      long_tab$coef[i] <- -long_tab$coef[i]
  }
  }
  
  bacs_in_paper <- c()
  # Add potencial to be in paper (bacs in paper AND Q value thr AND COEF thr)
head(long_tab)
long_tab %>% filter(reference == "temp")

long_tab <- long_tab %>% filter(Subset %in% subsets_in_paper)
long_tab$Subset[which(long_tab$Subset == "samples 9-12 Model")] <- "6+ months"
long_tab$Subset[which(long_tab$Subset == "samples 0-2 Model")] <- "0-2 months"
unique(long_tab$Subset)

unique(long_tab$metadata)
unique(long_tab$value)

long_tab$metadata[which(long_tab$metadata == "probiotics_firstyr")] <- "probiotics first year"
long_tab$metadata[which(long_tab$metadata == "lastDiet")] <- "diet"
long_tab$metadata[which(long_tab$metadata == "mode_of_delivery")] <- "delivery mode"

long_tab$metadata[which(long_tab$metadata == "visit_age_mo")] <- "age at visit (months)"

long_tab$reference[which(long_tab$reference == "visit_age_mo")] <- "age at visit (months)"
long_tab$value[which(long_tab$value == "visit_age_mo")] <- "age at visit (months)"


long_tab$value[which(long_tab$value == "Pre-")] <- "Pre-symptomaic"


res_tab <- long_tab
names(res_tab)[1:3] <- c("Feature", "Measured Variable", "Test Variable")
names(res_tab)[11:12] <- c("Model Name", "Reference Variable")


str(res_tab)
head(res_tab)

# Check al refernce is okay
res_tab %>% filter('Measured Variable' != "case_id")

res_tab %>% filter(coef > 0.1)

# filter by q value < 0.2 as asked in revision
res_tab <- res_tab %>% filter(qval < QVAL_THR)

res_tab <- res_tab %>% select(Subset, Feature, `Measured Variable`, `Test Variable`, `Reference Variable`, pval, qval, coef, stderr, N, N.not.0, `Model Name`)


if (1){
  write.table(res_tab, "Results/TSVs/SuppTable1_revision.tsv", quote = F, sep = "\t", col.names = NA, row.names = T)  
}

