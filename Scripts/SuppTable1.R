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
  long_tab$reference <- sapply(1:nrow(long_tab), function(i){
    case_when(stringr::str_detect(long_tab$fixed_effects_group[i], "Symptomatic") && long_tab$metadata[i] == "case_id" ~ "Symptomatic",
              stringr::str_detect(long_tab$fixed_effects_group[i], "Control") && long_tab$metadata[i] == "case_id" ~ "Control",
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
head(long_tab)

names(long_tab)[1:3] <- c("Feature", "Measured Variable", "Test Variable")
names(long_tab)[11:12] <- c("Model Name", "Reference Variable")


str(long_tab)
head(long_tab)

# Check al refernce is okay
long_tab %>% filter('Measured Variable' != "case_id")

write.table(long_tab, "Results/TSVs/SuppTable1.tsv", quote = F, sep = "\t", col.names = NA, row.names = T)
