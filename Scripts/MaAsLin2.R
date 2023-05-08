##########################################################
###################### MaAsLin2 Run ######################
##########################################################

# data
{
  rm(list = ls())
  try(setwd("~/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/Volumes/ehudda/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/vol/sci/bio/data/moran.yassour/lab/Projects/GMAP/Docs/GMAP_Paper/"), silent = T)
  metadata <- read.table("Results/TSVs/metadata.tsv", header=T)
  tab.n <- read.table("Results/TSVs/FeatureTable.tsv",header = T)
  
  ## I want to use these rows instide of the code genarate these files
  samples_raw <- readLines("Results/MaAsLin2/input/SubsetLists.csv")
}

# Install maaslin if needed
if (!"Maaslin2" %in% rownames(installed.packages())){
  if(!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
  BiocManager::install("Maaslin2")
}

# libraries
{
  library(Maaslin2)
  library(dplyr)
  library(clue)
  library(tibble)
}


# fixed effects
{
  fixed_effects     <-       list(    "case lastDiet pro" =                              c("case_id", "visit_age_mo", "mode_of_delivery", "lastDiet",                      "probiotics_firstyr"),
                                      "symptoms lastDiet pro (Control as reference)" =   c("symptoms","visit_age_mo", "mode_of_delivery", "lastDiet",                    "probiotics_firstyr"),
                                      "symptoms lastDiet pro (Symptomatic as reference)" =    c("symptoms","visit_age_mo", "mode_of_delivery", "lastDiet",                    "probiotics_firstyr")
  )
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

# random effects
{
  subset_with_rand_effects <- c("All Samples", "All Samples Partially BF", "All Samples Formula", 
                                "All Samples Exclusively BF", "samples 0-2 Model", "samples 0-4 Model", 
                                "samples 4-6 Model", "samples 6-9 Model")
  random_effects_list <- sapply(names(samples), function(x) ifelse(x %in%  subset_with_rand_effects, "record_id",""))
}

{
  reference_list <- list("case_id" = "case_id;AP Case",  # this is actually dont change for masslin. because this is 2 categories variable, the reference is outomatically by Alephbeit order
                         "symptoms" = "symptoms;Control",
                         "symptoms2" = "symptoms;Symptomatic",
                         "mode_of_delivery" = "mode_of_delivery;C-section",
                         "lastDiet" = "lastDiet;Exclusively BF",
                         "probiotics_firstyr" = "probiotics_firstyr;Pro -")
  
}

transform <- "AST"
path <- "Results/MaAsLin2/output/"
h <- 1
j <- 1
for (h         in 1:length(samples))        {
  chosenIdx <- which(names(tab.n) %in% samples[[h]])
  input_tab <- tab.n[,chosenIdx]
  for (j in 1:length(fixed_effects))  {
    subset_name <- names(fixed_effects)[j]
    random_effects <- random_effects_list[names(samples)[h]]
    
    vector_of_names <- c(names(samples[h]), subset_name)
    output_directory <- paste0(path, paste(vector_of_names, collapse = "/"), "/")
    dir.create(output_directory,recursive = T,showWarnings = F)

    input_metadata  <- metadata %>% 
      select(sampleID, fixed_effects[[j]], record_id) %>%
      filter(sampleID %in% samples[[h]])
    
    # define refenrence variables
    l <- sapply(names(input_metadata), function(x) if(x %in% names(reference_list)) reference_list[[x]])
    if (stringr::str_detect(subset_name,"Symptomatic")) l$symptoms <- reference_list$symptoms2
    i <- sapply(l, Negate(is.null))
    reference <- paste0(l[i], collapse = ",")
    
    if (length(unique(metadata$sampleID)) != nrow(metadata)){
      message("You have not enought unique sample IDs")
      message(table(metadata$sampleID))
    }
    rownames(input_metadata) <- input_metadata$sampleID
    input_metadata <- input_metadata %>% select(-sampleID)

    if (nrow(input_metadata) != nrow(input_metadata %>% na.omit())){
      message("You have some not valid values (with NA)")
    }
    
    meta_name <- paste0(output_directory, "metadata_input", ".tsv")
    feature_table_name <- paste0(output_directory, "feature_table_input", ".tsv")
    readme_file_loc <- paste0(output_directory, "README", ".txt")
    
    write.table(x = input_metadata,file = meta_name,sep = "\t",quote = F)
    write.table(x = input_tab,file = feature_table_name)
    write("####### Model Variables #########", file = readme_file_loc, append = F)
    write(c("fixed effects:", subset_name, fixed_effects[[j]]), file = readme_file_loc, append = T, ncolumns = 100, ",")
    write(c("random effects:", random_effects), file = readme_file_loc, append = T, ncolumns = 100, ",")
    write(c("reference:", reference), file = readme_file_loc, append = T, ncolumns = 100, ",")
    
      fit_data <- Maaslin2::Maaslin2(input_data = input_tab,
                                     input_metadata = input_metadata,
                                     output = output_directory,
                                     plot_heatmap = F,
                                     plot_scatter = F,
                                     reference = reference,
                                     transform = transform,
                                     fixed_effects = fixed_effects[[j]],
                                     random_effects = random_effects
      ) 
  }
  
}
