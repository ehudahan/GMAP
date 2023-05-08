##############################################
################# Subsets lists ##############
##############################################

# This script creat list of samples for each subset

# data
{
  rm(list = ls())
  try(setwd("~/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/Volumes/ehudda/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/vol/sci/bio/data/moran.yassour/lab/Projects/GMAP/Docs/GMAP_Paper/"), silent = T)
  
  metadata <- read.table("Results/TSVs/metadata.tsv", header=T)
  OneSamplePerKidPerTime <- read.csv("Results/CSVs/OneSamplePerKikPerTimePoint.csv", header = T)$x
}

# Subsets
{
  samples <- list()
  AllSubsets <- "All Samples"
  period_subsets <- list( c(0,2), c(0,4),c(4,6),c(6,9))
  oneModels <- list(
    c("first","Pre-symptoms"),
    c("last","Pre-symptoms"),
    c("first","Symptomatic"),
    c("last","Symptomatic"),
    c("first","Resolved"),
    c("last","Resolved")
  )
}

########################## subsetting with all kid subsets ###################
samples[[AllSubsets]] <- metadata$sampleID

for (diet in unique(metadata$lastDiet)){
  name <- paste0(AllSubsets, " ", diet)
  samples[[name]] <- metadata$sampleID[metadata$lastDiet == diet]
}


########################## subsetting for specific periods #####################
# define the function
subsetSamplesToSpecificPeriodFunction <- function(metadata, start_period, end_period){
  # start_period <- 0
  # end_period <- 2
  title              <- paste0(start_period, "-",end_period ,"mo_visits")
  
  allCasesIDx <- which(metadata$case_id=="AP Case")
  allControlIDx <- which(metadata$case_id=="No AP")
  
  # Sanity Checks ####
  intersect(allCasesIDx,allControlIDx) # need to be zero
  length(union(allCasesIDx,allControlIDx)) 
  
  # choose only from samples in the chosen period
  allInPeriodSamplesIDx <- which(start_period <= metadata$numericSampleTimeWithSick & metadata$numericSampleTimeWithSick <= end_period)
  
  # Sanity Checks ####
  table(metadata[allInPeriodSamplesIDx,"numericSampleTimeWithSick"])
  sum(table(metadata[allInPeriodSamplesIDx,"numericSampleTimeWithSick"]))
  
  casesIDx <- intersect(allCasesIDx, allInPeriodSamplesIDx)
  controlIDx <- intersect(allControlIDx, allInPeriodSamplesIDx)
  
  # Sanity Checks ####
  table(metadata[casesIDx,"numericSampleTimeWithSick"])
  table(metadata[casesIDx,"record_id"])
  table(metadata[controlIDx,"numericSampleTimeWithSick"])
  table(metadata[controlIDx,"record_id"])
  table(metadata[c(casesIDx,controlIDx),c("case_id","numericSampleTimeWithSick")])
  rowSums(table(metadata[c(casesIDx,controlIDx),c("case_id","numericSampleTimeWithSick")]))
  length(c(casesIDx,controlIDx))
  #### 
  samples <- metadata$sampleID[c(casesIDx,controlIDx)]
  length(samples)
  return(samples)
  # write(samples,file = paste0("~/Google Drive/YassourLabProjects/GMAP/Data/samples_lists/",title,".txt"))
}

# run function for each model
period_subsets <- list( c(0,2), c(0,4),c(4,6),c(6,9))
for (times in period_subsets) {
  name <- paste0("samples ",times[[1]],"-",times[[2]]," Model")
  samples[[name]] <- subsetSamplesToSpecificPeriodFunction(metadata,
                                                           start_period = times[[1]],
                                                           end_period = times[[2]])
}



########################## subsetting for one oer kid sbsets ###################
# functions
{
  ###########################
  # The Hungarian Algorithm #
  ###########################
  #################################################
  # Here I want to use the hungarian algorithm    #
  # to find perfect match between cases to        #
  # samples.                                      #
  #                                               #   
  # Assumptions:                                  #
  # 1. I get a group of n *samples* of cases      #
  # kids with the visit age                       #    
  # 2. I get a group of m => n controls *kids*    #
  # each kid have atlist 1 sample and for each    #
  # sample I have also the visit age of this      #
  # sample.                                       #
  # 3. Alls visit age in months                   #
  #                                               #
  # Target                                        #
  # Find a match between cases and control.       #
  #                                               #
  # restrictions:                                 #
  # 1. each control chose only once               #  
  # 2. sum of distance age between matches        #
  # are minimum. (mean that you cant find a       #
  # match have less distance)                     #
  #                                               #
  # Algorithm                                     #
  # 1. create a distance matrix.                  #
  # row are cases                                 #
  # columns are control                           #
  # define f(i,j) as minimum of                   #
  # (visit age of i)-(avalible visits age of j)   #
  #                                               #
  # min{age(i)-k_visit(j)} from all k             #
  #                                               #
  # - now each (i,j) contain the cost of match   #
  # between case i and control j.                 #
  #                                               #
  # 2. Using hungarian algorithm choose           #
  # 'chad chad erkit' function that optimze       #
  # the total cost.                               #
  # ###############################################
  library(clue) # for solve_LSAP
  library(dplyr)
  # Run These code if you want run function from here ####
  if (0) {
    rm(list = ls())
    metadata <- metadata <- read.table("Results/TSVs/metadata.tsv", header=T)
    firstOrLast <- "first"
    symptomsStage <- "Symptomatic"
    metadataFiltered <- metadata %>% filter(lastDiet == "Exclusively BF")
    name <- "Hungarian_match_first_symp_Exclusively_BF"
    seed <- NA
  }
  HungarianAlgoPerfectMatch <- function(metadataFiltered, firstOrLast ,symptomsStage, name, seed = NA){
    
    
    #' @param metadataFiltered - data frame with only relevant samples, assume have rows: "record_id","sampleID","visit_age_mo","symptoms"
    #' @param firstOrLast - string literal "first" or "last" for first\last sample in the symptomatic stage
    #' @param symptomsStage - "Resolved", "Symptomatic" or "Pre-symptoms"
    #' @param name - specify the name of the sunset
    #' @param seed - set the random number
    #' step 1: creat a distance matrix
    #' step 2: using Hungarian algo from solve_LSAP
    
    
    
    ##### find the all relevant cases ########
    
    # each record ID as different group  ####
    recordIDgroupsIdx <- group_rows(group_by(.data = metadataFiltered,  record_id))
    
    # symptoms stage indexes
    symptomsStageIdxInMetadata <- which(metadataFiltered$symptoms==symptomsStage)
    symptomsStageIdx <- lapply(recordIDgroupsIdx , function(x) x[which(x %in% symptomsStageIdxInMetadata)])  
    
    if (firstOrLast == "first") {
      choosenSymptomsStageIdx <- unlist(lapply(symptomsStageIdx, 
                                               function(x) x[which.min(metadataFiltered[unlist(x),"visit_age_mo"])]))
    } else if (firstOrLast == "last") {
      choosenSymptomsStageIdx <- unlist(lapply(symptomsStageIdx, 
                                               function(x) x[which.max(metadataFiltered[unlist(x),"visit_age_mo"])]))
    } else { stop("error. please use exactly 'first' or 'last'")}
    
    # seed <- 1
    if(!is.na(seed)){
      seed <- seed
      choosenSymptomsStageIdx <- sample(choosenSymptomsStageIdx)
    }
    # print(choosenSymptomsStageIdx)
    
    caseKids <- metadataFiltered$sampleID[choosenSymptomsStageIdx]
    controlKids <- unique(metadataFiltered$record_id[which(metadataFiltered$symptoms == "Control")])
    if(!is.na(seed)){
      set.seed(seed)
      controlKids <- sample(controlKids)
    }
    
    
    ### Matrix (case samples X control recID) ####
    n <- length(caseKids)
    m <- length(controlKids)
    mat <- matrix(data = NA, nrow = n, ncol = m)
    
    rownames(mat) <- caseKids
    colnames(mat) <- controlKids
    
    head(mat)
    
    matSamplesIDs <- as.data.frame(mat)
    head(matSamplesIDs)
    
    
    i <- 1
    j <- 1
    for (i in 1:n) {
      case <- rownames(mat)[i]
      case_age <- metadataFiltered$visit_age_mo[metadataFiltered$sampleID == case]
      
      for (j in 1:m){
        control <- colnames(mat)[j]
        controlIdx <- which(metadataFiltered$record_id == control)
        control_ages_vector <- metadataFiltered$visit_age_mo[controlIdx]
        
        chosenControlIdx <- which.min(abs(case_age - control_ages_vector))
        control_age <- control_ages_vector[chosenControlIdx]
        cost <- abs(case_age - control_age)
        
        sampleID <- metadataFiltered$sampleID[controlIdx][chosenControlIdx]
        
        mat[i, j] <- cost
        matSamplesIDs[i, j] <- sampleID
      }
    } 
    
    dim(mat)
    # mat[1:6,1:6]
    # matSamplesIDs[1:6,1:6]
    
    # solve_LSAP doesnt look at the col and row names.
    opt <- solve_LSAP(mat,maximum = F)
    costs <- mat[cbind(seq_along(opt), opt)]
    controls <- matSamplesIDs[cbind(seq_along(opt), opt)]
    
    
    matchedTable <- data.frame("caseSampleID" = rownames(mat),
                               "controlSampleID" = controls,
                               "recordIDCase" = NA,
                               "caseAge" = NA,
                               "recordIDControl" = NA,
                               "controlAge" = NA,
                               stringsAsFactors = F)
    
    
    matchedTable$recordIDCase <- sapply(matchedTable$caseSampleID, function(x)
      metadataFiltered$record_id[metadataFiltered$sampleID == x])
    matchedTable$caseAge <- sapply(matchedTable$caseSampleID, function(x)
      metadataFiltered$visit_age_mo[metadataFiltered$sampleID == x])
    matchedTable$recordIDControl <- sapply(matchedTable$controlSampleID, function(x)
      metadataFiltered$record_id[metadataFiltered$sampleID == x])
    matchedTable$controlAge <- sapply(matchedTable$controlSampleID, function(x)
      metadataFiltered$visit_age_mo[metadataFiltered$sampleID == x])
    
    head(matchedTable)
    
    resSamples <- c(matchedTable$caseSampleID, matchedTable$controlSampleID)    
    
    if(1){
      source("../../Analysis/Functions/plotMatchesFunction.R")
      gg <- plotMatches(resSamples, metadataFiltered, matchedTable, name)
      if(seed == 1){
        ggsave(paste0("Figures/Hung_matched/",name,".pdf"), device = "pdf", width = 9, height = 7)
        ggsave(paste0("Figures/Hung_matched/",name,".png"), device = "png", width = 9, height = 7)
      }
    }
    
    return(resSamples)
    
  }
}

for (vars in oneModels) {
  firstOrLast <- vars[1] 
  symptomsStage <- vars[2]
  name <- paste0(firstOrLast," ",symptomsStage)
  listOfSamples <- HungarianAlgoPerfectMatch(metadata, firstOrLast, symptomsStage, name, seed = 1)
  if(1){samples[[name]] <- listOfSamples}
  
  for (diet in c("Formula", "Exclusively BF", "Partially BF")) {
    name <- paste0(firstOrLast," ",symptomsStage," ", diet)
    dietIndx <- which(metadata$lastDiet == diet)
    dietMetadata <- metadata[dietIndx,]
    listOfSamples <- HungarianAlgoPerfectMatch(dietMetadata, firstOrLast, symptomsStage, name, seed = 1)
    if(1){samples[[name]] <- listOfSamples}
  }
}

# Add run of 9-12 months (without symptomtic samples)
{
  samples[["samples 9-12 Model"]] <- metadata %>% 
    filter(8.5 < visit_age_mo & visit_age_mo < 12.5) %>% 
    filter(symptoms %in% c("Control","Resolved")) %>% 
    pull(sampleID)
}

# OneSamplePerKidPerTime
{
  samples[["OneSamplePerKidPerTime"]] <- OneSamplePerKidPerTime
}

{
  subsets_to_save <- c("All Samples",
                       "OneSamplePerKidPerTime",
                       "samples 0-2 Model",
                       "last Pre-symptoms",
                       "first Symptomatic",
                       "first Resolved",
                       "last Resolved",
                       "samples 9-12 Model"
                       )
  final_samples <- samples[subsets_to_save]
}

# create new file to write in
README <- c("# Subsets File, each row has different subset, the first column is the subset name, and after that all tha sampleIDs in this subset")
write(README,"Results/CSVs/SubsetLists.csv", append=F)
lapply(X = 1:length(final_samples), function(i) {
  write(x = c(names(final_samples)[i], final_samples[[i]]), 
        "Results/CSVs/SubsetLists.csv", append=TRUE, ncolumns=length(final_samples[[i]]) + 1, sep = ",")})
# copy the file to input files for maaslin
file.copy("Results/CSVs/SubsetLists.csv","Results/MaAsLin2/input/SubsetLists.csv", overwrite = T)

