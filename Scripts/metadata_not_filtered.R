# Metadata version 4
# This scriprt take the oldest metadata I have called "GMAP\Data\GMAP.Broad Samples_Metadata_2018-04-06.4Rotem.csv"
# And make adaptations to our neccesarities

# The changes are:
# 1. 'sample' column
# 2a. 'sample_time' 
# 2b. edit 'sample_time' 
# 3a. 'sampleID' 
# 3b. 'closestDiet'
# 3c. 'lastDiet'
# 3d. 'numericSampleTimeWithSick'
# 3e. delete unsed columns
# 4. 'inData'
# 5. 'time_from_onset' - time from first onset
# 6. 'time_from_diagnose'
library(dplyr) # for %>% 
library(tidyr) # unite
library(stringr) # for str.detect
library(ggplot2)


rm(list = ls())
try(setwd("~/GMAP/Docs/GMAP_Paper/"), silent = T)
try(setwd("/Volumes/ehudda/GMAP/Docs/GMAP_Paper/"), silent = T)
try(setwd("/vol/sci/bio/data/moran.yassour/lab/Projects/GMAP/Docs/GMAP_Paper/"), silent = T)
# read combined file:
tab <- read.delim("Data/GMAP.Broad Samples_Metadata_2018-04-06.4Rotem.csv",as.is=T, header=T, sep = ",")


names(tab)
head(tab)
# 1 - 'sample' column ####
# change "_" to "." to enable matching with qiime
tab$sample <- gsub("_", ".", tab$sample)

# 2a. 'sample_time' ####
# - change the name of 'sample' column to 'sample time'
tab$sample_time <- tab$sample

# 2b - 'sample_time'####
#  - delete non ALEPHBEIT letters. for example: If original was "2.one.month". the updated would be "onemonth"
tab$sample_time <- str_remove_all(tab$sample_time, pattern = "[^a-z]")

# 3a 'sampleID' ####
#  - add sampleID column by concatination of sub, record_id, sample_time and replacing the "_" to "."
# sampleID is column build for qiime2. must to stay with point instead "_" and with the numbers becaues this are similar to the names of the reads
tab$sub <- "sub"
tab <- tab %>% tidyr::unite(sub, record_id, sample, col = sampleID, sep = ".", remove = F)
head(data)


# 3b. look at closestDiet ####
colnames(tab)[grep("diet0_5mo",colnames(tab))] <- "diet0.5mo"
times <- c(0,0.5,1,2,4,6) #times of well-visits
diet.colnames <- paste0("diet",times,"mo")
idx <- as.numeric(sapply(diet.colnames,grep,colnames(tab)))
tab$closestTime <- sapply(tab$visit_age_mo, function(x) which.min(abs(x -times)))
tab$closestDiet <- tab[cbind(1:nrow(tab), idx[tab$closestTime])]
## if you want, you can go over NAs here (sum(is.na(tab$closestDiet))) --> 78 such samples
# look at dietUpToNow
tab$untilNowIDx <- sapply(idx[tab$closestTime],function(x) seq(idx[1],x))

tab$dietUntilNow <- NA
for (i in seq(1,nrow(tab))){
  tab[i,"uniqueDietUntilNow"] <- paste(sort(unique(as.character(tab[i,tab[i,"untilNowIDx"][[1]]]))),collapse = ":")
  tab[i,"dietUntilNow"] <- paste(as.character(tab[i,tab[i,"untilNowIDx"][[1]]]),collapse = ":")
}

# for all NAs I will if they have a constant diet - I will use this diet
tab$constantDiet <- NA
for (i in seq(1,nrow(tab))){
  tab[i,"constantDiet"] <- length(unique(na.omit(as.character(tab[i,tab[i,"untilNowIDx"][[1]]]))))==1
}
# look at the constant
tab[tab[,"constantDiet"],c("dietUntilNow","constantDiet")]
tab[!tab[,"constantDiet"],c("dietUntilNow","constantDiet")]
tab[is.na(tab$closestDiet),c("dietUntilNow","constantDiet")]

# ggplot(metadata, aes(x=constantDiet, y=))
# replace NAs in closestDiet when constant diet is TRUE
tab$closestDiet <- sapply(1:nrow(tab), function(i) ifelse(tab[i,"constantDiet"] && is.na(tab[i,"closestDiet"]),
                                                          unique(na.omit(as.character(tab[i,tab[i,"untilNowIDx"][[1]]]))),
                                                          tab[i,"closestDiet"]))

# after replacing we left with 29 NAs samples without a constant Diet
# commonDietUntilNow
tab$commonDietUntilNow <- names(sapply(sapply(str_split(tab$dietUntilNow,pattern = ":"),table), which.max))

tab$LastDietUntilNow <- sapply(str_split(tab$dietUntilNow,":"), function(x) {
  i <- length(x) 
  while (x[i]=="NA") {i <- i-1} 
  return(x[i])
}
)

# 3c. lastDiet
# if the lastDiet would be equal to commonDietUntilNow I will use it
tab$lastDiet <- sapply(1:nrow(tab),function(i) ifelse(!is.na(tab[i, "closestDiet"]),
                                                      tab[i,"closestDiet"], 
                                                      tab[i,"LastDietUntilNow"]))


# 3d numericSampleTimeWithSick ####
# - Convert visit name to number (so 'initial' go with 0, 'twoweek' with 0.5 etc.)
# - sick sample considered by the closest time visit
visitTimes <- c(0,0.5,1,2,4,6,9,12) #times of well-visits
names(visitTimes) <- c("initial", "twoweek","onemonth","twomonth","fourmonth","sixmonth","ninemonth","oneyear")

tab$closestWellVisit <- sapply(tab$visit_age_mo, function(x) which.min(abs(x -visitTimes)))
tab$sickVisits <- tab$sample_time=="sick"
tab$numericSampleTimeWithSick <- sapply(1:nrow(tab), function(i) ifelse(tab$sickVisits[i], 
                                                                        visitTimes[tab[i,"closestWellVisit"]],
                                                                        visitTimes[tab[i,"sample_time"]]))

# Sanity checks:
unique(tab$sample_time)
tab[which(tab$sample_time=="sick"),c("sample_time","numericSampleTimeWithSick","visit_age_mo")]
table(tab[,c("sample_time","numericSampleTimeWithSick")])
if (0) {
  gg <- ggplot(tab, aes(x=visit_age_mo,y=numericSampleTimeWithSick)) +
    geom_point(aes(color=sample_time),alpha=0.4,position = position_jitter(height = 0.2),size=1) +
    labs(title = "Named Sample Time By Real Visit Time")+
    theme_bw()
  gg
  ggsave("Analysis/Figures/sampleTimeNameByNumericSampleTime.pdf",device = "pdf",width = 7)
}
# Check what about sick after 6 months
tab %>% filter(sample_time=="sick") %>% filter(visit_age_mo>9) %>% select(visit_age_mo,numericSampleTimeWithSick)
# 3e. delete unsed columns ####
# delete the columns:  "closestTime", "closestDiet", "untilNowIDx", "dietUntilNow", "constantDiet", "LastDietUntilNow"
# stay with: "commonDietUntilNow", "LastDiet", "uniqueDietUntilNow"
tab <- tab %>% select(-c("closestTime", "closestDiet", "untilNowIDx", "dietUntilNow", "constantDiet", "LastDietUntilNow"))
names(tab)



# tab2 <- tab[which(tab$sampleID %in% final_set),]

# 5 ####
tab3 <- tab %>% 
  mutate(time_from_diag = visit_age_mo*30-(age_diag_ap_day))

# order the last diet in a logical way
tab3$lastDiet <- factor(tab3$lastDiet, levels = c("Exclusively BF","Partially BF", "Formula"),ordered = T)
tab3$symptoms <- factor(tab3$symptoms,levels = c("Control","Pre-symptoms", "Symptomatic","Resolved"),ordered = T)
tab3$record_id <- factor(tab3$record_id)


tab3$probiotics_firstyr <- as.character(factor(tab3$probiotics_firstyr, levels = c(0,1,NA),labels = c("Pro -", "Pro +")))

# Add any antibiotics variable
tab3$any_antibiotics <- ifelse(tab3$antibiotics_during_delivery == "Yes" | tab3$perinatal_antibiotic_exposure == "Yes", "Yes", "No")
tab3 %>% select(antibiotics_during_delivery, perinatal_antibiotic_exposure, any_antibiotics)

# tab3$ever_breastfed <- ifelse(tab3$diet0mo == "Formula" &
#                                 tab3$diet0.5mo == "Formula" &
#                                 tab3$diet1mo == "Formula" &
#                                 tab3$diet2mo == "Formula" &
#                                 tab3$diet4mo == "Formula" &
#                                 tab3$diet6mo == "Formula", "No", "Yes")
# {
#   "diet0mo"                        "diet0.5mo"                      "diet1mo"                       
#   [65] "diet2mo"                        "diet4mo"                        "diet6mo"  
# }
tab4 <- tab3 %>% select(-other_resolution)



# export the new metadata
if (1){
  write.table(x = tab4, file = "Results/TSVs/metadata_not_filtered.tsv", sep = "\t", row.names = F, quote = F)
}

