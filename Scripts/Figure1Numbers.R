###########################################################
#################### Figure 1 numbers #####################
###########################################################

# data
{
  rm(list=ls())
  try(setwd("~/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/Volumes/ehudda/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/vol/sci/bio/data/moran.yassour/lab/Projects/GMAP/Docs/GMAP_Paper/"), silent = T)
  getwd()
  metadata <- read.table("Results/TSVs/metadata.tsv")
}

{
  library(dplyr)
  library(scales)
}

n_samples <- nrow(metadata)
metadata_kids <- metadata %>% group_by(record_id) %>% 
  summarise(mode_of_delivery = unique(mode_of_delivery),
            diet0mo = unique(diet0mo),
            diet2mo = unique(diet2mo),
            diet6mo = unique(diet6mo),
            antibiotics_during_delivery = unique(antibiotics_during_delivery),
            perinatal_antibiotic_exposure = unique(perinatal_antibiotic_exposure),
            case = unique(case_id)
  )
metadata_kids


# Factors at birth
n_infants <- nrow(metadata_kids)
n_cases <- nrow(metadata_kids %>% filter(case  == "AP Case"))
n_control <- nrow(metadata_kids %>% filter(case  == "No AP"))
# sanity check
if (n_infants != length(unique(metadata$record_id))) {
  message("Check number of kids")
}

C_section <-   sum(metadata_kids$mode_of_delivery == "C-section") / n_infants
Vaginaly <- sum(metadata_kids$mode_of_delivery == "Vaginal") / n_infants


# perinatal_antibiotic_exposure means infant antibiotics
# antibiotics_during_delivery means maternal antibiotics

# metadata_ants with only samples with mother and baby valid information about antibiotics
metadata_ants <- metadata_kids %>% 
  filter(!is.na(antibiotics_during_delivery)) %>% 
  filter(!is.na(perinatal_antibiotic_exposure))

ants_total <- metadata_ants %>% nrow()

mam_1_infant_1 <- metadata_ants %>% 
  filter(antibiotics_during_delivery == "Yes") %>% 
  filter(perinatal_antibiotic_exposure == "Yes") %>% 
  nrow() / ants_total

mam_1_infant_0 <- metadata_ants %>% 
  filter(antibiotics_during_delivery == "Yes") %>% 
  filter(perinatal_antibiotic_exposure == "No") %>% 
  nrow() / ants_total

mam_0_infant_1 <- metadata_ants %>% 
  filter(antibiotics_during_delivery == "No") %>% 
  filter(perinatal_antibiotic_exposure == "Yes") %>% 
  nrow() / ants_total

mam_0_infant_0 <- metadata_ants %>% 
  filter(antibiotics_during_delivery == "No") %>% 
  filter(perinatal_antibiotic_exposure == "No") %>% 
  nrow() / ants_total

mam_OR_infant_exposed_ABX <- metadata_ants %>% 
  filter(antibiotics_during_delivery == "Yes" |
           perinatal_antibiotic_exposure == "Yes") %>% 
  nrow() / ants_total

if (mam_1_infant_1 + mam_1_infant_0 + mam_0_infant_1 + mam_0_infant_0 != 1) {
  message("check your calcs about antibiotics")
}

# init_known <- n_infants - sum(is.na(metadata_kids$diet0mo))
# init_formula <-   sum(metadata_kids$diet0mo == "Formula", na.rm = T) / init_known
# init_BF <-   sum(metadata_kids$diet0mo == "Exclusively BF", na.rm = T) / init_known
# init_Mixed <-   sum(metadata_kids$diet0mo == "Partially BF", na.rm = T) / init_known
# init_unknown <- sum(is.na(metadata_kids$diet0mo)) / n_infants
diet0mo_known <- n_infants - sum(is.na(metadata_kids$diet0mo))
diet0mo_formula <-   sum(metadata_kids$diet0mo == "Formula", na.rm = T) / diet0mo_known
diet0mo_BF <-   sum(metadata_kids$diet0mo == "Exclusively BF", na.rm = T) / diet0mo_known
diet0mo_Mixed <-   sum(metadata_kids$diet0mo == "Partially BF", na.rm = T) / diet0mo_known
diet0mo_unknown <- sum(is.na(metadata_kids$diet0mo)) / n_infants


diet2mo_known <- n_infants - sum(is.na(metadata_kids$diet2mo))
diet2mo_formula <-   sum(metadata_kids$diet2mo == "Formula", na.rm = T) / diet2mo_known
diet2mo_BF <-   sum(metadata_kids$diet2mo == "Exclusively BF", na.rm = T) / diet2mo_known
diet2mo_Mixed <-   sum(metadata_kids$diet2mo == "Partially BF", na.rm = T) / diet2mo_known
diet2mo_unknown <- sum(is.na(metadata_kids$diet2mo)) / n_infants


diet6mo_known <- n_infants - sum(is.na(metadata_kids$diet6mo))
diet6mo_formula <-   sum(metadata_kids$diet6mo == "Formula", na.rm = T) / diet6mo_known
diet6mo_BF <-   sum(metadata_kids$diet6mo == "Exclusively BF", na.rm = T) / diet6mo_known
diet6mo_Mixed <-   sum(metadata_kids$diet6mo == "Partially BF", na.rm = T) / diet6mo_known
diet6mo_unknown <- sum(is.na(metadata_kids$diet6mo)) / n_infants

text1 <- c("Infants" = n_infants, 
           "Samples" = n_samples)

text2 <- label_percent(accuracy = 1.0)(c("C_section" = C_section, 
              "Vaginaly" = Vaginaly,
              "No antibiotics" = mam_0_infant_0,
              "Maternal ABX" = mam_1_infant_0,
              "Infant ABX" = mam_0_infant_1,
              "Maternal & Infant" = mam_1_infant_1,
              "ABX exposed (Maternal OR Infant)" = mam_OR_infant_exposed_ABX, 
              
              "diet0mo_formula" = diet0mo_formula,
              "diet0mo_BF" = diet0mo_BF,
              "diet0mo_Mixed" = diet0mo_Mixed,
              
              "diet2mo_formula" = diet2mo_formula,
              "diet2mo_BF" = diet2mo_BF,
              "diet2mo_Mixed" = diet2mo_Mixed,

              "diet6mo_formula" = diet6mo_formula, 
              "diet6mo_BF" = diet6mo_BF,
              "diet6mo_Mixed" = diet6mo_Mixed
))

message(paste0(names(text1)," - ",text1,"\n"))
message(paste0(names(text2)," - ",text2,"\n"))
