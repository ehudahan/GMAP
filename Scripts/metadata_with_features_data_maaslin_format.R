################################################################################
############################# Prepare data for run maaslin with abx ############
################################################################################

# Data
if(1){
  rm(list=ls())
  try(setwd("~/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/Volumes/ehudda/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/vol/sci/bio/data/moran.yassour/lab/Projects/GMAP/Docs/GMAP_Paper/"), silent = T)
  getwd()
  metadata <- read.delim("Results/TSVs/metadata.tsv", header=T)
  tab.n <- read.table("Results/TSVs/FeatureTable.tsv",header = T)
}

# transpose table with relative abundance becauseFormat is samples (rows) X features (cols)
t.tab.n <- tab.n %>% t()
head(t.tab.n)

# define the sampleIDs as a column (That enable 'left_join' funtion to merge the tables based on the sampleID column)
df.tab <- as.data.frame(t.tab.n) %>% rownames_to_column("sampleID")
df.tab[1:6,1:6]

# choose all relevant data from our metadata file
data <- metadata %>% select(sampleID, record_id, case_id, visit_age_mo, antibiotics_during_delivery, perinatal_antibiotic_exposure,lastDiet, probiotics_firstyr, mode_of_delivery)

# merge between metadata and features data
# left_join merge by all columns with same name. In our case there is exactly one common column
# the column "sampleID".
data <- data %>% left_join(df.tab)
head(data)

# writing the table to data folder with out quote
# One can choose to write the file as csv, both format are fine for the app
write.table(data, file = "Results/TSVs/metadata_with_features_data_maaslin_format.tsv", quote = F, row.names = F, sep = "\t")
