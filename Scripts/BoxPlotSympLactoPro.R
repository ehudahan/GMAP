################################################################################
############################# Box Plot Lacto Symp with probiotics ##############
################################################################################
# AST transformed

# Data
if(1){
  rm(list=ls())
  try(setwd("~/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/Volumes/ehudda/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/vol/sci/bio/data/moran.yassour/lab/Projects/GMAP/Docs/GMAP_Paper/"), silent = T)
  getwd()
  metadata <- read.delim("Results/TSVs/metadata.tsv", header=T, sep = '\t')
  tab.n <- read.table("Results/TSVs/FeatureTable.tsv",header = T)
  maaslin_results <- read.table("Results/TSVs/SuppTable1.tsv", header = T)
  samples_raw <- readLines("Results/CSVs/SubsetLists.csv")
  {
    # skip 1 row because this description
    samples <- list()
    splited <- strsplit(samples_raw, ",")
    for (i in 2:length(splited)) {
      # first argument is the subset name
      samples[[splited[[i]][1]]] <- splited[[i]][-1]
    }
  }
  
  
}


#libraries
{
  library(dplyr)
  library(ggplot2)
  library(reshape2)
  library(tibble)
  library(RColorBrewer)
  library(ggpubr)
  library(rstatix)
  library(tibble)
  library(arrangements) #for combinations
}

# plot detalis
{
  title <- "Lactobacillus (PRO +)"
  bac <- "g__Lactobacillus"
  facet_var <- "Subset"
}

#colors
{
  ######### Plot COLORS #################################################################
  # Copy paste this section to consistent colors
  source("Scripts/Functions/GMAPPlotsColors.R")
  colors <- GMAPPaperColors()
  # add this line to your ggplot
  # scale_fill_manual(values = colors, breaks = names(colors))+
  #######################################################################################
}

# filtering data
{
  data <- melt(tab.n[,] %>% rownames_to_column())
  head(data)
  names(data) <- c("Bac", "sampleID", "Abundance")
  metadata$symptoms <- factor(metadata$symptoms, levels = c("Control","Pre-symptoms", "Symptomatic","Resolved"))
  mm.data <- left_join(data, metadata) 
}

# add subset data (True == In the subset)
{
  All_Samples <- mm.data %>% filter(sampleID %in% samples$`All Samples`)
  zero_to_two_months <- mm.data %>% filter(sampleID %in% samples$`samples 0-2 Model`)
  First_Resolved <- mm.data %>% filter(sampleID %in% samples$`first Resolved`)
  mm.data.per.subset <- bind_rows(list(
    'All_Samples' = All_Samples,
    '0-2 months' = zero_to_two_months, 
    'First_Resolved' = First_Resolved),
    .id = "Subset")
}


# arrange data
{
  data_facet_var <- mm.data.per.subset %>% 
    filter(Bac %in% bac) %>% 
    filter(probiotics_firstyr == "Pro +" ) # %>%
    # filter(!is.na(!!!facet_var))
  data_facet_var[1:4,1:6]
}

## Add The AST transformation ##
source("Scripts/Functions/Arc-Sine_Square_Root_Transformation.R")
data_facet_var$AbundanceAST <- AST(data_facet_var$Abundance)
##


gg_list <- list()

# for (subset_name in names(samples_list_per_model)) {
# names(samples_list_per_model)
# subset_name <- "0-2 months"

# Plot 1
# subset_name <- names(samples_list_per_model)[1]

# remove outliers
n_removed <- data_facet_var %>% filter(AbundanceAST >= 1) %>% nrow()
data <- data_facet_var %>% filter(AbundanceAST < 1)

g <- ggplot(data, aes(x = symptoms, y = AbundanceAST)) +
  geom_point(aes(color = symptoms),alpha = 0.8, size = 2, position = position_jitterdodge(jitter.width = 0.8, seed = 1)) +
  geom_boxplot(aes(fill = symptoms),alpha = 0.8, outlier.size = 0, outlier.alpha = 0) +
  facet_grid(~Subset) +
  scale_fill_manual(values = brewer.pal(5, "Set1")[c(3,5,1,2)]) +
  scale_color_manual(values = brewer.pal(5, "Set1")[c(3,5,1,2)]) +
  # scale_y_continuous(labels = scales::percent_format(), breaks = c(0,0.25,0.5,0.75,1), limits = c(0,1.4)) +
  scale_y_continuous(limits = c(0, max(data$AbundanceAST) + 0.25)) +
  theme_classic() + 
  labs(title = title,
       subtitle = paste0(n_removed," outliers were removed manualy"),
       y = paste0("Relative Abundance (AST)")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor=element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1),  
        axis.title.y = element_text(size = 10))

g

quantile(data$Abundance)
quantile(data$AbundanceAST)


# Add significance t-test
{
combs <- combinations(k =  2, v = as.character(unique(data$symptoms)),replace = F, layout = "list")
y_max <- max(data$AbundanceAST[which(data$AbundanceAST < 1)], na.rm = T)
STEP <- 0.04
gg <- g + labs(caption = "P-Values calculated by students T-test") 

i <- 3
for (i in 1:length(combs)) {
  gg <- gg + stat_compare_means(comparisons = combs[i], method = "t.test", size = 3, label = "p.format",
                                label.y = y_max, hide.ns = F,  colour = NA)
  y_max <- y_max + STEP
  }

  
gg
}

# Save
{
  path <- "Figures/"
  fileName <- "BoxPlotSympLactoPro"
  if (1) {
    ggsave(gg,filename = paste0(path, "PDFs/",fileName, ".pdf"), "pdf", useDingbats=FALSE,
           width = 8, height = 5)
    ggsave(gg,filename = paste0(path, "PNGs/",fileName, ".png"), "png",
           width = 8, height = 5)
  }
  
}

