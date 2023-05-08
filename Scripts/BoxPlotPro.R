################################################################################
############################# Box Plot probiotica first year ###################
################################################################################
# For Diet - with AST transformation

# Data
if(1){
  rm(list=ls())
  try(setwd("~/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/Volumes/ehudda/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/vol/sci/bio/data/moran.yassour/lab/Projects/GMAP/Docs/GMAP_Paper/"), silent = T)
  metadata <- read.delim("Results/TSVs/metadata.tsv", header=T, sep = '\t')
  tab.n <- read.table("Results/TSVs/FeatureTable.tsv",header = T)
  maaslin_results <- read.table("Results/TSVs/SuppTable1.tsv", header = T)
}

#libraries
{
  library(dplyr)
  library(ggplot2)
  library(reshape2)
  library(ggpubr)
  library(rstatix)
  library(tibble)
  
}

# plot detalis
{
  title <- "Overall Taxonomic Composition"
  
  unique(maaslin_results$Measured.Variable)
  head(maaslin_results)
  maaslin_results_pro <- maaslin_results %>% 
    filter(Subset == "All Samples") %>%
    filter(Model.Name == "case lastDiet pro") %>%
    filter(Measured.Variable == "probiotics_firstyr") 
  bacs <- maaslin_results_pro$Feature
}

#colors
{
  source("Scripts/Functions/GMAPPlotsColors.R")
  colors <- colorGMAPDataFunction(varTitle = "feature")
}

# filtering data
{
  data <- melt(tab.n[,] %>% rownames_to_column())
  head(data)
  names(data) <- c("Bac", "sampleID", "Abundance")
  metadata$lastDiet <- factor(metadata$lastDiet,levels = c("Exclusively BF","Partially BF","Formula"))
  
  mm.data <- left_join(data, metadata) %>% filter(!is.na(probiotics_firstyr))
  unique(mm.data$probiotics_firstyr)
  mm.data$probiotics_firstyr <- factor(mm.data$probiotics_firstyr, levels = c(0,1), labels =  c("Pro -", "Pro +"))
  
}

# data_diet <- mm.data %>% filter(Bac %in% bacs_diet) %>% filter(!is.na(lastDiet))
# data_diet[1:4,1:6]

## Add The AST transformation ##
data_pro <- mm.data %>% filter(Bac %in% bacs)
source("Scripts/Functions/Arc-Sine_Square_Root_Transformation.R")
data_pro$AbundanceAST <- AST(data_pro$Abundance)
##
# find the highest dot for each bac
max_values_per_pro <- sapply(bacs, function(x) max(data_pro$AbundanceAST[which(data_pro$Bac == x)]))
# # order the plot from lowest to highest
data_pro$Bac <- factor(data_pro$Bac, levels = names(sort(max_values_per_pro)))

g <- ggplot(data_pro, aes(x = probiotics_firstyr, y = AbundanceAST)) +
  geom_point(aes(color = Bac),alpha = 0.7, size = 2, position = position_jitterdodge(jitter.width = 0.5, seed = 1)) +
  geom_boxplot(aes(fill = Bac),alpha = 0.8, outlier.size = 0, outlier.alpha = 0) +
  facet_grid(~Bac) +
  # scale_fill_manual(values = brewer.pal(9, "Set1")[c(5,7,8,3)]) +
  # scale_color_manual(values = brewer.pal(9, "Set1")[c(5,7,8,3)]) +
  # scale_y_continuous(labels = scales::percent_format(), breaks = c(0,0.25,0.5,0.75,1), limits = c(0,1.6)) +
  # scale_color_manual(values = c("#7c817e","#fbd68e","#e36a4c","#ed985e"))+
  scale_y_continuous(limits = c(0,max(max_values_per_pro) + 0.3)) +
  labs(title = title,
       y = paste0("Relative Abundance (AST)")) +
  theme_classic() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1),  
        axis.title.y = element_text(size = 10))

g


## Add statistics
{
  # unique(maaslin_results$Measured.Variable)
  # unique(maaslin_results$Feature)
  # head(maaslin_results)
  # maaslin_results_diet <- maaslin_results %>%
  #     filter(Subset == "All Samples ") %>%
  #     filter(Model.Name == "case lastDiet pro") %>%
  #     filter(Measured.Variable == "probiotics firstyr") %>%
  #     filter(Feature %in% bacs)
  # maaslin_results_diet
  stat_table <- maaslin_results_pro %>%
    mutate("value" = "Abundance") %>%
    select(Feature, value, Test.Variable, Reference.Variable, pval, qval, coef)
  
  names(stat_table) <- c("Bac", ".y.", "group1","group2","p", "q", "c")
  # create nicer format for long numbers
  stat_table$p_nice <- scales::scientific(  x = stat_table$p,digits = 3)
  stat_table$q_nice <- scales::scientific(  x = stat_table$q,digits = 3)
  stat_table$c_nice <- scales::scientific(  x = stat_table$c,digits = 3)
  
  # add position
  
  stat_table$y.position <- sapply(stat_table$Bac, function(x) max_values_per_pro[x] + 0.1)
  stat_table$Bac <- factor(stat_table$Bac, levels = names(sort(max_values_per_pro)))
  stat_table
  
  gg <- g + stat_pvalue_manual(stat_table,label = "p={p_nice}\nq={q_nice}\nc={c_nice}",size = 3)
  gg
}

# Save
{
  path <- "Figures/"
  fileName <- "BoxPlotPro"
  if (1) {
    ggsave(gg,filename = paste0(path, "PDFs/",fileName, ".pdf"), "pdf", useDingbats=FALSE,
           width = 15, height = 10)
    ggsave(gg,filename = paste0(path, "PNGs/",fileName, ".png"), "png",
           width = 15, height = 10)
  }
  
}
