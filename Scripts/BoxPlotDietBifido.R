################################################################################
############################# Box Plot #########################################
################################################################################
# For Diet - with AST transformation

# Data
if(1){
    rm(list=ls())
    try(setwd("~/GMAP/Docs/GMAP_Paper/"), silent = T)
    try(setwd("/Volumes/ehudda/GMAP/Docs/GMAP_Paper/"), silent = T)
    try(setwd("/vol/sci/bio/data/moran.yassour/lab/Projects/GMAP/Docs/GMAP_Paper/"), silent = T)
    metadata <- read.table("Results/TSVs/metadata.tsv", header=T)
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
    library(ggpubr)
    library(rstatix)
    library(tibble)
    
}

# plot detalis
{
    title <- "Overall Taxonomic Composition"
    subset <- "All Samples"
    bacs_vag <-  c("g__Bacteroides", "g__Parabacteroides", "g__Klebsiella", "g__Veillonella")
    bacs_diet <- c("g__Bifidobacterium")
    
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
    metadata$lastDiet <- factor(metadata$lastDiet,levels = c("Exclusively BF","Partially BF","Formula"))
    mm.data <- left_join(data, metadata) 
}

data_diet <- mm.data %>% 
    filter(sampleID %in% samples[[subset]]) %>% 
    filter(Bac %in% bacs_diet) # %>% 
    #filter(!is.na(lastDiet))
data_diet[1:4,1:6]

## Add The AST transformation ##
source("Scripts/Functions/Arc-Sine_Square_Root_Transformation.R")
data_diet$AbundanceAST <- AST(data_diet$Abundance)
##

# find the highest dot for each bac
max_values_per_diet <- sapply(bacs_diet, function(x) max(data_diet$AbundanceAST[which(data_diet$Bac == x)]))
# order the plot from lowest to highest
data_diet$Bac <- factor(data_diet$Bac, levels = names(sort(max_values_per_diet)))

g <- ggplot(data_diet, aes(x = lastDiet, y = AbundanceAST)) +
    geom_point(aes(color = Bac),alpha = 0.8, size = 2, position = position_jitterdodge(jitter.width = 0.8, seed = 1)) +
    geom_boxplot(aes(fill = Bac),alpha = 0.6, outlier.size = 0, outlier.alpha = 0) +
    facet_grid(~Bac) +
    scale_fill_manual(values = colors, breaks = names(colors))+
    scale_color_manual(values = colors, breaks = names(colors))+
    # scale_y_continuous(labels = scales::percent_format(), breaks = c(0,0.25,0.5,0.75,1), limits = c(0,1.4)) +
    scale_y_continuous(limits = c(0,max(max_values_per_diet) + 0.3)) +
    labs(title = title,
         subtitle = subset,
         y = paste0("Relative Abundance (AST)")) +
    theme_classic() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.text.x = element_text(angle = 45, hjust = 1),  
          axis.title.y = element_text(size = 10),
          legend.position = "bottom")

g


## Add statistics
{
    unique(maaslin_results$Measured.Variable)
    unique(maaslin_results$Feature)
    unique(maaslin_results$Subset)
    head(maaslin_results)
    maaslin_results_diet <- maaslin_results %>% 
        filter(Subset == subset) %>% 
        filter(Model.Name == "case lastDiet pro") %>%
        filter(Measured.Variable == "lastDiet") %>% 
        filter(Feature %in% bacs_diet)
    maaslin_results_diet
    stat_table <- maaslin_results_diet %>%
        mutate("value" = "Abundance") %>% 
        select(Feature, value, Test.Variable, Reference.Variable, pval, qval, coef)
    
    names(stat_table) <- c("Bac", ".y.", "group1","group2","p", "q", "c")
    # create nicer format for long numbers
    stat_table$p_nice <- scales::scientific(  x = stat_table$p,digits = 3)
    stat_table$q_nice <- scales::scientific(  x = stat_table$q,digits = 3)
    stat_table$c_nice <- scales::scientific(  x = stat_table$c,digits = 3)
    
    # add position 
    stat_table$y.position <- sapply(stat_table$Bac, function(x) max_values_per_diet[x] + 0.1)
    stat_table$Bac <- factor(stat_table$Bac, levels = names(sort(max_values_per_diet)))
    stat_table
    

    gg <- g + stat_pvalue_manual(stat_table,label = paste0(
        # "p={p_nice}\n",
    "q={q_nice}\n", 
    "c={c_nice}"),
    size = 3, step.increase = 0.2)
    gg
    if (nrow(stat_table) == 0) gg <- gg + labs(caption = "No Significant Results")
}

# Save
{
    path <- "Figures/"
    fileName <- "BoxPlotDietBifido"
    if (1) {
        ggsave(gg,filename = paste0(path, "PDFs/",fileName, ".pdf"), "pdf", useDingbats=FALSE,
               width = 3, height = 6)
        ggsave(gg,filename = paste0(path, "PNGs/",fileName, ".png"), "png",
               width = 3, height = 6)
    }
    
}

