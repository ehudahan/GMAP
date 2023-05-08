################################################################################
############################# Box Plot #########################################
################################################################################
# AST transformed

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
    
}

# plot detalis
{
    title <- "Overall Taxonomic Composition"
    subset <- "All Samples"
    bacs_vag <-  c("g__Bacteroides")
    bacs_diet <- c("g__Bifidobacterium", "g__Clostridium","f__Peptostreptococcaceae_unclsfd", "g__Lachnospira")
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
    mm.data <- left_join(data, metadata) 
}


data_vag <- mm.data %>% 
    filter(sampleID %in% samples[[subset]]) %>% 
    filter(Bac %in% bacs_vag) # %>% filter(!is.na(mode_of_delivery))
data_vag[1:4,1:6]

## Add The AST transformation ##
source("Scripts/Functions/Arc-Sine_Square_Root_Transformation.R")
data_vag$AbundanceAST <- AST(data_vag$Abundance)
##

# find the highest dot for each bac
max_values_per_delivery_mode <- sapply(bacs_vag, function(x) max(data_vag$AbundanceAST[which(data_vag$Bac == x)]))
# order the plot from lowest to highest
data_vag$Bac <- factor(data_vag$Bac, levels = names(sort(max_values_per_delivery_mode)))


g <- ggplot(data_vag, aes(x = mode_of_delivery, y = AbundanceAST)) +
    geom_point(aes(color = Bac),alpha = 0.8, size = 2, position = position_jitterdodge(jitter.width = 0.6, seed = 1)) +
    geom_boxplot(aes(fill = Bac),alpha = 0.6, outlier.size = 0, outlier.alpha = 0) +
    # facet_grid(~Bac) +
    scale_fill_manual(values = colors, breaks = names(colors))+
    scale_color_manual(values = colors, breaks = names(colors))+
    # scale_y_continuous(labels = scales::percent_format(), breaks = c(0,0.25,0.5,0.75,1), limits = c(0,1.4)) +
    scale_y_continuous(limits = c(0, max(max_values_per_delivery_mode) + 0.3)) + 
    theme_classic() + 
    labs(title = title,
         subtitle = subset,
         y = paste0("Relative Abundance (AST)")) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor=element_blank(), 
          axis.text.x = element_text(angle = 45, hjust = 1),  
          axis.title.y = element_text(size = 10),
          legend.position = "bottom")

g


{
    unique(maaslin_results$Measured.Variable)
    unique(maaslin_results$Reference.Variable)
    unique(maaslin_results$Subset)
    head(maaslin_results)
    maaslin_results_vag <- maaslin_results %>% 
        filter(Subset == subset) %>% 
        filter(Model.Name == "case lastDiet pro") %>%
        filter(Measured.Variable == "mode_of_delivery") %>% 
        filter(Feature %in% bacs_vag)
    
    stat_table <- maaslin_results_vag %>%
        mutate("value" = "Abundance") %>% 
        select(Feature, value, Test.Variable, Reference.Variable, pval, qval, coef)
    
    names(stat_table) <- c("Bac", ".y.", "group1","group2","p", "q", "c")
    
    # create nicer format for long numbers
    stat_table$p_nice <- scales::scientific(  x = stat_table$p,digits = 3)
    stat_table$q_nice <- scales::scientific(  x = stat_table$q,digits = 3)
    stat_table$c_nice <- scales::scientific(  x = stat_table$c,digits = 3)
    
    # add position 
    stat_table$y.position <- sapply(stat_table$Bac, function(x) max_values_per_delivery_mode[x] + 0.1)
    stat_table$Bac <- factor(stat_table$Bac, levels = names(sort(max_values_per_delivery_mode)))
    

    gg <- g + stat_pvalue_manual(size = 3,
    stat_table,label = paste0("q={q_nice}\n",
    #"p={p_nice}\n",
    "c={c_nice}"
    ))
    gg
}

# Save
{
    path <- "Figures/"
    fileName <- "BoxPlotVagBacteroides"
    if (1) {
        ggsave(gg,filename = paste0(path, "PDFs/",fileName, ".pdf"), "pdf", useDingbats=FALSE,
               width = 3, height = 6)
        ggsave(gg,filename = paste0(path, "PNGs/",fileName, ".png"), "png",
               width = 3, height = 6)
    }
    
}

