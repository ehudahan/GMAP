###########################################################
##################### Richness (Alpha Diversity) ##########
###########################################################

# data
{
    rm(list=ls())
    try(setwd("~/GMAP/Docs/GMAP_Paper/"), silent = T)
    try(setwd("/Volumes/ehudda/GMAP/Docs/GMAP_Paper/"), silent = T)
    try(setwd("/vol/sci/bio/data/moran.yassour/lab/Projects/GMAP/Docs/GMAP_Paper/"), silent = T)
    metadata <- read.table("Results/TSVs/metadata.tsv", header=T)
    samples_list <- read.csv("Results/CSVs/OneSamplePerKikPerTimePoint.csv", header = T)$x
    values <- read.table("Results/TSVs/DiversityOutput.tsv")
}

{
    TITLE = "Bacteria Richness Across Time - One Sample Per Kid"
}

# libraries
{ 
    library(dplyr) # for %>% 
    library(ggplot2)
    library(RColorBrewer)
    library(ggpubr)
    library(arrangements) #for combinations
}

# filtering and prepare data
{
    metadata$symptoms <- factor(metadata$symptoms, levels = c("Control","Pre-symptoms", "Symptomatic","Resolved"))
    metadata.one.per.kid <- metadata %>% filter(sampleID %in% samples_list)
    data.unfiltered <- left_join(metadata.one.per.kid, values)
    df_n_per_group <- data.unfiltered %>%
        filter(chao1<130) %>% 
        group_by(symptoms, numericSampleTimeWithSick) %>%
        summarise(n=n())
    
    data.un.with.n <- left_join(data.unfiltered, df_n_per_group)
    data <- data.un.with.n %>% 
        filter(n>3) # %>%
        # filter(chao1<130)
    
    data$sampleTimeLabels <-
        factor(
            data$numericSampleTimeWithSick,
            labels = c(
                "initial",
                "two week",
                "one month",
                "two month",
                "four month",
                "six month",
                "nine month",
                "one year"
            )
        )
    
}

colors <- brewer.pal(9, "Set1")


data_med <- data %>% 
    group_by(sampleTimeLabels) %>% 
    summarize(median = median(chao1)) %>%
    ungroup()


g <- ggplot(data, color="gray", aes(x = symptoms , y = chao1 , 
                                    color = symptoms, 
                                    fill = symptoms
)) + 
    geom_boxplot(alpha = .4, outlier.colour = NA) +
    geom_point(alpha = .7,
               position = position_jitterdodge(jitter.width = 0.25)
    ) +
    scale_color_manual(values = c("#7c817e","#fbd68e","#e36a4c","#ed985e"))+
    scale_fill_manual(values = c("#7c817e","#fbd68e","#e36a4c","#ed985e"))+
    facet_grid(~sampleTimeLabels, scales = "free_x", space = "free_x")+
    labs(y = "Richness (chao1)",
         x = "Time (months)",
         title = TITLE

    ) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
    # stat_compare_means(hide.ns = T, tip.length = c(0.1,0.1),
    #                    ref.group = "Control", 
    #                    label = "p", size =2)
g
gg <- g


# Add significance t-test
    combs <- combinations(k =  2, v = as.character(unique(data$symptoms)),replace = F, layout = "list")
    # y_max <- max(data$chao1, na.rm = T)
    y_max = 40
    STEP <- 2
    gg <- g + labs(caption = "P-Values calculated by Student T test") 
    
    i <- 3
    for (i in 1:length(combs)) {
        gg <- gg + stat_compare_means(comparisons = combs[i], method = "t.test", 
                                      size = 2.5, label = "p.format",
                                      label.y = y_max, 
                                      hide.ns = F,  colour = NA)
        y_max <- y_max + STEP
    }
    
gg


# Save
{
    path <- "Figures/"
    fileName <- "RichnessBySymp"
    if (1) {
        ggsave(gg,filename = paste0(path, "PDFs/",fileName, ".pdf"), "pdf", useDingbats=FALSE,
               width = 8, height = 5)
        ggsave(gg,filename = paste0(path, "PNGs/",fileName, ".png"), "png",
               width = 8, height = 5)
    }
    
}

