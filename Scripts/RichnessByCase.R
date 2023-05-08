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
}

# filtering and prepare data
{
    metadata.one.per.kid <- metadata %>% filter(sampleID %in% samples_list)
    data.unfiltered <- left_join(metadata.one.per.kid, values)
    df_n_per_group <- data.unfiltered %>%
        filter(chao1<130) %>% 
        group_by(symptoms, numericSampleTimeWithSick) %>%
        summarise(n=n())
    
    data.un.with.n <- left_join(data.unfiltered, df_n_per_group)
    data <- data.un.with.n %>% 
        filter(n>3) %>%
        filter(chao1<130)
    
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


g <- ggplot(data, color="gray", aes(x = case_id , y = chao1 , color = case_id
                      )) + 
    geom_boxplot(alpha = .5, outlier.colour = NA,fill="darkgray") +
    geom_point(alpha = .5,
               position = position_jitterdodge(jitter.width = 0.25),
               # aes(color = "gray")
               ) +
    facet_grid(~sampleTimeLabels)+
    labs(y = "Richness (chao1)",
         x = "Time (months)",
         title = TITLE,
         subtitle = "Wilk Test"
    ) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    stat_compare_means(hide.ns = T, label = "p", size = 2)
g



gg <- g
## Add statistics
# {
#     my_comparisons <- list(c("Control","Pre-symptoms"),
#                            c("Control","Symptomatic"),
#                            c("Control","Resolved"))
# 
#     yNum <- max(data$chao1, na.rm = T)
#     gg <- g +
#         stat_compare_means(comparisons = my_comparisons[3], method = "t.test", size = 3, label = "p.format",
#                            label.y = yNum, hide.ns = T,  colour = NA) +
#         stat_compare_means(comparisons = my_comparisons[2], method = "t.test", size = 3, label = "p.format",
#                            label.y = yNum*1.1, hide.ns = T)
#     gg
# }



# Save
{
    path <- "Figures/"
    fileName <- "RichnessByCase"
    if (1) {
        ggsave(gg,filename = paste0(path, "PDFs/",fileName, ".pdf"), "pdf", useDingbats=FALSE,
               width = 6, height = 4)
        ggsave(gg,filename = paste0(path, "PNGs/",fileName, ".png"), "png",
               width = 6, height = 4)
    }
    
}
