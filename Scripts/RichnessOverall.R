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

n_drop <- data.un.with.n %>%
        filter(n<=3) %>% .$sampleID
chao1_drop <- data.un.with.n %>%
        filter(chao1>=130) %>% .$sampleID
    
message("n<=3: ", length(n_drop), n_drop)
message("chao1>=130: ", length(chao1_drop), chao1_drop)

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


g <- ggplot(data, color="gray", aes(x = sampleTimeLabels , y = chao1 #, fill = sampleTimeLabels
                      )) + 
    geom_boxplot(alpha = .5, outlier.colour = NA,fill="darkgray") +
    # facet_grid(~sampleTimeLabels, switch = "x",scales = "free_x") +
    geom_point(alpha = .5,
               position = position_jitterdodge(jitter.width = 0.25),
               aes(color = "gray")
               ) +
#    geom_line(data = data_med, aes(y = median, group=1)) +
# geom_line(data = df_mean, 
#           mapping = aes(x = grp, y = average, group=1))
#    scale_fill_manual(values = colors) +
#    scale_color_manual(values = colors) +
    labs(y = "Richness (chao1)",
         x = "Time (months)",
         title = TITLE
    ) +
#    theme_minimal(base_size = 15) +
theme_bw()+
    theme(legend.position="none"
    #axis.text.x = element_blank(),
          #panel.grid = element_blank()
    ) 
g



gg <- g
# ## Add statistics
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
    fileName <- "RichnessOverall"
    if (1) {
        ggsave(gg,filename = paste0(path, "PDFs/",fileName, ".pdf"), "pdf", useDingbats=FALSE,
               width = 6, height = 4)
        ggsave(gg,filename = paste0(path, "PNGs/",fileName, ".png"), "png",
               width = 6, height = 4)
    }
    
}
