################################################################################
############################# Composition ######################################
################################################################################
# creates composition barplot showing 10 most abundant taxa                    #



# Data
if (1) {
    rm(list = ls())
    try(setwd("~/GMAP/Docs/GMAP_Paper/"), silent = T)
    try(setwd("/Volumes/ehudda/GMAP/Docs/GMAP_Paper/"), silent = T)
    try(setwd("/vol/sci/bio/data/moran.yassour/lab/Projects/GMAP/Docs/GMAP_Paper/"), silent = T)
    getwd()
    metadata <- read.table("Results/TSVs/metadata.tsv", header=T)
    values <- read.table("Results/TSVs/DiversityOutput.tsv")
    tab.n <- read.table("Results/TSVs/FeatureTable.tsv", header = T)
    samples_list <- read.csv("Results/CSVs/OneSamplePerKikPerTimePoint.csv", header = T)$x
}

# constants
{
    NUM_HIGHER_TAXA <- 15
    COHORT <- "One Sample Per Kid"
}

# libraries
{
    library(plyr)
    library(ggplot2)
    #library(vegan)
    library(tibble)
    library(ggpubr)
    library(tidyverse)
    library(RColorBrewer)
    library(reshape2) #for melt
    library(EnvStats) # stat_n_text
    library(dplyr) # for %>%
}

{
    metadata <- metadata %>% filter(sampleID %in% samples_list)
    tab.n <- tab.n[,samples_list]
}
######### Melt feature table (subset.tab.n) #########################
mat <- rownames_to_column(tab.n)
mat[1:4, 1:4]
mm <- melt(mat)
head(mm)
names(mm) <- c("short.names", "sampleID", "value")
mm.metadata <- left_join(mm, metadata)
mm.metadata %>% filter(is.na(numericSampleTimeWithSick))
######################################################################


####################### ADD Diversity ################
mm.metadata <- left_join(mm.metadata, values)
#######################################################


########## Find the most abundant taxa in the cohort ############
# Assumpted that: samples in columns, bacs in rows
df <- data.frame(value = sort(rowSums(tab.n), decreasing = T))
head(df)
df$variable <- "sum"
df$group <- "totalAbundance"
df <- rownames_to_column(df)
# df <- rename(df, c("rowname" = "short.names"))
df <- rename(df, c("short.names" = "rowname"))
head(df)
higherTaxa <- df$short.names[1:NUM_HIGHER_TAXA]

# Add Higher Taxa to mm.metadata
mm.metadata$higherTaxa <-
    sapply(mm.metadata$short.names, function(x)
        ifelse(x %in% higherTaxa, x, "other"))
#######################################################

####### COUNTS ###########
n_all <- nrow(metadata)
n_cases <- sum(metadata$case_id == "AP Case")
n_control <- sum(metadata$case_id == "No AP")
######################### 


######### Order variables ################
mm.metadata$higherTaxa <-
    factor(mm.metadata$higherTaxa, levels = c(higherTaxa, "other"))
mm.metadata$lastDiet <-
    factor(mm.metadata$lastDiet,
           levels = c("Exclusively BF", "Partially BF", "Formula"))
mm.metadata$sampleTimeLabels <-
    factor(
        mm.metadata$numericSampleTimeWithSick,
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
mm.metadata$case_id_label <- 
    factor(
        mm.metadata$case_id,
        labels = c("AP Case" = paste0("Cases (n=",n_cases,")"), 
                   "No AP" = paste0("Control (n=",n_control,")"
                   )))
###########################################################

# Title
title <-
    paste0("Microbiota Composition - ",
           gsub("_", " ", COHORT),
           " (n=",
           n_all,
           ")")
subtitle <- "Number of kids added above each row"


####### Count number of kids in each column ############################
# the d data frame contain the number of each group of samples
d <- mm.metadata %>% 
    dplyr::group_by(sampleTimeLabels) %>%
    dplyr::summarise(n = n(), uniqID = length(unique(sampleID)), chao1_median = median(chao1))
head(d)
d
d$position <- 1.05


nrow(d)

sum(d$uniqID) == n_all
################################################################

groupMetadata <- left_join(mm.metadata, d)
tmp_sum <- ddply(groupMetadata,c("sampleTimeLabels","higherTaxa","sampleID","uniqID","position", "chao1_median"),summarise, sum = sum(value))
tmp_dd <- ddply(tmp_sum,c("sampleTimeLabels","higherTaxa","uniqID","position", "chao1_median"),summarise, avg = mean(sum))

head(tmp_sum)
head(tmp_dd)


######### Plot COLORS #################################################################
# Copy paste this section to consistent colors
source("Scripts/Functions/GMAPPlotsColors.R")
colors <- GMAPPaperColors()
# add this line to your ggplot
# scale_fill_manual(values = colors, breaks = c("other", rev(higherTaxa)))
#######################################################################################


g <- ggplot(tmp_dd, aes(x=sampleTimeLabels,y=avg,fill=higherTaxa))+
    geom_point(aes(y=chao1_median / 100))+
    # scale_y_continuous(sec.axis = sec_axis(~ 2*.)) +
    # scale_y_continuous(sec.axis = sec_axis(trans = ~10*.)) +
    geom_bar(position = position_fill(reverse = TRUE), stat = "identity") +
    labs(title = title,
         caption = "group range {age - 2, age + 2}",
        x = "Sample Age",
        y = "Taxa Mean (Relative Abundance)",
        fill = paste0("Higher ", NUM_HIGHER_TAXA, " taxa")
    ) +
    scale_fill_manual(values = colors,
                      breaks = c("other", rev(higherTaxa)),
                      # guide = guide_legend(reverse = F)
                      ) +
    geom_text(check_overlap = T,
              aes(x = sampleTimeLabels, y = position, label = uniqID)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1),
          panel.grid = element_blank())

g

gg <- g

# Save
{
    path <- "Figures/"
    fileName <- "CompositionPlotOverall"
    width <- 12
    height <- 6
    if (1) {
        ggsave(gg,filename = paste0(path, "PDFs/",fileName, ".pdf"), "pdf", useDingbats=FALSE,
               width = width, height = height)
        ggsave(gg,filename = paste0(path, "PNGs/",fileName, ".png"), "png",
               width = width, height = height)
    }
    
}


    
