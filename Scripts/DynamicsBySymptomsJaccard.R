###########################################################
##################### Dynamics by symptoms ################
##################### pre-> symtomatic ####################
##################### symptomatic -> resolved #############
###########################################################

# libraries
{ 
  library(dplyr) # for %>% 
  library(ggplot2)
  library(RColorBrewer)
  library(ggpubr)
  library(arrangements) #for combinations
}


# data
{
  rm(list=ls())
  try(setwd("~/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/Volumes/ehudda/lab/Project/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/vol/sci/bio/data/moran.yassour/lab/Projects/GMAP/Docs/GMAP_Paper/"), silent = T)
  getwd()
  metadata <- read.delim("Results/TSVs/metadata.tsv", header=T)
  samples_list <- read.csv("Results/CSVs/OneSamplePerKikPerTimePoint.csv", header = T)$x
  beta_df <- read.table("../../Data/distance-matrix-jaccard.tsv", header = T, row.names = 1)
  METHOD <- "jaccard"
  samples_raw <- readLines("Results/MaAsLin2/input/SubsetLists.csv")
  samples <- list()
  df <- data.frame(sampleID = metadata$sampleID)
  splited <- strsplit(samples_raw, ",")
  for (i in 2:length(splited)) {
    # first argument is the subset name
    samples[[splited[[i]][1]]] <- splited[[i]][-1]
    df[[splited[[i]][1]]] <- df$sampleID %in% splited[[i]]
  }
  

  if(0){
    # Here I start to pick sample with help of the models but I realize that samples appear in two different models, I go to other method.
    metadata <- metadata %>% left_join(df)
    metadata$model[metadata$`last Pre-symptoms`] <- "last Pre-symptoms"
    metadata$model[metadata$`first Symptomatic`] <- paste0(metadata$model[metadata$`first Symptomatic`], "first Symptomatic")
    metadata$model[metadata$`first Resolved`] <- "first Resolved"
    metadata$model[metadata$`last Resolved`] <- "last Resolved"
    
    # check I dont delete something
    sort(table(metadata$model))
    sort(colSums(df[,one_sample_models]))
    one_sample_models <- c("last Pre-symptoms", "first Symptomatic", "first Resolved", "last Resolved")
    summary <- metadata %>% 
      group_by(record_id) %>% 
      summarise(last_pre = sum(`last Pre-symptoms`), fir_sym = sum(`first Symptomatic`), fir_res = sum(`first Resolved`), las_res = sum(`last Resolved`))
    
    colSums(summary > 1)
    
    # filter step to remain with only one sample per period
    # After that step I can select the consequtive sample and assumethis is the only sample for this kid in this time period
    # 
    samples_to_save <- unique(unlist(samples[c("last Pre-symptoms", "first Symptomatic", "first Resolved", "last Resolved")]))
    
    samples_to_save
    sum_per_sample <- metadata %>% 
      group_by(sampleID) %>% 
      summarise(last_pre = sum(`last Pre-symptoms`), fir_sym = sum(`first Symptomatic`), fir_res = sum(`first Resolved`), las_res = sum(`last Resolved`))
    sum_per_sample
    
    mm.data3 <- mm.data2 %>% filter(sampleID %in% c(samples_to_save))
  
  
  }
}





{
  TITLE = paste0("Bacteria Dynamics Across symptoms (", METHOD, ")")
}



# gathering all sample with all pairs
{
  metadata$symptoms <- factor(metadata$symptoms, levels = c("Control","Pre-symptoms", "Symptomatic","Resolved"))
  metadata.one.per.kid <- metadata %>% filter(sampleID %in% samples_list)
  
  values <- reshape2::melt(beta_df %>% tibble::rownames_to_column())
  values$variable <- as.character(values$variable)
  mm.data <- values %>% rename(sampleID = rowname, sampleID_con = variable)
  metadata_con <- metadata.one.per.kid %>% rename(sampleID_con = sampleID, record_id_con = record_id, visit_age_mo_con = visit_age_mo, symptoms_con = symptoms)
  mm.data2 <- mm.data %>% 
    left_join(metadata.one.per.kid %>% select(sampleID, record_id, visit_age_mo, symptoms), by = "sampleID") %>%
    left_join(metadata_con %>% select(sampleID_con, record_id_con, visit_age_mo_con, symptoms_con), by = "sampleID_con")
}


# choose the consequitive sample
{
  data <- mm.data2 %>% 
    filter(record_id == record_id_con) %>% 
    mutate(age_diff = visit_age_mo_con - visit_age_mo) %>% 
    filter(age_diff > 0.0) 
  
  data$group <- paste0(data$symptoms, ":",data$symptoms_con)
  data$group <- factor(data$group, levels = c("Control:Control", "Pre-symptoms:Pre-symptoms", "Pre-symptoms:Symptomatic", 
                                              "Pre-symptoms:Resolved", "Symptomatic:Symptomatic", "Symptomatic:Resolved", "Resolved:Resolved"), ordered = T)
  # pairs <- combinations(x = c("Resolved","Symptomatic","Pre-symptoms"), k = 2, replace = TRUE, layout = list)
  # pairs
  # 
  # for (pair in pairs) {
  #   
  # }
  # pairs.data <- data %>% 
  #   group_by(sampleID) %>% 
  #   summarise(sampleID_con = first(x = sampleID_con, order_by = age_diff)) # %>% 
  
  # add to pairs all medatada
  # pairs.data.metadata <- pairs.data %>% 
  #   inner_join(data, by = c("sampleID", "sampleID_con")) %>%
  #   left_join(metadata %>% select(sampleID, symptoms, numericSampleTimeWithSick))
  # # mutate(avg_time = (visit_age_mo.x + visit_age_mo.y)/2)
}


df_n_per_group <- data %>%
  group_by(group) %>%
  summarise(n=n())

pairs.data.metadata <- left_join(data, df_n_per_group)

# data <- data.un.with.n %>% 
#   filter(n>3) # %>%
# filter(chao1<130)

# pairs.data.metadata$sampleTimeLabels <-
#   factor(
#     pairs.data.metadata$numericSampleTimeWithSick,
#     labels = c(
#       "initial",
#       "two week",
#       "one month",
#       "two month",
#       "four month",
#       "six month",
#       "nine month"
#       # "one year"
#     )
#   )
# 
# if (pairs.data.metadata %>% filter(sampleTimeLabels == "one year") %>% nrow() > 0){
#   message("You have one year sample eve should not to be")
# }

colors <- brewer.pal(9, "Set1")

plot_data <- pairs.data.metadata %>% filter(group != "Control:Control")


g <- ggplot(plot_data, color="gray", aes(x = group , y = value, 
                                                   color = group, 
                                                   fill = group
)) + 
  geom_boxplot(alpha = .4, outlier.colour = NA) +
  geom_point(alpha = .7,
             position = position_jitterdodge(jitter.width = 0.8)
  ) +
  scale_color_manual(values = colors)+
  scale_fill_manual(values = colors)+
  labs(y = paste0("Dynamics (",METHOD,")"),
       x = "Time (months)",
       title = TITLE
       
  ) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # stat_compare_means(hide.ns = T, tip.length = c(0.1,0.1),
  #                    comparisons = my_comps,
  #                    # ref.group = "Control",
  #                    label = "p", size =2)
g
gg <- g


# Add significance t-test
combs <- combinations(k =  2, v = as.character(unique(plot_data$group)),replace = F, layout = "list")
# y_max <- max(data$chao1, na.rm = T)
y_max = max(plot_data$value)
STEP <- 0.05
gg <- g + labs(caption = "P-Values calculated by Student T test") 

i <- 3
for (i in 1:length(combs)) {
  gg <- gg + stat_compare_means(comparisons = combs[i], method = "t.test", 
                                size = 2.5, label = "p.format",
                                label.y = y_max, 
                                hide.ns = T,  colour = NA)
  y_max <- y_max + STEP
}

gg


# Save
{
  path <- "Figures/Microbiome_revision/"
  fileName <- paste0("DynamicsBySymptoms", METHOD)
  if (1) {
    ggsave(gg,filename = paste0(path, "PDFs/",fileName, ".pdf"), "pdf", useDingbats=FALSE,
           width = 8, height = 8)
    ggsave(gg,filename = paste0(path, "PNGs/",fileName, ".png"), "png",
           width = 8, height = 8)
  }
  
}

