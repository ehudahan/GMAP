###########################################################
##################### Dynamics (Beta Diversity) ###########
###########################################################

# data
{
  rm(list=ls())
  try(setwd("~/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/Volumes/ehudda/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/vol/sci/bio/data/moran.yassour/lab/Projects/GMAP/Docs/GMAP_Paper/"), silent = T)
  metadata <- read.delim("Results/TSVs/metadata.tsv", header=T)
  samples_list <- read.csv("Results/CSVs/OneSamplePerKikPerTimePoint.csv", header = T)$x
  beta_df <- read.table("../../Data/distance-matrix-braycurtis.tsv", header = T, row.names = 1)
  METHOD <- "braycurtis"
}

{
  TITLE = "Bacteria Dynamics Across Time - One Sample Per Kid"
}

# libraries
{ 
  library(dplyr) # for %>% 
  library(ggplot2)
  library(RColorBrewer)
  library(ggpubr)
  library(arrangements) #for combinations
}

# gathering all sample with all pairs
{
  metadata$symptoms <- factor(metadata$symptoms, levels = c("Control","Pre-symptoms", "Symptomatic","Resolved"))
  metadata.one.per.kid <- metadata %>% filter(sampleID %in% samples_list)
  
  values <- reshape2::melt(beta_df %>% rownames_to_column())
  values$variable <- as.character(values$variable)
  mm.data <- values %>% rename(sampleID = rowname, sampleID_con = variable)
  metadata_con <- metadata.one.per.kid %>% rename(sampleID_con = sampleID, record_id_con = record_id, visit_age_mo_con = visit_age_mo)
  mm.data2 <- mm.data %>% 
    left_join(metadata.one.per.kid %>% select(sampleID, record_id, visit_age_mo), by = "sampleID") %>%
    left_join(metadata_con %>% select(sampleID_con, record_id_con, visit_age_mo_con), by = "sampleID_con")
}

# choose the consequitive sample
{
  data <- mm.data2 %>% 
    filter(record_id == record_id_con) %>% 
    mutate(age_diff = visit_age_mo_con - visit_age_mo) %>% 
    filter(age_diff > 0.0) 
  
  pairs.data <- data %>% 
    group_by(sampleID) %>% 
    summarise(sampleID_con = first(x = sampleID_con, order_by = age_diff)) # %>% 
  
  # add to pairs all medatada
  pairs.data.metadata <- pairs.data %>% 
    inner_join(data, by = c("sampleID", "sampleID_con")) %>%
    left_join(metadata %>% select(sampleID, symptoms, numericSampleTimeWithSick))
    # mutate(avg_time = (visit_age_mo.x + visit_age_mo.y)/2)
}
  
  
  df_n_per_group <- pairs.data.metadata %>%
    group_by(symptoms, numericSampleTimeWithSick) %>%
    summarise(n=n())
  
  pairs.data.metadata <- left_join(pairs.data.metadata, df_n_per_group)
  
  # data <- data.un.with.n %>% 
  #   filter(n>3) # %>%
  # filter(chao1<130)
  
  pairs.data.metadata$sampleTimeLabels <-
    factor(
      pairs.data.metadata$numericSampleTimeWithSick,
      labels = c(
        "initial",
        "two week",
        "one month",
        "two month",
        "four month",
        "six month",
        "nine month"#,
        # "one year"
      )
    )



colors <- brewer.pal(9, "Set1")


g <- ggplot(pairs.data.metadata, color="gray", aes(x = symptoms , y = value, 
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
  labs(y = paste0("Dynamics (",METHOD,")"),
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
combs <- combinations(k =  2, v = as.character(unique(pairs.data.metadata$symptoms)),replace = F, layout = "list")
# y_max <- max(data$chao1, na.rm = T)
y_max = 1
STEP <- 0.1
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
  path <- "Figures/revision/"
  fileName <- "DynamicsBySympBrayCurtis"
  if (1) {
    ggsave(gg,filename = paste0(path, "PDFs/",fileName, ".pdf"), "pdf", useDingbats=FALSE,
           width = 8, height = 5)
    ggsave(gg,filename = paste0(path, "PNGs/",fileName, ".png"), "png",
           width = 8, height = 5)
  }
  
}

