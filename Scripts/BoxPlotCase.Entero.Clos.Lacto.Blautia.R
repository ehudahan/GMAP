################################################################################
############################# Box Plot Lacto Case  #############################
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
  
  gg_list <- list()
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
    title <- "Control vs Cases"
    subtitle <- "From Flag post panel"
    bacs <- c("o__Clostridiales_unknwn", 
              # "g__Streptococcus",
              "f__Enterobacteriaceae_unclsfd",
              "g__Lactobacillus",
              # "f__Enterobacteriaceae_unknwn",
              # "f__Peptostreptococcaceae_unclsfd", 
              # "f__Clostridiaceae_unclsfd",
              "g__Blautia"
              )
    facet_var <- "Bac"
  }
  
  #colors
  {
    source("Scripts/Functions/GMAPPlotsColors.R")
    colors <- colorGMAPDataFunction(varTitle = "feature")
  }
  
  # filtering data
  {
    data <- melt(tab.n %>% rownames_to_column())
    head(data)
    names(data) <- c("Bac", "sampleID", "Abundance")
    metadata$symptoms <- factor(metadata$symptoms, levels = c("Control","Pre-symptoms", "Symptomatic","Resolved"))
    metadata$zero_to_two <- metadata$sampleID %in% samples$`samples 0-2 Model`
    metadata$last_Pre_symptoms <- metadata$sampleID %in% samples$`last Pre-symptoms`
    metadata$first_Symptomatic <- metadata$sampleID %in% samples$`first Symptomatic`
    metadata$first_Resolved <- metadata$sampleID %in% samples$`first Resolved`

    mm.data <- left_join(data, metadata) 
  }
  
  
  # arrange data
  {
    
    data_facet_var <- mm.data%>% 
      filter(Bac %in% bacs)# %>% 

    # filter(!is.na(!!!facet_var))
    
    # CHECK WHY I HAVE NAs?
    ## Add The AST transformation ##
    source("Scripts/Functions/Arc-Sine_Square_Root_Transformation.R")
    data_facet_var$AbundanceAST <- AST(data_facet_var$Abundance)
    ##
    
    data_facet_var[1:4,]
  }
  
  
  
  # for (subset_name in names(samples_list_per_model)) {
  # names(samples_list_per_model)
  # subset_name <- "0-2 months"
  
  # Plot 1
  # subset_name <- names(samples_list_per_model)[1]
  
  # # remove outliers
  # n_removed <- data_facet_var %>% filter(AbundanceAST >= 1) %>% nrow()
{
  
  data_facet_var$Subset <- case_when(data_facet_var$last_Pre_symptoms & data_facet_var$Bac == "f__Enterobacteriaceae_unclsfd" ~ "last Pre-symptoms",
                             data_facet_var$first_Symptomatic & data_facet_var$Bac %in% c("o__Clostridiales_unknwn", "f__Enterobacteriaceae_unclsfd") ~ "first Symptomatic",
                             data_facet_var$first_Resolved & data_facet_var$Bac %in% c("g__Lactobacillus", "g__Blautia") ~ "first Resolved",
                             # data_facet_var$zero_to_two & data_facet_var$Bac %in% c("o__Clostridiales_unknwn",
                             #                                                        "f__Enterobacteriaceae_unknwn",
                             #                                                        "f__Clostridiaceae_unclsfd",
                             #                                                        "f__Enterobacteriaceae_unclsfd") ~ "samples 0-2 Model",
                             TRUE ~ "")

   data <- data_facet_var[which(data_facet_var$Subset != ""),]
}
  
  # RColorBrewer::display.brewer.all()
  
  g <- ggplot(data, aes(x = symptoms, y = AbundanceAST)) +
    geom_point(aes(color = Bac),alpha = 0.5, size = 1, position = position_jitterdodge(jitter.width = 0.6, seed = 1)) +
    geom_boxplot(aes(fill = Bac),alpha = 0.8, outlier.size = 0, outlier.alpha = 0) +
    facet_wrap(Subset~get(facet_var) , scales = "free", ncol = 5) +
    scale_fill_manual(values = brewer.pal(9, "Set1")) +
    scale_color_manual(values = brewer.pal(9, "Set1")) +
    # scale_y_continuous(labels = scales::percent_format(), breaks = c(0,0.25,0.5,0.75,1), limits = c(0,1.4)) +
    # scale_y_continuous(limits = c(0, max(data$AbundanceAST) + 0.25)) +
    theme_classic() + 
    labs(title = title,
         subtitle = subtitle,
         # subtitle = paste0(n_removed," outliers were removed manualy"),
         y = paste0("Relative Abundance (AST)")) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor=element_blank(), 
          axis.text.x = element_text(angle = 45, hjust = 1),  
          axis.title.y = element_text(size = 10))
  
  g
  
  
  
  {
    unique(maaslin_results$Measured.Variable)
    unique(maaslin_results$Model.Name)
    unique(maaslin_results$Subset)
    
    head(maaslin_results)
    bacs
    maaslin_results_case <- maaslin_results %>%
      filter(Subset != "samples 0-2 Model") %>% 
      filter(Feature %in% bacs) %>%
      filter(Model.Name == "case lastDiet pro") %>%
      filter(Measured.Variable  == "case_id")
# 
#     maaslin_results_case
#     maaslin_results_symp_control_as_ref <- maaslin_results %>%
#       filter(Subset == "samples 0-2 Model") %>%
#       filter(Model.Name == "symptoms lastDiet pro (Control as reference)") %>%
#       filter(Measured.Variable == "symptoms") %>%
#       filter(Feature %in% bacs)
# 
#     maaslin_results_symp_symptoms_as_ref <- maaslin_results %>%
#       filter(Subset == "samples 0-2 Model") %>%
#       filter(Model.Name == "symptoms lastDiet pro (Symptomatic as reference)") %>%
#       filter(Measured.Variable == "symptoms") %>%
#       filter(Feature %in% bacs)
#     
#     maaslin_results_symp <- rbind(maaslin_results_symp_control_as_ref, maaslin_results_symp_symptoms_as_ref)
    # rbind(maaslin_results_symp, maaslin_results_case)
    
    # maaslin_results_symp <- rbind(maaslin_results_symp, maaslin_results_case)
    stat_table <- maaslin_results_case %>%
      mutate("value" = "Abundance") %>%
      select(Feature, value, Test.Variable, Reference.Variable, pval, qval, coef, Subset)
    
    names(stat_table) <- c("Bac", ".y.", "group1","group2","p", "q", "c", "Subset")
    stat_table$group1 <- case_when(stat_table$Subset == "last Pre-symptoms" ~ "Pre-symptoms",
                                   stat_table$Subset == "first Symptomatic" ~ "Symptomatic",
                                   stat_table$Subset == "first Resolved" ~ "Resolved",
                                   TRUE ~ "NA")
    stat_table$group2 <- "Control"
    # create nicer format for long numbers
    stat_table$p_nice <- scales::scientific(  x = stat_table$p,digits = 3)
    stat_table$q_nice <- scales::scientific(  x = stat_table$q,digits = 3)
    stat_table$c_nice <- scales::scientific(  x = stat_table$c,digits = 3)
    
    # add position
    # find the highest dot for each bac
    max_values_per_symp <- sapply(bacs, function(x) sort(data$AbundanceAST[which(data$Bac == x)], decreasing = T)[3])
    # # order the plot from lowest to highest
    # data_vag$Bac <- factor(data_vag$Bac, levels = names(sort(max_values_per_delivery_mode)))
    
    STEP <- 0
    stat_table$y.position <- sapply(stat_table$Bac, function(x) max_values_per_symp[x] + STEP)
    stat_table$Bac <- factor(stat_table$Bac, levels = names(sort(max_values_per_symp)))


    gg <- g + stat_pvalue_manual(stat_table,label = paste0(#"p={p_nice}\n",
                                                           "q={q_nice}\n",
                                                           "c={c_nice}"),size = 2)
                                 # step.increase = 0.1, 
                                 # step.group.by = "Bac")
    gg
  }
  




# Save
{
  path <- "Figures/"
  fileName <- "BoxPlotCase.Entero.Clos.Blautia"
  width <- 12
  height <- 6
  if (1) {
    ggsave(gg,filename = paste0(path, "PDFs/",fileName, ".pdf"), "pdf", useDingbats=FALSE,
           width = width, height = height)
    ggsave(gg,filename = paste0(path, "PNGs/",fileName, ".png"), "png",
           width = width, height = height)
  }
  
}

