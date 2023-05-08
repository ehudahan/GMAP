library(RColorBrewer)
colorGMAPDataFunction <- function(varTitle) {
  '
  @param varTitle - get one of "case_id", "symptoms", "lastDiet", "feature", "higherTaxa"
  
  In general this function called from ggplot with the function scale_fill_manual (example at the end od comments)
  @input - var is variable title to plot (as symptoms, feature, etc)
  @output - a list of color (any format that ggplot can hanlde with)
  
  example:
  ggplot(data, (x = age, y = value, fill = label, color = label)) +
  geom_point()+
    scale_fill_manual(input$colored_by, values = )+
    scale_color_manual(input$colored_by, values = )+

  '
  all_bacteria <- c("other",
                "g__Haemophilus",
                "g__Bifidobacterium", 
                "g__Bacteroides", 
                "f__Enterobacteriaceae_unclsfd", 
                "o__Clostridiales_unknwn", 
                "g__[Ruminococcus]",    
                "f__Enterobacteriaceae_unknwn", 
                "g__Blautia",
                "f__Lachnospiraceae_unclsfd",
                "g__Klebsiella","g__Lactobacillus", "g__Clostridium", "g__Veillonella", "g__Streptococcus", 
                "g___Ruminococcus_", "g__Parabacteroides", 
                "f__Clostridiaceae_unclsfd", "f__Peptostreptococcaceae_unclsfd", 
                 "f__Erysipelotrichaceae_unclsfd", 
                "g__Akkermansia", "g__Enterococcus", "g__Staphylococcus", "g__Faecalibacterium", 
                "g__Citrobacter", "g__Dorea", "g__Actinomyces", "g__Roseburia", 
                "g__Lachnospira", "g__Coprococcus", "f__Rikenellaceae_unclsfd", 
                "g___Eubacterium_", "g__Oscillospira", "f__Ruminococcaceae_unclsfd", 
                "g__Serratia", "g__Sutterella", 
                "g__Prevotella", "g__Anaerococcus", "g__Collinsella", "g__Ruminococcus", 
                "g__Aggregatibacter", "g__Erwinia", 
                "g__Megasphaera", "g__Salmonella", "g__Finegoldia", "g__Enterobacter", 
                "f__Lachnospiraceae_unknwn", "g__Megamonas", "g__Peptoniphilus", 
                "g__Trabulsiella", "g__Coprobacillus", "o__Clostridiales_unclsfd", 
                "k__Bacteria_unknwn", "g__Epulopiscium", "g__Fusobacterium", 
                "g__SMB53", "g__Eggerthella", "g__Corynebacterium", "g__Rothia", 
                "g__Paenibacillus", "g__Dysgonomonas", "g__Actinobacillus", "g__Phascolarctobacterium", 
                "g__Campylobacter", "f__Coriobacteriaceae_unclsfd", "g__Proteus", 
                "g__Bilophila", "g__Acidaminococcus", "f___Barnesiellaceae__unclsfd", 
                "g__Varibaculum", "g__Christensenella", "g__Pseudoramibacter_Eubacterium", 
                "g__Dialister", "f__Gemellaceae_unknwn", "f__Ruminococcaceae_unknwn", 
                "g__Atopobium", "g__Morganella", "f__Pasteurellaceae_unknwn", 
                "g__Neisseria", "Unassigned_unknwn", "g__Peptostreptococcus", 
                "g__Butyricimonas", "g__Propionibacterium", "f__Fusobacteriaceae_unclsfd", 
                "f__Lactobacillaceae_unclsfd", "g__Pseudomonas", "o__Lactobacillales_unknwn", 
                "g__Catenibacterium", "g__Odoribacter", "g__Anaerotruncus", "g__Scardovia", 
                "g__Turicibacter", "g__Providencia", "c__Bacilli_unknwn", "g__Lactococcus", 
                "g__Porphyromonas", "g__Sarcina", "f__Carnobacteriaceae_unknwn", 
                "g__Dickeya", "g__Paraprevotella", "f__Aeromonadaceae_unclsfd", 
                "g__Pediococcus")
  
  
  
  l <- length(bacteria)
  PAIRED_NUM <- 10
  SET3_NUM <- 10
  num_of_repeats <- round(l / SET3_NUM)
  
  colorsForBacs <- c("gray",rev(brewer.pal(PAIRED_NUM,"Paired")), 
                     rep(length.out = l - PAIRED_NUM,  brewer.pal(SET3_NUM,"Set3"),times = num_of_repeats))
  
  names(colorsForBacs) <- bacteria
  
  
  # Colors to choose from:
  colors <- brewer.pal(9, "Set1")
  
  
  # groups of variablesTitles and variables
  colors_groups <- list("case_id" = colors[c(1,2)],
                        "symptoms" = c("Control" = colors[2], 
                                       "Pre-symptoms" = colors[5], 
                                       "Resolved" = colors[3], 
                                       "Symptomatic" = colors[1], 
                                       "NA" = "grey"),
                        "lastDiet" = c("Formula" = colors[7],
                                       "Partially BF" = colors[8],
                                       "Exclusively BF" = colors[9]),
                        "feature" = colorsForBacs,
                        "higherTaxa" = colorsForBacs)
  
  return(colors_groups[[varTitle]])
}


GMAPPaperColors <- function(){
  paper_bacs <- c("other",
                  "f__Clostridiaceae_unclsfd",
                  "f__Enterobacteriaceae_unclsfd",
                  "o__Clostridiales_unknwn", 
                  "g__Bacteroides",
                  "g__Lactobacillus",
                  "g__Streptococcus",
                  "f__Enterobacteriaceae_unknwn", 
                  "g__Blautia",
                  "g__[Ruminococcus]",
                  "g__Parabacteroides",
                  "g__Haemophilus","g__Bifidobacterium", 
                  
                  "f__Lachnospiraceae_unclsfd",
                  "g__Klebsiella",
                 "g__Enterococcus",  
                 "g__Clostridium", "g__Veillonella",
                 "g___Ruminococcus_",  
                 "f__Peptostreptococcaceae_unclsfd", 
                 "f__Erysipelotrichaceae_unclsfd", 
                  "g__Citrobacter",  "g__Actinomyces", "g__Roseburia", 
                  "g__Lachnospira", "g__Coprococcus", "f__Rikenellaceae_unclsfd", 
                  "g___Eubacterium_", "g__Oscillospira", "f__Ruminococcaceae_unclsfd", 
                  "g__Serratia", "g__Sutterella", 
                  "g__Prevotella", "g__Anaerococcus", "g__Collinsella", "g__Ruminococcus", 
                  "g__Aggregatibacter", "g__Erwinia", 
                  "g__Megasphaera", "g__Salmonella", "g__Finegoldia", "g__Enterobacter", 
                  "f__Lachnospiraceae_unknwn", "g__Megamonas", "g__Peptoniphilus", 
                  "g__Trabulsiella", "g__Coprobacillus", "o__Clostridiales_unclsfd", 
                  "k__Bacteria_unknwn", "g__Epulopiscium", "g__Fusobacterium", 
                  "g__SMB53", "g__Eggerthella", "g__Corynebacterium", "g__Rothia", 
                  "g__Paenibacillus", "g__Dysgonomonas", "g__Actinobacillus", "g__Phascolarctobacterium", 
                  "g__Campylobacter", "f__Coriobacteriaceae_unclsfd", "g__Proteus", 
                  "g__Bilophila", "g__Acidaminococcus", "f___Barnesiellaceae__unclsfd", 
                  "g__Varibaculum", "g__Christensenella", "g__Pseudoramibacter_Eubacterium", 
                  "g__Dialister", "f__Gemellaceae_unknwn", "f__Ruminococcaceae_unknwn", 
                  "g__Atopobium", "g__Morganella", "f__Pasteurellaceae_unknwn", 
                  "g__Neisseria", "Unassigned_unknwn", "g__Peptostreptococcus", 
                  "g__Butyricimonas", "g__Propionibacterium", "f__Fusobacteriaceae_unclsfd", 
                  "f__Lactobacillaceae_unclsfd", "g__Pseudomonas", "o__Lactobacillales_unknwn", 
                  "g__Catenibacterium", "g__Odoribacter", "g__Anaerotruncus", "g__Scardovia", 
                  "g__Turicibacter", "g__Providencia", "c__Bacilli_unknwn",
                  "g__Porphyromonas", "g__Sarcina", "f__Carnobacteriaceae_unknwn", 
                  "g__Dickeya", "g__Paraprevotella", "f__Aeromonadaceae_unclsfd", 
                  "g__Pediococcus"
                  )
  GreysBacs <- c("g__Faecalibacterium",
                 "g__Akkermansia", 
                 "f__Bifidobacteriaceae_unknwn",
                 "g__Staphylococcus",
                 "g__Dorea",
                 "g__.Eubacterium.")
  sort(table(c(paper_bacs, GreysBacs)))
  l <- length(paper_bacs)
  lg <- length(GreysBacs)
  i <- 8
  paper_bacs_colors <- c("grey", brewer.pal(i, "Set1"), 
                         rep(length.out = (l - i - 1),  
                             brewer.pal(10,"Set3"),
                             times = l,
                             ), brewer.pal(9, "Greys")[2:(lg+1)])
  length(paper_bacs_colors)
  names(paper_bacs_colors) <- c(paper_bacs, GreysBacs)
  return(paper_bacs_colors)
}
