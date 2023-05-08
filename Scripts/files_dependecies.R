
try(setwd("~/GMAP/Docs/GMAP_Paper/"), silent = T)
try(setwd("/Volumes/ehudda/lab/Projects/GMAP/Docs/GMAP_Paper/"), silent = T)
try(setwd("/vol/sci/bio/data/moran.yassour/lab/Projects/GMAP/Docs/GMAP_Paper/"), silent = T)
getwd()


########
# Note: read metadata with :
# metadata <- read.delim("Results/TSVs/metadata.tsv", header=T, sep = '\t')
########

source("Scripts/FeatureTable_not_filtered.R")
source("Scripts/metadata_not_filtered.R") 
  source("Scripts/FinalSetOfSamples.R") 
    source("Scripts/metadata.R")
    source("Scripts/DemographicTable.R")
    source("Scripts/FeatureTable.R")
      source("Scripts/OneSamplePerKidPerTimePoint.R")
        # create the file distance-matrix-braycurtis.tsv unknown script)
            source("Scripts/DiversityOutput.R")
                source("Scripts/RichnessOverall.R")
                source("Scripts/CompositionPlotOverall_MY_line.R")
                source("Scripts/DynamicsBySympJaccard.R")
            source("Scripts/Figure1Numbers.R")
            source("Scripts/venn.R") 
            source("Scripts/SampleMapSubsets.R")
      source("Scripts/SubsetsLists.R")
        source("Scripts/MaAsLin2.R")
          source("Scripts/SuppTable1.R")
          source("Scripts/SuppTable1_revision.R")
            source("Scripts/AppRdata.R")
              source("Scripts/AppFunc.R")
                source("Scripts/AppDeploy.R")
            source("Scripts/BoxPlotVagBacteroides.R")
            source("Scripts/BoxPlotDietBifido.R")
            source("Scripts/BoxPlotProLacto.R")
            source("Scripts/FlagPostCases0.01.R")
            source("Scripts/FlagPostSymp0.01.R")
            source("Scripts/FlagPostCases0.025.R")
            source("Scripts/FlagPostCases0.025_revision.R")
            source("Scripts/FlagPostCases0.025_revision_anti.R")
            source("Scripts/FlagPostSymp0.025.R")
            source("Scripts/FlagPostSymp0.025_revision.R")
            source("Scripts/FlagPostSymp0.025_revision_anti.R")
            source("Scripts/FlagPostCases0.05.R")
            source("Scripts/FlagPostSymp0.05.R")
            source("Scripts/BoxPlotSympLactoEnteroPeptoClostrid.0-2.all.R")
            source("Scripts/BoxPlotSymp.Entero.Clos.Strep.Lacto.R")


source("Scripts/Functions/GMAPPlotsColors.R")
# FeatureTable_not_filtered.R
# metadata_not_filtered.R
#         finalSetOfSamples.R > metadata.R
#                             > FeatureTable.R
#                                               > SubsetsLists.R
#                                                                 > MaAsLin2.R
#                                                                               > SuppTable1.R
#                                                                                               >BoxPlotVagBacteroides.R
      