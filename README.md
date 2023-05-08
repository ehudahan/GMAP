# Longitudinal disease-associated gut microbiome differences in infants with food protein-induced allergic proctocolitis

This repository contains the data, figures, and scripts used in the research paper "<a href="https://pubmed.ncbi.nlm.nih.gov/36138438/"> Longitudinal disease-associated gut microbiome differences in infants with food protein-induced allergic proctocolitis</a>". The paper investigates the differences in the gut microbiomes of infants with food protein-induced allergic proctocolitis (FPIAP) compared to healthy controls and provides insights into the potential causal relationship between gut microbiome dysbiosis and FPIAP.

## Data

The `data` folder contains the raw sequencing data from the gut microbiome samples collected from infants with FPIAP and healthy controls. The data has been processed and analyzed using QIIME2 and R, and the results have been compiled into the figures presented in the paper.

## Figures

The `figures` folder contains the figures presented in the paper, including bar plots, alpha and beta diversity plots, and heatmaps showing the differences in the gut microbiomes of infants with FPIAP and healthy controls over time.

## Scripts

The `scripts` folder contains the R scripts used to analyze the gut microbiome data and generate the figures presented in the paper. The scripts are organized by figure and include detailed comments to explain the code.

## UnknownVsUnclsf

In the "UnknownVsUnclsf" subproject, we aimed to explore the differences between unknown and unclassified OTUs, and investigate how to assign appropriate taxonomy names to these OTUs. 

In the context of this study, "unknown" refers to OTUs that have no match to any known sequence in the reference database, while "unclassified" refers to OTUs that match a sequence in the reference database, but could not be assigned to any known taxonomic group. 

To assign appropriate taxonomy names to these unknown and unclassified OTUs, we used a combination of BLAST searches and manual curation. The detailed methodology can be found in the "UnknownVsUnclsf" folder, which contains the relevant scripts and data files.

## Conclusion

This repository provides a comprehensive overview of the data and analysis methods used in the research paper "Longitudinal disease-associated gut microbiome differences in infants with food protein-induced allergic proctocolitis". The results of this study highlight the potential role of gut microbiome dysbiosis in the development of FPIAP and may inform future interventions to prevent or treat this condition.
