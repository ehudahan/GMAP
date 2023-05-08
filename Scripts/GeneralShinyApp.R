# To deploy app, first save all data include shiny app function in the Rdata file


# load libraries
library(shiny)
library(shinydashboard)
library(plotly)
library(shinyWidgets)
library(scales)
library(RColorBrewer)
library(dplyr)
library(tibble)
library(reshape2)
# library(Cairo)
library(ggpubr)
library(arrangements) #for combinations



# load data
LAST_UPDATE <- as.character(file.info("Scripts/DeployApp.R")$mtime)
load("EasyMAP.Rdata")


# launch app
  ShinyAppFunction(metadata, 
                   maaslin_results, 
                   tab.ast, 
                   samples, 
                   colorGMAPDataFunction,
                   LAST_UPDATE)
  