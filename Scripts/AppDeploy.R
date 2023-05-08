
if (1) {
  rm(list = ls())
  try(setwd("~/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/Volumes/ehudda/GMAP/Docs/GMAP_Paper/"), silent = T)
  try(setwd("/vol/sci/bio/data/moran.yassour/lab/Projects/GMAP/Docs/GMAP_Paper/"), silent = T)
  getwd()
  source("Scripts/AppRdata.R")
  source("Scripts/AppFunc.R")
  LAST_UPDATE <- as.character(file.info("Scripts/AppFunc.R")$mtime)
}


#Create new folder with relevant files to deploy the app:
setwd("Scripts/shiny/")
app.ver.name <- paste0("EasyMAP")
dir.create(app.ver.name,recursive = T,showWarnings = F)
save.image(file = paste0(app.ver.name, "/EasyMAP.Rdata"))
# remove "rsconnect" folder if you get HTTP error
file.copy("../GeneralShinyApp.R", to = paste0(app.ver.name, "/app.R"), overwrite = T)
rsconnect::deployApp(app.ver.name, forceUpdate = T)
