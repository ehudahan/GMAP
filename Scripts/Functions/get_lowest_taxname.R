####################################################
################### Get lowest taxa names ##########
####################################################

####### Ehud's version 2 #########
# old versions below
# updates:
# A. assign 'a__XXX;b__' to a__XXX_unclsfd. (Example: "k__Bacteria;p__Tenericutes;c__Mollicutes;o__;f__;g__"  --> "c__Mollicutes_unclsfd"
# B. assign 'a__XXX;__' to 'a__XXX_unknown'. (Example: "k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;__;__;__" --> "c__Gammaproteobacteria_unknwn")
get_lowest_taxaname <- function(taxanames, start_lvl=7,sep=";") {
  
  new_names <- sapply(strsplit(taxanames,sep), function(x) x[start_lvl])
  start_lvl = start_lvl - 1
  idx.orig <- c(which(nchar(new_names)==3), grep("unclassified", new_names))
  idx.orig.unknown <- c(which(new_names=="__"))
  while (length(idx <- c(which(nchar(new_names)==3),which(new_names=="__"), grep("unclassified",new_names)))) {
    idx <- c(which(nchar(new_names)==3),which(new_names=="__"), grep("unclassified",new_names))
    new_names[idx] <- sapply(strsplit(taxanames[idx],sep), function(x) x[start_lvl])
    start_lvl <- start_lvl - 1
  }
  
  new_names[idx.orig] <- paste(new_names[idx.orig],"_unclsfd",sep="")
  new_names[idx.orig.unknown] <- paste(new_names[idx.orig.unknown],"_unknwn",sep="")
  return(new_names)
}


####### Moran's version #########
# get_lowest_taxaname <- function(taxanames, start_lvl=7,sep=";") {
#   # """The pattern ;f__;g__ assigned as -unclsfd"""
#   new_names <- sapply(strsplit(taxanames,sep), function(x) x[start_lvl])
#   start_lvl = start_lvl - 1
#   idx.orig <- c(which(nchar(new_names)==3),grep("unclassified",new_names))
#   while (length(idx <- c(which(nchar(new_names)==3),grep("unclassified",new_names)))) {
#     idx <- c(which(nchar(new_names)==3),grep("unclassified",new_names))
#     new_names[idx] <- sapply(strsplit(taxanames[idx],sep), function(x) x[start_lvl])
#     start_lvl <- start_lvl - 1
#   }
#   
#   new_names[idx.orig] <- paste(new_names[idx.orig],"-unclsfd",sep="")
#   return(new_names)
# }
# 

##### Ehud's version #######
# get_lowest_taxaname2 <- function(taxanames, start_lvl=7,sep=";") {
#   # """The pattern __;__;__ assigned as -unknwn"""
#   new_names <- sapply(strsplit(taxanames,sep), function(x) x[start_lvl])
#   start_lvl = start_lvl - 1
#   idx.orig <- c(which(nchar(new_names)==3), grep("unclassified", new_names))
#   idx.orig.unknown <- c(which(new_names=="__"))
#   while (length(idx <- c(which(nchar(new_names)==3),which(new_names=="__"), grep("unclassified",new_names)))) {
#     idx <- c(which(nchar(new_names)==3),which(new_names=="__"), grep("unclassified",new_names))
#     new_names[idx] <- sapply(strsplit(taxanames[idx],sep), function(x) x[start_lvl])
#     start_lvl <- start_lvl - 1
#   }
#   
#   new_names[idx.orig] <- paste(new_names[idx.orig],"-unclsfd",sep="")
#   new_names[idx.orig.unknown] <- paste(new_names[idx.orig.unknown],"-unknwn",sep="")
#   return(new_names)
# }
# 

