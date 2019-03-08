rm(list=ls())
cat("\014")
gc()

#For this script to work, you need to change "YOUR_PATH" to where you have located the pages from skyscrapercenter

#List files
files <- paste0("YOUR_PATH",list.files("YOUR_PATH")[grep("wget",list.files("YOUR_PATH"),invert=TRUE)])
source("ScrapeFunction.R")


### Scrape  ###
library(parallel)
cores <- detectCores()-1


ss <- mclapply(files, function(x) Sky_Scrape(x), mc.cores=cores)




#####   Give all same columns (Must be repeated due to old code. This can be made more efficient with bind_rows (dplyr))
"%notin%" <- function(x,y)!("%in%"(x,y))

namelist <- lapply(ss, function(x) colnames(x))
namelist <- sort(unique(unlist(namelist)))
for(i in 1:length(ss)){
  ss[[i]][,namelist[namelist %notin% colnames(ss[[i]])]] <- NA
}
namelist <- lapply(ss, function(x) colnames(x))
namelist <- sort(unique(unlist(namelist)))
for(i in 1:length(ss)){
  ss[[i]][,namelist[namelist %notin% colnames(ss[[i]])]] <- NA
}
namelist <- lapply(ss, function(x) colnames(x))
namelist <- sort(unique(unlist(namelist)))
for(i in 1:length(ss)){
  ss[[i]][,namelist[namelist %notin% colnames(ss[[i]])]] <- NA
}


#Make data
full <- do.call("rbind", ss) #

#Save file
save(full, file="full.rdata")

