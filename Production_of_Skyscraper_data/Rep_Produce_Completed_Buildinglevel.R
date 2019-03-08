rm(list=ls())
cat("\014")
gc()


#Load data
load("vdemv61.rdata")             #Make sure this is your path to the V-Dem Data
sc <- read.csv("skyscrapers.csv") #Make sure this is your path to the output from "Clean_Skyscraperdata.R"


#Make Height-variable
sc$highest_point <- unlist(apply(sc[,grep("height.*meter",colnames(sc),value=TRUE)],1,function(x) max(x,na.rm=TRUE)))
sc$highest_point[which(sc$highest_point==-Inf)] <- NA

#Filter
completed <- sc[which(sc$completion<=2015 & sc$completion>1899),]

#Merge
completed <- merge(completed,vdem,by.x=c("country_id","completion"),by.y=c("country_id","year"),all.x=TRUE)

#Make some variables
completed$out <- ifelse(completed$status %in% c("Proposed","Vision"),1,0)
completed <- completed[which(completed$out==0),]
completed <- completed[which(is.na(completed$highest_point)==FALSE),]
completed <- completed[which(completed$highest_point>=150),]


##  Decade  ##
completed$decade <- completed$completion-1900
completed$decade <- ifelse(completed$decade %in% 100:109, 10,
                           ifelse(completed$decade > 109, 11,
                                  ifelse(completed$decade < 10, 0,
                                         as.numeric(as.character(substr(completed$decade,1,1))))))


##  9-year-period-median  ##
meandf<- data.frame(t = 1900:2015,mheight = NA, maxheight = NA)
for(i in 1:nrow(meandf)){
  temp <- completed[completed$completion %in% (meandf$t[i]-4):(meandf$t[i]+4),]
  meandf$mheight[i] <- mean(temp$highest_point)
}

for(i in 3:nrow(meandf)){
  temp <- completed[which(completed$completion < meandf$t[i]),]
  meandf$maxheight[i] <- max(temp$highest_point,na.rm=TRUE)
}

completed <- merge(completed,meandf,by.x="completion",by.y="t",all.x=TRUE)
completed$deltaheight_mean <- completed$highest_point - completed$mheight
completed$deltaheight_max  <- completed$highest_point - completed$maxheight

#Make vanity-measures
completed$vanity           <- completed$height_to_tip_meter - completed$height_occupied_meter
completed$height_gfa_ratio <- completed$height_to_tip_meter/completed$tower_gfa_m2


####    Write file    ####
save(completed, file="buildings.rdata",row.names=FALSE)
