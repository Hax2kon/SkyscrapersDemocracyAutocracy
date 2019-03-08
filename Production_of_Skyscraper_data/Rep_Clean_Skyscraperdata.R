### CLEAN DATA ####
rm(list=ls())
cat("\014")
gc()
load("full.rdata") #Path to where you saved the output from "Rep_Produce_Skyscraperdata.R"


full[full==""] <- NA
colnames(full) <- gsub("(not specified)","not_specified",colnames(full),fixed=TRUE)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
#Remove empty columns


for(j in colnames(full)){
  if(sum(is.na(full[,j]))==nrow(full)){
    full[,j] <- NULL
  }
}

####################
### Fix Columns ####
####################
full <- full[,setdiff(colnames(full),setdiff(grep("^Floors Above Ground.",colnames(full),value=TRUE),c("Floors Above Ground.1","Floors Above Ground.2")))]
full <- full[,setdiff(colnames(full),setdiff(grep("^est Floors Above Ground.",colnames(full),value=TRUE),c("est Floors Above Ground.1","est Floors Above Ground.2")))]
full <- full[,setdiff(colnames(full),setdiff(grep("^Floors Below Ground.",colnames(full),value=TRUE),c("Floors Below Ground.1","Floors Below Ground.2")))]
full[,"Tower GFA m   ftNumber of ApartmentsNumber of Parking Spaces"] <- NULL
full[,"Number of ElevatorsTop Elevator Speed ms"] <- NULL


##################
### Elevators ####
##################
#Number of
full$elevators <- full$`Number of Elevators`
full$elevators <- ifelse(is.na(full$elevators)==TRUE,full[,"# of Elevators"],full$elevators)
full$`Number of Elevators` <- NULL
full[,"# of Elevators"] <- NULL
full$`Number of Elevators.1` <- NULL



#Speed
full$elevator_speed_ms <- full$ms
full$elevator_speed_ms <- ifelse(is.na(full$elevator_speed_ms)==TRUE,full$`Top Elevator Speed ms`,full$elevator_speed_ms)
full$elevator_speed_ms <- ifelse(is.na(full$elevator_speed_ms)==TRUE,full$ms,full$elevator_speed_ms)
full$`Top Elevator Speed ms` <- NULL
full$`Top Elevator Speed` <- NULL
full$ms <- NULL
full$elevator_speed_ms <- as.numeric(as.character(trim(gsub("/","",full$elevator_speed_ms))))



#Material supplier
full$elevator_material_supplier <- full$`Material Supplier_subcat_Elevator`
full[,grep("Material.*Elevator",colnames(full),value=TRUE)] <- NULL


#Consultant
colnames(full)[grep("Elevator",colnames(full))] <- "elevator_consultant"


###################
### Apartments ####
###################

#Number of
full$apartments <- full$`Number of Apartments`
full$apartments <- ifelse(is.na(full$apartments)==TRUE,full[,"# of Apartments"],full$apartments)
full$`Number of Apartments` <- NULL
full[,"# of Apartments"] <- NULL

##############
### Rooms ####
##############
full$hotel_rooms <- full$`Number of Hotel Rooms`
full[,c("Number of Hotel Rooms","Number of Hotel Rooms.1")] <- NULL

###############
### Height ####
###############

#To tip
full$height_to_tip <- full$`Height To Tip`
full$height_to_tip <- ifelse(is.na(full$height_to_tip)==TRUE,full[,"Height: To Tip"],full$height_to_tip)
full$`Height: To Tip` <- NULL
full$`Height To Tip` <- NULL


#Architectual
full$height_architectual <- full$`Height Architectural.1`
full$height_architectual <- ifelse(is.na(full$height_architectual)==TRUE,full[,"Height Architectural"],full$height_architectual)
full$height_architectual <- ifelse(is.na(full$height_architectual)==TRUE,full[,"Height Architectural.2"],full$height_architectual)
full$height_architectual <- ifelse(is.na(full$height_architectual)==TRUE,full[,"Height: Architectural"],full$height_architectual)
full$height_architectual <- ifelse(is.na(full$height_architectual)==TRUE,full[,"est Height Architectural.1"],full$height_architectual)
full[,grep("Height Architec",colnames(full),value=TRUE)] <- NULL
full$`Height: Architectural` <- NULL


#Occupied
full$height_occupied <- full$`Height Occupied`
full[,grep("Height Occupied",colnames(full),value=TRUE)] <- NULL

#Observatory
full$height_observatory <- full$`Height Observatory`
full[,grep("Height Observat",colnames(full),value=TRUE)] <- NULL


#Helipad
full$height_helipad <- full$`Height Helipad`
full[,grep("Height Heli",colnames(full),value=TRUE)] <- NULL

### Clean ###

heights <- grep("height",colnames(full),value=TRUE)
for(j in heights){
  full[,j] <- gsub("+","",gsub(",","",full[,j],fixed=TRUE),fixed=TRUE)
}
for(j in heights){
  full[,paste0(j,"_meter")] <- as.numeric(trim(gsub("/.*","",full[,j])))
}
for(j in heights){
  full[,paste0(j,"_feet")] <- as.numeric(trim(gsub(".*/","",full[,j])))
}
full[,heights] <- NULL
rm(heights)


###############
### Floors ####
###############

#Above ground
full$floors_above <- ifelse(is.na(full$`Floors Above Ground`)==FALSE,full$`Floors Above Ground`,full$`Floors Above Ground.1`)
full[,grep("Floors Above",colnames(full),value=TRUE)] <- NULL



#Above ground
full$floors_below <- ifelse(is.na(full$`Floors Below Ground`)==FALSE,full$`Floors Below Ground`,full$`Floors Below Ground.1`)
full[,grep("Floors Below",colnames(full),value=TRUE)] <- NULL

###################
### Fix Spaces ####
###################
colnames(full) <- gsub(" ","_",colnames(full))


#################################
### Remove est and estimated ####
#################################
full[,grep("est",colnames(full),value=TRUE)] <- NULL


############
### GFA ####
############

colnames(full)[which(colnames(full)=="m")] <- "tower_gfa"
colnames(full)[which(colnames(full)=="m.1")] <- "development_gfa"
full$tower_gfa <- ifelse(is.na(full$tower_gfa)==TRUE,full$Tower_GFA_m,full$tower_gfa)
full$development_gfa <- ifelse(is.na(full$development_gfa)==TRUE,full$Development_GFA_m,full$development_gfa)
full[,grep("GFA",colnames(full),value=TRUE)] <- NULL


gfa <- grep("gfa",colnames(full),value=TRUE)

for(j in gfa){
  full[,paste0(j,"_m2")] <- as.numeric(as.character(trim(gsub("/.*","",gsub("[^0-9/]","",full[,j])))))
  full[,paste0(j,"_ft2")] <- as.numeric(as.character(trim(gsub(".*/","",gsub("[^0-9/]","",full[,j])))))
}
full[,gfa] <- NULL
rm(gfa)

################
### parking ####
################
full$parking_spaces <- full$Number_of_Parking_Spaces
full$Number_of_Parking_Spaces <- NULL

################
### Address ####
################
full$address <- ifelse(is.na(full$`Street_Address_&_Map`)==FALSE,full$`Street_Address_&_Map`,full$Street_Address)
full[,c("Street_Address_&_Map","Street_Address")] <- NULL

#################
### Retrofit ####
#################
full$Retrofit_Start <- ifelse(is.na(full$Retrofit_Start)==TRUE,full$Retrofit,full$Retrofit_Start)
full$Retrofit <- NULL


###############
### Region ####
###############
full$region <- trim(trim(gsub("Tallest|in","",gsub("[#0-9]","",full$Regional_Ranking))))


#############
### Rank ####
#############
ranks <- grep("Rank",colnames(full),value=TRUE)
for(j in ranks){
  full[,j] <- as.numeric(as.character(trim(gsub("[^0-9]","",full[,j]))))
}
rm(ranks)


############################
### Lower case colnames ####
############################
colnames(full) <- tolower(colnames(full))
library(countrycode)
full$country_iso3c <- countrycode(full$country,"country.name","iso3c")
full$country_iso3c <- ifelse(full$country=="Kosovo","UNK",full$country_iso3c)
full$country_cown <- countrycode(full$country,"country.name","cown")


identifiers <- c("country","country_iso3c","country_cown","city","official_name","name_of_complex","other_names","address")
rank <- c("global_ranking","regional_ranking","national_ranking","city_ranking")
lifecycle <- c("status","proposed","construction_start","completion","building_function","current_function","original_function","retrofit_start","retrofit_end","recladding")
rest <- setdiff(colnames(full),c(identifiers,rank,lifecycle))

full <- full[order(full$country,full$completion),c(identifiers,rank,lifecycle,rest)]


#################################################################
###           Remove Duplicates and test-buildings            ###
###  Only situations where everything but URL is duplicated!  ###
#################################################################
full$missing <- rowSums(is.na(full))
full <- full[order(full$official_name,full$missing),]
full <- full[grep("test ",full$official_name,invert=TRUE),]
full <- full[grep("Test Building",full$official_name,invert=TRUE),]
full <- full[grep("^Test Tower",full$official_name,invert=TRUE),]
full <- full[!duplicated(full[,setdiff(colnames(full),"url")]),]
full <- full[which(is.na(full$country)==FALSE),]
full <- full[which(full$official_name!="Q1"),]


################
### Numeric ####
################
numerics <- c("proposed","construction_start","completion",grep("retrofit",colnames(full),value=TRUE),"recladding",
              grep("floors",colnames(full),value=TRUE),grep("height",colnames(full),value=TRUE),"parking_spaces","apartments",
              "elevators","elevator_speed_ms",grep("gfa",colnames(full),value=TRUE),rank)

for(j in numerics){
  full[,j] <- as.numeric(trim(as.character(full[,j])))
}

#############################
##### Recode countries  #####
#############################
cr <- read.csv("CountryRecodeFile.csv") #Make sure this is the path to this file.

full$country_orig <- full$country

full$country <- ifelse(full$country=="Gibraltar","United Kingdom",
                       ifelse(full$country=="Aruba","Netherlands",
                              ifelse(full$country=="Greenland","Denmark",
                                     ifelse(full$country=="Martinique","France",
                                            ifelse(full$country=="New Caledonia","France",
                                                   ifelse(full$country=="Northern Mariana Islands","United States",
                                                          ifelse(full$country=="Puerto Rico","United States",full$country)))))))

links <- setdiff(colnames(cr),"country")

full <- merge(full,cr,by="country")


#################
####  Order  ####
#################
full <- full[order(full$country,full$completion),c(identifiers,links,rank,lifecycle,rest,"country_orig")]


#####################
#### Write data  ####
#####################
write.csv(full, file="skyscrapers.csv", row.names=FALSE)

