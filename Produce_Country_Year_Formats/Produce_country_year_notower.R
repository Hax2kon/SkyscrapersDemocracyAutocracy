rm(list=ls())
cat("\014")
gc()

load("buildings.rdata")

remove <- c("chimney","bridge","industrial",
            "telecommunications / observation",
            "observation / telecommunications",
            "telecommunications")

sc <- sc[!sc$building_function %in% remove,]

#####   \\    -- Prepare SkyScraperData  --    //    #####

#Remove Visions and other unrealistic projects
sc$out <- ifelse(sc$status %in% c("Proposed","Vision","Never Completed","On Hold"),1,0)
sc <- sc[which(sc$out==0),]
sc$year <- sc$completion


#Make Height-variable and floor variable
sc$highest_point <- unlist(apply(sc[,grep("height.*meter",colnames(sc),value=TRUE)],1,function(x) max(x,na.rm=TRUE)))
sc$highest_point[which(sc$highest_point==-Inf)] <- NA


sc <- sc[which(is.na(sc$highest_point)==FALSE),] #Buildings with unknown height. Seems to be <150m

sc$floors_below2 <- ifelse(is.na(sc$floors_above)==FALSE & is.na(sc$floors_below)==TRUE,0,sc$floors_below)
sc$floors <- sc$floors_above + sc$floors_below2

#Make vanitymeasures
sc$vanity <- sc$highest_point - sc$height_occupied_meter
sc$vanitymissing <- (-1*is.na(sc$vanity))+1
sc$vanitypercent <- (sc$vanity/sc$highest_point)*100
sc$height_gfa_ratio <- sc$highest_point/(sc$tower_gfa_m2/1000)
sc$tower_gfa_m2_nodev_notower <- ifelse(is.na(sc$name_of_complex)==TRUE,sc$tower_gfa_m2,NA)

remove <- c("chimney","bridge","industrial",
            "telecommunications / observation",
            "observation / telecommunications",
            "telecommunications")
sc$tower_gfa_m2_nodev_notower <- ifelse(sc$building_function %in% remove,NA,sc$tower_gfa_m2_nodev_notower)
sc$height_gfa_ratio_nodev_notower <- sc$highest_point/sc$tower_gfa_m2_nodev_notower

#####    Remove known duplicates   #####
sc$out <- 0
sc$out <- ifelse(sc$official_name=="Tokyo Sky Tree" & sc$status=="Under Construction",1,0)
sc <- sc[which(sc$out==0),]



#####  Foreign Territories in China  #####
sc$city <- as.character(sc$city)
sc$country <- as.character(sc$country)
sc$country <- ifelse(is.na(sc$city)==TRUE,as.character(sc$country),
                     ifelse(sc$city=="Hong Kong",sc$city,as.character(sc$country)))
sc$country_id[which(sc$country=="Hong Kong")] <- NA



#####   Recode Yugoslavia- and USSR-countries, Czech Republic, and Hong Kong (Follows V-Dem's country definition)   #####

#Yugoslavia
sc$country_id[which(sc$country=="Kosovo" & sc$year<1999)] <- 198
sc$country_id[which(sc$country=="Bosnia and Herzegovina" & sc$year<1992)] <- 198
sc$country_id[which(sc$country=="Croatia" & sc$year %in% 1945:1991)] <- 198
sc$country_id[which(sc$country=="Macedonia" & sc$year <1991)] <- 198
sc$country_id[which(sc$country=="Slovenia" & sc$year <1989)] <- 198

sc$country[which(sc$country=="Kosovo" & sc$year<1999)] <- "Serbia"
sc$country[which(sc$country=="Bosnia and Herzegovina" & sc$year<1992)] <- "Serbia"
sc$country[which(sc$country=="Croatia" & sc$year %in% 1945:1991)] <- "Serbia"
sc$country[which(sc$country=="Macedonia" & sc$year <1991)] <- "Serbia"
sc$country[which(sc$country=="Slovenia" & sc$year <1989)] <- "Serbia"



#USSR
sc$country_id[which(sc$country=="Ukraine" & sc$year<1990)] <-  11
sc$country_id[which(sc$country=="Armenia" & sc$year<1990)] <-  11
sc$country_id[which(sc$country=="Belarus" & sc$year<1990)] <-  11
sc$country_id[which(sc$country=="Georgia" & sc$year<1990)] <-  11
sc$country_id[which(sc$country=="Kazakhstan" & sc$year<1990)] <-  11
sc$country_id[which(sc$country=="Moldova" & sc$year<1990)] <-  11
sc$country_id[which(sc$country=="Uzbekistan" & sc$year<1990)] <- 11
sc$country_id[which(sc$country=="Estonia" & sc$year %in% 1940:1990)] <- 11
sc$country_id[which(sc$country=="Lithuania" & sc$year %in% 1940:1990)] <- 11
sc$country_id[which(sc$country=="Latvia" & sc$year %in% 1940:1990)] <- 11

sc$country[which(sc$country=="Ukraine" & sc$year<1990)] <- "Russia"
sc$country[which(sc$country=="Armenia" & sc$year<1990)] <- "Russia"
sc$country[which(sc$country=="Belarus" & sc$year<1990)] <- "Russia"
sc$country[which(sc$country=="Georgia" & sc$year<1990)] <- "Russia"
sc$country[which(sc$country=="Kazakhstan" & sc$year<1990)] <- "Russia"
sc$country[which(sc$country=="Moldova" & sc$year<1990)] <- "Russia"
sc$country[which(sc$country=="Uzbekistan" & sc$year<1990)] <- "Russia"
sc$country[which(sc$country=="Estonia" & sc$year %in% 1940:1990)] <- "Russia"
sc$country[which(sc$country=="Lithuania" & sc$year %in% 1940:1990)] <- "Russia"
sc$country[which(sc$country=="Latvia" & sc$year %in% 1940:1990)] <- "Russia"


#Czech Republic
sc$country_id[which(sc$country=="Slovakia" & sc$year %in% 1945:1993)] <- 157
sc$country[which(sc$country=="Slovakia" & sc$year %in% 1945:1993)] <- "Czech Republic"



#####   \\    --    Add capital     --    //    #####
capitals <- data.frame(read.csv("../Data/country-capitals.csv", stringsAsFactors = FALSE))
capitals <- capitals[which(is.na(capitals$CapitalLongitude)==FALSE),]
capitals$CountryName[which(capitals$CountryName=="Cote d'Ivoire")] <- "Ivory Coast"
capitals$CountryName[which(capitals$CountryName=="Taiwan")] <- "Republic of China (Taiwan)"

sc <- merge(sc, capitals, by.x="country", by.y="CountryName", all.x=TRUE)
rm(capitals)

#Correct spellingmistakes
#Fix symbols
sc$city <- trimws(gsub("[^a-zA-Z0-9 ]", " ", sc$city))
sc$city <- trimws(gsub("[[:space:]]", " ", sc$city))

sc$CapitalName <- trimws(gsub("[^a-zA-Z0-9 ]", " ", sc$CapitalName))
sc$CapitalName <- trimws(gsub("[[:space:]]", " ", sc$CapitalName))

#Fix different spellings
sc$city[which(sc$city=="Sana a")] <- "Sanaa"
sc$city[which(sc$city=="Prishtina")] <- "Pristina"
sc$city[which(sc$city=="Ashkhabad")] <- "Ashgabat"
sc$city[which(sc$city=="Yaound")] <- "Yaounde"
sc$city[which(sc$city=="San Jos")] <- "San Jose"
sc$city[grep("Damascus", sc$city)] <- "Damascus"
sc$city[which(sc$city=="Andorra La Vela")] <- "Andorra la Vella"
sc$city[which(sc$city=="Washington D C")] <- "Washington DC"


sc$CapitalName[which(sc$CapitalName=="Rangoon")] <- "Yangon"
sc$CapitalName[which(sc$CapitalName=="Prague")] <- "Praha"
sc$CapitalName[which(sc$CapitalName=="Kyiv")] <- "Kiev"
sc$CapitalName[which(sc$CapitalName=="Washington")] <- "Washington DC"

#Make dummey
sc$capital <- ifelse(sc$city==sc$CapitalName, 1, 0)



#####   \\  --  Global comparisons  --    //    ####


#Get max height in each country each year
frame <- data.frame(year=rep(min(sc$year,na.rm=TRUE):max(sc$completion,na.rm=TRUE),length(na.omit(unique(sc$country)))),
                    country=rep(na.omit(unique(sc$country)),each=length(min(sc$completion,na.rm=TRUE):max(sc$completion,na.rm=TRUE))))

library(dplyr)
temp <- sc %>%
  group_by(country,year) %>%
  summarise(tallest=max(highest_point,na.rm=TRUE))
temp$tallest[which(is.na(temp$tallest)==TRUE)] <- 0
temp <- temp[which(is.na(temp$year)==FALSE),]
temp <- temp[which(is.na(temp$country)==FALSE),]

#Overwrite smaller, newer building with older higher ones
for(i in 2:nrow(temp)){
  if(temp$country[i]==temp$country[i-1]){
    temp$tallest[i] <- ifelse(temp$tallest[i-1]>temp$tallest[i],temp$tallest[i-1],temp$tallest[i])
  }
}

#Attach to full frame
frame <- merge(frame,temp,by=c("country","year"),all=TRUE)
rm(temp)

library(zoo)
frame <-frame[order(frame$country,frame$year),]
frame <- frame %>%
  group_by(country) %>%
  arrange(country,year) %>%
  mutate(tallest=na.locf(tallest,na.rm=FALSE))


#Get mean max and max height each year
frame <- frame %>%
  group_by(year) %>%
  mutate(global_meanmax=mean(tallest,na.rm=TRUE),
         globalmax=max(tallest,na.rm=TRUE))

#Get distance from globals
frame$delta_globalmax <- frame$tallest - frame$globalmax
frame$delta_global_meanmax <- frame$tallest - frame$global_meanmax

#Get Lag
frame <-frame[order(frame$country,frame$year),]
frame <- frame %>%
  group_by(country) %>%
  mutate(delta_globalmax_lag1=lag(delta_globalmax),
         delta_global_meanmax_lag1=lag(delta_global_meanmax))

#Get 1-year change
frame$change_delta_globalmax <- frame$delta_globalmax - frame$delta_globalmax_lag1
frame$change_delta_global_meanmax <- frame$delta_global_meanmax - frame$delta_global_meanmax_lag1




#####    \\    -- Ranking of >=150m    --    //  #####
high <- sc[which(sc$highest_point>=150),]
high <- high[which(is.na(high$year)==FALSE),]
high$building_id <- 1:nrow(high)

##  Make row for each year of existence ##
high$existence <- max(high$year,na.rm=TRUE) - high$year
high <- high[rep(row.names(high), high$existence),]

#Fix year
high <- high[order(high$building_id),]
high$one <- 1
high <- high %>%
  group_by(building_id) %>%
  mutate(yearadd=cumsum(one))
high$year <- high$year+high$yearadd-1

#Fix rank
high <- high[order(high$country,high$year,high$highest_point),]

#Make rank
high <- high %>%
  group_by(country,year) %>%
  arrange(country,year,-highest_point) %>%
  mutate(country_year_rank=cumsum(one))


#####   \\    --  Make yearly summaries   --    //  #####

#Make numeric again:
high$highest_point <- as.numeric(as.character(high$highest_point))
high$floors <- as.numeric(as.character(high$floors))

#Make NAs 0
high$highest_point <- ifelse(is.na(high$highest_point)==TRUE & is.na(high$official_name)==TRUE,0,high$highest_point)
high$floors <- ifelse(is.na(high$floors)==TRUE & is.na(high$official_name)==TRUE,0,high$floors)

#Make MetersPerFloor
high$MetersPerFloorsAbove <- high$highest_point/high$floors_above
high$MetersPerFloor <- high$highest_point/high$floors


#Summary to make vanity
high$highest_point_vanityonly <- ifelse(is.na(high$vanity)==TRUE, 0, high$highest_point)


#Meters in capital
high$meters_capitalonly <- ifelse(high$capital==0, 0, high$highest_point)



#Summary
temp <- high %>%
  group_by(country,year) %>%
  summarise(nSkyScrapers=max(country_year_rank,na.rm=TRUE),
            MetersOfSkyScrapers=sum(highest_point,na.rm=TRUE),
            VarOfMetersOfSkyScrapers=var(highest_point,na.rm=TRUE),
            MeanMetersOfSkyScrapers=mean(highest_point,na.rm=TRUE),
            
            SkyscrapersInCapital=sum(capital, na.rm=TRUE),
            meters_in_capital=sum(meters_capitalonly, na.rm=TRUE),
            
            FloorsOfSkyScrapers=sum(floors,na.rm=TRUE),
            FloorsOfSkyScrapersAboveGround=sum(floors_above,na.rm=TRUE),
            
            MeanMetersPerFloorsAbove=mean(MetersPerFloorsAbove,na.rm=TRUE),
            MeanMetersPerFloor=mean(MetersPerFloor,na.rm=TRUE),
            
            SumVanity=sum(vanity,na.rm=TRUE),
            MaxVanity=max(vanity,na.rm=TRUE),
            MaxVanityPercent=max(vanitypercent,na.rm=TRUE),
            Vanitybuildings=sum(vanitymissing,na.rm=TRUE),
            MetersAmongVanities=sum(highest_point_vanityonly,na.rm=TRUE),
            
            MeanHeightGFARatio=mean(height_gfa_ratio),
            MaxHeightGFARatio=max(height_gfa_ratio),
            MeanHeightGFARatioNoDev=mean(height_gfa_ratio_nodev_notower),
            MaxHeightGFARatioNoDev=max(height_gfa_ratio_nodev_notower))

temp <- data.frame(temp)

#Vanity
temp$SumVanity <- ifelse(temp$MetersAmongVanities==0,NA,temp$SumVanity)
temp$MeanVanity <- ifelse(temp$Vanitybuildings==0,NA,temp$SumVanity/temp$Vanitybuildings)
temp$VanityPercent <- ifelse(temp$MetersAmongVanities==0,NA,(temp$SumVanity/temp$MetersAmongVanities)*100)


##  Merge with Frame  ##
frame <- merge(frame,temp,by=c("country","year"),all=TRUE)
rm(temp)

#####   \\    --  Alternative Delta Vanity    --    //    #####
van <- sc[which(sc$highest_point>=150),]
van <- van[which(is.na(van$year)==FALSE),]

van2 <- van %>%
  group_by(country, year) %>%
  summarise(SumVanity_alt=sum(vanity,na.rm=TRUE),
            SumVanitybuildings_alt=sum(vanitymissing))


van2$SumVanity_alt[which(van2$SumVanitybuildings_alt==0)] <- 0

frame <- merge(frame, van2, by=c("country","year"), all=TRUE)


#Divide between >=150m and >=300m
supertall <- high[which(high$highest_point>=300),]
supertall <- supertall %>%
  group_by(country,year) %>%
  summarise(nSkyScrapers_supertall=length(building_id))

frame <- merge(frame,supertall,by=c("country","year"),all=TRUE)


tall <- high[which(high$highest_point<300),]
tall <- tall %>%
  group_by(country,year) %>%
  summarise(nSkyScrapers_tall=length(building_id))

frame <- merge(frame,tall,by=c("country","year"),all=TRUE)


#####     \\    --  Make rankframe    --    //  #####

#Make a rankframe
nranks <- 20

rankframe <- data.frame(year=rep(min(high$year,na.rm=TRUE):max(high$completion,na.rm=TRUE),
                                 length(na.omit(unique(high$country)))*nranks),
                        
                        country=rep(na.omit(unique(high$country)),
                                    each=length(min(high$completion,na.rm=TRUE):max(high$completion,na.rm=TRUE))*nranks),
                        
                        country_year_rank=rep(1:nranks,
                                              length(min(high$completion,na.rm=TRUE):max(high$completion,na.rm=TRUE))*length(na.omit(unique(high$country)))))

#Extract wanted variables
identifiers <- c("country","year","country_year_rank")
rankvars <- c("highest_point","city","energy_label","floors","floors_above","floors_below","structural_material","official_name","address",
              "MetersPerFloorsAbove","MetersPerFloor")
high <- high[which(high$country_year_rank<=nranks),c(identifiers,rankvars)]
high <- high[which(is.na(high$year)==FALSE),]
high <- high[which(is.na(high$country)==FALSE),]

#Merge with rankframe
rankframe <- merge(rankframe,high,by=c("country","year","country_year_rank"),all=TRUE)


##  na.locf ##

#Necessary for data.table
for(j in rankvars){
  rankframe[,j] <- as.character(rankframe[,j])
}

#Order
rankframe <- rankframe[order(rankframe$country,rankframe$country_year_rank,rankframe$year),]


#na.locf
library(dtplyr);library(data.table)
setDT(rankframe)
rankframe[, (rankvars) := na.locf(.SD, na.rm = FALSE), by = list(country, country_year_rank), .SDcols = rankvars]
rankframe <- as.data.frame(rankframe)


#####   \\    --  Make Wide Rankframe   --    //  #####

wf <- reshape(rankframe, idvar = c("country","year"), timevar = "country_year_rank", direction = "wide")
colnames(wf) <- gsub(".","_rank",colnames(wf),fixed=TRUE)

#Cleanup
rm(high,rankframe)
gc()

#####   \\    -- Merge (Wide) Rankframe and Frame --    // #####
#Frame and Wide Rankframe
df <- merge(frame,wf,by=c("country","year"),all=TRUE)
df <- merge(df,unique(sc[,c("country","country_id")]), by="country",all=TRUE)


#####   \\  -- Merge with V-Dem   --    //  #####

#Merge V-Dem
load("../Data/vdemv61.rdata") #Use VDem to set country_id-standard
df <- merge(df,vdem,by=c("country_id","year"),all.y=TRUE)

#####   Remove countries not in Vdem, and add missing country-names   #####
df <- df[which(is.na(df$country_id)==FALSE),]
df$country <- ifelse(is.na(df$country)==TRUE,as.character(df$country_name),as.character(df$country))




#####     \\    --    Fix variables   --    //  #####
df$FloorsOfSkyScrapers <- ifelse(is.na(df$nSkyScrapers)==TRUE & is.na(df$FloorsOfSkyScrapers)==TRUE,0,df$FloorsOfSkyScrapers)
df$FloorsOfSkyScrapersAboveGround <- ifelse(is.na(df$nSkyScrapers)==TRUE & is.na(df$FloorsOfSkyScrapersAboveGround)==TRUE,0,df$FloorsOfSkyScrapersAboveGround)

df$nSkyScrapers[which(is.na(df$nSkyScrapers)==TRUE)] <- 0
df$nSkyScrapers_tall[which(is.na(df$nSkyScrapers_tall)==TRUE)] <- 0
df$nSkyScrapers_supertall[which(is.na(df$nSkyScrapers_supertall)==TRUE)] <- 0

df$MetersOfSkyScrapers[which(is.na(df$MetersOfSkyScrapers)==TRUE)] <- 0
df$MeanMetersOfSkyScrapers[which(is.na(df$MeanMetersOfSkyScrapers)==TRUE)] <- 0


df$MaxVanity[which(is.na(df$MaxVanity)==TRUE & df$nSkyScrapers==0)] <- 0
df$MaxVanityPercent[which(is.na(df$MaxVanityPercent)==TRUE & df$nSkyScrapers==0)] <- 0
df$MeanVanity[which(is.na(df$MeanVanity)==TRUE & df$nSkyScrapers==0)] <- 0
df$SumVanity[which(is.na(df$SumVanity)==TRUE & df$nSkyScrapers==0)] <- 0
df$VanityPercent[which(is.na(df$VanityPercent)==TRUE & df$nSkyScrapers==0)] <- 0

df$SumVanity_alt[which(is.na(df$SumVanity_alt)==TRUE & df$nSkyScrapers==0)] <- 0

df$SkyscrapersInCapital[which(is.na(df$SkyscrapersInCapital)==TRUE & df$nSkyScrapers==0)] <- 0
df$meters_in_capital[which(is.na(df$meters_in_capital)==TRUE & df$SkyscrapersInCapital==0)] <- 0
df$SkyscrapersNotInCapital <- df$nSkyScrapers - df$SkyscrapersInCapital
df$meters_not_in_capital   <- df$MetersOfSkyScrapers - df$meters_in_capital


#####   Remove prior to 1900    #####
df <- df[which(df$year>=1899),]


####    Add Anckar    ####
load("../Data/anckar.rda")
anckar$anckar_regimes_edited <- as.character(anckar$regimenarrowcat)
anckar$anckar_regimes_edited <- ifelse(anckar$democracy==1, "democracy", as.character(anckar$regimenarrowcat))
anckar$anckar_regimes_edited[which(anckar$regimenarrowcat=="Multi-party authoritarian rule")] <- "Party-based autocracy"
anckar$anckar_regimes_edited[which(anckar$regimenarrowcat=="Single-party rule")]   <- "Party-based autocracy"
anckar$anckar_regimes_edited[which(anckar$regimenarrowcat=="Absolute monarchy")]   <- "Monarchy"
anckar$anckar_regimes_edited[which(anckar$regimenarrowcat=="Monarchic oligarchy")] <- "Other dictatorship"
anckar$anckar_regimes_edited[which(anckar$regimenarrowcat=="Other oligarchy")] <- "Other dictatorship"



df <- merge(df, anckar, by.x=c("COWcode", "year"), by.y=c("cown", "year"), all.x=TRUE)


#####     Save      #####
save(df, file="main_dataframe.rdata")
