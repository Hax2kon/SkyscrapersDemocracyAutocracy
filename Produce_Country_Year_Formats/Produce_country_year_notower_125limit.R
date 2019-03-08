rm(list=ls())
cat("\014")
gc()

#Government building?
load("buildings.rdata")

remove <- c("chimney","bridge","industrial",
            "telecommunications / observation",
            "observation / telecommunications",
            "telecommunications")

sc <- sc[!sc$building_function %in% remove,]

#####   \\    --  Prepare SkyScraperData    --    //  #####

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




#####   \\    -- Ranking of >=150m    --    //  #####
high <- sc[which(sc$highest_point>=125),]
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
high$highest_point_vanityonly <- ifelse(is.na(high$vanity)==TRUE,0,high$highest_point)



#Summary
temp <- high %>%
  group_by(country,year) %>%
  summarise(nSkyScrapers=max(country_year_rank,na.rm=TRUE),
            MetersOfSkyScrapers=sum(highest_point,na.rm=TRUE),
            VarOfMetersOfSkyScrapers=var(highest_point,na.rm=TRUE),
            MeanMetersOfSkyScrapers=mean(highest_point,na.rm=TRUE),
            
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


#####   \\    -- Make rankframe  --   //    #####


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


#####   \\    --  Make Wide Rankframe  --   //    #####
wf <- reshape(rankframe, idvar = c("country","year"), timevar = "country_year_rank", direction = "wide")

colnames(wf) <- gsub(".","_rank",colnames(wf),fixed=TRUE)

#Cleanup
rm(high,rankframe)
gc()

#####   \\  --   Merge (Wide) Rankframe and Frame  --   //    #####
#Frame and Wide Rankframe
df <- merge(frame,wf,by=c("country","year"),all=TRUE)
df <- merge(df,unique(sc[,c("country","country_id")]), by="country",all=TRUE)


#####   \\  --   Merge with V-Dem  --   //    #####

#Merge V-Dem
load("../Data/vdemv61.rdata") #Use VDem to set country_id-standard
df <- merge(df,vdem,by=c("country_id","year"),all.y=TRUE)

#####   Remove countries not in Vdem, and add missing country-names   #####
df <- df[which(is.na(df$country_id)==FALSE),]
df$country <- ifelse(is.na(df$country)==TRUE,as.character(df$country_name),as.character(df$country))



#####   \\    --   Fix variables  --    //    #####
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



#####   Remove prior to 1900    #####
df <- df[which(df$year>=1899),]


######  \\    --    Usually done in analysis-script       --    //      #####

#Indicate 0 for nonexisting SkyScrapers
for(j in grep("highest_point_rank",colnames(df),value=TRUE)){
  df[which(is.na(df[,j])==TRUE),j] <- 0
  df[,j] <- as.numeric(as.character(df[,j]))
}




#####     Variable types     #####
ids <- c("country_id","country","year")
skyscrapers <- c("global_meanmax","globalmax","delta_globalmax","change_delta_globalmax","change_delta_global_meanmax",
                 "delta_global_meanmax","tallest","nSkyScrapers","nSkyScrapers_tall","nSkyScrapers_supertall",
                 "MetersOfSkyScrapers","MeanMetersOfSkyScrapers","VarOfMetersOfSkyScrapers","MeanMetersPerFloor","MeanMetersPerFloorsAbove",
                 "SumVanity","VanityPercent","MeanVanity","MaxVanity","MaxVanityPercent","MeanHeightGFARatio","MaxHeightGFARatio",
                 "MeanHeightGFARatioNoDev","MaxHeightGFARatioNoDev",
                 grep("highest_point_rank",colnames(df),value=TRUE),grep("MetersPerFloorsAbove_rank",colnames(df),value=TRUE),
                 grep("MetersPerFloor_rank",colnames(df),value=TRUE))

democracy <- c("v2x_polyarchy","v2x_libdem", "v2x_partipdem", "v2x_delibdem", "v2x_egaldem", "v2xeg_eqdr","v2pepwrses",
               "v2pepwrsoc","v2pepwrgen","v2x_api","e_polity2",
               "v2x_mpi","e_boix_regime","v2x_egal","e_mivanhan","e_chga_demo")
international <- c("v2svdomaut","v2svinlaut","v2svstterr","v2svstpop","v2svindep")
economy <- c("e_migdpgrolns","e_migdppcln","e_mipopula","e_population","e_peginiwi","e_miurbani","e_migdppc",
             "e_Vanhanen_urban_ipo","e_peinfmor","e_Fiscal_Reliance","e_Total_Oil_Income_PC","e_Coal_Income_PC",
             "e_natural_gas_income_PC","e_Total_Fuel_Income_PC","e_metals_income_PC","e_Total_Resources_Income_PC",
             "e_area","e_miurbpop","v2clstown")
land <- c("e_region_world_2","e_regionpol","e_regiongeo")
mediators <- c("v2xme_altinf","v2x_freexp_thick","v2x_freexp","v2xel_frefair","v2xlg_legcon","v2x_jucon",
               "v2x_corr","v2pepwrses","v2pepwrsoc","v2x_suffr")
conflict <- c("e_miinteco","e_miinterc")

#Extract Subset
df <- df[which(df$year>=1899 & df$year<=2016),c(ids,skyscrapers,democracy,economy,conflict,international,land,mediators)]


##### Lag, stock and delta Variables  #####
library(dplyr)
library(dtplyr)
library(data.table)


#####  Lags  #####
time <- 30
lagvars <- c(skyscrapers,democracy,economy,conflict,international,land,mediators)
df <- df[order(df$country,df$year),]
setDT(df)
for(i in lagvars){
  df[, paste0(i,"_lag",1:time) :=  shift(.SD,1:time), by=country, .SDcols=noquote(i)]
}
df <- as.data.frame(df)


##### Delta  #####
df$delta_nSkyScrapers <- df$nSkyScrapers - df$nSkyScrapers_lag1
df$delta_nSkyScrapers_tall <- df$nSkyScrapers_tall - df$nSkyScrapers_tall_lag1
df$delta_nSkyScrapers_supertall <- df$nSkyScrapers_supertall - df$nSkyScrapers_supertall_lag1
df$delta_nSkyScrapers_ln <- log(df$delta_nSkyScrapers+1)

df$delta_MetersOfSkyScrapers <- df$MetersOfSkyScrapers - df$MetersOfSkyScrapers_lag1
df$delta_MeanMetersOfSkyScrapers <- df$MeanMetersOfSkyScrapers - df$MeanMetersOfSkyScrapers_lag1
df$delta_MeanMetersPerFloorsAbove <- df$MeanMetersPerFloorsAbove - df$MeanMetersPerFloorsAbove_lag1

df$delta_urbanization <- df$e_miurbani - df$e_miurbani_lag1

df$delta_VanityPercent <- df$VanityPercent - df$VanityPercent_lag1
df$delta_SumVanity <- df$SumVanity - df$SumVanity_lag1
df$delta_MeanVanity <- df$MeanVanity - df$MeanVanity_lag1
df$delta_MaxVanityPercent <- df$MaxVanityPercent - df$MaxVanityPercent_lag1
df$delta_MaxVanity <- df$MaxVanity - df$MaxVanity_lag1


df$delta_heightgfa <- df$MeanHeightGFARatio - df$MeanHeightGFARatio_lag1
df$delta_maxheightgfa <- df$MaxHeightGFARatio - df$MaxHeightGFARatio_lag1
df$delta_heightgfaNoDev <- df$MeanHeightGFARatioNoDev - df$MeanHeightGFARatioNoDev_lag1
df$delta_maxheightgfaNoDev <- df$MaxHeightGFARatioNoDev - df$MaxHeightGFARatioNoDev_lag1


##   Growth  ##
df$growth5 <- (log(df$e_migdppc) - log(df$e_migdppc_lag5))/5
df$growth10 <- (log(df$e_migdppc) - log(df$e_migdppc_lag10))/10


##  Lag deltas  ##
lagvars <- c("delta_nSkyScrapers","delta_nSkyScrapers_tall","delta_nSkyScrapers_supertall","delta_urbanization","delta_MetersOfSkyScrapers","growth5","growth10")
df <- df[order(df$country,df$year),]
setDT(df)
for(i in lagvars){
  df[, paste0(i,"_lag",1:time) :=  shift(.SD,1:time), by=country, .SDcols=noquote(i)]
}
df <- as.data.frame(df)


#####  Stock #####
#Formula: depreciation^(lag)*X
depreciation <- 0.9
time <- 30

#Functions:
lineardecay <- c(depreciation^(0:time))
exponentialdecay <- exp((depreciation-1)*0:time)

#Stocks:
df$v2x_polyarchy_stocklinear <- as.matrix(df[,c("v2x_polyarchy",paste0("v2x_polyarchy","_lag",1:time))]) %*% lineardecay
df$v2x_polyarchy_stockexponential <- as.matrix(df[,c("v2x_polyarchy",paste0("v2x_polyarchy","_lag",1:time))]) %*% exponentialdecay
df$v2x_libdem_stocklinear <- as.matrix(df[,c("v2x_libdem",paste0("v2x_libdem","_lag",1:time))]) %*% lineardecay
df$v2x_libdem_stockexponential <- as.matrix(df[,c("v2x_libdem",paste0("v2x_libdem","_lag",1:time))]) %*% exponentialdecay
df$v2x_egaldem_stockexponential <- as.matrix(df[,c("v2x_egaldem",paste0("v2x_egaldem","_lag",1:time))]) %*% exponentialdecay
df$e_migdpgrolns_stockexponential <- as.matrix(df[,c("e_migdpgrolns",paste0("e_migdpgrolns","_lag",1:time))]) %*% exponentialdecay
df$urbanization_stock <- as.matrix(df[,c("delta_urbanization",paste0("delta_urbanization","_lag",1:time))]) %*% lineardecay
df$e_boix_regime_stock <- as.matrix(df[,c("e_boix_regime",paste0("e_boix_regime","_lag",1:time))]) %*% lineardecay

#Lag Stocks
lagvars <- grep("stock",colnames(df),value=TRUE)
setDT(df)
for(i in lagvars){
  df[, paste0(i,"_lag",1:20) :=  shift(.SD,1:20), by=country, .SDcols=noquote(i)]
}
df <- as.data.frame(df)

#Change in stock
for(i in unique(gsub("_lag.*","",grep("stock",colnames(df),value=TRUE)))){
  df[,paste0(i,"_change")] <- df[,i] - df[,paste0(i,"_lag1")]
}

#Lag Stock_change
lagvars <- grep("stock.*_change",colnames(df),value=TRUE)
setDT(df)
for(i in lagvars){
  df[, paste0(i,"_lag",1:20) :=  shift(.SD,1:20), by=country, .SDcols=noquote(i)]
}
df <- as.data.frame(df)


#####   Make decade         #####
df$decade <- substr(df$year,1,3)

#####   Make binomial       #####
df$buildornot <- ifelse(df$delta_nSkyScrapers>0,1,0)


#Save
df_125 <- df
save(df_125,file="countryyear_125mLimit.rdata")
