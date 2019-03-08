

library(stargazer);library(ebal);library(MatchIt);library(cem)
load("../Replication/Data/buildings.rdata")



#####     \\    Make Categories   //     #####

##      Economy     ##
sc$gdp_median <- ifelse(sc$e_migdppc < median(sc$e_migdppc, na.rm=TRUE), 0, 1)

##      Urban       ##
sc$urban_median <- ifelse(sc$e_miurbani < median(sc$e_miurbani, na.rm=TRUE), 0, 1)


##    Height    ##
sc$occupied_median <- ifelse(sc$height_occupied_meter < median(sc$height_occupied_meter, na.rm=TRUE), 0, 1)

##      Democracy      ##
sc$treatment_median <- ifelse(sc$v2x_polyarchy<median(sc$v2x_polyarchy,na.rm=TRUE),0,1)




#####     \\    --    Table A.22 and A.23    --    //      #####
matchvars_buildingmod <- c("occupied_median", 
                           "height_occupied_meter",
                           "vanity",
                           "vanityper",
                           "treatment_median",
                           "v2x_polyarchy",
                           "decade",
                           "complex",
                           "function_office",
                           "function_residential")


#Remove missing:
matchdf_buildingmod <- na.omit(sc[,matchvars_buildingmod])

#Define factors
matchdf_buildingmod$complex              <-  as.factor(matchdf_buildingmod$complex)
matchdf_buildingmod$function_office      <-  as.factor(matchdf_buildingmod$function_office)
matchdf_buildingmod$function_residential <-  as.factor(matchdf_buildingmod$function_residential)
matchdf_buildingmod$treatment_median     <-  as.factor(matchdf_buildingmod$treatment_median)
matchdf_buildingmod$occupied             <-  as.factor(matchdf_buildingmod$occupied)

#Execute CEM
matched_buildingmod <- matchit(treatment_median ~ 
                                 occupied_median +
                                 decade + 
                                 complex +
                                 function_office + 
                                 function_residential
                               , data=matchdf_buildingmod, method="cem")


#Extract results
matchdata_buildingmod        <- match.data(matched_buildingmod)


#####     Table A.22
match_percent_nopost <-lm(vanityper ~ treatment_median + factor(subclass), 
                          data=matchdata_buildingmod,
                          weights=matchdata_buildingmod$weights)

match_meters_log_nopost <- lm(log(vanity+1) ~ treatment_median + factor(subclass),
                              data=matchdata_buildingmod, 
                              weights=matchdata_buildingmod$weights)

match_meters_nolog_nopost <-lm(vanity ~ treatment_median + factor(subclass),
                               data=matchdata_buildingmod,
                               weights=matchdata_buildingmod$weights)

#####     Print Table A.22 in console      #####
stargazer(match_percent_nopost,
          match_meters_log_nopost,
          match_meters_nolog_nopost,
          covariate.labels = c("Polyarchy above median"),
          title="Robustness test of skyscraper excessiveness: CEM-matching at the building-level, with no post-matching controls",
          style="ajps",
          type="text",
          digits=2,
          font.size="scriptsize",
          align=FALSE,
          single.row=FALSE,
          dep.var.labels.include=FALSE,
          notes=c("Standard error in parentheses"),
          dep.var.caption = "",
          add.lines = list(rep("",8)),
          model.names=FALSE, column.labels=c("Percent","Ln meters", "Absolute meters"),
          omit=c("decade", "subclass"),
          omit.labels = c("Decade FE","Matched subclass FE"),
          omit.stat = c("f","adj.rsq","ser","ll","aic","bic"))


#####     Table A.23
match_percent_scaled <-lm(vanityper ~ v2x_polyarchy + log(height_occupied_meter) +
                            decade + complex + function_office + function_residential +
                            + factor(subclass),
                          data=matchdata_buildingmod,
                          weights=matchdata_buildingmod$weights)

match_meters_log_scaled <- lm(log(vanity+1) ~ v2x_polyarchy + log(height_occupied_meter) +
                                decade + complex + function_office + function_residential +
                                + factor(subclass),
                              data=matchdata_buildingmod,
                              weights=matchdata_buildingmod$weights)


match_meters_nolog_scaled <-lm(vanity ~ v2x_polyarchy + log(height_occupied_meter) +
                                             decade + complex + function_office + function_residential +
                                             + factor(subclass),
                               data=matchdata_buildingmod,
                               weights=matchdata_buildingmod$weights)


#####      Print Table A.23 in console      #####
stargazer(match_percent_scaled,
          match_meters_log_scaled,
          match_meters_nolog_scaled,
          covariate.labels = c("Polyarchy (scalar)",
                               "Ln height to top occupied floor",
                               "Part of building-complex",
                               "Building function: Office",
                               "Building function: Residential"),
          title="Robustness test of skyscraper excessiveness: CEM-matching at the building-level, with numeric polyarchy",
          style="ajps",
          type="text",
          digits=2,
          font.size="scriptsize",
          align=FALSE,
          single.row=FALSE,
          #report=c("vc*s"),
          dep.var.labels.include=FALSE,
          #star.cutoffs = c(0.05,0.01,0.001),
          notes=c("Standard error in parentheses"),
          dep.var.caption = "",
          add.lines = list(rep("",8)),
          model.names=FALSE, column.labels=c("Percent","Ln meters", "Absolute meters"),
          omit=c("decade", "subclass"),
          omit.labels = c("Decade FE","Matched subclass FE"),
          omit.stat = c("f","adj.rsq","ser","ll","aic","bic"))


#####     \\    --    Table A.24      --    //    #####

#Matching-variables:
matchvars_building_and_gdp <- c("occupied_median",
                                "height_occupied_meter",
                                "gdp_median",
                                "e_migdppc",
                                "country",
                                "vanity",
                                "vanityper",
                                "treatment_median",
                                "v2x_polyarchy",
                                "decade",
                                "complex",
                                "function_office",
                                "function_residential")

#Remove missing:
matchdf_building_and_gdp <- na.omit(sc[,matchvars_building_and_gdp])

#Define factors
matchdf_building_and_gdp$complex              <-  as.factor(matchdf_building_and_gdp$complex)
matchdf_building_and_gdp$function_office      <-  as.factor(matchdf_building_and_gdp$function_office)
matchdf_building_and_gdp$function_residential <-  as.factor(matchdf_building_and_gdp$function_residential)
matchdf_building_and_gdp$treatment_median     <-  as.factor(matchdf_building_and_gdp$treatment_median)
matchdf_building_and_gdp$gdp_median           <-  as.factor(matchdf_building_and_gdp$gdp_median)
matchdf_building_and_gdp$occupied             <-  as.factor(matchdf_building_and_gdp$occupied)

#Match
matched_building_and_gdp <- matchit(treatment_median ~ 
                                      gdp_median +
                                      occupied_median +
                                      decade + 
                                      complex +
                                      function_office + 
                                      function_residential
                                    ,data=matchdf_building_and_gdp, method="cem")


####    Extract results
matchdata_building_and_gdp        <- match.data(matched_building_and_gdp)


####    Regressions
match_percent_building_and_gdp <-lm(vanityper ~ treatment_median + log(e_migdppc) + log(height_occupied_meter) +
                                      decade + complex + function_office + function_residential +
                                      + factor(subclass)
                                    ,data=matchdata_building_and_gdp,
                                    weights=matchdata_building_and_gdp$weights)

match_meters_log_building_and_gdp <- lm(log(vanity+1) ~ treatment_median + log(e_migdppc) + log(height_occupied_meter) +
                                          decade + complex + function_office + function_residential +
                                          + factor(subclass),
                                        data=matchdata_building_and_gdp,
                                        weights=matchdata_building_and_gdp$weights)

match_meters_nolog_building_and_gdp <-lm(vanity ~ treatment_median + log(e_migdppc) + log(height_occupied_meter) +
                                           decade + complex + function_office + function_residential +
                                           + factor(subclass),
                                         data=matchdata_building_and_gdp,
                                         weights=matchdata_building_and_gdp$weights)


#####     Print Table A.24 in console     #####
stargazer(match_percent_building_and_gdp,
          match_meters_log_building_and_gdp,
          match_meters_nolog_building_and_gdp,
          covariate.labels = c("Polyarchy above median",
                               "Ln GDP per capita",
                               "Ln height to top occupied floor",
                               "Part of building-complex",
                               "Building function: Office",
                               "Building function: Residential"),
          title="Robustness test of skyscraper excessiveness: CEM-matching at the building-level and GDP",
          style="ajps",
          type="text",
          digits=2,
          font.size="scriptsize",
          align=FALSE,
          single.row=FALSE,
          dep.var.labels.include=FALSE,
          notes=c("Standard error in parentheses"),
          dep.var.caption = "",
          add.lines = list(rep("",8)),
          model.names=FALSE, column.labels=c("Percent","Ln meters", "Absolute meters"),
          omit=c("decade", "subclass"),
          omit.labels = c("Decade FE","Matched subclass FE"),
          omit.stat = c("f","adj.rsq","ser","ll","aic","bic"))






#####     \\      Table A.25        //      #####

#Matching variables
matchvars_occupied_gdp <- c("occupied_median",
                            "height_occupied_meter",
                            "gdp_median",
                            "e_migdppc",
                            "country",
                            "vanity",
                            "vanityper",
                            "treatment_median",
                            "v2x_polyarchy",
                            "decade")

#Remove missing
matchdf_occupied_gdp <- na.omit(sc[,matchvars_occupied_gdp])

#Define factor
matchdf_occupied_gdp$treatment_median     <-  as.factor(matchdf_occupied_gdp$treatment_median)
matchdf_occupied_gdp$occupied_median      <-  as.factor(matchdf_occupied_gdp$occupied_median)

#Execute matching
matched_occupied_gdp <- matchit(treatment_median ~ 
                                  gdp_median +
                                  occupied_median +
                                  decade, 
                                data=matchdf_occupied_gdp, method="cem")


#Extract results
matchdata_occupied_gdp        <- match.data(matched_occupied_gdp)

#Regressions
match_percent_occupied_gdp <-lm(vanityper ~ treatment_median + log(e_migdppc) + 
                                  log(height_occupied_meter) + decade + factor(subclass),
                                data=matchdata_occupied_gdp,
                                weights=matchdata_occupied_gdp$weights)


match_meters_log_occupied_gdp <- lm(log(vanity+1) ~ treatment_median + log(e_migdppc) + 
                                      log(height_occupied_meter) +
                                      decade + factor(subclass),
                                    data=matchdata_occupied_gdp,
                                    weights=matchdata_occupied_gdp$weights)


match_meters_nolog_occupied_gdp <-lm(vanity ~ treatment_median + log(e_migdppc) + 
                                       log(height_occupied_meter) + decade + factor(subclass),
                                     data=matchdata_occupied_gdp,
                                     weights=matchdata_occupied_gdp$weights)


#####     Print Table A.25 in console   #####
stargazer(match_percent_occupied_gdp,
          match_meters_log_occupied_gdp,
          match_meters_nolog_occupied_gdp,
          covariate.labels = c("Polyarchy above median",
                               "Ln GDP per capita",
                               "Ln height to top occupied floor"),
          title="Robustness test of skyscraper excessiveness: CEM-matching on height and GDP",
          style="ajps",
          type="text",
          digits=2,
          font.size="scriptsize",
          align=FALSE,
          single.row=FALSE,
          dep.var.labels.include=FALSE,
          notes=c("Standard error in parentheses"),
          dep.var.caption = "",
          add.lines = list(rep("",8)),
          model.names=FALSE, column.labels=c("Percent","Ln meters", "Absolute meters"),
          omit=c("decade", "subclass"),
          omit.labels = c("Decade FE","Matched subclass FE"),
          omit.stat = c("f","adj.rsq","ser","ll","aic","bic"))

#####   \\    --      Table A.26    --    //    #####

#Variables:
matchvars_buildingmod <- c("occupied_median",
                           "height_occupied_meter",
                           "country",
                           "vanity",
                           "vanityper",
                           "completion",
                           "treatment_median",
                           "v2x_polyarchy",
                           "decade",
                           "complex",
                           "function_office",
                           "function_residential")


#####       Only non-capital buildings:     #####
#Remove missing:
matchdf_nocapital <- na.omit(sc[which(sc$capital==0), matchvars_buildingmod])

matchdf_nocapital$complex              <-  as.factor(matchdf_nocapital$complex)
matchdf_nocapital$function_office      <-  as.factor(matchdf_nocapital$function_office)
matchdf_nocapital$function_residential <-  as.factor(matchdf_nocapital$function_residential)
matchdf_nocapital$treatment_median     <-  as.factor(matchdf_nocapital$treatment_median)
matchdf_nocapital$occupied             <-  as.factor(matchdf_nocapital$occupied)


#Match
matched_nocapital <- matchit(treatment_median ~ 
                               occupied_median +
                               decade + 
                               complex +
                               function_office + 
                               function_residential
                             , data=matchdf_nocapital, method="cem")

#Make data
matchdata_nocapital        <- match.data(matched_nocapital)
matchdf_nocapital$weights  <- matched_nocapital$weights
matchdf_nocapital$subclass <- matched_nocapital$subclass
matchdf_nocapital          <- matchdf_nocapital[which(matchdf_nocapital$weights>0),]


#Model
match_meters_nolog_nocapital <-lm(vanity ~ treatment_median + log(height_occupied_meter) +
                                    decade + complex + function_office + function_residential
                                  + factor(subclass)
                                  ,data=matchdata_nocapital,weights=matchdata_nocapital$weights)

match_meters_log_nocapital <- lm(log(vanity+1) ~ treatment_median + log(height_occupied_meter) +
                                   decade + complex + function_office + function_residential
                                 + factor(subclass)
                                 ,data=matchdata_nocapital,weights=matchdata_nocapital$weights)

match_percent_nocapital <-lm(vanityper ~ treatment_median + log(height_occupied_meter) +
                               decade + complex + function_office + function_residential
                             + factor(subclass)
                             ,data=matchdata_nocapital,weights=matchdata_nocapital$weights)

#####     Only capital buildings:     #####
matchdf_capital <- na.omit(sc[which(sc$capital==1), matchvars_buildingmod])

matchdf_capital$complex              <-  as.factor(matchdf_capital$complex)
matchdf_capital$function_office      <-  as.factor(matchdf_capital$function_office)
matchdf_capital$function_residential <-  as.factor(matchdf_capital$function_residential)
matchdf_capital$treatment_median     <-  as.factor(matchdf_capital$treatment_median)
matchdf_capital$occupied             <-  as.factor(matchdf_capital$occupied)


#Match
matched_capital <- matchit(treatment_median ~ 
                             occupied_median +
                             decade + 
                             complex +
                             function_office + 
                             function_residential
                           , data=matchdf_capital, method="cem")

#Make data
matchdata_capital        <- match.data(matched_capital)
matchdf_capital$weights  <- matched_capital$weights
matchdf_capital$subclass <- matched_capital$subclass
matchdf_capital          <- matchdf_capital[which(matchdf_capital$weights>0),]


#Model
match_meters_nolog_capital <-lm(vanity ~ treatment_median + log(height_occupied_meter) +
                                  decade + complex + function_office + function_residential
                                + factor(subclass)
                                ,data=matchdata_capital,weights=matchdata_capital$weights)

match_meters_log_capital <- lm(log(vanity+1) ~ treatment_median + log(height_occupied_meter) +
                                 decade + complex + function_office + function_residential
                               + factor(subclass)
                               ,data=matchdata_capital,weights=matchdata_capital$weights)

match_percent_capital <-lm(vanityper ~ treatment_median + log(height_occupied_meter) +
                             decade + complex + function_office + function_residential
                           + factor(subclass)
                           ,data=matchdata_capital,weights=matchdata_capital$weights)

#####     Print Table A.26      #####
stargazer(match_meters_nolog_nocapital, match_meters_log_nocapital, match_percent_nocapital,
          match_meters_nolog_capital, match_meters_log_capital, match_percent_capital,
          covariate.labels = c("Polyarchy (above median)",
                               "Ln height to top occupied floor",
                               "Part of building-complex",
                               "Building function: Office",
                               "Building function: Residential"),
          title="Skyscraper excessiveness: Split-sample by capital and non-capital buildings",
          style="ajps",
          type="text",
          digits=2,
          font.size="scriptsize",
          align=FALSE,
          single.row=FALSE,
          dep.var.labels.include=FALSE,
          notes=c("Standard error in parentheses"),
          dep.var.caption = "",
          add.lines = list(c("Sample:",
                             "Non-capital", "Non-capital", "Non-capital",
                             "Capital", "Capital" , "Capital"),
                           rep("",8)),
          model.names=FALSE,
          column.labels=c("Percent","Ln meters", "Absolute meters", "Percent","Ln meters", "Absolute meters"),
          omit=c("decade", "subclass"),
          omit.labels = c("Decade FE","Matched subclass FE"),
          omit.stat = c("f","adj.rsq","ser","ll","aic","bic"))



##### \\ --         Table A.27         -- // #####
sc$ln_gdp             <- log(sc$e_migdppc)
sc$ln_occupied_height <- log(sc$height_occupied_meter)

#Variables to use:
enthropyvars <- c("vanity",
                  "vanityper",
                  "treatment_median",
                  "ln_gdp",
                  "ln_occupied_height",
                  "e_miurbani",
                  "occupied_median",
                  "complex",
                  "function_office",
                  "function_residential",
                  "completion",
                  "country")

#Remove missing:
matchdf_enthropy        <- na.omit(sc[,enthropyvars])

#Variables to match on
xes <- c("ln_gdp",
         "ln_occupied_height",
         "e_miurbani",
         "complex",
         "completion",
         "function_office",
         "function_residential")


####    Execute Entropy-matching
ebalres  <- ebalance(Treatment = matchdf_enthropy$treatment_median,
                     X=as.matrix(matchdf_enthropy[,xes]))

#Make weights:
matchdf_enthropy$weights <- 1
matchdf_enthropy[row.names(ebalres$co.xdata), "weights"] <- ebalres$w


#Regressions:
enthropy_percent <- lm(vanityper   ~ treatment_median + ln_gdp + ln_occupied_height + e_miurbani + complex + function_office + function_residential + factor(completion), data=matchdf_enthropy)
enthropy_log     <- lm(log(vanity) ~ treatment_median + ln_gdp + ln_occupied_height + e_miurbani + complex + function_office + function_residential + factor(completion), data=matchdf_enthropy)
enthropy_nolog   <- lm(vanity      ~ treatment_median + ln_gdp + ln_occupied_height + e_miurbani + complex + function_office + function_residential + factor(completion), data=matchdf_enthropy)


stargazer(enthropy_percent,
          enthropy_log,
          enthropy_nolog,
          covariate.labels = c("Polyarchy above median",
                               "Ln GDP per capita",
                               "Ln height to top occupied floor",
                               "Urbanization",
                               "Part of building-complex",
                               "Building function: Office",
                               "Building function: Residential"),
          title="Robustness test of skyscraper excessiveness: Enthropy-matching",
          style="ajps",
          type="text",
          digits=2,
          font.size="scriptsize",
          align=FALSE,
          single.row=FALSE,
          dep.var.labels.include=FALSE,
          notes=c("Standard error in parentheses"),
          dep.var.caption = "",
          add.lines = list(rep("",8)),
          model.names=FALSE, column.labels=c("Percent","Ln meters", "Absolute meters"),
          omit=c("completion"),
          omit.labels = c("Year FE"),
          omit.stat = c("f","adj.rsq","ser","ll","aic","bic"))



##### \\ -- Table A.27 -- // #####

#Define as log
sc$ln_gdp             <- log(sc$e_migdppc)
sc$ln_occupied_height <- log(sc$height_occupied_meter)

#Variables to use:
enthropyvars <- c("vanity",
                  "vanityper",
                  "e_boix_regime",
                  "ln_gdp",
                  "ln_occupied_height",
                  "e_miurbani",
                  "occupied_median",
                  "complex",
                  "function_office",
                  "function_residential",
                  "completion",
                  "country")

#Remove missing:
matchdf_enthropy        <- na.omit(sc[,enthropyvars])

xes <- c("ln_gdp",
         "ln_occupied_height",
         "e_miurbani",
         "complex",
         "completion",
         "function_office",
         "function_residential")

##  Execute Entropy
ebalres  <- ebalance(Treatment = matchdf_enthropy$e_boix_regime, X=as.matrix(matchdf_enthropy[,xes]))

#Make weights:
matchdf_enthropy$weights <- 1
matchdf_enthropy[row.names(ebalres$co.xdata), "weights"] <- ebalres$w


#Regressions
enthropy_percent <- lm(vanityper   ~ e_boix_regime + ln_gdp + ln_occupied_height + e_miurbani + complex + function_office + function_residential + factor(completion), data=matchdf_enthropy)
enthropy_log     <- lm(log(vanity) ~ e_boix_regime + ln_gdp + ln_occupied_height + e_miurbani + complex + function_office + function_residential + factor(completion), data=matchdf_enthropy)
enthropy_nolog   <- lm(vanity      ~ e_boix_regime + ln_gdp + ln_occupied_height + e_miurbani + complex + function_office + function_residential + factor(completion), data=matchdf_enthropy)


#####     Print Table A.28 in console     #####
stargazer(enthropy_percent,
          enthropy_log,
          enthropy_nolog,
          covariate.labels = c("BMR",
                               "Ln GDP per capita",
                               "Ln height to top occupied floor",
                               "Urbanization",
                               "Part of building-complex",
                               "Building function: Office",
                               "Building function: Residential"),
          title="Robustness test of skyscraper excessiveness: Enthropy-matching using Boix, Miller, Rosato",
          style="ajps",
          type="text",
          digits=2,
          font.size="scriptsize",
          align=FALSE,
          single.row=FALSE,
          dep.var.labels.include=FALSE,
          notes=c("Standard error in parentheses"),
          dep.var.caption = "",
          add.lines = list(rep("",8)),
          model.names=FALSE, column.labels=c("Percent","Ln meters", "Absolute meters"),
          omit=c("completion"),
          omit.labels = c("Year FE"),
          omit.stat = c("f","adj.rsq","ser","ll","aic","bic"))



#####     \\    --    Table A.29      --      //      #####

#####     Model 1       #####
#####     Variables to match
matchvars_gfa <- c("occupied_median",
                   "country",
                   "height_gfa_ratio",
                   "treatment_median",
                   "decade",
                   "complex",
                   "function_office",
                   "function_residential")


#####   Remove missing
matchdf_gfa <- na.omit(sc[,matchvars_gfa])

#####     Define factor
matchdf_gfa$complex              <-  as.factor(matchdf_gfa$complex)
matchdf_gfa$function_office      <-  as.factor(matchdf_gfa$function_office)
matchdf_gfa$function_residential <-  as.factor(matchdf_gfa$function_residential)
matchdf_gfa$treatment_median     <-  as.factor(matchdf_gfa$treatment_median)
matchdf_gfa$occupied_median      <-  as.factor(matchdf_gfa$occupied_median)


#####   Execute matching
matched_gfa <- matchit(treatment_median ~ 
                         occupied_median +
                         decade + 
                         complex +
                         function_office + 
                         function_residential,
                       data=matchdf_gfa, method="cem")

#####     Extract results
matchdata_gfa <- match.data(matched_gfa)

#####   Regression
mod_gfa <-lm(height_gfa_ratio ~ treatment_median + factor(subclass),
             data=matchdata_gfa, weights=matchdata_gfa$weights)



#####     Model 2     #####
#####     Variables to match
matchvars_floor <- c("occupied_median",
                     "country",
                     "height_floor_ratio",
                     "treatment_median",
                     "decade",
                     "complex",
                     "function_office",
                     "function_residential")


#####   Remove missing
matchdf_floor <- na.omit(sc[,matchvars_floor])

#####     Define factor
matchdf_floor$complex              <-  as.factor(matchdf_floor$complex)
matchdf_floor$function_office      <-  as.factor(matchdf_floor$function_office)
matchdf_floor$function_residential <-  as.factor(matchdf_floor$function_residential)
matchdf_floor$treatment_median     <-  as.factor(matchdf_floor$treatment_median)
matchdf_floor$occupied_median      <-  as.factor(matchdf_floor$occupied_median)


#####   Execute Match
matched_floor <- matchit(treatment_median ~ 
                           occupied_median +
                           decade + 
                           complex +
                           function_office + 
                           function_residential,
                         data=matchdf_floor, method="cem")

####    Extract results
matchdata_floor <- match.data(matched_floor)


#####   Regression
mod_floor <-lm(height_floor_ratio ~ treatment_median + factor(subclass),
               data=matchdata_floor,
               weights=matchdata_floor$weights)


#####     Print Table A.29 in console     #####
stargazer(mod_gfa, mod_floor,
          covariate.labels = "Polyarchy (above or below median)",
          title="CEM-matching at the building-level: Alternative proxies of the excessiveness of skyscrapers",
          style="ajps",
          type="text",
          digits=2,
          font.size="scriptsize",
          align=FALSE,
          table.placement = "!h",
          single.row=FALSE,
          dep.var.labels.include=FALSE,
          notes=c("Standard error in parentheses"),
          dep.var.caption = "",
          add.lines = list(rep("",2)),
          model.names=FALSE, column.labels=c("Height / GFA", "Height / Floors"),
          omit=c("subclass"),
          omit.labels = c("matched subclass FE"),
          omit.stat = c("f","adj.rsq","ser","ll","aic","bic"))




