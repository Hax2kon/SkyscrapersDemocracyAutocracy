


library(stargazer);library(ebal);library(MatchIt);library(cem)
load("../Replication/Data/buildings.rdata")


#####       Make variables categorical     #####
sc$occupied_median  <- ifelse(sc$height_occupied_meter < median(sc$height_occupied_meter, na.rm=TRUE), 0, 1)
sc$treatment_median <- ifelse(sc$v2x_polyarchy<median(sc$v2x_polyarchy,na.rm=TRUE),0,1)



#####       Matching        #####

#Variables to be used in matching:
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

#Make reduced dataframe, removing missing and unwanted variables
matchdf_buildingmod <- na.omit(sc[,matchvars_buildingmod])


#Make sure all are defined as factors
matchdf_buildingmod$complex              <-  as.factor(matchdf_buildingmod$complex)
matchdf_buildingmod$function_office      <-  as.factor(matchdf_buildingmod$function_office)
matchdf_buildingmod$function_residential <-  as.factor(matchdf_buildingmod$function_residential)
matchdf_buildingmod$treatment_median     <-  as.factor(matchdf_buildingmod$treatment_median)
matchdf_buildingmod$occupied             <-  as.factor(matchdf_buildingmod$occupied)


####    Run CEM matching
matched_buildingmod <- matchit(treatment_median ~ 
                                 occupied_median +
                                 decade + 
                                 complex +
                                 function_office + 
                                 function_residential,
                               data=matchdf_buildingmod, method="cem")


####      Extract CEM-results
matchdata_buildingmod        <- match.data(matched_buildingmod)



#####     Run regressions
match_percent_buildingmod <-lm(vanityper ~ treatment_median + log(height_occupied_meter) +
                                 decade + complex + function_office + function_residential +
                                 + factor(subclass)
                               ,data=matchdata_buildingmod,weights=matchdata_buildingmod$weights)


match_meters_nolog_buildingmod <-lm(vanity ~ treatment_median + log(height_occupied_meter) +
                                      decade + complex + function_office + function_residential +
                                      + factor(subclass)
                                    ,data=matchdata_buildingmod,weights=matchdata_buildingmod$weights)


match_meters_log_buildingmod <- lm(log(vanity+1) ~ treatment_median + log(height_occupied_meter) +
                                     decade + complex + function_office + function_residential +
                                     + factor(subclass)
                                   ,data=matchdata_buildingmod,weights=matchdata_buildingmod$weights)



#####     Print Table 2 in Console        #####
stargazer(match_percent_buildingmod, match_meters_log_buildingmod, match_meters_nolog_buildingmod,
          covariate.labels = c("Polyarchy (above median)",
                               "Ln height to top occupied floor",
                               "Part of building-complex",
                               "Building function: Office",
                               "Building function: Residential"),
          title="Skyscraper excessiveness: CEM-matching at the building-level",
          style="ajps",
          digits=2,
          type="text",
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


