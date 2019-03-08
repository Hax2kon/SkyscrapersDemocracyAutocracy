

library(censReg);library(survival);library(plm);library(pscl);library(xtable);library(stargazer)


#####   \\    --    Datasets needed    --    //      #####
load("../Replication/Data/main_dataframe.rdata")
load("../Replication/Data/countryyear_125mLimit.rdata")
load("../Replication/Data/countryyear_InclTowers.rdata")
load("../Replication/Data/countryyear_ExclDemolished.rdata")




#####       \\      --      Table A.1       --      //        #####
descdata <- na.omit(df[,c("delta_nSkyScrapers","delta_MetersOfSkyScrapers","v2x_polyarchy_lag3","e_migdppc_lag3","e_miurbani_lag3","e_mipopula_lag3","e_Total_Resources_Income_PC_lag3")])
descdata$e_mipopula_lag3 <- descdata$e_mipopula_lag3/1000
vanitydesc <- na.omit(df[,c("delta_SumVanity","v2x_polyarchy_lag3","e_migdppc_lag3","e_miurbani_lag3","e_mipopula_lag3","e_Total_Resources_Income_PC_lag3")])[,"delta_SumVanity"]
sumtab <- apply(descdata,2,function(x) round(c(min(x),median(x),mean(x),max(x),sd(x)),2))
sumtab <- cbind(sumtab,"vanity"=round(c("Min"=min(vanitydesc),"Median"=median(vanitydesc),"Mean"=mean(vanitydesc),"Max"=max(vanitydesc),"S.D."=sd(vanitydesc)),2))
colnames(sumtab) <- c("New skyscrapers","New meters of skyscrapers","Polyarchy","GDP per capita","Urbanization, share","Population\n(here shown in 1000s)","Total resources\nincome per capita",
                      "New vanity meters")
sumtab <- t(sumtab)

#####       Print Table A.1 in console      #####
xtable(sumtab,align="lrrrrr",caption="Descriptive statistics for main variables")



#####       \\      --      Table A.2 - A.3      --      //        #####

#####   A.2     #####
cu <- df[,c("v2x_polyarchy", "v2xme_altinf", "v2xlg_legcon", "v2x_corr", "v2pepwrses", "country")]
cu <- data.frame(cor(cu[,setdiff(colnames(cu), "country")], use="pairwise"))

cumatrix <- apply(cu, 2, function(x) sprintf("%.2f", round(x,2)))
cumatrix <- as.matrix(cumatrix)
cumatrix[upper.tri(cumatrix)] <- ""
cumatrix <- data.frame(cumatrix)

colnames(cumatrix) <-  c("Polyarchy", "ASI","LCE",
                         "PCI","PPSP")
rownames(cumatrix) <- c("Polyarchy", "Alternative sources of information","Legislative constraints on executive",
                        "Political Corruption Index","Political Power by socioeconomic position")

#####   Print A.2 in console     #####
cumatrix

#####   A.3   #####
wu <- df[,c("v2x_polyarchy", "v2xme_altinf", "v2xlg_legcon", "v2x_corr", "v2pepwrses", "country")]
wu <- wu %>% group_by(country) %>% mutate_all(funs(. - mean(., na.rm=TRUE)))
wu <- data.frame(cor(wu[,setdiff(colnames(wu), "country")], use="pairwise"))

wumatrix <- apply(wu, 2, function(x) sprintf("%.2f", round(x,2)))
wumatrix <- as.matrix(wumatrix)
wumatrix[upper.tri(wumatrix)] <- ""
wumatrix <- data.frame(wumatrix)

colnames(wumatrix) <-  c("Polyarchy", "ASI","LCE",
                         "PCI","PPSP")
rownames(wumatrix) <- c("Polyarchy", "Alternative sources of information","Legislative constraints on executive",
                        "Political Corruption Index","Political Power by socioeconomic position")


#####   Print A.3 in console     #####
wumatrix


#####   A.4   #####
wucu <- wu/cu

wucumatrix <- apply(wucu, 2, function(x) sprintf("%.2f", round(x,2)))
wucumatrix <- as.matrix(wucumatrix)
wucumatrix[upper.tri(wucumatrix)] <- ""
wucumatrix <- data.frame(wucumatrix)

colnames(wucumatrix) <-  c("Polyarchy", "ASI","LCE",
                           "PCI","PPSP")
rownames(wucumatrix) <- c("Polyarchy", "Alternative sources of information","Legislative constraints on executive",
                          "Political Corruption Index","Political Power by socioeconomic position")


#####   Print A.4 in console     #####
wucumatrix




#####       \\      --      Table A.5       --      //        #####

##### Baseline
baseline_125 <- lm(log(delta_nSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                     log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                     country + factor(year),
                   data=df_125)


#####   No fixed effects
baseline_nocountryFE_125 <- lm(log(delta_nSkyScrapers2+1) ~ v2x_polyarchy_lag3 + 
                                 log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) + 
                                 factor(year),
                               data=df_125)



#####   No log on dependent
baseline_unlogged_125 <- lm(delta_nSkyScrapers ~ v2x_polyarchy_lag3 +
                              log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) + 
                              factor(country) + factor(year),
                            data=df_125)


#####   Zero-inflated
baseline_zeroinflated_125 <- zeroinfl(delta_nSkyScrapers2 ~ 
                                        v2x_polyarchy_lag3 +
                                        log(e_migdppc_lag3) + 
                                        e_miurbani_lag3 + 
                                        log(e_mipopula_lag3) + 
                                        log(e_Total_Resources_Income_PC_lag3+1) +
                                        factor(year) + factor(continents) | 
                                        
                                        v2x_polyarchy_lag3 +
                                        log(e_migdppc_lag3) +
                                        e_miurbani_lag3 +
                                        log(e_mipopula_lag3) + 
                                        log(e_Total_Resources_Income_PC_lag3+1) +
                                        factor(decade2) + factor(continents),
                                      data = df_125,dist="negbin",na.action="na.exclude")

#####   Tobit
pd125 <- pdata.frame(df_125,index=c("country","year"))
tmod_125 <- censReg(log(delta_nSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                      log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1),
                    left = 0, right = Inf, data = pd125, method="BHHH", iterlim=20000)


##### Baseline meters
baseline_meters_125 <- lm(log(delta_MetersOfSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                            log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                            country + factor(year),
                          data=df_125)


##### Baseline meters unlogged
baseline_meters_unlogged_125 <- lm(delta_MetersOfSkyScrapers ~ v2x_polyarchy_lag3 +
                                     log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                                     country + factor(year),
                                   data=df_125)


#####       Print Table A.5 in console      #####
varlist <- c("Polyarchy","Ln GDPpc","Percent urbanization",
             "Ln Population","Ln Resource Income pc")

stargazer(baseline_125,baseline_nocountryFE_125,baseline_unlogged_125,baseline_zeroinflated_125,
          tmod_125,
          baseline_meters_125,baseline_meters_unlogged_125,
          covariate.labels = varlist,
          title="Regime type and the number and meters of Skyscrapers: Cutoff at 125 meter",
          style="ajps",
          digits=2,
          type="text",
          font.size="scriptsize",
          align=FALSE,
          single.row=FALSE,
          notes=c("All covariates lagged by 3 years","Standard error in parentheses"),
          dep.var.labels.include=TRUE,
          dep.var.caption = "",
          dep.var.labels = c("Ln Skyscrapers","Ln Skyscrapers",
                             "Skyscrapers","Skyscrapers","Ln Skyscrapers",
                             "Ln Meters","Meters"),
          column.separate = c(1,1,1,1,1,1,1,1),
          add.lines = list(rep("",8)),
          table.placement = "!h",
          model.names=FALSE,
          column.labels=c("OLS","OLS without country FE",
                          "OLS", "Zeroinflated","Tobit","OLS","OLS"),
          omit=c("country","year","continents"),
          omit.labels = c("Country FE","Year FE","Continent FE"),
          omit.stat = c("f","adj.rsq","ser","ll","aic","bic"))




#####     \\    --    Table A.6   --    //   ####

tallmod <- lm(log(delta_nSkyScrapers_tall+1) ~ v2x_polyarchy_lag3 +
                log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                country + factor(year),
              data=df)

supertallmod <- lm(log(delta_nSkyScrapers_supertall+1) ~ v2x_polyarchy_lag3 +
                     log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                     country + factor(year),
                   data=df)

varlist <- c("Polyarchy","Ln GDPpc","Percent urbanization",
               "Ln Population","Ln Resource Income pc")

stargazer(tallmod,supertallmod,
          covariate.labels = varlist,
          title="Regime type and the number of tall (150m--300m) and supertall (350m+) skyscrapers",
          style="ajps",
          type="text",
          digits=2,
          font.size="scriptsize",
          align=FALSE,
          table.placement = "!h",
          single.row=FALSE,
          dep.var.labels.include=FALSE,
          notes=c("All covariates lagged by 3 years","Standard error in parentheses"),
          dep.var.caption = "",
          add.lines = list(rep("",8)),
          model.names=FALSE, column.labels=c("Tall","Supertall"),
          omit=c("country","year"),
          omit.labels = c("Country FE","Year FE"),
          omit.stat = c("f","adj.rsq","ser","ll","aic","bic"))


#####     \\  --  Table A.7  --  //    #####

##### Baseline
baseline_incltower <- lm(log(delta_nSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                           log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                           country + factor(year),
                         data=df_incltower)

#####   No fixed effects
baseline_nocountryFE_incltower <- lm(log(delta_nSkyScrapers2+1) ~ v2x_polyarchy_lag3 + 
                                       log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) + 
                                       factor(year),
                                     data=df_incltower)

#####   No log on dependent 
baseline_unlogged_incltower <- lm(delta_nSkyScrapers ~ v2x_polyarchy_lag3 +
                                    log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) + 
                                    factor(country) + factor(year),
                                  data=df_incltower)

#####     Zeroinflated
baseline_zeroinflated_incltower <- zeroinfl(delta_nSkyScrapers2 ~ 
                                              v2x_polyarchy_lag3 +
                                              log(e_migdppc_lag3) + 
                                              e_miurbani_lag3 + 
                                              log(e_mipopula_lag3) + 
                                              log(e_Total_Resources_Income_PC_lag3+1) +
                                              factor(year) + factor(continents) | 
                                              
                                              v2x_polyarchy_lag3 +
                                              log(e_migdppc_lag3) +
                                              e_miurbani_lag3 +
                                              log(e_mipopula_lag3) + 
                                              log(e_Total_Resources_Income_PC_lag3+1) +
                                              factor(decade2) + factor(continents),
                                            data = df_incltower,dist="negbin",na.action="na.exclude")


#####   Tobit
pdincltower <- pdata.frame(df_incltower,index=c("country","year"))

tmod_incltower <- censReg(log(delta_nSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                            log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1),
                          left = 0, right = Inf, data = pdincltower, method="BHHH", iterlim=20000)


##### Baseline meters
baseline_meters_incltower <- lm(log(delta_MetersOfSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                                  log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                                  country + factor(year),
                                data=df_incltower)


##### Baseline meters unlogged
baseline_meters_unlogged_incltower <- lm(delta_MetersOfSkyScrapers ~ v2x_polyarchy_lag3 +
                                           log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                                           country + factor(year),
                                         data=df_incltower)

#####             Print table A.4 in console            #####
varlist <- c("Polyarchy","Ln GDPpc","Percent urbanization", "Ln Population","Ln Resource Income pc")
stargazer(baseline_incltower,baseline_nocountryFE_incltower,baseline_unlogged_incltower,
          baseline_zeroinflated_incltower,
          tmod_incltower,
          baseline_meters_incltower,baseline_meters_unlogged_incltower,
          covariate.labels = varlist,
          title="Regime type and the number and meters of Skyscrapers - include towers",
          style="ajps",
          digits=2,
          type="text",
          font.size="scriptsize",
          table.placement = "!h",
          align=FALSE,
          single.row=FALSE,
          notes=c("All covariates lagged by 3 years","Standard error in parentheses"),
          dep.var.labels.include=TRUE,
          dep.var.caption = "",
          dep.var.labels = c("Ln Skyscrapers","Ln Skyscrapers","Skyscrapers","Skyscrapers",
                             "Ln Skyscrapers",
                             "Ln Meters","Meters"),
          column.separate = c(1,1,1,1,1,1,1,1),
          add.lines = list(rep("",8)),
          model.names=FALSE,
          column.labels=c("OLS","OLS without country FE",
                          "OLS",
                          "Zeroinflated","Tobit","OLS","OLS"),
          omit=c("country","year","continents"),
          omit.labels = c("Country FE","Year FE","Continent FE"),
          omit.stat = c("f","adj.rsq","ser","ll","aic","bic"))






#####     \\  --  Table A.8  --  //    #####

##### Baseline
baseline_demolished <- lm(log(delta_nSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                            log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                            country + factor(year),
                          data=df_demolished)

#####   No fixed effects
baseline_nocountryFE_demolished <- lm(log(delta_nSkyScrapers2+1) ~ v2x_polyarchy_lag3 + 
                                        log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) + 
                                        factor(year),
                                      data=df_demolished)

#####   No log on dependent 
baseline_unlogged_demolished <- lm(delta_nSkyScrapers ~ v2x_polyarchy_lag3 +
                                     log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) + 
                                     factor(country) + factor(year),
                                   data=df_demolished)


#####   Zero-inflated
baseline_zeroinflated_demolished <- zeroinfl(delta_nSkyScrapers2 ~ 
                                               v2x_polyarchy_lag3 +
                                               log(e_migdppc_lag3) + 
                                               e_miurbani_lag3 + 
                                               log(e_mipopula_lag3) + 
                                               log(e_Total_Resources_Income_PC_lag3+1) +
                                               factor(year) + factor(continents) | 
                                               
                                               v2x_polyarchy_lag3 +
                                               log(e_migdppc_lag3) +
                                               e_miurbani_lag3 +
                                               log(e_mipopula_lag3) + 
                                               log(e_Total_Resources_Income_PC_lag3+1) +
                                               factor(decade2) + factor(continents),
                                             data = df_demolished,dist="negbin",na.action="na.exclude")


#####   Tobit
pddemolished <- pdata.frame(df_demolished,index=c("country","year"))

tmod_demolished <- censReg(log(delta_nSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                             log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1),
                           left = 0, right = Inf, data = pddemolished, method="BHHH", iterlim=20000)


##### Baseline meters
baseline_meters_demolished <- lm(log(delta_MetersOfSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                                   log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                                   country + factor(year),
                                 data=df_demolished)


##### Baseline meters unlogged
baseline_meters_unlogged_demolished <- lm(delta_MetersOfSkyScrapers ~ v2x_polyarchy_lag3 +
                                            log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                                            country + factor(year),
                                          data=df_demolished)



#####            Print Table A.5 in console             #####
varlist <- c("Polyarchy","Ln GDPpc","Percent urbanization",
             "Ln Population","Ln Resource Income pc")

stargazer(baseline_demolished,baseline_nocountryFE_demolished,baseline_unlogged_demolished,
          baseline_zeroinflated_demolished,
          tmod_demolished,
          baseline_meters_demolished,baseline_meters_unlogged_demolished,
          covariate.labels = varlist,
          title="Regime type and the number and meters of Skyscrapers - exclude demolished buildings",
          style="ajps",
          type="text",
          digits=2,
          font.size="scriptsize",
          align=FALSE,
          single.row=FALSE,
          notes=c("All covariates lagged by 3 years","Standard error in parentheses"),
          dep.var.labels.include=TRUE,
          dep.var.caption = "",
          dep.var.labels = c("Ln Skyscrapers","Ln Skyscrapers",
                             "Skyscrapers","Skyscrapers",
                             "Ln Skyscrapers",
                             "Ln Meters","Meters"),
          column.separate = c(1,1,1,1,1,1,1,1),
          add.lines = list(rep("",8)),
          model.names=FALSE,
          table.placement = "!h",
          column.labels=c("OLS","OLS without country FE",
                          "OLS",
                          "Zeroinflated","Tobit","OLS","OLS"),
          omit=c("country","year","continents"),
          omit.labels = c("Country FE","Year FE","Continent FE"),
          omit.stat = c("f","adj.rsq","ser","ll","aic","bic"))



#####     \\    --    Table A.9     --      //      #####
#Model 1
baseline_capital <- lm(log(delta_SkyscrapersInCapital+1) ~ v2x_polyarchy_lag3 +
                         log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                         country +  factor(year),
                       data=df)

#Model 2
baseline_meters_capital <- lm(log(meters_in_capital+1) ~ v2x_polyarchy_lag3 +
                                log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                                country +  factor(year),
                              data=df)

#Model 3
baseline_notcapital <- lm(log(delta_SkyscrapersNotInCapital+1) ~ v2x_polyarchy_lag3 +
                            log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                            country +  factor(year),
                          data=df)

#Model 4
baseline_meters_notcapital <- lm(log(meters_not_in_capital+1) ~ v2x_polyarchy_lag3 +
                                   log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                                   country +  factor(year),
                                 data=df)


varlist <- c("Polyarchy", "Ln GDPpc","Percent\nurbanization","Ln Population","Ln Resource\nIncome pc")
stargazer(baseline_capital,baseline_meters_capital,
          baseline_notcapital, baseline_meters_notcapital,
          covariate.labels = varlist,
          title="Skyscrapers inside and outside the capital city",
          style="ajps",
          type="text",
          digits=2,
          font.size="scriptsize",
          align=FALSE,
          table.placement = "!htbp",
          single.row=FALSE,
          notes=c("All covariates lagged by 3 years","Standard error in parentheses"),
          dep.var.labels.include=TRUE,
          dep.var.caption = "",
          dep.var.labels = c("Ln Skyscrapers","Ln Meters",
                             "Ln Skyscrapers","Ln Meters"),
          column.separate = c(1,1,1,1,1,1,1,1),
          add.lines = list(c("Sample:", "Capital", "Capital", "Not capital", "Not capital"),
                           rep("",8)),
          model.names=FALSE,
          column.labels=c("OLS", "OLS", "OLS","OLS"),
          omit=c("country","year"),
          omit.labels = c("Country FE","Year FE"),
          omit.stat = c("f","adj.rsq","ser","ll","aic","bic"))




#####     \\    --    Table A.10     --      //      #####

#Equalize N across models
seriatim_data <- na.omit(df[,c("delta_nSkyScrapers","v2x_polyarchy_lag3","e_migdppc_lag3","e_miurbani_lag3",
                               "e_mipopula_lag3","e_Total_Resources_Income_PC_lag3","country","year")])

baseline <- lm(log(delta_nSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                 log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                 country + factor(year),
               data=seriatim_data)
seriatim_resources <- lm(log(delta_nSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                           log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) +
                           country + factor(year),
                         data=seriatim_data)
seriatim_population <- lm(log(delta_nSkyScrapers+1) ~ v2x_polyarchy_lag3 + 
                            log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_Total_Resources_Income_PC_lag3+1) +
                            country + factor(year),
                          data=seriatim_data)
seriatim_urban <- lm(log(delta_nSkyScrapers+1) ~ v2x_polyarchy_lag3 + 
                       log(e_migdppc_lag3) + log(e_mipopula_lag3) +  log(e_Total_Resources_Income_PC_lag3+1) +
                       country + factor(year),
                     data=seriatim_data)
seriatim_gdp <- lm(log(delta_nSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                     e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                     country + factor(year),
                   data=seriatim_data)

#####       Print Table A.6 in console          #####
varlist <- c("Polyarchy","Ln GDPpc","Percent urbanization",
             "Ln Population","Ln Resource Income pc")

stargazer(baseline,seriatim_resources,seriatim_population,seriatim_urban,seriatim_gdp,
          covariate.labels = varlist,
          title="Regime type and ln number of new skyscrapers -- dropping controls",
          style="ajps",
          type="text",
          digits=2,
          font.size="scriptsize",
          align=FALSE,
          single.row=FALSE,
          notes=c("All covariates lagged by 3 years","Standard error in parentheses"),
          dep.var.labels.include=FALSE,
          dep.var.caption = "",
          add.lines = list(rep("",8)),
          table.placement = "!h",
          model.names=FALSE,
          omit=c("country","year"),
          omit.labels = c("Country FE","Year FE"),
          omit.stat = c("f","adj.rsq","ser","ll","aic","bic"))



#####     \\  --  Table A.11  --  //      #####
extracontrolmod_growth5 <- lm(log(delta_nSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                                log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                                growth5_lag3 +
                                country + factor(year),
                              data=df)

extracontrolmod_delta_urbanization <- lm(log(delta_nSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                                           log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                                           delta_urbanization_lag3 +
                                           country + factor(year),
                                         data=df)

extracontrolmod_v2clstown <- lm(log(delta_nSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                                  log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                                  v2clstown_lag3 +
                                  country + factor(year),
                                data=df)

extracontrolmod_e_miinteco <- lm(log(delta_nSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                                   log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                                   e_miinteco_lag3 +
                                   country + factor(year),
                                 data=df)

extracontrolmod_e_miinterc <- lm(log(delta_nSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                                   log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                                   e_miinterc_lag3 +
                                   country + factor(year),
                                 data=df)

extracontrolmod_nSkyscrapers <- lm(log(delta_nSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                                     log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                                     log(df$nSkyScrapers_lag1 + 1) +
                                     country + factor(year),
                                   data=df)

extracontrolmod <- lm(log(delta_nSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                        log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                        growth5_lag3 + delta_urbanization_lag3 + v2clstown_lag3 + e_miinteco_lag3 + e_miinterc_lag3 +
                        log(df$nSkyScrapers_lag1 + 1) +
                        country + factor(year),
                      data=df)

varlist <- c("Polyarchy","Ln GDPpc","Percent urbanization",
             "Ln Population","Ln Resource Income pc",
             "GDP Growth in the past 5 years",
             "Change urbanization",
             "State control over resources",
             "International conflict",
             "Internal conflict",
             "Ln Skyscrapers\\textsubscript{t-1}")

#####     Print Table A.11 in console      #####
stargazer(extracontrolmod_growth5, extracontrolmod_delta_urbanization, extracontrolmod_v2clstown,
          extracontrolmod_e_miinteco, extracontrolmod_e_miinterc, extracontrolmod_nSkyscrapers,
          extracontrolmod,
          covariate.labels = varlist,
          title="Regime type and the ln number of Skyscrapers with extra controls",
          style="ajps",
          digits=2,
          type="text",
          font.size="scriptsize",
          align=FALSE,
          table.placement = "!h",
          single.row=FALSE,
          dep.var.labels.include=FALSE,
          notes=c("All covariates lagged by 3 years","Standard error in parentheses"),
          dep.var.caption = "",
          add.lines = list(rep("",8)),
          model.names=FALSE,
          omit=c("country","year"),
          omit.labels = c("Country FE","Year FE"),
          omit.stat = c("f","adj.rsq","ser","ll","aic","bic"))


#####     \\    --    Table A.12      --    //      #####

# Liberal democracy
baseline_liberal <- lm(log(delta_nSkyScrapers+1) ~ v2x_libdem_lag3 +
                         log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                         country + factor(year),
                       data=df)



#Participatory democracy
baseline_participatory <- lm(log(delta_nSkyScrapers+1) ~ v2x_partipdem_lag3 +
                               log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                               country + factor(year),
                             data=df)



#Deliberatory democracy
baseline_deliberatory <- lm(log(delta_nSkyScrapers+1) ~ v2x_delibdem_lag3 +
                              log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                              country + factor(year),
                            data=df)

#Egalitarian democracy
baseline_egalitarian <- lm(log(delta_nSkyScrapers+1) ~ v2x_egaldem_lag3 +
                             log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                             country + factor(year),
                           data=df)

#BMR
baseline_boix <- lm(log(delta_nSkyScrapers+1) ~ e_boix_regime_lag3 +
                      log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                      country + factor(year),
                    data=df)

#Polity
baseline_polity <- lm(log(delta_nSkyScrapers+1) ~ e_polity2_lag3 +
                        log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                        country + factor(year),
                      data=df)

#Vanhanen
baseline_vanhan <- lm(log(delta_nSkyScrapers+1) ~ e_mivanhan_lag3 +
                        log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                        country + factor(year),
                      data=df)

#CHGA
baseline_chga <- lm(log(delta_nSkyScrapers+1) ~ e_chga_demo_lag3 +
                      log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                      country + factor(year),
                    data=df)

#####  Print Table A.12 in console   #####
varlist <- c("Democracy-measure","Ln GDPpc","Percent urbanization",
             "Ln Population","Ln Resource Income pc")


#Replace names for table
names(baseline_liberal$coefficients)[2] <- "Democracy measure"
names(baseline_participatory$coefficients)[2] <- "Democracy measure"
names(baseline_deliberatory$coefficients)[2] <- "Democracy measure"
names(baseline_egalitarian$coefficients)[2] <- "Democracy measure"
names(baseline_boix$coefficients)[2] <- "Democracy measure"
names(baseline_polity$coefficients)[2] <- "Democracy measure"
names(baseline_vanhan$coefficients)[2] <- "Democracy measure"
names(baseline_chga$coefficients)[2] <- "Democracy measure"


stargazer(baseline_liberal, baseline_participatory, baseline_deliberatory,
          baseline_egalitarian, baseline_boix, baseline_polity, baseline_vanhan, baseline_chga,
          covariate.labels = varlist,
          title="Alternative democracy measures",
          style="ajps",
          type="text",
          digits=3,
          font.size="tiny",
          align=FALSE,
          table.placement = "!h",
          single.row=FALSE,
          dep.var.labels.include=FALSE,
          notes=c("All covariates lagged by 3 years", "Standard error in parentheses"),
          dep.var.caption = "",
          add.lines = list(rep("",8)),
          model.names=FALSE,
          column.labels=c("V-Dem Liberal", "V-Dem Participatory",
                          "V-Dem Deliberatory","V-Dem Egalitarian", 
                          "BMR", "Polity2", "Vanhanen", "DD"),
          omit=c("country","year"),
          omit.labels = c("Country FE","Year FE"),
          omit.stat = c("f","adj.rsq","ser","ll","aic","bic"))

#####     \\      --      Table A.13    --    //    #####

#Fix correct reference category
df$anckar_regimes_edited_lag3 <- as.factor(df$anckar_regimes_edited_lag3)
df$anckar_regimes_edited_lag3 <- factor(df$anckar_regimes_edited_lag3, levels=c("democracy", "Military rule", "Monarchy",
                                                                                "Other dictatorship", 
                                                                                "Party-based autocracy", "Personalist rule"))


##### Baseline
anckar <- lm(log(delta_nSkyScrapers+1) ~ anckar_regimes_edited_lag3 +
               log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
               country +  factor(year),
             data=df)

##### anckar meters
anckar_meters <- lm(log(delta_MetersOfSkyScrapers+1) ~ anckar_regimes_edited_lag3 +
                      log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                      country + factor(year),
                    data=df)

##### anckar meters unlogged
anckar_meters_unlogged <- lm(delta_MetersOfSkyScrapers ~ anckar_regimes_edited_lag3 +
                               log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                               country + factor(year),
                             data=df)


#####   No fixed effects
df$delta_nSkyScrapers2 <- df$delta_nSkyScrapers
anckar_nocountryFE <- lm(log(delta_nSkyScrapers2+1) ~ anckar_regimes_edited_lag3 + 
                           log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) + 
                           factor(year),
                         data=df)

#####   No log on dependent
anckar_unlogged <- lm(delta_nSkyScrapers ~ anckar_regimes_edited_lag3 +
                        log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) + 
                        factor(country) + factor(year),
                      data=df)

#####   Tobit
pd <- pdata.frame(df,index=c("country","year"))
tmod<- censReg(log(delta_nSkyScrapers+1) ~ anckar_regimes_edited_lag3 +
                 log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1),
               left = 0, right = Inf, data = pd,method="BHHH", iterlim=20000)


#####   Zero-inflated
anckar_zeroinflated <- zeroinfl(delta_nSkyScrapers2 ~ 
                                  anckar_regimes_edited_lag3 +
                                  log(e_migdppc_lag3) + 
                                  e_miurbani_lag3 + 
                                  log(e_mipopula_lag3) + 
                                  log(e_Total_Resources_Income_PC_lag3+1) +
                                  factor(year) + factor(continents) | 
                                  
                                  anckar_regimes_edited_lag3 +
                                  log(e_migdppc_lag3) +
                                  e_miurbani_lag3 +
                                  log(e_mipopula_lag3) + 
                                  log(e_Total_Resources_Income_PC_lag3+1) +
                                  factor(decade2) + factor(continents),
                                data = df, dist="negbin",na.action="na.exclude")


#####     Print A.13 in console     #####
varlist <- c("Military rule", "Monarchy",
             "Other dictatorship", 
             "Party-based autocracy", "Personalist rule",
             "Ln GDPpc","Percent\nurbanization","Ln Population","Ln Resource\nIncome pc")

stargazer(anckar,anckar_nocountryFE,anckar_unlogged,
          anckar_zeroinflated, tmod,
          anckar_meters,anckar_meters_unlogged,
          covariate.labels = varlist,
          title="Autocratic regime type and the number and meters of Skyscrapers",
          style="ajps",
          type="text",
          digits=2,
          font.size="scriptsize",
          align=FALSE,
          table.placement = "!htbp",
          single.row=FALSE,
          notes=c("All covariates lagged by 3 years","Standard error in parentheses"),
          dep.var.labels.include=TRUE,
          dep.var.caption = "",
          dep.var.labels = c("Ln Skyscrapers","Ln Skyscrapers","Skyscrapers",
                             "Skyscrapers","Ln Skyscrapers",
                             "Ln Meters","Meters"),
          column.separate = c(1,1,1,1,1,1,1,1),
          add.lines = list(rep("",8)),
          model.names=FALSE,
          column.labels=c("OLS","OLS w.o. country FE",
                          "OLS",
                          "Zeroinflated","Tobit","OLS","OLS"),
          omit=c("country","year","continents"),
          omit.labels = c("Country FE","Year FE","Continent FE"),
          omit.stat = c("f","adj.rsq","ser","ll","aic","bic"))




#####     \\      --      Table A.14     --      //      #####

##### Baseline
baseline_growthvar <- lm(log(delta_nSkyScrapers +1) ~ v2x_polyarchy_lag3 +
                           stddev10growth_lag3 +
                           log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                           country +  factor(year),
                         data=df)

####    Unlogged
baseline_unlogged_growthvar <- lm(delta_nSkyScrapers ~ v2x_polyarchy_lag3 +
                                    stddev10growth_lag3 +
                                    log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                                    country +  factor(year),
                                  data=df)

#####   Meters
baseline_meters_growthvar <- lm(log(delta_MetersOfSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                                  stddev10growth_lag3 +
                                  log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                                  country +  factor(year),
                                data=df)

#####   Meters unlogged
baseline_meters_unlogged_growthvar <- lm(delta_MetersOfSkyScrapers ~ v2x_polyarchy_lag3 +
                                           stddev10growth_lag3 +
                                           log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                                           country +  factor(year),
                                         data=df)

#####     Print Table A.14 in console     #####
varlist <- c("Polyarchy", "Standard deviation growth past 10 years", "Ln GDPpc","Percent urbanization",
             "Ln Population","Ln Resource Income pc")

stargazer(baseline_growthvar,baseline_unlogged_growthvar,
          baseline_meters_growthvar,baseline_meters_unlogged_growthvar,
          covariate.labels = varlist,
          title="Boom cycles and the number and meters of Skyscrapers",
          style="ajps",
          type="text",
          digits=2,
          font.size="scriptsize",
          align=FALSE,
          table.placement = "!htbp",
          single.row=FALSE,
          notes=c("All covariates lagged by 3 years","Standard error in parentheses"),
          dep.var.labels.include=TRUE,
          dep.var.caption = "",
          dep.var.labels = c("Ln Skyscrapers","Skyscrapers", "Ln Meters","Meters"),
          column.separate = c(1,1,1,1,1,1,1,1),
          add.lines = list(rep("",8)),
          model.names=FALSE,
          column.labels=c("OLS", "OLS", "OLS","OLS"),
          omit=c("country","year"),
          omit.labels = c("Country FE","Year FE"),
          omit.stat = c("f","adj.rsq","ser","ll","aic","bic"))


#####     \\      --      Table A.15     --      //      #####
df$delta_nSkyScrapers2 <- df$delta_nSkyScrapers #Just makes it easier with Stargazer

##### Baseline
baseline_lag4 <- lm(log(delta_nSkyScrapers+1) ~ v2x_polyarchy_lag4 +
                      log(e_migdppc_lag4) + e_miurbani_lag4 + log(e_mipopula_lag4) + log(e_Total_Resources_Income_PC_lag4+1) +
                      country + factor(year),
                    data=df)

#####   No fixed effects
baseline_nocountryFE_lag4 <- lm(log(delta_nSkyScrapers2+1) ~ v2x_polyarchy_lag4 + 
                                  log(e_migdppc_lag4) + e_miurbani_lag4 + log(e_mipopula_lag4) + log(e_Total_Resources_Income_PC_lag4+1) + 
                                  factor(year),
                                data=df)


#####   No log on dependent 
baseline_unlogged_lag4 <- lm(delta_nSkyScrapers ~ v2x_polyarchy_lag4 +
                               log(e_migdppc_lag4) + e_miurbani_lag4 + log(e_mipopula_lag4) + log(e_Total_Resources_Income_PC_lag4+1) + 
                               factor(country) + factor(year),
                             data=df)


#####   Zero-inflated
baseline_zeroinflated_lag4 <- zeroinfl(delta_nSkyScrapers2 ~ 
                                         v2x_polyarchy_lag4 +
                                         log(e_migdppc_lag4) + 
                                         e_miurbani_lag4 + 
                                         log(e_mipopula_lag4) + 
                                         log(e_Total_Resources_Income_PC_lag4+1) +
                                         factor(year) + factor(continents) | 
                                         
                                         v2x_polyarchy_lag4 +
                                         log(e_migdppc_lag4) +
                                         e_miurbani_lag4 +
                                         log(e_mipopula_lag4) + 
                                         log(e_Total_Resources_Income_PC_lag4+1) +
                                         factor(decade2) + factor(continents),
                                       data = df,dist="negbin",na.action="na.exclude")

#####   Tobit
pd <- pdata.frame(df,index=c("country","year"))
tmod_lag4 <- censReg(log(delta_nSkyScrapers+1) ~ v2x_polyarchy_lag4 +
                       log(e_migdppc_lag4) + e_miurbani_lag4 + log(e_mipopula_lag4) + log(e_Total_Resources_Income_PC_lag4+1),
                     left = 0, right = Inf, data = pd, method="BHHH", iterlim=20000)


##### Baseline meters
baseline_meters_lag4 <- lm(log(delta_MetersOfSkyScrapers+1) ~ v2x_polyarchy_lag4 +
                             log(e_migdppc_lag4) + e_miurbani_lag4 + log(e_mipopula_lag4) + log(e_Total_Resources_Income_PC_lag4+1) +
                             country + factor(year),
                           data=df)

##### Baseline meters unlogged
baseline_meters_unlogged_lag4 <- lm(delta_MetersOfSkyScrapers ~ v2x_polyarchy_lag4 +
                                      log(e_migdppc_lag4) + e_miurbani_lag4 + log(e_mipopula_lag4) + log(e_Total_Resources_Income_PC_lag4+1) +
                                      country + factor(year),
                                    data=df)


#####   Print Table A.15 in console   #####
varlist <- c("Polyarchy","Ln GDPpc","Percent urbanization", "Ln Population","Ln Resource Income pc")

stargazer(baseline_lag4,baseline_nocountryFE_lag4,baseline_unlogged_lag4,baseline_zeroinflated_lag4,
          tmod_lag4, baseline_meters_lag4,baseline_meters_unlogged_lag4,
          covariate.labels = varlist,
          title="Regime type and the number and meters of Skyscraper - 4 years lag",
          style="ajps",
          digits=2,
          type="text",
          font.size="scriptsize",
          align=FALSE,
          single.row=FALSE,
          notes=c("All covariates lagged by 4 years","Standard error in parentheses"),
          dep.var.labels.include=TRUE,
          dep.var.caption = "",
          dep.var.labels = c("Ln Skyscrapers","Ln Skyscrapers",
                             "Skyscrapers","Skyscrapers",
                             "Ln Skyscrapers",
                             "Ln Meters","Meters"),
          column.separate = c(1,1,1,1,1,1,1,1),
          add.lines = list(rep("",8)),
          model.names=FALSE,
          table.placement = "!h",
          column.labels=c("OLS","OLS without country FE",
                          "OLS",
                          "Zeroinflated","Tobit","OLS","OLS"),
          omit=c("country","year","continents"),
          omit.labels = c("Country FE","Year FE","Continent FE"),
          omit.stat = c("f","adj.rsq","ser","ll","aic","bic"))



#####       \\      --    Table A.16, A.17 and A.18    --     //    #####
df45 <- df[which(df$year>=1945),]
df60 <- df[which(df$year>=1960),]
df80 <- df[which(df$year>=1980),]


#####   Baseline
baseline45 <- lm(log(delta_nSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                   log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                   country + factor(year),
                 data=df45)
baseline60 <- lm(log(delta_nSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                   log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                   country + factor(year),
                 data=df60)
baseline80 <- lm(log(delta_nSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                   log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                   country + factor(year),
                 data=df80)


#####   No fixed effects
baseline_nocountryFE45 <- lm(log(delta_nSkyScrapers2+1) ~ v2x_polyarchy_lag3 + 
                               log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) + 
                               factor(year),
                             data=df45)
baseline_nocountryFE60 <- lm(log(delta_nSkyScrapers2+1) ~ v2x_polyarchy_lag3 + 
                               log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) + 
                               factor(year),
                             data=df60)
baseline_nocountryFE80 <- lm(log(delta_nSkyScrapers2+1) ~ v2x_polyarchy_lag3 + 
                               log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) + 
                               factor(year),
                             data=df80)

#####   No log on dependent 
baseline_unlogged45 <- lm(delta_nSkyScrapers ~ v2x_polyarchy_lag3 +
                            log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) + 
                            factor(country) + factor(year),
                          data=df45)
baseline_unlogged60 <- lm(delta_nSkyScrapers ~ v2x_polyarchy_lag3 +
                            log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) + 
                            factor(country) + factor(year),
                          data=df60)
baseline_unlogged80 <- lm(delta_nSkyScrapers ~ v2x_polyarchy_lag3 +
                            log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) + 
                            factor(country) + factor(year),
                          data=df80)

#####   Zero-inflated
baseline_zeroinflated45 <- zeroinfl(delta_nSkyScrapers2 ~ 
                                      v2x_polyarchy_lag3 +
                                      log(e_migdppc_lag3) + 
                                      e_miurbani_lag3 + 
                                      log(e_mipopula_lag3) + 
                                      log(e_Total_Resources_Income_PC_lag3+1) +
                                      factor(year) + factor(continents) | 
                                      
                                      v2x_polyarchy_lag3 +
                                      log(e_migdppc_lag3) +
                                      e_miurbani_lag3 +
                                      log(e_mipopula_lag3) + 
                                      log(e_Total_Resources_Income_PC_lag3+1) +
                                      factor(decade2) + factor(continents),
                                    data = df45,dist="negbin",na.action="na.exclude")

baseline_zeroinflated60 <- zeroinfl(delta_nSkyScrapers2 ~ 
                                      v2x_polyarchy_lag3 +
                                      log(e_migdppc_lag3) + 
                                      e_miurbani_lag3 + 
                                      log(e_mipopula_lag3) + 
                                      log(e_Total_Resources_Income_PC_lag3+1) +
                                      factor(year) + factor(continents) | 
                                      
                                      v2x_polyarchy_lag3 +
                                      log(e_migdppc_lag3) +
                                      e_miurbani_lag3 +
                                      log(e_mipopula_lag3) + 
                                      log(e_Total_Resources_Income_PC_lag3+1) +
                                      factor(decade2) + factor(continents),
                                    data = df60,dist="negbin",na.action="na.exclude")

baseline_zeroinflated80 <- zeroinfl(delta_nSkyScrapers2 ~ 
                                      v2x_polyarchy_lag3 +
                                      log(e_migdppc_lag3) + 
                                      e_miurbani_lag3 + 
                                      log(e_mipopula_lag3) + 
                                      log(e_Total_Resources_Income_PC_lag3+1) +
                                      factor(year) + factor(continents) | 
                                      
                                      v2x_polyarchy_lag3 +
                                      log(e_migdppc_lag3) +
                                      e_miurbani_lag3 +
                                      log(e_mipopula_lag3) + 
                                      log(e_Total_Resources_Income_PC_lag3+1) +
                                      factor(decade2) + factor(continents),
                                    data = df80,dist="negbin",na.action="na.exclude")


#####   Tobit
pd45 <- pdata.frame(df45,index=c("country","year"))
pd60 <- pdata.frame(df60,index=c("country","year"))
pd80 <- pdata.frame(df80,index=c("country","year"))

tmod45 <- censReg(log(delta_nSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                    log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1),
                  left = 0, right = Inf, data = pd45, method="BHHH", iterlim=20000)

tmod60 <- censReg(log(delta_nSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                    log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1),
                  left = 0, right = Inf, data = pd60, method="BHHH", iterlim=20000)

tmod80 <- censReg(log(delta_nSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                    log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1),
                  left = 0, right = Inf, data = pd80, method="BHHH", iterlim=20000)




#####   Baseline meters
baseline_meters45 <- lm(log(delta_MetersOfSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                          log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                          country + factor(year),
                        data=df45)
baseline_meters60 <- lm(log(delta_MetersOfSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                          log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                          country + factor(year),
                        data=df60)
baseline_meters80 <- lm(log(delta_MetersOfSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                          log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                          country + factor(year),
                        data=df80)


##### Baseline meters unlogged
baseline_meters_unlogged45 <- lm(delta_MetersOfSkyScrapers ~ v2x_polyarchy_lag3 +
                                   log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                                   country + factor(year),
                                 data=df45)
baseline_meters_unlogged60 <- lm(delta_MetersOfSkyScrapers ~ v2x_polyarchy_lag3 +
                                   log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                                   country + factor(year),
                                 data=df60)
baseline_meters_unlogged80 <- lm(delta_MetersOfSkyScrapers ~ v2x_polyarchy_lag3 +
                                   log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                                   country + factor(year),
                                 data=df80)

#####     Print Table A.16 in console    #####
varlist <- c("Polyarchy","Ln GDPpc","Percent urbanization", "Ln Population","Ln Resource Income pc")

stargazer(baseline45,baseline_nocountryFE45,baseline_unlogged45,baseline_zeroinflated45,
          tmod45,
          baseline_meters45,baseline_meters_unlogged45,
          covariate.labels = varlist,
          title="Regime type and the number and meters of Skyscrapers post 1945",
          style="ajps",
          type="text",
          digits=2,
          font.size="scriptsize",
          align=FALSE,
          single.row=FALSE,
          dep.var.labels = c("Ln Skyscrapers","Ln Skyscrapers",
                             "Skyscrapers", "Skyscrapers",
                             "Ln Skyscrapers",
                             "Ln Meters","Meters"),
          notes=c("All covariates lagged by 3 years","Standard error in parentheses"),
          dep.var.caption = "",
          add.lines = list(rep("",8)),
          model.names=FALSE, column.labels=c("OLS","OLS w.o. country FE",
                                             "OLS",
                                             "Zeroinflated","Tobit","OLS","OLS"),
          omit=c("country","year","continents"),
          omit.labels = c("Country FE","Year FE","Continent FE"),
          omit.stat = c("f","adj.rsq","ser","ll","aic","bic"))

#####     Print Table A.17 in console    #####
stargazer(baseline60,baseline_nocountryFE60,baseline_unlogged60,baseline_zeroinflated60,
          tmod60, baseline_meters60,baseline_meters_unlogged60,
          covariate.labels = varlist,
          title="Regime type and the number and meters of Skyscrapers post 1960",
          style="ajps",
          type="text",
          digits=2,
          font.size="scriptsize",
          align=FALSE,
          single.row=FALSE,
          dep.var.labels = c("Ln Skyscrapers","Ln Skyscrapers",
                             "Skyscrapers", "Skyscrapers",
                             "Ln Skyscrapers",
                             "Ln Meters","Meters"),
          notes=c("All covariates lagged by 3 years","Standard error in parentheses"),
          dep.var.caption = "",
          add.lines = list(rep("",8)),
          model.names=FALSE, column.labels=c("OLS","OLS w.o. country FE",
                                             "OLS",
                                             "Zeroinflated","Tobit","OLS","OLS"),
          omit=c("country","year","continents"),
          omit.labels = c("Country FE","Year FE","Continent FE"),
          omit.stat = c("f","adj.rsq","ser","ll","aic","bic"))


######      Print Table A.18 in console     #####
stargazer(baseline80,baseline_nocountryFE80, baseline_unlogged80,
          baseline_zeroinflated80, tmod80, baseline_meters80,baseline_meters_unlogged80,
          covariate.labels = varlist,
          title="Regime type and the number and meters of Skyscrapers post 1980",
          style="ajps",
          type="text",
          digits=2,
          font.size="scriptsize",
          align=FALSE,
          single.row=FALSE,
          dep.var.labels = c("Ln Skyscrapers","Ln Skyscrapers",
                             "Skyscrapers", "Skyscrapers",
                             "Ln Skyscrapers",
                             "Ln Meters","Meters"),
          notes=c("All covariates lagged by 3 years","Standard error in parentheses"),
          dep.var.caption = "",
          add.lines = list(rep("",8)),
          model.names=FALSE, column.labels=c("OLS","OLS w.o. country FE",
                                             "OLS",
                                             "Zeroinflated","Tobit","OLS","OLS"),
          omit=c("country","year","continents"),
          omit.labels = c("Country FE","Year FE","Continent FE"),
          omit.stat = c("f","adj.rsq","ser","ll","aic","bic"))




#####   \\    --    Table A.19   --    //   #####

baseline_zeroinflated <- zeroinfl(delta_nSkyScrapers ~ 
                                    v2x_polyarchy_lag3 +
                                    log(e_migdppc_lag3) + 
                                    e_miurbani_lag3 + 
                                    log(e_mipopula_lag3) + 
                                    log(e_Total_Resources_Income_PC_lag3+1) +
                                    factor(year) + factor(continents) | 
                                    
                                    v2x_polyarchy_lag3 +
                                    log(e_migdppc_lag3) +
                                    e_miurbani_lag3 +
                                    log(e_mipopula_lag3) + 
                                    log(e_Total_Resources_Income_PC_lag3+1) +
                                    factor(decade2) + factor(continents),
                                  data = df, dist="negbin",na.action="na.exclude")

#####       Print Table A.13 in console     #####
stargazer(baseline_zeroinflated,
          covariate.labels = varlist,
          zero.component=TRUE,
          title="First stafe of main zero-inflated model",
          style="ajps",
          digits=2,
          type="text",
          font.size="scriptsize",
          align=FALSE,
          single.row=FALSE,
          notes=c("All covariates lagged by 3 years","Standard error in parentheses"),
          dep.var.labels.include=FALSE,
          dep.var.caption = "",
          column.separate = c(1,1,1,1,1,1,1,1),
          add.lines = list(rep("",8)),
          model.names=FALSE,
          omit=c("decade2","continents"),
          omit.labels = c("Decade FE","Continent FE"),
          omit.stat = c("f","adj.rsq","ser","ll","aic","bic"))




######        \\    --    Figure A.2       --      //        #####
baseline <- lm(log(delta_nSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                 log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                 country +  factor(year),
               data=df)

modeldata <- model.frame(baseline)
modeldata$poly_median <- ifelse(modeldata$v2x_polyarchy_lag3 <= median(modeldata$v2x_polyarchy_lag3, na.rm=TRUE), 0, 1)
modeldata_nozero <- modeldata[which(modeldata[,1]>0), ]

boxplot(modeldata_nozero[, 1] ~ modeldata_nozero$poly_median, 
        axes=FALSE, ylab= "Ln change in Skyscrapers")
axis(1, at=c(1, 2), labels=c("Autocracies", "Democracies"), col="white")
axis(2, at=seq(0, 5, 1), las=2)





######          \\    --      Figure A.3 and A.4   --    //      #####
cooksd <- cooks.distance(baseline)
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
coefvec <- rep(NA, length(influential))

#Jackknife influetial observations
for(i in 1:length(influential)){
  mod <- update(baseline, data=df[-influential[i], ])
  coefvec[i]   <- summary(mod)$coefficients[2, 1]
}

#####   Figure A.4
plot(cooksd, pch="*", cex=2, main=" ", xlab= "Observation", ylab= "Cook's D")
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line

#####   Figure A.5
hist(coefvec, breaks=10, axes=FALSE, main="", xlab="Point-estimate for polyarchy")
axis(2, at=seq(-20, 200, 20), las=2)
axis(1, at=seq(-0.09, 0, 0.001))



#####       \\      --      Table A.20    --      //      #####

property_base <- lm(log(delta_nSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                      v2xcl_prpty_lag3 +
                      log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                      country + factor(year),
                    data=df)


#####     Print Table A.20 in console     #####
varlist <- c("Polyarchy", "Property rights index", "Ln GDPpc", "Percent urbanization",
             "Ln Population","Ln Resource Income pc")
stargazer(property_base,
          covariate.labels = varlist,
          title="Property rights",
          style="ajps",
          type="text",
          digits=2,
          font.size="scriptsize",
          align=FALSE,
          single.row=FALSE,
          dep.var.labels.include=FALSE,
          notes=c("All covariates lagged by 3 years", "Standard error in parentheses"),
          table.placement = "!h",
          add.lines = list(rep("",8)),
          model.names=FALSE,
          omit=c("country","year"),
          omit.labels = c("Country FE","Year FE"),
          omit.stat = c("f","adj.rsq","ser","ll","aic","bic"))



#####       \\      --      Table A.21    --      //      #####

#####   Baseline vanity meters log
baseline_vanity_meters_log <- lm(log(delta_SumVanity+1) ~ v2x_polyarchy_lag3  +
                                   log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                                   country + factor(year),
                                 data=df)


#####   Baseline vanity meters absolute
baseline_vanity_meters_nolog <- lm(delta_SumVanity ~ v2x_polyarchy_lag3  +
                                     log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                                     country + factor(year),
                                   data=df)

#####   Baseline vanity percent
baseline_vanity_percent <- lm(delta_VanityPercent ~ v2x_polyarchy_lag3 + 
                                log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                                country + factor(year),
                              data=df)

#####   Baseline vanity max percent
baseline_vanity_maxpercent <- lm(delta_MaxVanityPercent ~ v2x_polyarchy_lag3  +
                                   log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                                   country + factor(year),
                                 data=df)


#####     Print Table A.21 in console   #####
varlist <- c("Polyarchy", "Ln GDPpc", "Percent urbanization", "Ln Population", "Ln Resource Income pc")
stargazer(baseline_vanity_meters_log,baseline_vanity_meters_nolog,baseline_vanity_percent,
          baseline_vanity_maxpercent,
          covariate.labels = varlist,
          title="Regime type and excessiveness of skyscrapers",
          style="ajps",
          type="text",
          digits=2,
          font.size="scriptsize",
          align=FALSE,
          single.row=FALSE,
          dep.var.labels.include=FALSE,
          notes=c("All covariates lagged by 3 years","Standard error in parentheses"),
          table.placement = "!h",
          dep.var.caption = "",
          add.lines = list(rep("",4)),
          model.names=FALSE, column.labels=c("Ln vanity meters","Absolute vanity meters","Vanity percent","Max vanity percent"),
          omit=c("country","year"),
          omit.labels = c("Country FE","Year FE"),
          omit.stat = c("f","adj.rsq","ser","ll"))




#####     \\    --    Table A.31 and Figure A.5    --    //     #####

##### Regressions
baseline_polyarchyXGDP <- lm(log(delta_nSkyScrapers+1) ~ v2x_polyarchy_lag3*log(e_migdppc_lag3) +
                               e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                               country + factor(year),
                             data=df)

baseline_polyarchyXGDP_meters <- lm(log(delta_MetersOfSkyScrapers+1) ~ v2x_polyarchy_lag3*log(e_migdppc_lag3) +
                                      e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                                      country + factor(year),
                                    data=df)


#####   Print Table A.30 in Console
varlist <- c("Polyarchy","Ln GDPpc","Percent urbanization", "Ln Population","Ln Resource Income pc", "Polyarchy x Ln GDP per Capita")
stargazer(baseline_polyarchyXGDP, baseline_polyarchyXGDP_meters,
          covariate.labels = varlist,
          zero.component=TRUE,
          title="Interaction models: Regime type and ln GDP per capita",
          style="ajps",
          type="text",
          digits=2,
          font.size="scriptsize",
          align=FALSE,
          single.row=FALSE,
          notes=c("All covariates lagged by 3 years","Standard error in parentheses"),
          dep.var.labels.include=FALSE,
          dep.var.caption = "",
          column.separate = c(1,1,1,1,1,1,1,1),
          add.lines = list(rep("",8)),
          model.names=FALSE,
          column.labels = c("Ln Skyscrapers", "Ln Meters"),
          table.placement = "!h",
          omit=c("country","year"),
          omit.labels = c("Country FE","Year FE"),
          omit.stat = c("f","adj.rsq","ser","ll","aic","bic"))


#####     Figure A.5
#Marginal effect of democracy
b1 <- coef(baseline_polyarchyXGDP)[grep("polyarchy",names(coef(baseline_polyarchyXGDP)))][1]
b3 <- coef(baseline_polyarchyXGDP)[grep("polyarchy",names(coef(baseline_polyarchyXGDP)))][2]
x2 <- seq(min(log(df$e_migdppc_lag3),na.rm=T),max(log(df$e_migdppc_lag3),na.rm=T),0.1)
varb1 <- vcov(baseline_polyarchyXGDP)[grep("polyarchy",colnames(vcov(baseline_polyarchyXGDP)))[1],grep("polyarchy",colnames(vcov(baseline_polyarchyXGDP)))[1]]
varb3 <- vcov(baseline_polyarchyXGDP)[grep("polyarchy",colnames(vcov(baseline_polyarchyXGDP)))[2],grep("polyarchy",colnames(vcov(baseline_polyarchyXGDP)))[2]]
covb1b3 <- vcov(baseline_polyarchyXGDP)[grep("polyarchy",colnames(vcov(baseline_polyarchyXGDP)))[1],
                                        grep("polyarchy",colnames(vcov(baseline_polyarchyXGDP)))[2]]
beta <- b1+(b3*x2)
se <-sqrt(varb1 + ((x2^2)*varb3) + ((2*x2)*covb1b3))
effectframe_dem <- data.frame("beta"=beta,
                              "upper"=beta+1.96*se,
                              "lower"=beta-1.96*se,
                              "x2"=x2)


#Marginal effect of GDP
b1 <- coef(baseline_polyarchyXGDP)[grep("e_migdppc_lag3",names(coef(baseline_polyarchyXGDP)))][1]
b3 <- coef(baseline_polyarchyXGDP)[grep("e_migdppc_lag3",names(coef(baseline_polyarchyXGDP)))][2]
x2 <- seq(min(df$v2x_polyarchy_lag3,na.rm=T),max(df$v2x_polyarchy_lag3,na.rm=T),0.1)
varb1 <- vcov(baseline_polyarchyXGDP)[grep("e_migdppc_lag3",colnames(vcov(baseline_polyarchyXGDP)))[1],grep("e_migdppc_lag3",colnames(vcov(baseline_polyarchyXGDP)))[1]]
varb3 <- vcov(baseline_polyarchyXGDP)[grep("e_migdppc_lag3",colnames(vcov(baseline_polyarchyXGDP)))[2],grep("e_migdppc_lag3",colnames(vcov(baseline_polyarchyXGDP)))[2]]
covb1b3 <- vcov(baseline_polyarchyXGDP)[grep("e_migdppc_lag3",colnames(vcov(baseline_polyarchyXGDP)))[1],
                                        grep("e_migdppc_lag3",colnames(vcov(baseline_polyarchyXGDP)))[2]]
beta <- b1+(b3*x2)
se <-sqrt(varb1 + ((x2^2)*varb3) + ((2*x2)*covb1b3))
effectframe_GDP <- data.frame("beta"=beta,
                              "upper"=beta+1.96*se,
                              "lower"=beta-1.96*se,
                              "x2"=x2)


#####   Plot Figure A.5   #####
par(mfrow=c(1,2))
#Left Panel
plot(0,0,type="n",bty="n",main="",
     xlim=c(floor(min(effectframe_dem$x2)),ceiling(max(effectframe_dem$x2))),
     ylim=c(round(min(effectframe_dem$lower),2),round(max(effectframe_dem$upper),2)),
     axes=FALSE,ylab="Effect of Polyarchy",xlab="Ln GDP per capita")
axis(1,round(effectframe_dem$x2,1))
axis(2,at=round(seq(min(effectframe_dem$lower),round(max(effectframe_dem$upper)+1,2),.1),1),las=2)
grid(col="#423E3D70")
polygon(c(effectframe_dem$x2,rev(effectframe_dem$x2)),c(effectframe_dem$lower,rev(effectframe_dem$upper)),
        border=FALSE,col="#38A59150")
lines(effectframe_dem$x2,effectframe_dem$beta)
abline(h=0,lty="dashed")
rug(jitter(model.frame(baseline_polyarchyXGDP)[,"log(e_migdppc_lag3)"]))

#Right Panel
plot(0,0,type="n",bty="n",main="",
     xlim=c(min(effectframe_GDP$x2),max(effectframe_GDP$x2)),
     ylim=c(.15, .31),
     axes=FALSE,ylab="Effect of Ln GDP per capita",xlab="Level of Polyarchy")
axis(1,round(effectframe_GDP$x2,1))
axis(2,at=round(seq(.1, .4,.05),2),las=2)
grid(col="#423E3D70")
polygon(c(effectframe_GDP$x2,rev(effectframe_GDP$x2)),c(effectframe_GDP$lower,rev(effectframe_GDP$upper)),
        border=FALSE,col="#38A59150")
lines(effectframe_GDP$x2,effectframe_GDP$beta)
abline(h=0,lty="dashed")
rug(jitter(model.frame(baseline_polyarchyXGDP)[,"v2x_polyarchy_lag3"]))

