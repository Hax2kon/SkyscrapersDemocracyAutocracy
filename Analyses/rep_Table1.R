


library(censReg);library(survival);library(plm);library(pscl);library(stargazer)
load("../Replication/Data/main_dataframe.rdata")

#####   \\ --  Table 1: The main results  -- //   #####
#This script estimates the 7 models in Table 1, and prints the table in the console.

##### Baseline  #####
baseline <- lm(log(delta_nSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                 log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                 country +  factor(year),
               data=df)


#####   No fixed effects    #####
df$delta_nSkyScrapers2 <- df$delta_nSkyScrapers #Changing the name just makes it easier to design stargazer later
baseline_nocountryFE <- lm(log(delta_nSkyScrapers2+1) ~ v2x_polyarchy_lag3 + 
                             log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) + 
                             factor(year),
                           data=df)


#####   No log on dependent   #####
baseline_unlogged <- lm(delta_nSkyScrapers ~ v2x_polyarchy_lag3 +
                          log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) + 
                          factor(country) + factor(year),
                        data=df)



#####   Zero-inflated   #####
baseline_zeroinflated <- zeroinfl(delta_nSkyScrapers2 ~ 
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

#####   Tobit   #####
pd   <- pdata.frame(df,index=c("country","year"))
tmod <- censReg(log(delta_nSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                 log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1),
               left = 0, right = Inf, data = pd,method="BHHH", iterlim=20000)



##### Baseline meters #####
baseline_meters <- lm(log(delta_MetersOfSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                        log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                        country + factor(year),
                      data=df)


##### Baseline meters unlogged  #####
baseline_meters_unlogged <- lm(delta_MetersOfSkyScrapers ~ v2x_polyarchy_lag3 +
                                 log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                                 country + factor(year),
                               data=df)


#####         \\    Print Table 1 in console    //         #####


varlist <- c("Polyarchy", "Ln GDPpc", "Percent urbanization", "Ln Population", "Ln Resource Income pc")
stargazer(baseline,baseline_nocountryFE,baseline_unlogged,baseline_zeroinflated, tmod,
          baseline_meters,baseline_meters_unlogged,
          covariate.labels = varlist,
          title="Main results: Regime type and the number and meters of Skyscrapers",
          style="ajps",
          digits=2,
          type = "text",
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
          column.labels=c("OLS", "OLS w.o. country FE", "OLS", "Zeroinflated", "Tobit", "OLS", "OLS"),
          omit=c("country","year","continents"),
          omit.labels = c("Country FE","Year FE","Continent FE"),
          omit.stat = c("f","adj.rsq","ser","ll","aic","bic"))
