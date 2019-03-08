
library(stargazer)
load("../Replication/Data/main_dataframe.rdata")


#####     \\  --  Table 3    --    //      #####

##### Baseline model (for comparison)
baseline <- lm(log(delta_nSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                 log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                 country + factor(year),
               data=df)


##### Mediation Channel 1: Vertical Accountability
va1_base <- lm(log(delta_nSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                 v2xme_altinf_lag3 +
                 log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                 country + factor(year),
               data=df)

#####   Mediation Channel 2: Horizontal Accountability
ha1_base <- lm(log(delta_nSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                 v2xlg_legcon_lag3 +
                 log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                 country + factor(year),
               data=df)

#####   Mediation Channel 3: Corruption
corr1_base <- lm(log(delta_nSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                   v2x_corr_lag3 +
                   log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                   country + factor(year),
                 data=df)

#####   Mediation Channel 4: Coalition Group
cg1_base <- lm(log(delta_nSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                 v2pepwrses_lag3 +
                 log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                 country + factor(year),
               data=df)


#####   All channels
mediation_base <- lm(log(delta_nSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                       v2xme_altinf_lag3 + v2xlg_legcon_lag3 + v2x_corr_lag3 + v2pepwrses_lag3 +
                       log(e_migdppc_lag3) + e_miurbani_lag3 + log(e_mipopula_lag3) + log(e_Total_Resources_Income_PC_lag3+1) +
                       country + factor(year),
                     data=df)



#####   Print Table 3 in console   ######
varlist <- c("Polyarchy","Alternative source of information","Legislative constraints on executive",
             "Political corruption","Power distributed by \nsocioeconomic position",
             "Ln GDPpc","Percent urbanization", "Ln Population","Ln Resource Income pc")

stargazer(baseline, va1_base, ha1_base, corr1_base, cg1_base, mediation_base,
          covariate.labels = varlist,
          title="Investigating the mechanisms",
          style="ajps",
          type="text",
          digits=2,
          font.size="scriptsize",
          align=FALSE,
          single.row=FALSE,
          dep.var.labels.include=FALSE,
          notes=c("All covariates lagged by 3 years", "Standard error in parentheses"),
          dep.var.caption = "",
          table.placement = "!h",
          add.lines = list(rep("",8)),
          model.names=FALSE,
          column.labels=c("Baseline","Vertical","Horizontal",
                          "Corruption","Support Coalition",
                          "All"),
          omit=c("country","year"),
          omit.labels = c("Country FE","Year FE"),
          omit.stat = c("f","adj.rsq","ser","ll","aic","bic"))




