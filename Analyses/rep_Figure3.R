library(stargazer)
load("Data/main_dataframe.rdata")


#####     Run regression       #####
interactmod_number <- lm(log(delta_nSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                           log(e_migdppc_lag3) + e_miurbani_lag3 +
                           log(e_mipopula_lag3) +
                           log(e_Total_Resources_Income_PC_lag3+1) +
                           v2x_polyarchy_lag3*e_miurbani_lag3 +
                           country + factor(year),
                         data=df)



#####       Calculate  marginal  effects    #####
#Effect of Democracy
b1 <- coef(interactmod_number)[grep("polyarchy",names(coef(interactmod_number)))][1]
b3 <- coef(interactmod_number)[grep("polyarchy",names(coef(interactmod_number)))][2]
x2 <- seq(min(df$e_miurbani_lag3,na.rm=T),max(df$e_miurbani_lag3,na.rm=T),0.1)
varb1 <- vcov(interactmod_number)[grep("polyarchy",colnames(vcov(interactmod_number)))[1],grep("polyarchy",colnames(vcov(interactmod_number)))[1]]
varb3 <- vcov(interactmod_number)[grep("polyarchy",colnames(vcov(interactmod_number)))[2],grep("polyarchy",colnames(vcov(interactmod_number)))[2]]
covb1b3 <- vcov(interactmod_number)[grep("polyarchy",colnames(vcov(interactmod_number)))[1],
                                    grep("polyarchy",colnames(vcov(interactmod_number)))[2]]
beta <- b1+(b3*x2)
se <-sqrt(varb1 + ((x2^2)*varb3) + ((2*x2)*covb1b3))
effectframe_dem <- data.frame("beta"=beta,
                              "upper"=beta+1.96*se,
                              "lower"=beta-1.96*se,
                              "x2"=x2)

#Effect of Urbanization
b1 <- coef(interactmod_number)[grep("e_miurbani_lag3",names(coef(interactmod_number)))][1]
b3 <- coef(interactmod_number)[grep("e_miurbani_lag3",names(coef(interactmod_number)))][2]
x2 <- seq(min(df$v2x_polyarchy_lag3,na.rm=T),max(df$v2x_polyarchy_lag3,na.rm=T),0.1)
varb1 <- vcov(interactmod_number)[grep("e_miurbani_lag3",colnames(vcov(interactmod_number)))[1],grep("e_miurbani_lag3",colnames(vcov(interactmod_number)))[1]]
varb3 <- vcov(interactmod_number)[grep("e_miurbani_lag3",colnames(vcov(interactmod_number)))[2],grep("e_miurbani_lag3",colnames(vcov(interactmod_number)))[2]]
covb1b3 <- vcov(interactmod_number)[grep("e_miurbani_lag3",colnames(vcov(interactmod_number)))[1],
                                    grep("e_miurbani_lag3",colnames(vcov(interactmod_number)))[2]]
beta <- b1+(b3*x2)
se <-sqrt(varb1 + ((x2^2)*varb3) + ((2*x2)*covb1b3))
effectframe_urban <- data.frame("beta"=beta,
                                "upper"=beta+1.96*se,
                                "lower"=beta-1.96*se,
                                "x2"=x2)



#####     Plot Figure 3     #####
par(mfrow=c(1,2))

#Left-panel
plot(0,0,type="n",bty="n",main="",
     xlim=c(floor(min(effectframe_dem$x2)),ceiling(max(effectframe_dem$x2))),
     ylim=c(round(min(effectframe_dem$lower),2),round(max(effectframe_dem$upper),2)),
     axes=FALSE,ylab="Effect of Polyarchy",xlab="Urbanization")
axis(1,round(effectframe_dem$x2,1))
axis(2,at=round(seq(min(effectframe_dem$lower),round(max(effectframe_dem$upper),2),.1),1),las=2)

grid(col=grey(0.8))

polygon(c(effectframe_dem$x2,rev(effectframe_dem$x2)),c(effectframe_dem$lower,rev(effectframe_dem$upper)),
        border=FALSE,col=grey(0.2, 0.7))
lines(effectframe_dem$x2,effectframe_dem$beta)
abline(h=0,lty="dashed")
points(jitter(model.frame(interactmod_number)[,"e_miurbani_lag3"]),
       rep(-.376, length(model.frame(interactmod_number)[,"e_miurbani_lag3"])),
       pch="|", col=grey(0.1, 0.04))

#Right-panel
plot(0,0,type="n",bty="n",main="",
     xlim=c(min(effectframe_urban$x2),max(effectframe_urban$x2)),
     ylim=c(min(effectframe_urban$lower),max(effectframe_urban$upper)),
     axes=FALSE,ylab="Effect of Urbanization",xlab="Level of Polyarchy")
axis(1,round(effectframe_urban$x2,1))
axis(2,at=round(seq(min(effectframe_urban$lower)-0.1, round(max(effectframe_urban$upper),2)+0.1, .1), 1),las=2)

grid(col=grey(0.8))

polygon(c(effectframe_urban$x2,rev(effectframe_urban$x2)),c(effectframe_urban$lower,rev(effectframe_urban$upper)),
        border=FALSE,col=grey(0.2, 0.7))
lines(effectframe_urban$x2,effectframe_urban$beta)
abline(h=0,lty="dashed")
points(jitter(model.frame(interactmod_number)[,"v2x_polyarchy_lag3"]),
       rep(-.14, length(model.frame(interactmod_number)[,"v2x_polyarchy_lag3"])),
       pch="|", col=grey(0.1, 0.04))




######      Table A.22       #####
interactmod_meters <- lm(log(delta_MetersOfSkyScrapers+1) ~ v2x_polyarchy_lag3 +
                           log(e_migdppc_lag3) + e_miurbani_lag3 +
                           log(e_mipopula_lag3) +
                           log(e_Total_Resources_Income_PC_lag3+1) +
                           v2x_polyarchy_lag3*e_miurbani_lag3 +
                           country + factor(year),
                         data=df)

varlist <- c("Polyarchy","Ln GDPpc","Percent\nurbanization",
             "Ln Population","Ln Resource\nIncome pc","Polyarchy*Urbanization","Intercept")
stargazer(interactmod_number,interactmod_meters,
          covariate.labels = varlist,
          title="Interaction models: Regime type and urbanization",
          style="ajps",
          type="text",
          digits=2,
          font.size="scriptsize",
          align=FALSE,
          single.row=FALSE,
          dep.var.labels.include=FALSE,
          notes=c("All covariates lagged by 3 years",
                  "Standard error in parentheses"),
          dep.var.caption = "",
          add.lines = list(rep("",7)),
          model.names=FALSE, column.labels=c("Ln number of skyscraper",
                                             "Ln meters of skyscrapers"),
          omit=c("country","year"),
          omit.labels = c("Country FE","Year FE"),
          omit.stat = c("f","adj.rsq","ser","ll"))


