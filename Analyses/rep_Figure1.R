rm(list=ls())
cat("\014")
gc()

#####     Load data     #####
load("../Data/buildings.rdata")


#####   Make plot     #####
plot(sc$year, sc$highest_point, type="n",
     axes=FALSE,ylab="Height in meters",xlab="(Expected) year of completion")
grid()
abline(h=150, lty="dashed")
points(sc$year, sc$highest_point,bg=grey(0.5, 0.5), pch=21,col=grey(0.2, 1), cex=0.8)
text(1931,443,"Empire State\nBuilding\nUSA"         , pos=2, cex=0.8, family="A")
text(2010,829.8,"Burj Khalifa\nUnited Arab Emirates", pos=2, cex=0.8, family="A")
text(1972,530,"One World\nTrade Center\nUSA"        , pos=2, cex=0.8, family="A")
text(2019,1000,"Jeddah Tower\nSaudi Arabia"         , pos=2, cex=0.8, family="A")
axis(2,at=c(150,seq(0,1000,100)),las=2,col="white")
axis(1,seq(1900,2030,10),col="white")



