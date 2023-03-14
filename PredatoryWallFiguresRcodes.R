####
R version 4.1.3 (2022-03-10) -- "One Push-Up"
Copyright (C) 2022 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64 (64-bit)

### Data/Codes for figures and Results
### February 2023
# North Sea
load("NorthSea Data.RData")

NSdataModelCentre
NSdataModel

# Barents Sea
load("BarentsSea_Data.RData")

BSdataModelCentre
BSdataModel


# Models
require(mgcv)
NSGAMmodel <- gam(list(LonHad ~ s(Ncod,k=3, bs="ts")+te(LonCod,LatCod, bs="ts")+s(ST,k=3, bs="ts")+s(Nhad,k=3, bs="ts")+s(Size, bs = "re")+s(Year, bs = "re"),
                       LatHad ~ s(Ncod,k=3, bs="ts")+te(LonCod,LatCod, bs="ts")+s(ST,k=3, bs="ts")+s(Nhad,k=3, bs="ts")+s(Size, bs = "re")+s(Year, bs = "re")),
                       data=NSdataModelCentre,correlation=corGaus(form=~(CenLon+CenLat)|f_Year), family = mvn(d = 2), method= "REML")


BSGAMmodel <- gam(list(LonHad ~ s(Ncod,k=3, bs="ts")+te(LonCod,LatCod, bs="ts")+s(ST,k=3, bs="ts")+s(Nhad,k=3, bs="ts")+s(Size, bs = "re")+s(Year, bs = "re"),
                       LatHad ~ s(Ncod,k=3, bs="ts")+te(LonCod,LatCod, bs="ts")+s(ST,k=3, bs="ts")+s(Nhad,k=3, bs="ts")+s(Size, bs = "re")+s(Year, bs = "re")),
                       data=BSdataModelCentre,correlation=corGaus(form=~(CenLon+CenLat)|f_Year), family = mvn(d = 2), method= "REML")


# Figure 2
# Barents Sea
windows(5,5)
 par(mar=c(5,5,1,1))
 plot(BSGAMmodel, select=2, residuals=FALSE, se=FALSE, main="", axes=FALSE, xlab="", ylab="", type="n")
   u <- par("usr") # The coordinates of the plot area
   rect(u[1], u[3], u[2], u[4], col="lightblue1", border=NA)
   par(new=TRUE)
 plot(BSGAMmodel, select=2, residuals=FALSE, se=FALSE, main="", xlab="", ylab= "", yaxt="n", xaxt="n",cex.lab=1.5, type="n")# edf=20.77
 # Coasts in the Barents Sea
  COASTbarentsSeaCorrected ()
 box()
  axis(1, seq(7,12,1), labels=rep("",6),tcl=-0.5)
  #axis(1, seq(7,12,1),labels=round(seq(7,12,1)/cos(pi* 70/180),1))
  axis(2, seq(71,74,1),labels=rep("",4),tcl=-0.5)

windows(5,5)
 par(mar=c(5,5,1,1))
 plot(BSGAMmodel, select=8, residuals=FALSE, se=FALSE, main="", axes=FALSE, xlab="", ylab="", type="n")
   u <- par("usr") # The coordinates of the plot area
   rect(u[1], u[3], u[2], u[4], col="lightblue1", border=NA)
   par(new=TRUE)
 plot(BSGAMmodel, select=8, residuals=FALSE, se=FALSE, main="", xlab="", ylab="", yaxt="n", xaxt="n",cex.lab=1.5, type="n")# edf=23.67
 # Coasts in the Barents Sea
  COASTbarentsSeaCorrected ()
 box()
  axis(1, seq(7,12,1), labels=rep("",6),tcl=-0.5)
  #axis(1, seq(7,12,1),labels=round(seq(7,12,1)/cos(pi* 70/180),1))
  axis(2, seq(71,74,1),labels=rep("",4),tcl=-0.5)

# North Sea

windows(5,5)
 par(mar=c(5,5,1,1))
 plot(NSGAMmodel, select=2, se=FALSE, main="", axes=FALSE, xlab="", ylab="", type="n")
   u <- par("usr") # The coordinates of the plot area
   rect(u[1], u[3], u[2], u[4], col="lightblue1", border=NA)
   par(new=TRUE)
 plot(NSGAMmodel, select=2, se=FALSE, main="", xlab="", ylab= "", xaxt="n", yaxt="n",cex.lab=1.5, type="n")# edf=23.48
 # Coasts in the North Sea
 COASTnorthSeaCorrected() 
 box()
# axis(1, seq(0,5,1),labels=round(seq(0,5,1)/cos(pi* 58/180),1))
  axis(1, seq(0,5,1),labels=rep("",6),tcl=-0.5)
  axis(2, seq(54,61,1),labels=rep("",8),tcl=-0.5)

windows(5,5)
 par(mar=c(5,5,1,1))
 plot(NSGAMmodel, select=8, se=FALSE, main="", axes=FALSE, xlab="", ylab="", type="n")
   u <- par("usr") # The coordinates of the plot area
   rect(u[1], u[3], u[2], u[4], col="lightblue1", border=NA)
   par(new=TRUE)
 plot(NSGAMmodel, select=8, se=FALSE, main="", xlab="", ylab= "", xaxt="n",yaxt="n", cex.lab=1.5, type="n")# edf =22.79
 # Coasts in the North Sea
 COASTnorthSeaCorrected() 
 box()
# axis(1, seq(0,5,1),labels=round(seq(0,5,1)/cos(pi* 58/180),1))
  axis(1, seq(0,5,1),labels=rep("",6),tcl=-0.5)
  axis(2, seq(54,61,1),labels=rep("",8),tcl=-0.5)


# Figure 3
 require(marmap)
 require(mapdata)
 require(mgcv)
 require("RColorBrewer")
 require(MASS)


# North Sea
NoLeg.filled.contour(NSzBoot,  xlim=c(-3,11),ylim=c(49,61),  
                     plot.axes={axis(1, seq(-2,10,2), labels=c("2°W","0°E","2°E","4°E","6°E","8°E","10°E"),tcl=-0.5)
                               axis(2, seq(50,60,2), labels=c("50°N","52°N","54°N","56°N","58°N","60°N"), tcl=-0.5)
                     polygon(NorthMAP $x,NorthMAP$y,col="grey",lwd=1)},
                     color.palette = function(n) c("#FFFFFF",rev(hcl.colors(n, "Blues"))))

# Barents Sea
NoLeg.filled.contour(BSzBoot,  xlim=c(12,40),ylim=c(68,80), nlevels = 100,
           color.palette = function(n) c("#FFFFFF",rev(hcl.colors(n, "Blues"))),  
           plot.axes = {axis(1, seq(15,35,5), labels=c("15°E","20°E","25°E","30°E","35°E"),tcl=-0.5)
                        axis(2, seq(70,80,2), labels=c("70°N","72°N","74°N","76°N","78°N","80°N"), tcl=-0.5)
                          polygon(BarentsMAP$x,BarentsMAP$y,col="grey",lwd=1)})




#### Supplementary
# Variograme

windows2(1,2)
  plot(NSVario1$dist,NSVario1$gamma, main="Longitude", ylim=c(0,1), xlab="distance",ylab="semivariance", col="blue")
  plot(NSVario2$dist,NSVario2$gamma, main="Latitude", ylim=c(0,1), xlab="distance",ylab="semivariance", col="blue")

windows2(1,2)
  plot(BSVario1$dist,BSVario1$gamma, main="Longitude", ylim=c(0,1), xlab="distance",ylab="semivariance", col="blue")
  plot(BSVario2$dist,BSVario2$gamma, main="Latitude", ylim=c(0,1), xlab="distance",ylab="semivariance", col="blue")



# Figure S2 Barents Sea
 # Longitude part
windows(5,5)
 par(mar=c(5,5,1,1))
   plot(BSGAMmodel, select=1, rug=FALSE, shade=T, xlab="", ylab="", xaxt="",yaxt="",bty="")
   u <- par("usr") # The coordinates of the plot area
   rect(u[1], u[3], u[2], u[4], col="lightblue1", border=NA)
   par(new=TRUE)
   plot(BSGAMmodel, select=1, rug=FALSE, shade=T, xlab=bquote(Abund [" cod"] ), ylab= bquote(s(Abund[" cod"], 0.83)), cex.lab=1.5, bg="blue")

windows(5,5)
 par(mar=c(5,5,1,1))
 plot(BSGAMmodel, select=2, residuals=FALSE, se=FALSE, main="", xlab="", ylab="",xaxt="",yaxt="",bty="")
   u <- par("usr") # The coordinates of the plot area
   rect(u[1], u[3], u[2], u[4], col="lightblue1", border=NA)
   par(new=TRUE)
 plot(BSGAMmodel, select=2, residuals=FALSE, se=FALSE, main="", xlab=bquote(lon [" cod"]), ylab= bquote(lat [" cod"]), xaxt="n", cex.lab=1.5, type="")# edf=20.77
    axis(1, seq(7,12,1),labels=round(seq(7,12,1)/cos(pi* 70/180),1))
 # Coasts in the Barents Sea
  COASTbarentsSeaCorrected ()
 box()

windows(5,5)
 par(mar=c(5,5,1,1))
 plot(BSGAMmodel, select=3, rug=FALSE, shade=T, xlab="", ylab="", xaxt="",yaxt="",bty="")
   u <- par("usr") # The coordinates of the plot area
   rect(u[1], u[3], u[2], u[4], col="grey97", border=NA)
   par(new=TRUE)
 plot(BSGAMmodel, select=3, rug=FALSE, shade=T, xlab=expression("ST, °C"), ylab=expression("s(ST, 1.99)"), cex.lab=1.5)

windows(5,5)
 par(mar=c(5,5,1,1))
 plot(BSGAMmodel, select=4, rug=FALSE, shade=T, xlab="", ylab="", xaxt="",yaxt="",bty="")
   u <- par("usr") # The coordinates of the plot area
   rect(u[1], u[3], u[2], u[4], col="grey97", border=NA)
   par(new=TRUE)
 plot(BSGAMmodel, select=4, rug=FALSE, shade=T, xlab=bquote(Abund [" haddock"] ), ylab= bquote(s(Abund[" haddock"], 0.97)), cex.lab=1.5)

 #### latitude part 

windows(5,5)
 par(mar=c(5,5,1,1))
 plot(BSGAMmodel, select=7, rug=FALSE, shade=T, xlab="", ylab= "", xaxt="",yaxt="",bty="")
   u <- par("usr") # The coordinates of the plot area
   rect(u[1], u[3], u[2], u[4], col="lightblue1", border=NA)
   par(new=TRUE)
 plot(BSGAMmodel, select=7, rug=FALSE, shade=T, xlab=bquote(Abund [" cod"]), ylab= bquote(s(Abund[" cod"], 0.89)), cex.lab=1.5)

windows(5,5)
 par(mar=c(5,5,1,1))
 plot(BSGAMmodel, select=8, residuals=FALSE, se=FALSE, main="", xlab="", ylab= "",xaxt="",yaxt="",bty="")
   u <- par("usr") # The coordinates of the plot area
   rect(u[1], u[3], u[2], u[4], col="lightblue1", border=NA)
   par(new=TRUE)
 plot(BSGAMmodel, select=8, residuals=FALSE, se=FALSE, main="", xlab=bquote(lon ["cod"]), ylab= bquote(lat [" cod"]), xaxt="n", cex.lab=1.5, type="")# edf=23.67
 # Coasts in the Barents Sea
  COASTbarentsSeaCorrected ()
 box()

windows(5,5)
 par(mar=c(5,5,1,1))
 plot(BSGAMmodel, select=9, rug=FALSE, shade=T, xlab="", ylab="", xaxt="",yaxt="",bty="")
   u <- par("usr") # The coordinates of the plot area
   rect(u[1], u[3], u[2], u[4], col="grey97", border=NA)
   par(new=TRUE)
 plot(BSGAMmodel, select=9, rug=FALSE, shade=T, xlab=expression("ST, °C"), ylab=expression("s(ST, 1.99)"), cex.lab=1.5)

windows(5,5)
 par(mar=c(5,5,1,1))
 plot(BSGAMmodel, select=10, rug=FALSE, shade=T, xlab="", ylab="", xaxt="",yaxt="",bty="")
   u <- par("usr") # The coordinates of the plot area
   rect(u[1], u[3], u[2], u[4], col="grey97", border=NA)
   par(new=TRUE)
 plot(BSGAMmodel, select=10, rug=FALSE, shade=T, xlab=bquote(Abund [" haddock"] ), ylab= bquote(s(Abund[" haddock"], 0)), cex.lab=1.5)


###########################
# Figure S2 North Sea
 # Longitude part
windows(5,5)
 par(mar=c(5,5,1,1))
 plot(NSGAMmodel, select=1, shade=T, xlab="", ylab="",xaxt="",yaxt="",bty="")
   u <- par("usr") # The coordinates of the plot area
   rect(u[1], u[3], u[2], u[4], col="lightblue1", border=NA)
   par(new=TRUE)
 plot(NSGAMmodel, select=1, shade=T, xlab=bquote(Abund [" cod"] ), ylab= bquote(s(Abund[" cod"], 0.70)), cex.lab=1.5)

windows(5,5)
 par(mar=c(5,5,1,1))
 plot(NSGAMmodel, select=2, se=FALSE, main="", xlab="", ylab= "", xaxt="",yaxt="",bty="")
   u <- par("usr") # The coordinates of the plot area
   rect(u[1], u[3], u[2], u[4], col="lightblue1", border=NA)
   par(new=TRUE)
 plot(NSGAMmodel, select=2, se=FALSE, main="", xlab=bquote(lon [ "cod"]), ylab= bquote(lat [" cod"]), xaxt="n", cex.lab=1.5)# edf=23.48
    axis(1, seq(0,5,1),labels=round(seq(0,5,1)/cos(pi* 58/180),1))
 # Coasts in the North Sea
 COASTnorthSeaCorrected() 
 box()

windows(5,5)
 par(mar=c(5,5,1,1))
 plot(NSGAMmodel, select=3, shade=T, xlab="", ylab="", xaxt="",yaxt="",bty="")
   u <- par("usr") # The coordinates of the plot area
   rect(u[1], u[3], u[2], u[4], col="grey97", border=NA)
   par(new=TRUE)
 plot(NSGAMmodel, select=3, shade=T, xlab=expression("ST, °C"), ylab=expression("s(ST, 1.99)"), cex.lab=1.5)


windows(5,5)
 par(mar=c(5,5,1,1))
 plot(NSGAMmodel, select=4, shade=T, xlab="", ylab= "", xaxt="",yaxt="",bty="")
   u <- par("usr") # The coordinates of the plot area
   rect(u[1], u[3], u[2], u[4], col="grey97", border=NA)
   par(new=TRUE)
 plot(NSGAMmodel, select=4, shade=T, xlab=bquote(Abund [" haddock"]), ylab= bquote(s(Abund[" haddock"], 1.98)), cex.lab=1.5)


 # Latitude part
windows(5,5)
 par(mar=c(5,5,1,1))
 plot(NSGAMmodel, select=7, shade=T, xlab="", ylab="", xaxt="",yaxt="",bty="")
   u <- par("usr") # The coordinates of the plot area
   rect(u[1], u[3], u[2], u[4], col="lightblue1", border=NA)
   par(new=TRUE)
 plot(NSGAMmodel, select=7, shade=T, xlab=bquote(Abund [" cod"]), ylab= bquote(s(Abund[" cod"], 0.88)), cex.lab=1.5)

windows(5,5)
 par(mar=c(5,5,1,1))
 plot(NSGAMmodel, select=8, se=FALSE, main="", xlab="", ylab="", xaxt="",yaxt="",bty="")
   u <- par("usr") # The coordinates of the plot area
   rect(u[1], u[3], u[2], u[4], col="lightblue1", border=NA)
   par(new=TRUE)
 plot(NSGAMmodel, select=8, se=FALSE, main="", xlab=bquote(lon [" cod"]), xaxt="n", ylab= bquote(lat [" cod"]), cex.lab=1.5)# edf =22.79
    axis(1, seq(0,5,1),labels=round(seq(0,5,1)/cos(pi* 58/180),1))
 # Coasts in the North Sea
 COASTnorthSeaCorrected() 
 box()

windows(5,5)
 par(mar=c(5,5,1,1))
 plot(NSGAMmodel, select=9, shade=T, xlab="", ylab="", xaxt="",yaxt="",bty="")
   u <- par("usr") # The coordinates of the plot area
   rect(u[1], u[3], u[2], u[4], col="grey97", border=NA)
   par(new=TRUE)
 plot(NSGAMmodel, select=9, shade=T, xlab=expression("ST, °C"), ylab=expression("s(ST, 1.99)"), cex.lab=1.5)

windows(5,5)
 par(mar=c(5,5,1,1))
 plot(NSGAMmodel, select=10, shade=T, xlab="", ylab= "", xaxt="",yaxt="",bty="")
   u <- par("usr") # The coordinates of the plot area
   rect(u[1], u[3], u[2], u[4], col="grey97", border=NA)
   par(new=TRUE)
 plot(NSGAMmodel, select=10, shade=T, xlab=bquote(Abund [" haddock"]), ylab= bquote(s(Abund[" haddock"], 1.86)), cex.lab=1.5)






