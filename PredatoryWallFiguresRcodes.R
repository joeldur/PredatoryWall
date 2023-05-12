####
R version 4.1.3 (2022-03-10) -- "One Push-Up"
Copyright (C) 2022 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64 (64-bit)

### Data/Codes for figures and Results
### February 2023
# Barents Sea
load("BarentsSea_Data.RData")

BSdataModelCentre
BSdataModel

# North Sea
load("NorthSea_Data.RData")

NSdataModelCentre
NSdataModel



# Models
require(mgcv)
BSGAMmodel <- gam(list(LonHad ~ s(Ncod,k=3, bs="ts")+te(LonCod,LatCod, bs="ts")+s(ST,k=3, bs="ts")+s(Nhad,k=3, bs="ts")+s(f_Size, bs = "re")+s(f_Year, bs = "re"),
                       LatHad ~ s(Ncod,k=3, bs="ts")+te(LonCod,LatCod, bs="ts")+s(ST,k=3, bs="ts")+s(Nhad,k=3, bs="ts")+s(f_Size, bs = "re")+s(f_Year, bs = "re")),
                       data=BSdataModelCentre,correlation=corGaus(form=~(CenLon+CenLat)|f_Year), family = mvn(d = 2), method= "REML")



NSGAMmodel <- gam(list(LonHad ~ s(Ncod,k=3, bs="ts")+te(LonCod,LatCod, bs="ts")+s(ST,k=3, bs="ts")+s(Nhad,k=3, bs="ts")+s(f_Size, bs = "re")+s(f_Year, bs = "re"),
                       LatHad ~ s(Ncod,k=3, bs="ts")+te(LonCod,LatCod, bs="ts")+s(ST,k=3, bs="ts")+s(Nhad,k=3, bs="ts")+s(f_Size, bs = "re")+s(f_Year, bs = "re")),
                       data=NSdataModelCentre,correlation=corGaus(form=~(CenLon+CenLat)|f_Year), family = mvn(d = 2), method= "REML")



# Figure 2
# Barents Sea
windows(5,5)
 par(mar=c(5,5,1,1))
 # longitude
 plot(BSGAMmodel, select=2, residuals=FALSE, se=FALSE, main="", axes=FALSE, xlab="", ylab="", type="n")
   u <- par("usr") # The coordinates of the plot area
   rect(u[1], u[3], u[2], u[4], col="lightblue1", border=NA)
   par(new=TRUE)
 plot(BSGAMmodel, select=2, residuals=FALSE, se=FALSE, main="", xlab="", ylab= "", yaxt="n", xaxt="n",cex.lab=1.5, type="n")# edf=23.02
 # Coasts in the Barents Sea
  polygon(BScoast  , col="grey")
 box()
  axis(1, seq(7,12,1), labels=rep("",6),tcl=-0.5)
  #axis(1, seq(7,12,1),labels=round(seq(7,12,1)/cos(pi* 70/180),1))
  axis(2, seq(71,74,1),labels=rep("",4),tcl=-0.5)

windows(5,5)
 par(mar=c(5,5,1,1))
 # latitude
 plot(BSGAMmodel, select=8, residuals=FALSE, se=FALSE, main="", axes=FALSE, xlab="", ylab="", type="n")
   u <- par("usr") # The coordinates of the plot area
   rect(u[1], u[3], u[2], u[4], col="lightblue1", border=NA)
   par(new=TRUE)
 plot(BSGAMmodel, select=8, residuals=FALSE, se=FALSE, main="", xlab="", ylab="", yaxt="n", xaxt="n",cex.lab=1.5, type="n")# edf=23.62
 # Coasts in the Barents Sea
  polygon(BScoast  , col="grey") 
 box()
  axis(1, seq(7,12,1), labels=rep("",6),tcl=-0.5)
  #axis(1, seq(7,12,1),labels=round(seq(7,12,1)/cos(pi* 70/180),1))
  axis(2, seq(71,74,1),labels=rep("",4),tcl=-0.5)

# North Sea

windows(5,5)
 par(mar=c(5,5,1,1))
 # longitude
 plot(NSGAMmodel, select=2, se=FALSE, main="", axes=FALSE, xlab="", ylab="", type="n")
   u <- par("usr") # The coordinates of the plot area
   rect(u[1], u[3], u[2], u[4], col="lightblue1", border=NA)
   par(new=TRUE)
 plot(NSGAMmodel, select=2, se=FALSE, main="", xlab="", ylab= "", xaxt="n", yaxt="n",cex.lab=1.5, type="n")# edf=23.48
 # Coasts in the North Sea
 Hide()
 polygon(NScoast  , col="grey")  
 box()
# axis(1, seq(0,5,1),labels=round(seq(0,5,1)/cos(pi* 58/180),1))
  axis(1, seq(0,5,1),labels=rep("",6),tcl=-0.5)
  axis(2, seq(54,61,1),labels=rep("",8),tcl=-0.5)

windows(5,5)
 par(mar=c(5,5,1,1))
 # latitude
 plot(NSGAMmodel, select=8, se=FALSE, main="", axes=FALSE, xlab="", ylab="", type="n")
   u <- par("usr") # The coordinates of the plot area
   rect(u[1], u[3], u[2], u[4], col="lightblue1", border=NA)
   par(new=TRUE)
 plot(NSGAMmodel, select=8, se=FALSE, main="", xlab="", ylab= "", xaxt="n",yaxt="n", cex.lab=1.5, type="n")# edf =22.79
 # Coasts in the North Sea
 Hide()
 polygon(NScoast  , col="grey") 
 box()
# axis(1, seq(0,5,1),labels=round(seq(0,5,1)/cos(pi* 58/180),1))
  axis(1, seq(0,5,1),labels=rep("",6),tcl=-0.5)
  axis(2, seq(54,61,1),labels=rep("",8),tcl=-0.5)



# Figure 3
 require(marmap)
 require(mapdata)
 require(mgcv)
 require(RColorBrewer)
 require(MASS)

# Barents Sea
filled.contour(BSzBoot,  xlim=c(12,40),ylim=c(68,80), nlevels = 100,
           color.palette = function(n) c("#FFFFFF",rev(hcl.colors(n, "Blues"))),  
           plot.axes = {axis(1, seq(15,35,5), labels=expression(15~degree~E,20~degree~E,25~degree~E,30~degree~E,35~degree~E),tcl=-0.5)
                        axis(2, seq(70,80,2), labels=expression(70~degree~N,72~degree~N,74~degree~N,76~degree~N,78~degree~N,80~degree~N), tcl=-0.5)
                          polygon(BarentsMAP$x,BarentsMAP$y,col="grey",lwd=1)})



# North Sea
filled.contour(NSzBoot,  xlim=c(-3,11),ylim=c(49,61),  
                     plot.axes={axis(1, seq(-2,10,2), labels=expression(2~degree~W,0~degree~E,2~degree~E,4~degree~E,6~degree~E,8~degree~E,10~degree~E),tcl=-0.5)
                               axis(2, seq(50,60,2), labels=expression(50~degree~N,52~degree~N,54~degree~N,56~degree~N,58~degree~N,60~degree~N), tcl=-0.5)
                     polygon(NorthMAP $x,NorthMAP$y,col="grey",lwd=1)},
                     color.palette = function(n) c("#FFFFFF",rev(hcl.colors(n, "Blues"))))


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
   plot(BSGAMmodel, select=1, rug=FALSE, shade=T, xlab=bquote(Abund [" cod"] ), ylab= bquote(s(Abund[" cod"], 0.01)), cex.lab=1.5, bg="blue")

windows(5,5)
 par(mar=c(5,5,1,1))
 plot(BSGAMmodel, select=2, residuals=FALSE, se=FALSE, main="", xlab="", ylab="",xaxt="",yaxt="",bty="")
   u <- par("usr") # The coordinates of the plot area
   rect(u[1], u[3], u[2], u[4], col="lightblue1", border=NA)
   par(new=TRUE)
 plot(BSGAMmodel, select=2, residuals=FALSE, se=FALSE, main="", xlab=bquote(lon [" cod"]), ylab= bquote(lat [" cod"]), xaxt="n", cex.lab=1.5, type="")# edf=20.77
    axis(1, seq(7,12,1),labels=round(seq(7,12,1)/cos(pi* 70/180),1))
 # Coasts in the Barents Sea
  polygon(BScoast  , col="grey")
 box()

windows(5,5)
 par(mar=c(5,5,1,1))
 plot(BSGAMmodel, select=3, rug=FALSE, shade=T, xlab="", ylab="", xaxt="",yaxt="",bty="")
   u <- par("usr") # The coordinates of the plot area
   rect(u[1], u[3], u[2], u[4], col="grey97", border=NA)
   par(new=TRUE)
 plot(BSGAMmodel, select=3, rug=FALSE, shade=T, xlab=expression("ST," ~degree~C), ylab=expression("s(ST, 1.99)"), cex.lab=1.5)

windows(5,5)
 par(mar=c(5,5,1,1))
 plot(BSGAMmodel, select=4, rug=FALSE, shade=T, xlab="", ylab="", xaxt="",yaxt="",bty="")
   u <- par("usr") # The coordinates of the plot area
   rect(u[1], u[3], u[2], u[4], col="grey97", border=NA)
   par(new=TRUE)
 plot(BSGAMmodel, select=4, rug=FALSE, shade=T, xlab=bquote(Abund [" haddock"] ), ylab= bquote(s(Abund[" haddock"], 0.89)), cex.lab=1.5)

 #### latitude part 

windows(5,5)
 par(mar=c(5,5,1,1))
 plot(BSGAMmodel, select=7, rug=FALSE, shade=T, xlab="", ylab= "", xaxt="",yaxt="",bty="")
   u <- par("usr") # The coordinates of the plot area
   rect(u[1], u[3], u[2], u[4], col="lightblue1", border=NA)
   par(new=TRUE)
 plot(BSGAMmodel, select=7, rug=FALSE, shade=T, xlab=bquote(Abund [" cod"]), ylab= bquote(s(Abund[" cod"], 0.01)), cex.lab=1.5)

windows(5,5)
 par(mar=c(5,5,1,1))
 plot(BSGAMmodel, select=8, residuals=FALSE, se=FALSE, main="", xlab="", ylab= "",xaxt="",yaxt="",bty="")
   u <- par("usr") # The coordinates of the plot area
   rect(u[1], u[3], u[2], u[4], col="lightblue1", border=NA)
   par(new=TRUE)
 plot(BSGAMmodel, select=8, residuals=FALSE, se=FALSE, main="", xlab=bquote(lon ["cod"]), ylab= bquote(lat [" cod"]), xaxt="n", cex.lab=1.5, type="")# edf=23.67
 # Coasts in the Barents Sea
  polygon(BScoast  , col="grey")
 box()

windows(5,5)
 par(mar=c(5,5,1,1))
 plot(BSGAMmodel, select=9, rug=FALSE, shade=T, xlab="", ylab="", xaxt="",yaxt="",bty="")
   u <- par("usr") # The coordinates of the plot area
   rect(u[1], u[3], u[2], u[4], col="grey97", border=NA)
   par(new=TRUE)
 plot(BSGAMmodel, select=9, rug=FALSE, shade=T, xlab=expression("ST," ~degree~C), ylab=expression("s(ST, 1.99)"), cex.lab=1.5)

windows(5,5)
 par(mar=c(5,5,1,1))
 plot(BSGAMmodel, select=10, rug=FALSE, shade=T, xlab="", ylab="", xaxt="",yaxt="",bty="")
   u <- par("usr") # The coordinates of the plot area
   rect(u[1], u[3], u[2], u[4], col="grey97", border=NA)
   par(new=TRUE)
 plot(BSGAMmodel, select=10, rug=FALSE, shade=T, xlab=bquote(Abund [" haddock"] ), ylab= bquote(s(Abund[" haddock"], 0.96)), cex.lab=1.5)






###########################
# Figure S3 North Sea
 # Longitude part
windows(5,5)
 par(mar=c(5,5,1,1))
 plot(NSGAMmodel, select=1, shade=T, xlab="", ylab="",xaxt="",yaxt="",bty="")
   u <- par("usr") # The coordinates of the plot area
   rect(u[1], u[3], u[2], u[4], col="lightblue1", border=NA)
   par(new=TRUE)
 plot(NSGAMmodel, select=1, shade=T, xlab=bquote(Abund [" cod"] ), ylab= bquote(s(Abund[" cod"], 0.0)), cex.lab=1.5)

windows(5,5)
 par(mar=c(5,5,1,1))
 plot(NSGAMmodel, select=2, se=FALSE, main="", xlab="", ylab= "", xaxt="",yaxt="",bty="")
   u <- par("usr") # The coordinates of the plot area
   rect(u[1], u[3], u[2], u[4], col="lightblue1", border=NA)
   par(new=TRUE)
 plot(NSGAMmodel, select=2, se=FALSE, main="", xlab=bquote(lon [ "cod"]), ylab= bquote(lat [" cod"]), xaxt="n", cex.lab=1.5)# edf=23.75
    axis(1, seq(0,5,1),labels=round(seq(0,5,1)/cos(pi* 58/180),1))
 # Coasts in the North Sea
 Hide()
 polygon(NScoast  , col="grey") 
 box()

windows(5,5)
 par(mar=c(5,5,1,1))
 plot(NSGAMmodel, select=3, shade=T, xlab="", ylab="", xaxt="",yaxt="",bty="")
   u <- par("usr") # The coordinates of the plot area
   rect(u[1], u[3], u[2], u[4], col="grey97", border=NA)
   par(new=TRUE)
 plot(NSGAMmodel, select=3, shade=T, xlab=expression("ST," ~degree~C), ylab=expression("s(ST, 1.99)"), cex.lab=1.5)


windows(5,5)
 par(mar=c(5,5,1,1))
 plot(NSGAMmodel, select=4, shade=T, xlab="", ylab= "", xaxt="",yaxt="",bty="")
   u <- par("usr") # The coordinates of the plot area
   rect(u[1], u[3], u[2], u[4], col="grey97", border=NA)
   par(new=TRUE)
 plot(NSGAMmodel, select=4, shade=T, xlab=bquote(Abund [" haddock"]), ylab= bquote(s(Abund[" haddock"], 1.97)), cex.lab=1.5)


 # Latitude part
windows(5,5)
 par(mar=c(5,5,1,1))
 plot(NSGAMmodel, select=7, shade=T, xlab="", ylab="", xaxt="",yaxt="",bty="")
   u <- par("usr") # The coordinates of the plot area
   rect(u[1], u[3], u[2], u[4], col="lightblue1", border=NA)
   par(new=TRUE)
 plot(NSGAMmodel, select=7, shade=T, xlab=bquote(Abund [" cod"]), ylab= bquote(s(Abund[" cod"], 0.83)), cex.lab=1.5)

windows(5,5)
 par(mar=c(5,5,1,1))
 plot(NSGAMmodel, select=8, se=FALSE, main="", xlab="", ylab="", xaxt="",yaxt="",bty="")
   u <- par("usr") # The coordinates of the plot area
   rect(u[1], u[3], u[2], u[4], col="lightblue1", border=NA)
   par(new=TRUE)
 plot(NSGAMmodel, select=8, se=FALSE, main="", xlab=bquote(lon [" cod"]), xaxt="n", ylab= bquote(lat [" cod"]), cex.lab=1.5)# edf =23.89
    axis(1, seq(0,5,1),labels=round(seq(0,5,1)/cos(pi* 58/180),1))
 # Coasts in the North Sea
 Hide()
 polygon(NScoast  , col="grey")
 box()

windows(5,5)
 par(mar=c(5,5,1,1))
 plot(NSGAMmodel, select=9, shade=T, xlab="", ylab="", xaxt="",yaxt="",bty="")
   u <- par("usr") # The coordinates of the plot area
   rect(u[1], u[3], u[2], u[4], col="grey97", border=NA)
   par(new=TRUE)
 plot(NSGAMmodel, select=9, shade=T, xlab=expression("ST," ~degree~C), ylab=expression("s(ST, 2)"), cex.lab=1.5)

windows(5,5)
 par(mar=c(5,5,1,1))
 plot(NSGAMmodel, select=10, shade=T, xlab="", ylab= "", xaxt="",yaxt="",bty="")
   u <- par("usr") # The coordinates of the plot area
   rect(u[1], u[3], u[2], u[4], col="grey97", border=NA)
   par(new=TRUE)
 plot(NSGAMmodel, select=10, shade=T, xlab=bquote(Abund [" haddock"]), ylab= bquote(s(Abund[" haddock"], 0.0)), cex.lab=1.5)


# Figure S4. Random effects 

## https://mfasiolo.github.io/mgcViz/reference/plot.random.effect.html
require(mgcViz)

# For the Barents Sea
BSrandom <- getViz(BSGAMmodel)

windows(5,5)
 par(mar=c(5,5,1,1))
 plot(sm(BSrandom, 5)) + l_fitLine(colour = 2, linetype = 2) + l_points() +
     l_ciLine(colour = 4, linetype = 3)
windows(5,5)
 par(mar=c(5,5,1,1))
 plot(sm(BSrandom, 6)) + l_fitLine(colour = 2, linetype = 2) + l_points() +
     l_ciLine(colour = 4, linetype = 3)
windows(5,5)
 par(mar=c(5,5,1,1))
 plot(sm(BSrandom, 11)) + l_fitLine(colour = 2, linetype = 2) + l_points() +
     l_ciLine(colour = 4, linetype = 3)
windows(5,5)
 par(mar=c(5,5,1,1))
 plot(sm(BSrandom, 12)) + l_fitLine(colour = 2, linetype = 2) + l_points() +
     l_ciLine(colour = 4, linetype = 3)

# For the North Sea
  NSrandom <- getViz(NSGAMmodel)

windows(5,5)
 par(mar=c(5,5,1,1))
 plot(sm(NSrandom, 5)) + l_fitLine(colour = 2, linetype = 2) + l_points() +
     l_ciLine(colour = 4, linetype = 3)
windows(5,5)
 par(mar=c(5,5,1,1))
 plot(sm(NSrandom, 6)) + l_fitLine(colour = 2, linetype = 2) + l_points() +
     l_ciLine(colour = 4, linetype = 3)
windows(5,5)
 par(mar=c(5,5,1,1))
 plot(sm(NSrandom, 11)) + l_fitLine(colour = 2, linetype = 2) + l_points() +
     l_ciLine(colour = 4, linetype = 3)
windows(5,5)
 par(mar=c(5,5,1,1))
 plot(sm(NSrandom, 12)) + l_fitLine(colour = 2, linetype = 2) + l_points() +
     l_ciLine(colour = 4, linetype = 3)

