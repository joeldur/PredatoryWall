# R-codes to draw the figure 2

# load the necessary packages
require(mapdata)
require(mgcv)
require(RColorBrewer) 
require(MASS)

# Variables in the data frame “dataBS” here for the Barents Sea
# "Year"     "Strata"   "CenLat"   "CenLon"   "Size"     "Nhad"    "Ncod"     "LatHad"   "LonHad"
#  "LatCod"   "LonCod"   "ST"  "f_Year"  "f_Size" 
# with f_Year Year factor transformed variable needed for the corGaus() term.
# Nhad and Ncod are z-score abundances
# CenLat and CenLon are the centre of gravity for the different sub-area
# LatCod and LatHad are latidudes
# LonHad and LonCod are longitudes
# note that the Longitudes must be corrected for earth curvature and latitudinal distance differences.
# Size is the haddock size categories
# ST the sea temperature at the location defined by LonHad/LatHad for the year defined by Year.

# GAM model used
GAMmodel <- 
gam(list(LonHad ~ s(Ncod,k=3, bs="ts")+te(LonCod,LatCod, bs="ts")+s(ST,k=3, bs="ts")+
                  s(Nhad,k=3, bs="ts")+s(f_Size,k=3, bs="re")+s(f_Year,k=3, bs = "re"),
         LatHad ~ s(Ncod,k=3, bs="ts")+te(LonCod,LatCod, bs="ts")+s(ST,k=3, bs="ts")+
                  s(Nhad,k=3, bs="ts")+s(f_Size,k=3, bs = "re")+s(f_Year,k=3, bs = "re")),
   data=dataBS, 
   correlation=corGaus(form=~(CenLon+CenLat)|f_Year), 
   family = mvn(d = 2), method= "REML")

latBS<-c(min(dataBS $LatCod):max(dataBS $LatCod))    # range of distribution of cod in latitude
lonBS<-c(min(dataBS $LonCod):max(dataBS $LonCod)) # range of distribution of cod in longitude

# build a data frame to put the results
BSRes1 <- data.frame(lon=NA, lat=NA)

# for each cells defined by latBS and lonBS estimates lonHad and latHad using predict (GAMmodel)
# build a new data frame 
newdataBS <- data.frame(Ncod=median(dataBS$Ncod),
                                           LonCod=NA, LatCod=NA,
                                           Ncod=median(dataBS $Ncod),
                                           ST=median(dataBS $ST,na.rm=T),
                                           Nhad=median(dataBS $Nhad,na.rm=T),
                                           CenLon= median(dataBS $CenLon,na.rm=T),  
                                           CenLat= median(dataBS $CenLat,na.rm=T),   
                                           Size=NA, Year=NA,  f_Year=NA, f_Size=NA)
# SizeVal is a 100 values long list of the relative haddock abundance for each size category 
# E.g. size 10cm corresponds to 27% of the haddocks, SizeVal contains then 27 times size 10.
for (Si in 1:100){ # size category 
 for (yr in unique(dataBS$Year)){
   for(lat in 1:length(latBS)){
     for(lon in 1:length(lonBS)){
  # fill the missing data in newdata
        newdataBS$LonCod <- lonBS[lon]
        newdataBS$LatCod <- latBS[lat]
        newdataBS$f_Year  <- as.factor(yr)
        newdataBS$Size      <- SizeVal[Si]
        newdataBS$f_Size    <- as.factor(SizeVal[Si])
        newdataBS$Year     <- yr
 # predict of the first part of the model -> Longitude
       predLon <- predict(GAMmodel, newdataBS)[1]      
 # predict of the second part of the model -> Latitude
       predLat <- predict(GAMmodel, newdataBS)[2]
 # select only the prediction that are inside the studied area (here for the Barents Sea)
       if(predLon<15 |predLon>37 | predLat>73.5 | predLat<69){next}else{
             BSRes1[nrow(BSRes1)+1,1] <-  predLon # Longitude
             BSRes1[nrow(BSRes1),2]   <-  predLat }   # Latitude
       rm(predLon,predLat)
     }; rm(lon)
    }; rm(lat)
  };rm(yr)
};rm(Si)

# remove the “NA”
BSRes1 <- na.exclude(BSRes1)

# Two-Dimensional Kernel Density Estimation
BSz <- kde2d(BSRes1[,1],BSRes1[,2], n = 1000)

# map of the Barents Sea
BarentsMAP <- map("world2Hires", xlim = c(20,69), ylim=c(65,81), plot=F, fill=T)

# plot the contour figure
filled.contour(BSz,  xlim=c(12,40),ylim=c(68,80), nlevels = 10,
      color.palette = function(n) c("#FFFFFF",rev(hcl.colors(n, "Blues"))),  # blue pattern
      plot.axes = {axis(1)
                             axis(2)
                             polygon(BarentsMAP$x,BarentsMAP$y,col="grey",lwd=1)} )


