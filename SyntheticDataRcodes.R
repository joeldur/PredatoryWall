#### Synthetics data to simulate the process and test the model
#### gam(list(LonH ~ s(Ncod,k=3, bs="ts")+te(LonC,LatC, bs="ts")+s(Nhad,k=3, bs="ts")+s(f_Size,k=3, bs = "re")+s(f_Year,k=3, bs = "re"),
####          LatH ~ s(Ncod,k=3, bs="ts")+te(LonC,LatC, bs="ts")+s(Nhad,k=3, bs="ts")+s(f_Size,k=3, bs = "re")+s(f_Year,k=3, bs = "re")),
####                 data=mgcvDATA,correlation=corGaus(form=~(CenLon+CenLat)|f_Year), family = mvn(d = 2), method= "REML")

#### Build Data frame for a GAM analysis with mgcv package
# Define an empty data frame
mgcvDATA <- data.frame(Year=2000, CenLon=NA, CenLat=NA, LonH=NA, LatH=NA, LonC=NA, LatC=NA, Size=NA, Ncod=NA, Nhad=NA) 

# Variables in the data frame mgcvDATA:
# CenLat and CenLon are the centre of gravity for the different cell
# Size is the haddock size categories
# LatC and LatH are latidudes
# LonH and LonC are longitudes
# Nhad and Ncod are z-score abundances
# f_Year (factor(Year)) needed for the corGaus() term will be added at a later stage

# Define the space, a matrix 6*6
Space <- matrix(0, 6, 6)

# Define the list of spaces for the different size categories for haddock and cod affecting each haddock size category 
listHad   <- c("SpaceH05","SpaceH10","SpaceH15","SpaceH20","SpaceH25")
listCod_H <- c("SpaceC_H05","SpaceC_H10","SpaceC_H15","SpaceC_H20","SpaceC_H25") 

### Size categories
SizeH <- c(5, 10, 15, 20, 25)

## Loop over 10 years
for (yr in 2000:2009){

# Locate the centre of gravity of each stock for the first size category 
 ### Cod Data likely to be located lower left area
 LonC05 <- sample(1:4, 1)
 LatC05 <- sample(1:4, 1)
 ### Haddock Data likely to be located upper right area
 LonH05 <- sample(3:6, 1)
 LatH05 <- sample(3:6, 1)


### Distribute abundance in each cells of the 6*6 space
##  Cod distribution
#   first size class of Cod
SpaceC05 <- Space
SpaceC05[LatC05, LonC05] <- 30 # Max abundance for the size category 
 if(LonC05<6){SpaceC05[LatC05, LonC05+1] <- 25}
 if(LonC05>1){SpaceC05[LatC05, LonC05-1] <- 25}
 if(LatC05<6){SpaceC05[LatC05+1, LonC05] <- 25}
 if(LatC05>1){SpaceC05[LatC05-1, LonC05] <- 25}
 if(LatC05>1&LonC05<6){SpaceC05[LatC05-1, LonC05+1] <- 15}
 if(LatC05<6&LonC05>1){SpaceC05[LatC05+1, LonC05-1] <- 15}
 if(LatC05<6&LonC05<6){SpaceC05[LatC05+1, LonC05+1] <- 15}
 if(LatC05>1&LonC05>1){SpaceC05[LatC05-1, LonC05-1] <- 15}

# second size class of Cod
SpaceC10 <- Space
LonC10 <- LonC05 + sample(-1:1, 1) # the location is linked to location of size 1, but allowed to vary by 1 cell.
  if(LonC10>6){LonC10<-6}
  if(LonC10<1){LonC10<-1}
LatC10 <- LatC05 + sample(-1:1, 1) # the location is linked to location of size 1, but allowed to vary by 1 cell.
  if(LatC10>6){LatC10<-6}
  if(LatC10<1){LatC10<-1}


SpaceC10[LatC10, LonC10] <- 25 
 if(LonC10<6){SpaceC10[LatC10, LonC10+1] <- 20}
 if(LonC10>1){SpaceC10[LatC10, LonC10-1] <- 20}
 if(LatC10<6){SpaceC10[LatC10+1, LonC10] <- 20}
 if(LatC10>1){SpaceC10[LatC10-1, LonC10] <- 20}
 if(LatC10>1&LonC10<6){SpaceC10[LatC10-1, LonC10+1] <- 10}
 if(LatC10<6&LonC10>1){SpaceC10[LatC10+1, LonC10-1] <- 10}
 if(LatC10<6&LonC10<6){SpaceC10[LatC10+1, LonC10+1] <- 10}
 if(LatC10>1&LonC10>1){SpaceC10[LatC10-1, LonC10-1] <- 10}

# Third size class of Cod
SpaceC15 <- Space
LonC15 <- LonC05 + sample(-1:1, 1)
  if(LonC15>6){LonC15<-6}
  if(LonC15<1){LonC15<-1}
LatC15 <- LatC05 + sample(-1:1, 1)
  if(LatC15>6){LatC15<-6}
  if(LatC15<1){LatC15<-1}

SpaceC15[LatC15, LonC15] <- 20 
 if(LonC15<6){SpaceC15[LatC15, LonC15+1] <- 15}
 if(LonC15>1){SpaceC15[LatC15, LonC15-1] <- 15}
 if(LatC15<6){SpaceC15[LatC15+1, LonC15] <- 15}
 if(LatC15>1){SpaceC15[LatC15-1, LonC15] <- 15}
 if(LatC15>1&LonC15<6){SpaceC15[LatC15-1, LonC15+1] <- 5}
 if(LatC15<6&LonC15>1){SpaceC15[LatC15+1, LonC15-1] <- 5}
 if(LatC15<6&LonC15<6){SpaceC15[LatC15+1, LonC15+1] <- 5}
 if(LatC15>1&LonC15>1){SpaceC15[LatC15-1, LonC15-1] <- 5}

# fourth size class of Cod
SpaceC20 <- Space
LonC20 <- LonC05 + sample(-1:1, 1)
  if(LonC20>6){LonC20<-6}
  if(LonC20<1){LonC20<-1}
LatC20 <- LatC05 + sample(-1:1, 1)
  if(LatC20>6){LatC20<-6}
  if(LatC20<1){LatC20<-1}

SpaceC20[LatC20, LonC20] <- 10 
 if(LonC20<6){SpaceC20[LatC20, LonC20+1] <- 5}
 if(LonC20>1){SpaceC20[LatC20, LonC20-1] <- 5}
 if(LatC20<6){SpaceC20[LatC20+1, LonC20] <- 5}
 if(LatC20>1){SpaceC20[LatC20-1, LonC20] <- 5}
 if(LatC20>1&LonC20<6){SpaceC20[LatC20-1, LonC20+1] <- 1}
 if(LatC20<6&LonC20>1){SpaceC20[LatC20+1, LonC20-1] <- 1}
 if(LatC20<6&LonC20<6){SpaceC20[LatC20+1, LonC20+1] <- 1}
 if(LatC20>1&LonC20>1){SpaceC20[LatC20-1, LonC20-1] <- 1}


# Fith size class of Cod
SpaceC25 <- Space
LonC25 <- LonC05 + sample(-1:1, 1)
  if(LonC25>6){LonC25<-6}
  if(LonC25<1){LonC25<-1}
LatC25 <- LatC05 + sample(-1:1, 1)
  if(LatC25>6){LatC25<-6}
  if(LatC25<1){LatC25<-1}

SpaceC25[LatC25, LonC25] <- 5 
 if(LonC25<6){SpaceC25[LatC25, LonC25+1] <- 2}
 if(LonC25>1){SpaceC25[LatC25, LonC25-1] <- 2}
 if(LatC25<6){SpaceC25[LatC25+1, LonC25] <- 2}
 if(LatC25>1){SpaceC25[LatC25-1, LonC25] <- 2}

## Distribution of Cod that can eat/affect H (sum the different cod size categories spaces, one space per H size category)
SpaceC_H05 <- SpaceC05 + SpaceC10 + SpaceC15 + SpaceC20 + SpaceC25
 # add noise
 for (l in 1:6){
   for(L in 1:6){
     SpaceC_H05[L,l] <- SpaceC_H05[L,l] + rnorm(1,0,0.5) #add observation noise
   };rm(L)
 };rm(l)
 # remove negative values
 SpaceC_H05[which(SpaceC_H05<0)] <- 0

SpaceC_H10 <- SpaceC10 + SpaceC15 + SpaceC20 + SpaceC25
 # add noise
 for (l in 1:6){
   for(L in 1:6){
     SpaceC_H10[L,l] <- SpaceC_H10[L,l] + rnorm(1,0,0.5)
   };rm(L)
 };rm(l)
 # remove negative values
 SpaceC_H10[which(SpaceC_H10<0)] <- 0

SpaceC_H15 <- SpaceC15 + SpaceC20 + SpaceC25
 # add noise
 for (l in 1:6){
   for(L in 1:6){
     SpaceC_H15[L,l] <- SpaceC_H15[L,l] + rnorm(1,0,0.5)
   };rm(L)
 };rm(l)
 # remove negative values
 SpaceC_H15[which(SpaceC_H15<0)] <- 0

SpaceC_H20 <- SpaceC20 + SpaceC25
 # add noise
 for (l in 1:6){
   for(L in 1:6){
     SpaceC_H20[L,l] <- SpaceC_H20[L,l] + rnorm(1,0,0.5)
   };rm(L)
 };rm(l)
 # remove negative values
 SpaceC_H20[which(SpaceC_H20<0)] <- 0

SpaceC_H25 <- SpaceC25
 # add noise
 for (l in 1:6){
   for(L in 1:6){
     SpaceC_H25[L,l] <- SpaceC_H25[L,l] + rnorm(1,0,0.5)
   };rm(L)
 };rm(l)
 # remove negative values
 SpaceC_H25[which(SpaceC_H25<0)] <- 0


##  Haddock distribution
#   first size class of Haddock
SpaceH05 <- Space
SpaceH05[LatH05, LonH05] <- 30 
 if(LonH05<6){SpaceH05[LatH05, LonH05+1] <- 25}
 if(LonH05>1){SpaceH05[LatH05, LonH05-1] <- 25}
 if(LatH05<6){SpaceH05[LatH05+1, LonH05] <- 25}
 if(LatH05>1){SpaceH05[LatH05-1, LonH05] <- 25}
 if(LatH05>1&LonH05<6){SpaceH05[LatH05-1, LonH05+1] <- 15}
 if(LatH05<6&LonH05>1){SpaceH05[LatH05+1, LonH05-1] <- 15}
 if(LatH05<6&LonH05<6){SpaceH05[LatH05+1, LonH05+1] <- 15}
 if(LatH05>1&LonH05>1){SpaceH05[LatH05-1, LonH05-1] <- 15}

# second size class of Haddock
SpaceH10 <- Space
LonH10 <- LonH05 + sample(-1:1, 1)
  if(LonH10>6){LonH10<-6}
  if(LonH10<1){LonH10<-1}
LatH10 <- LatH05 + sample(-1:1, 1)
  if(LatH10>6){LatH10<-6}
  if(LatH10<1){LatH10<-1}

SpaceH10[LatH10, LonH10] <- 25 
 if(LonH10<6){SpaceH10[LatH10, LonH10+1] <- 20}
 if(LonH10>1){SpaceH10[LatH10, LonH10-1] <- 20}
 if(LatH10<6){SpaceH10[LatH10+1, LonH10] <- 20}
 if(LatH10>1){SpaceH10[LatH10-1, LonH10] <- 20}
 if(LatH10>1&LonH10<6){SpaceH10[LatH10-1, LonH10+1] <- 10}
 if(LatH10<6&LonH10>1){SpaceH10[LatH10+1, LonH10-1] <- 10}
 if(LatH10<6&LonH10<6){SpaceH10[LatH10+1, LonH10+1] <- 10}
 if(LatH10>1&LonH10>1){SpaceH10[LatH10-1, LonH10-1] <- 10}

# Third size class of Haddock
SpaceH15 <- Space
LonH15 <- LonH05 + sample(-1:1, 1)
  if(LonH15>6){LonH15<-6}
  if(LonH15<1){LonH15<-1}
LatH15 <- LatH05 + sample(-1:1, 1)
  if(LatH15>6){LatH15<-6}
  if(LatH15<1){LatH15<-1}

SpaceH15[LatH15, LonH15] <- 20 
 if(LonH15<6){SpaceH15[LatH15, LonH15+1] <- 15}
 if(LonH15>1){SpaceH15[LatH15, LonH15-1] <- 15}
 if(LatH15<6){SpaceH15[LatH15+1, LonH15] <- 15}
 if(LatH15>1){SpaceH15[LatH15-1, LonH15] <- 15}
 if(LatH15>1&LonH15<6){SpaceH15[LatH15-1, LonH15+1] <- 5}
 if(LatH15<6&LonH15>1){SpaceH15[LatH15+1, LonH15-1] <- 5}
 if(LatH15<6&LonH15<6){SpaceH15[LatH15+1, LonH15+1] <- 5}
 if(LatH15>1&LonH15>1){SpaceH15[LatH15-1, LonH15-1] <- 5}

# fourth size class of Haddock
SpaceH20 <- Space
LonH20 <- LonH05 + sample(-1:1, 1)
  if(LonH20>6){LonH20<-6}
  if(LonH20<1){LonH20<-1}
LatH20 <- LatH05 + sample(-1:1, 1)
  if(LatH20>6){LatH20<-6}
  if(LatH20<1){LatH20<-1}

SpaceH20[LatH20, LonH20] <- 15 
 if(LonH20<6){SpaceH20[LatH20, LonH20+1] <- 10}
 if(LonH20>1){SpaceH20[LatH20, LonH20-1] <- 10}
 if(LatH20<6){SpaceH20[LatH20+1, LonH20] <- 10}
 if(LatH20>1){SpaceH20[LatH20-1, LonH20] <- 10}
 if(LatH20>1&LonH20<6){SpaceH20[LatH20-1, LonH20+1] <- 2}
 if(LatH20<6&LonH20>1){SpaceH20[LatH20+1, LonH20-1] <- 2}
 if(LatH20<6&LonH20<6){SpaceH20[LatH20+1, LonH20+1] <- 2}
 if(LatH20>1&LonH20>1){SpaceH20[LatH20-1, LonH20-1] <- 2}


# Fith size class of Haddock
SpaceH25 <- Space
LonH25 <- LonH05 + sample(-1:1, 1)
  if(LonH25>6){LonH25<-6}
  if(LonH25<1){LonH25<-1}
LatH25 <- LatH05 + sample(-1:1, 1)
  if(LatH25>6){LatH25<-6}
  if(LatH25<1){LatH25<-1}

SpaceH25[LatH25, LonH25] <- 5 
 if(LonH25<6){SpaceH25[LatH25, LonH25+1] <- 2}
 if(LonH25>1){SpaceH25[LatH25, LonH25-1] <- 2}
 if(LatH25<6){SpaceH25[LatH25+1, LonH25] <- 2}
 if(LatH25>1){SpaceH25[LatH25-1, LonH25] <- 2}
 if(LatH25>1&LonH25<6){SpaceH25[LatH25-1, LonH25+1] <- 1}
 if(LatH25<6&LonH25>1){SpaceH25[LatH25+1, LonH25-1] <- 1}
 if(LatH25<6&LonH25<6){SpaceH25[LatH25+1, LonH25+1] <- 1}
 if(LatH25>1&LonH25>1){SpaceH25[LatH25-1, LonH25-1] <- 1}

## Link Haddock to Cod distribution
# first size class of Haddock depending on SpaceC_H05
SpaceH05 <- SpaceH05 - SpaceC_H05
# add noise
 for (l in 1:6){
   for(L in 1:6){
     SpaceH05[L,l] <- SpaceH05[L,l] + rnorm(1,0,0.5) #add observation noise
   };rm(L)
 };rm(l)
 # remove negative values
 SpaceH05[which(SpaceH05<0)] <- 0

# second size class of Haddock depending on SpaceC_H05
SpaceH10 <- SpaceH10 - SpaceC_H10
# add noise
 for (l in 1:6){
   for(L in 1:6){
     SpaceH10[L,l] <- SpaceH10[L,l] + rnorm(1,0,0.5)
   };rm(L)
 };rm(l)
 # remove negative values
 SpaceH10[which(SpaceH10<0)] <- 0

# third size class of Haddock depending on SpaceC_H05
SpaceH15 <- SpaceH15 - SpaceC_H15
# add noise
 for (l in 1:6){
   for(L in 1:6){
     SpaceH15[L,l] <- SpaceH15[L,l] + rnorm(1,0,0.5)
   };rm(L)
 };rm(l)
 # remove negative values
  SpaceH15[which(SpaceH15<0)] <- 0

# Fourth size class of Haddock depending on SpaceC_H25
SpaceH20 <- SpaceH20 - SpaceC_H20
# add noise
 for (l in 1:6){
   for(L in 1:6){
     SpaceH20[L,l] <- SpaceH20[L,l] + rnorm(1,0,0.5)
   };rm(L)
 };rm(l)
 # remove negative values
  SpaceH20[which(SpaceH20<0)] <- 0

# Fith size class of Haddock depending on SpaceC_H30
SpaceH25 <- SpaceH25 - SpaceC_H25
# add noise
 for (l in 1:6){
   for(L in 1:6){
     SpaceH25[L,l] <- SpaceH25[L,l] + rnorm(1,0,0.5)
   };rm(L)
 };rm(l)
 # remove negative values
  SpaceH25[which(SpaceH25<0)] <- 0


# Fill the data in the mgcvDATA using the simulated distributions
 for (size in 1:5){
   provHad <- get(listHad[size])
   provCod <- get(listCod_H[size])
   # Build provisory dataframe to calculate the Centre of gravity
   # use the command COGravity() from the package SDMTools.
   # https://rdrr.io/cran/SDMTools/
   prov<- data.frame(lat=rep(1:6,6), lon=1, AbunH=NA, AbunC=NA)
   lonI <- 0
   for (i in 1:length(prov[,1])){
        if (prov[i,1] ==1){lonI<-lonI+1}
        prov[i,2] <- lonI 
   }; rm(i,lonI)
   # entre in prov (data.frame) the abunance for the weight
   lonI <- 0
   for (l in 1:6){
    for(L in 1:6){
      lonI <- lonI + 1
      prov$AbunH[lonI] <- provHad[L,l]
      prov$AbunC[lonI] <- provCod[L,l]
    };rm(L)
   };rm(l, lonI)
  ## 
  lonI <- 0
  for (l in 1:6){
    for(L in 1:6){
   lonI <- lonI + 1
   mgcvDATA[nrow(mgcvDATA)+1,2]    <- l  
   mgcvDATA[nrow(mgcvDATA),3]      <- L  
   mgcvDATA[nrow(mgcvDATA),]$Nhad  <-  prov$AbunH[lonI]
   mgcvDATA[nrow(mgcvDATA),1]      <-  yr
   mgcvDATA$LatC[nrow(mgcvDATA)]   <-  COGravity(prov$lat,prov$lon,wt=prov$AbunC)[1]
   mgcvDATA$LonC[nrow(mgcvDATA)]   <-  COGravity(prov$lat,prov$lon,wt=prov$AbunC)[3]
   mgcvDATA$LatH[nrow(mgcvDATA)]   <-  COGravity(prov$lat,prov$lon,wt=prov$AbunH)[1]
   mgcvDATA$LonH[nrow(mgcvDATA)]   <-  COGravity(prov$lat,prov$lon,wt=prov$AbunH)[3]   
   mgcvDATA[nrow(mgcvDATA),]$Ncod  <-  sum(prov$AbunC)   
   mgcvDATA$Size[nrow(mgcvDATA)]   <-  SizeH[size]
   }}; rm(l,L, lonI)
   rm(prov)
   };rm(size)
};rm(yr)

mgcvDATA <-mgcvDATA[-1,] # remove the first row containing only "NA"
 
mgcvDATA$Ncod     <- scale(mgcvDATA$Ncod)     # z-score the Abundance of Cod
mgcvDATA$Nhad     <- scale(mgcvDATA$Nhad)     # z-score the Abundance of Haddock
mgcvDATA$f_Year   <- as.factor(mgcvDATA$Year) # create factor caterogy for Year
mgcvDATA$f_Size   <- as.factor(mgcvDATA$Size) # create factor caterogy for Year



#### Run a multivariate GAM model
require(mgcv)
SimulGAM <- gam(list(LonH ~ s(Ncod,k=3, bs="ts")+te(LonC,LatC, bs="ts")+s(Nhad,k=3, bs="ts")+s(f_Size,k=3, bs = "re")+s(f_Year,k=3, bs = "re"),
                     LatH ~ s(Ncod,k=3, bs="ts")+te(LonC,LatC, bs="ts")+s(Nhad,k=3, bs="ts")+s(f_Size,k=3, bs = "re")+s(f_Year,k=3, bs = "re")),
                     data=mgcvDATA,correlation=corGaus(form=~(CenLon+CenLat)|f_Year), family = mvn(d = 2), method= "REML")

# plot the 
plot(SimulGAM, select=2,residuals=FALSE, se=FALSE, main="Effect on Haddock longitude") # similar Figure 1a and 1c in the paper
plot(SimulGAM, select=7,residuals=FALSE, se=FALSE, main="Effect on Haddock latitude")  # similar Figure 1b and 1d in the paper


#### Plot a figure similar to Figure 2 in the paper
 require("RColorBrewer")
 library(MASS)


latRes<-c(1:5); lonRes<-c(1:5) # consider only the "observed" distribution of cod 1-4 ± 1
Res1 <- data.frame(lon=NA, lat=NA)

for(lat in 1:length(latRes)){
   for(lon in 1:length(lonRes)){
     newdata <- data.frame(Ncod=median(mgcvDATA$Ncod),LonC=lonRes[lon], LatC=latRes[lat],
                           Nhad=median(mgcvDATA$Nhad,na.rm=T),
                           CenLon= median(mgcvDATA$CenLon,na.rm=T), 
                           CenLat= median(mgcvDATA$CenLat,na.rm=T),  
                           Size=median(mgcvDATA$Size,na.rm=T), 
                           Year=median(mgcvDATA$Year,na.rm=T), 
                           f_Year= as.factor(median(mgcvDATA$Year, na.rm=T)),
                           f_Size= as.factor(median(mgcvDATA$Size, na.rm=T)))                
      predLon <- predict(SimulGAM, newdata)[1]
      predLat <- predict(SimulGAM, newdata)[2]
      # select only the prediction that are inside the space considered
      if(predLon<1 |predLon>6 | predLat>6 | predLat<1){next}else{
          Res1[nrow(Res1)+1,1] <-  predLon   # Lon
           Res1[nrow(Res1),2]   <-  predLat }# Lat
      rm(predLon,predLat)
     }; rm(lon)
    }; rm(lat)

Res1 <- na.exclude(Res1)

z <- kde2d(Res1[,1],Res1[,2], n = 1000)

#windows()
filled.contour(z,  xlim=c(1,6),ylim=c(1,6), nlevels = 20,
           color.palette = function(n) c("#FFFFFF",rev(hcl.colors(n, "Blues"))),  
           plot.axes = {axis(1, tcl=-0.5)
                        axis(2,  tcl=-0.5)})

#########



