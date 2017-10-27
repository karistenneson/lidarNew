### This file is for checking for spatial autocorrelation in the data
## Written by MS Patterson (maspatte@uw.edu)
# Last updated: October 19, 2017

setwd("~/Documents/R/lidarNew/scripts") #Mac
setwd("~/R/lidarNew/scripts") #WinCampus
setwd("~/R/projects/lidarNew/scripts") #WinCampus

### Load required packages
library(sp)
library(raster)
library(gstat)
library(tidyverse)


### Bring in data
Coco <- read_csv("../Data/Coconino07162017.csv")
Sit <- read_csv("../Data/Sitgreaves07162017.csv")
SWJM <- read_csv("../Data/SWJM07162017.csv")
Tonto <- read_csv("../Data/Tonto07162017.csv")
Kaibab <- read_csv("../Data/NKaibab07192017.csv")

AllData <- rbind(Coco, Sit, SWJM, Tonto, Kaibab)

#Bring in Environmental Data
Aux <- read_csv("../Data/Auxillary/Merged_10172017.csv")
AuxTrim <- select(Aux, PLOT_ID, R3ERUCODE, elevation, aspect, slope)
AllData <- merge(AllData, AuxTrim, by="PLOT_ID", all=F)
AllData$R3ERUCODE <- as.factor(AllData$R3ERUCODE)

NDVI <- read_csv("../Data/Auxillary/NDVIamplitude.csv")

AllData <- merge(AllData, select(NDVI, PLOT_ID, NDVI_Sample_NDVI_amplitude_1), by="PLOT_ID", all=F)
AllData <- rename(AllData, NDVI_Amp=NDVI_Sample_NDVI_amplitude_1)

#Remove 'zero' biomass outliers
AllData <- AllData[AllData$STBIOMS>0,]
AllData <- AllData[AllData$TCUFT>0,]

### Partion data
Variables <- colnames(AllData) #pull variable names for use with select()
LidarNames <- c(Variables[23:75]) #subset of lidar metrics
FieldNames <- Variables[1:20] #subset of field variables
AuxNames <- Variables[72:76] #subset of additional environmental variables
RandUnif <- Variables[11] #The Random Uniform Variable

### Build Spatial Data Frame
projection <- crs(shapefile("../Data/Merged_EPSG102009_101717.shp"))
coords <- data.frame(cbind(AllData$X_albers, AllData$Y_albers))
colnames(coords) <- c("X_Albers", "Y_Albers")
AllData.sp <- SpatialPointsDataFrame(coords=(coords), data=AllData, proj4string=projection)

## Filter data down for modeling
DATA.mod.sp <- AllData.sp[AllData.sp$RandomUniform<0.75,]

#Select predictors, chuck unused variables
DATA.mod.sp <- DATA.mod.sp[, c("Forest", "STBIOMS", "TCUFT", LidarNames, AuxNames, FieldNames[9])]

#Objects for each forest. Might not be necessary, but whatevs.
summary(as.factor(DATA.mod.sp$Forest))

Coco.sp <- DATA.mod.sp[DATA.mod.sp$Forest=="Coconino",]
Kaibab.sp <- DATA.mod.sp[DATA.mod.sp$Forest=="NorthKaibab",]
Sit.sp <- DATA.mod.sp[DATA.mod.sp$Forest=="Sitgreaves",]
SWJM.sp <- DATA.mod.sp[DATA.mod.sp$Forest=="SWJM",]
Tonto.sp <-DATA.mod.sp[DATA.mod.sp$Forest=="Tonto",]

### Fit a variogram to the residuals from the data
Vgam.coco <- variogram(residuals(FinModB)[which(DATA.mod.sp$Forest=="Coconino")]~1, 
                       data = Coco.sp) #produce a variogram
plot(Vgam.coco, type='b', main='Residual Variogram') 
#pretty level, but very low semivariance.  

Vgam.kai <- variogram(residuals(FinModB)[which(DATA.mod.sp$Forest=="NorthKaibab")]~1, 
                       data = Kaibab.sp) #produce a variogram
plot(Vgam.kai, type='b', main='Residual Variogram') 
#erratic, but essentially flatish, higher semivariance than Coco.

Vgam.sit <- variogram(residuals(FinModB)[which(DATA.mod.sp$Forest=="Sitgreaves")]~1, 
                      data = Sit.sp) #produce a variogram
plot(Vgam.sit, type='b', main='Residual Variogram') 
#erratic, but some trend upwards, could be some spatial correlation here.

Vgam.swjm <- variogram(residuals(FinModB)[which(DATA.mod.sp$Forest=="SWJM")]~1, 
                      data = SWJM.sp) #produce a variogram
plot(Vgam.swjm, type='b', main='Residual Variogram') 
#erratic, but mostly flat. No clear distance to filter on. 

Vgam.ton <- variogram(residuals(FinModB)[which(DATA.mod.sp$Forest=="Tonto")]~1, 
                      data = Tonto.sp) #produce a variogram
plot(Vgam.ton, type='b', main='Residual Variogram') 
#pretty flat, small trend up to 15,000, but very low change in semivariance. 

### Overall, it generally seems like there may be some spatial autocorrelation in the data,
### but there's not really a clear distance to filter on in any of the data sets, and 
### fitting a variogram model is unlikely to work well in any instance. 

#Vmod <- fit.variogram(Vgam, vgm(model="Exp", range=75000)) #fit variogram model
#lot(Vgam, Vmod, main='Fitted Variogram') #Exponential seems best fit?

detach(package:raster)
detach(package:sp)
detach(package:gstat)
