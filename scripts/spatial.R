### This file is for checking for spatial autocorrelation in the data
## Written by MS Patterson (maspatte@uw.edu)
# Last updated: October 19, 2017

setwd("~/Documents/R/lidarNew/scripts") #Mac
setwd("~/R/lidarNew/scripts") #WinCampus
setwd("~/R/projects/lidarNew/scripts") #WinCampus

### Load required packages
library(BAS)
library(stats)
library(tidyverse)
library(corrgram)
library(sp)
library(raster)
library(gstat)

### Check for spatial autocorrelation
projection <- crs(shapefile("../Data/Merged_EPSG102009_101717.shp"))
coords <- data.frame(cbind(DATA.test$X_albers, DATA.test$Y_albers))
colnames(coords) <- c("X_Albers", "Y_Albers")
BModsp <- SpatialPointsDataFrame(coords=(coords), data=BMod, proj4string=projection)

### Fit a variogram to the residuals from the data
Vgam <- variogram(residuals(BPMlm)~1, BModsp, cutoff = 75000) #produce a variogram

plot(Vgam, type='b', main='Residual Variogram') #very peculiar! 

# variogram is very poorly behaved. Could be because of outliers, or random sampling seeming not
# to be representative at times.

#Vmod <- fit.variogram(Vgam, vgm(model="Exp", range=75000)) #fit variogram model
#lot(Vgam, Vmod, main='Fitted Variogram') #Exponential seems best fit?
