### This file is for processing the raw validation data into working data frames, and fitting the models. 
## Written by MS Patterson (maspatte@uw.edu)
# Last updated: Oct 27 2017

### Set working environment, necessary for any next steps. 

#setwd("~/Documents/R/lidarNew/scripts") #Mac
#setwd("~/R/lidarNew/scripts") #WinCampus
#setwd("~/R/projects/lidarNew/scripts") #WinHome

### Load required packages
library(tidyverse)
library(robustbase)
library(broom)
library(cvTools)

### Source functions
source(file="functions.R")

### Bring in data
Apache <- read_csv("../Data/Apache_09252017.csv")
Sit2 <- read_csv("../Data/SitgreavesPhase2_09252017.csv")

ValData <- rbind(Apache, Sit2)

#Bring in Environmental Data
Aux <- read_csv("../Data/Auxillary/Merged_10172017.csv")
AuxTrim <- select(Aux, PLOT_ID, R3ERUCODE, elevation, aspect, slope)
ValData <- merge(ValData, AuxTrim, by="PLOT_ID", all=F)
ValData$R3ERUCODE <- as.factor(ValData$R3ERUCODE)
ValData$Forest <- as.factor(ValData$Forest)

NDVI <- read_csv("../Data/Auxillary/NDVIamplitude.csv")

ValData <- merge(ValData, select(NDVI, PLOT_ID, NDVI_Sample_NDVI_amplitude_1), by="PLOT_ID", all=F)
ValData <- rename(ValData, NDVI_Amp=NDVI_Sample_NDVI_amplitude_1)

#Remove 'zero' biomass outliers
ValData <- ValData[ValData$STBIOMS>0,]
ValData <- ValData[ValData$TCUFT>0,]

### Partion data
Variables <- colnames(AllData) #pull variable names for use with select()
LidarNames <- Variables[23:71] #subset of lidar metrics
FieldNames <- Variables[1:20] #subset of field variables
AuxNames <- Variables[72:76] #subset of additional environmental variables
RandUnif <- Variables[11] #The Random Uniform Variable

Predictors <- c(LidarNames, AuxNames, FieldNames[c(7,9)])

### Modeling Data
ValData.mod <- select(ValData, STBIOMS, TCUFT, Predictors)

## Prediction Model formulas
PredModB <- lm(log(STBIOMS)~ Elev_skewness + Elev_LCV + Elev_P80 + Total_first_returns + 
                         Pct_all_returns_above_ht + elevation + slope + PlotSizeAcres + R3ERUCODE,
               data=DATA.mod.t)

PredModT <- lm(log(TCUFT)~ Elev_skewness + Elev_LCV + Elev_P80 + Total_first_returns + 
                         Pct_all_returns_above_ht + elevation + slope + PlotSizeAcres + R3ERUCODE,
               data=DATA.mod.t)

### Validate Predictions against Validation set. Can't include Forest, because new Forests! 
### Generate Predictions 
ValBiomass <- predict(object=PredModB, newdata=ValData.mod)
ValTCUFT <- predict(object=PredModT, newdata=ValData.mod)

### Compare to real data

# plots
qqplot(exp(ValBiomass), ValData.mod$STBIOMS)
qqplot(exp(ValTCUFT), ValData.mod$TCUFT)

qplot(exp(ValBiomass), ValData.mod$STBIOMS) +geom_smooth(method=lm, se=T)
qplot(exp(ValTCUFT), ValData.mod$TCUFT) +geom_smooth(method=lm, se=T)


# metrics, Root Mean Square Error, Mean Summed Error, Mean Bias Error
ValBioRMSPE <- rmspe(ValData.mod$STBIOMS, exp(ValBiomass), includeSE=T)
ValBioMBE <- sum((exp(ValBiomass) - ValData.mod$STBIOMS))/length(NewBiomass)

ValCufRMSPE <- rmspe(ValData.mod$TCUFT, exp(ValTCUFT), includeSE=T)
ValCufMBE <- sum((exp(ValTCUFT) - ValData.mod$TCUFT))/length(NewBiomass)

# Robust metrics, drops outliers from data to calculate metrics 
ValBioRMSPE.r <- rtmspe(ValData.mod$STBIOMS, exp(ValBiomass), includeSE=T)

ValCufRMSPE.r <- rtmspe(ValData.mod$TCUFT, exp(ValTCUFT), includeSE=T)

### Predictions from BAS object itself.

VBMABio <- predict(BioMass.Mod, ValData.mod, top='10000')

VBmaBioRMSPE <- rmspe(ValData.mod$STBIOMS, exp(VBMABio$fit), includeSE=T)
VBmaBioMBE <- sum((exp(VBMABio$fit) - ValData.mod$STBIOMS))/length(NewBiomass)


VBMAWood <- predict(TCUFT.Mod, ValData.mod, top='10000')

vBmaWoodRMSPE <- rmspe(ValData.mod$TCUFT, exp(VBMAWood$fit), includeSE=T)
VBmaWoodMBE <- sum((exp(VBMAWood$fit) - ValData.mod$TCUFT))/length(NewBiomass)


# ### Generate robust predictions
# ValBiomass.r <- predict(object=FinModB.r, newdata=ValData.mod)
# ValTCUFT.r <- predict(object=FinModT.r, newdata=ValData.mod)
# 
# ### Compare robust predictions to real data
# 
# # plots
# qqplot(exp(ValBiomass.r), ValData.mod$STBIOMS)
# qqplot(exp(ValTCUFT.r), ValData.mod$TCUFT)
# 
# qplot(exp(ValBiomass.r), ValData.mod$STBIOMS) +geom_smooth(method=lm, se=T)
# qplot(exp(ValTCUFT.r), ValData.mod$TCUFT) +geom_smooth(method=lm, se=T)
# 
# # metrics, Root Mean Square Error, Mean Summed Error, Mean Bias Error
# ValBioRMSPE.rob <- rmspe(ValData.mod$STBIOMS, exp(ValBiomass.r), includeSE=T)
# ValBioMBE.rob <- sum((exp(ValBiomass.r) - ValData.mod$STBIOMS))/length(NewBiomass)
# 
# ValCufRMSPE.rob <- rmspe(ValData.mod$TCUFT, exp(ValTCUFT.r), includeSE=T)
# ValCufMBE.rob <- sum((exp(ValTCUFT.r) - ValData.mod$TCUFT))/length(NewBiomass)
