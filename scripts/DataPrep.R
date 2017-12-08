### This file is for processing the raw data into working data frames. 
## Written by MS Patterson (maspatte@uw.edu)
# Last updated: Oct 20 2017

### Set working environment, necessary for any next steps. 

#setwd('C:\\Users\\krtenneson\\Desktop\\lidarPaper\\lidarNew\\scripts')
#setwd('\\\\166.2.126.25\\rseat\\Programs\\Reimbursibles\\fy2016\\R3_lidar_equation_transferability\\Analysis\\VersionControl\\lidarNew\\scripts')
#setwd("~/Documents/R/lidarNew/scripts") #Mac
#setwd("~/R/lidarNew/scripts") #WinCampus
#setwd("~/R/projects/lidarNew/scripts") #WinHome

### Load required packages
library(tidyverse)

### Source functions
source(file="functions.R")

### Bring in data
Coco <- read.csv("../Data/Coconino07162017.csv", stringsAsFactors=FALSE)
Sit <- read.csv("../Data/Sitgreaves07162017.csv", stringsAsFactors=FALSE)
SWJM <- read.csv("../Data/SWJM07162017.csv", stringsAsFactors=FALSE)
Tonto <- read.csv("../Data/Tonto07162017.csv", stringsAsFactors=FALSE)
Kaibab <- read.csv("../Data/NKaibab07192017.csv", stringsAsFactors=FALSE)
Kaibab[,'fpc']
AllData <- rbind(Coco, Sit, SWJM, Tonto, Kaibab)
unique(AllData[,'fpc'])

#Bring in Environmental Data
Aux <- read.csv("../Data/Auxillary/Merged_10172017.csv", stringsAsFactors=FALSE)
AuxTrim <- select(Aux, PLOT_ID, R3ERUCODE, elevation, aspect, slope)
AllData <- merge(AllData, AuxTrim, by="PLOT_ID", all=F)
AllData$R3ERUCODE <- as.factor(AllData$R3ERUCODE)
AllData$Forest <- as.factor(AllData$Forest)
AllData$Site <- as.factor(AllData$Site)

NDVI <- read.csv("../Data/Auxillary/NDVIamplitude.csv")

AllData <- merge(AllData, select(NDVI, PLOT_ID, NDVI_Sample_NDVI_amplitude_1), by="PLOT_ID", all=F)
AllData <- rename(AllData, NDVI_Amp=NDVI_Sample_NDVI_amplitude_1)

#Remove 'zero' biomass outliers
AllData <- AllData[AllData$STBIOMS>0,]
AllData <- AllData[AllData$TCUFT>0,]

### Partion data
Variables <- colnames(AllData) #pull variable names for use with select()
LidarNames <- Variables[23:71] #subset of lidar metrics
FieldNames <- Variables[1:20] #subset of field variables
AuxNames <- Variables[72:76] #subset of additional environmental variables
RandUnif <- Variables[11] #The Random Uniform Variable

Predictors <- c(LidarNames, AuxNames, FieldNames[c(6, 7,9, 15)])

### Filter data down for modeling
DATA.mod <- AllData[AllData$RandomUniform<0.75,]

#Select predictors, chuck unused variables
DATA.mod <- select(DATA.mod, STBIOMS, TCUFT, fpc, Predictors)
# Centering data
DATA.modC <- cbind(DATA.mod[, c(1,2)], center(DATA.mod[,c(-1, -2, -53, -58, -59)]), DATA.mod[c(53, 58, 59)])
data.modC <- cbind(DATA.mod[, c(1,2)], (DATA.mod[,c(-1, -2, -53, -58, -59)]), DATA.mod[c(53, 58, 59)])

#########################################################
### Filter data down for modeling
DATA.val <- AllData[AllData$RandomUniform>=0.75,]

#Select predictors, chuck unused variables
DATA.val <- select(DATA.val, STBIOMS, TCUFT, fpc, Predictors)

# Centering data
DATA.valC <- cbind(DATA.val[, c(1,2)], center(DATA.val[,c(-1, -2, -53, -58, -59)]), DATA.val[c(53, 58, 59)])
data.valC <- cbind(DATA.val[, c(1,2)], (DATA.val[,c(-1, -2, -53, -58, -59)]), DATA.val[c(53, 58, 59)])

#########################################################
library(survey)
# average biomass and volume
data.modC.svy <- svydesign(ids = ~1, data = data.modC, strata = data.modC$, fpc = data.modC$fpc) 
unique(data.modC[,'fpc'])

data.valC.svy <- svydesign(ids = ~1, data = data.valC, fpc = data.valC$fpc) 

boxplot(data.modC$STBIOMS ~ data.modC$R3ERUCODE, varwidth = T)
boxplot(data.valC$STBIOMS ~ data.valC$R3ERUCODE, varwidth = T)

boxplot(data.modC$STBIOMS ~ data.modC$Forest, varwidth = T)
boxplot(data.valC$STBIOMS ~ data.valC$Forest, varwidth = T)
