### This file is for processing the raw data into working data frames. 
## Written by MS Patterson (maspatte@uw.edu)
# Last updated: Oct 20 2017

### Set working environment, necessary for any next steps. 

#setwd('C:\\Users\\krtenneson\\Desktop\\lidarModel\\VersionControl\\lidarNew')
#setwd('\\\\166.2.126.25\\rseat\\Programs\\Reimbursibles\\fy2016\\R3_lidar_equation_transferability\\Analysis\\VersionControl\\lidarNew')
#setwd("~/Documents/R/lidarNew/scripts") #Mac
setwd("~/R/lidarNew/scripts") #WinCampus
#setwd("~/R/projects/lidarNew/scripts") #WinHome

### Load required packages
library(tidyverse)

### Source functions
source(file="functions.R")

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

### Partion data
Variables <- colnames(AllData) #pull variable names for use with select()
LidarNames <- c(Variables[23:75]) #subset of lidar metrics
FieldNames <- Variables[1:20] #subset of field variables
AuxNames <- Variables[72:76] #subset of additional environmental variables
RandUnif <- Variables[11] #The Random Uniform Variable

### Filter data down for modeling
DATA.mod <- AllData[AllData$RandomUniform<0.75,]

#Select predictors, chuck unused variables
DATA.mod <- select(DATA.mod, STBIOMS, TCUFT, LidarNames, AuxNames, FieldNames[9])

# Centering data
DATA.modC <- cbind(center(DATA.mod[,-52]), DATA.mod[52])
