### This file is for developing the BMA components of the analysis
## Written by MS Patterson (maspatte@uw.edu)
# Last updated: July 11, 2017

setwd('C:\\Users\\krtenneson\\Desktop\\lidarModel\\VersionControl\\lidarNew')
setwd('\\\\166.2.126.25\\rseat\\Programs\\Reimbursibles\\fy2016\\R3_lidar_equation_transferability\\Analysis\\VersionControl\\lidarNew')

### Load required packages
library(BMS)
library(stats)
library(tidyverse)

### Bring in data
Coco <- read_csv("./Data/Coconino07162017.csv")
Sit <- read_csv("./Data/Sitgreaves07162017.csv")
SWJM <- read_csv("./Data/SWJM07162017.csv")
Tonto <- read_csv("./Data/Tonto07162017.csv")
Kaibab <- read_csv("./Data/NKaibab07192017.csv")

AllData <- rbind(Coco, Sit, SWJM, Tonto, Kaibab)

### Partion data
Variables <- colnames(AllData) #pull variable names for use with select()
PredNames <- c(Variables[23:71], Variables[11]) #subset of lidar metrics
FieldNames <- Variables[1:20] #subset of field variables

### Need to add in bit here for selecting rows using the Random Uniform, 
### or alternately add that to the Independent variables

#STBIOMS <- AllData$STBIOMS #Standing biomass
#TCUFT <- AllData$TCUFT #Total timber volume

#PRED <- select(AllData, PredNames) #Select lidar metric data

### Model development
 
### Generate Predictions

### Validate Predictions against Validation set
