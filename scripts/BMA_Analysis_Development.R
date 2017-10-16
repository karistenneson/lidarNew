### This file is for developing the BMA components of the analysis
## Written by MS Patterson (maspatte@uw.edu)
# Last updated: July 11, 2017

setwd('C:\\Users\\krtenneson\\Desktop\\lidarModel\\VersionControl\\lidarNew')
setwd('\\\\166.2.126.25\\rseat\\Programs\\Reimbursibles\\fy2016\\R3_lidar_equation_transferability\\Analysis\\VersionControl\\lidarNew')
setwd("~/Documents/R/lidar/")

### Load required packages
library(BAS)
library(stats)
library(tidyverse)
library(corrgram)

### Bring in data
Coco <- read_csv("../Data/Coconino07162017.csv")
Sit <- read_csv("../Data/Sitgreaves07162017.csv")
SWJM <- read_csv("../Data/SWJM07162017.csv")
Tonto <- read_csv("../Data/Tonto07162017.csv")
Kaibab <- read_csv("../Data/NKaibab07192017.csv")

AllData <- rbind(Coco, Sit, SWJM, Tonto, Kaibab)

### Partion data
Variables <- colnames(AllData) #pull variable names for use with select()
PredNames <- c(Variables[23:71], Variables[11]) #subset of lidar metrics
FieldNames <- Variables[1:20] #subset of field variables

### Need to add in bit here for selecting rows using the Random Uniform, 
### or alternately add that to the Independent variables

STBIOMS <- AllData$STBIOMS #Standing biomass
TCUFT <- AllData$TCUFT #Total timber volume

PRED <- select(AllData, PredNames) #Select lidar metric data

### Exploring relationships between predictors
corrgram(PRED, order=T, lower.panel=panel.shade,
         upper.panel=panel.ellipse, text.panel=panel.txt,
         main="Lidar Predictor Data in PC2/PC1 Order")
# A lot of positive correlation, but some things relatively uncorrelated with
# everything else, like Kurtosis. Total returns also only weakly correlated with
# most everything else. 


### Model development
### Filter data down for development, package development
DATA.test <- AllData[AllData$RandomUniform<0.25,]

STBIOMS.test <- DATA.test$STBIOMS #Standing biomass
TCUFT.test <- DATA.test$TCUFT #Total timber volume

PRED.test <- select(DATA.test, PredNames) #Select lidar metric data

BMod <- cbind(STBIOMS.test, PRED.test[,-50])
BModC <- cbind(STBIOMS.test, center(PRED.test[,-50]))

# Full variable pool
BioBLM <- bas.lm(log(STBIOMS.test+.00001)~ ., 
                 data=BMod, 
                 prior="g-prior", 
                 modelprior=uniform())

summary(BioBLM)
plot(BioBLM)


# Full variable pool, centered
BioBLMc <- bas.lm(log(STBIOMS.test+.00001)~ ., 
                 data=BModC, 
                 prior="g-prior", 
                 modelprior=uniform())

summary(BioBLMc)
plot(BioBLMc)
### Generate Predictions

### Validate Predictions against Validation set
