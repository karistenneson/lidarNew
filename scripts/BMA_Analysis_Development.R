### This file is for developing the BMA components of the analysis
## Written by MS Patterson (maspatte@uw.edu)
# Last updated: July 11, 2017

setwd('C:\\Users\\krtenneson\\Desktop\\lidarModel\\VersionControl\\lidarNew')
setwd('\\\\166.2.126.25\\rseat\\Programs\\Reimbursibles\\fy2016\\R3_lidar_equation_transferability\\Analysis\\VersionControl\\lidarNew')
setwd("~/Documents/R/lidar/") #Mac
setwd("~/R/lidarNew/scripts") #Win

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
PredNames[which(BioBLM$probne0>0.5)-1]

plot(BioBLM, ask=F)

#Plot CI of all parameters
plot(confint(coef(BioBLM), parm=2:50))

#Confidence Intervals on median/highest probability model
plot(confint(coef(BioBLM, estimator="MPM")))
plot(confint(coef(BioBLM, estimator="HPM")))

# Full variable pool, centered
BioBLMc <- bas.lm(log(STBIOMS.test+.00001)~ ., 
                 data=BModC, 
                 prior="g-prior", 
                 modelprior=uniform())

summary(BioBLMc)
PredNames[which(BioBLMc$probne0>0.5)-1]

plot(BioBLMc, ask=F)

#Plot CI of all parameters
plot(confint(coef(BioBLMc), parm=2:50))

#Confidence Intervals on median/highest probability model
plot(confint(coef(BioBLMc, estimator="MPM")))
plot(confint(coef(BioBLMc, estimator="HPM")))

# Full variable pool, centered, truncated poisson prior, g = N (unit information prior)
BioBLMp <- bas.lm(log(STBIOMS.test+.00001)~ ., 
                  data=BModC, 
                  prior="g-prior",
                  alpha = nrow(BModC),
                  modelprior=tr.poisson(9,30))

summary(BioBLMp)
PredNames[which(BioBLMp$probne0>0.5)-1]

plot(BioBLMc, ask=F)

# Full variable pool, truncated poisson prior, hyper-g
BioBLMh <- bas.lm(log(STBIOMS.test+.00001)~ ., 
                  data=BMod, 
                  prior="hyper-g",
                  alpha = 3,
                  modelprior=tr.poisson(9,30))

summary(BioBLMh)
PredNames[which(BioBLMh$probne0>0.5)-1]


# Full variable pool, centered, truncated poisson prior, hyper-g
BioBLMhc <- bas.lm(log(STBIOMS.test+.00001)~ ., 
                  data=BMod, 
                  prior="hyper-g",
                  alpha = 3,
                  modelprior=tr.poisson(9,30))

summary(BioBLMhc)
PredNames[which(BioBLMhc$probne0>0.5)-1]

# Correlation in variables in the median model
MedModVar <- select(BMod, PredNames[which(BioBLMhc$probne0>0.5)-1]) #Select lidar metric data

corrgram(MedModVar, order=T, lower.panel=panel.ellipse,
         upper.panel=panel.cor, text.panel=panel.txt,
         main="Lidar Predictor Data in PC2/PC1 Order")

#Fairly strong correlation between a number of pairs, some odd relationships!

### Add in additional variables
# VarNames <- c(PredNames, FieldNames[c(9,7,20)])
# 
# PRED.test2 <- select(DATA.test, VarNames) #Select lidar metric data
# PRED.test2 <- PRED.test2[,-50]
# PRED.test2$Forest <- as.factor(PRED.test2$Forest)
# PRED.test2$ForestType <- as.factor(PRED.test2$ForestType)
# 
# BMod2 <- cbind(STBIOMS.test, PRED.test2)
# BMod2c <- cbind(STBIOMS.test, center(PRED.test2[,c(-51,-52)]), PRED.test2[,51], PRED.test2[,52]) 
# 
# 
# BioBLM2hc <- bas.lm(log(STBIOMS.test+.00001)~ ., 
#                    data=BMod2c, 
#                    prior="hyper-g",
#                    alpha = 3,
#                    modelprior=tr.poisson(9,30))
# 
# summary(BioBLMhc)
# PredNames[which(BioBLMhc$probne0>0.5)-1]

### Generate Predictions

### Validate Predictions against Validation set
