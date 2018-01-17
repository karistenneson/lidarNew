### This file is for running the BMA components of the analysis
## Written by MS Patterson (maspatte@uw.edu) and Karis Tenneson (krtenneson@fs.fed.us)
# Last updated: Jan 17, 2018

### set working directory
#setwd('C:\\Users\\krtenneson\\Desktop\\lidarPaper\\lidarNew\\scripts')
#setwd('\\\\166.2.126.25\\rseat\\Programs\\Reimbursibles\\fy2016\\R3_lidar_equation_transferability\\Analysis\\VersionControl\\lidarNew\\scripts')
#setwd("~/Documents/R/lidarNew/scripts") #Mac
#setwd("~/R/lidarNew/scripts") #WinCampus

### Load required packages
library(cvTools)
library(BAS)
library(corrgram)
library(survey)
options(survey.lonely.psu="certainty")
options(survey.lonely.psu="adjust")
library(tidyverse)
library(dplyr)
#library(robustbase)

### data files with plots that have no biomass
#data.mod.full <- read.csv('Data//datamod.csv')
#data.val.full <- read.csv('Data//dataval.csv')
#data.val.ind.full <- read.csv('Data//datavalind.csv')

### load data
data.mod <- read.csv('Data//datamodNoZero.csv')
data.val <- read.csv('Data//datavalNoZero.csv')
data.val.ind <- read.csv('Data//datavalindNoZero.csv')

### remove the plots with huge kurtosis values (impacted by bird/cloud hits)
#hist(data.mod$Elev_kurtosis)
data.mod <- data.mod[(data.mod$Elev_kurtosis < 50), ]
data.val <- data.val[(data.val$Elev_kurtosis < 50), ]
data.val.ind <- data.val.ind[(data.val.ind$Elev_kurtosis < 50), ]

######################################################
# log
#BioMass.Mod.log <- bas.lm(logSTBIOMSha ~ poly(Elev_mode, 2) + poly(Elev_P01, 2) + poly(Elev_P10, 2) + poly(Elev_P30, 2) + poly(Elev_P60, 2) + poly(Elev_P90, 2) +  Elev_stddev +  Elev_kurtosis + Elev_MAD_median + Elev_MAD_mode + Elev_L3 + Elev_L4 + Elev_LCV + Elev_Lskewness + Elev_Lkurtosis + Pct_all_returns_above_ht + all_returns_above_ht_div_Total_first_returns_x_100 + pct_all_returns_above_mean + All_returns_above_mode_div_Total_first_returns_x_100 + mode_pctAllOver3m + mode_pctAlloverModeDiv1st + P01_pctAllOver3m + P10_pctAllOver3m + P30_pctAllOver3m + P60_pctAllOver3m + P90_pctAllOver3m + elevation + aspect + slope + NDVI_Amp + R3ERUlabel, data = data.mod, prior="hyper-g", alpha = 3, modelprior=tr.poisson(10,30), method="MCMC+BAS")

# no transform
#BioMass.Mod <- bas.lm(STBIOMSha ~ poly(Elev_mode, 2) + poly(Elev_P01, 2) + poly(Elev_P10, 2) + poly(Elev_P30, 2) + poly(Elev_P60, 2) + poly(Elev_P90, 2) +   Elev_stddev +  Elev_kurtosis + Elev_MAD_median + Elev_MAD_mode + Elev_L3 + Elev_L4 + Elev_LCV + Elev_Lskewness + Elev_Lkurtosis + Pct_all_returns_above_ht + all_returns_above_ht_div_Total_first_returns_x_100 + pct_all_returns_above_mean + All_returns_above_mode_div_Total_first_returns_x_100 + mode_pctAllOver3m + mode_pctAlloverModeDiv1st + P01_pctAllOver3m + P10_pctAllOver3m + P30_pctAllOver3m + P60_pctAllOver3m + P90_pctAllOver3m + elevation + aspect + slope + NDVI_Amp + R3ERUlabel,  data = data.mod, prior="hyper-g", alpha = 3, modelprior=tr.poisson(10,30), method="MCMC+BAS")

#################################################################### 
## Median Models:
BioMass.Mod.log$namesx[which(BioMass.Mod.log$probne0>0.5)][-1]
BioMass.Mod$namesx[which(BioMass.Mod$probne0>0.5)][-1]

# Highest Probability Model
HPM <- predict(BioMass.Mod.log, estimator="HPM")
BioMass.Mod.log$namesx[HPM$bestmodel+1][-1]

HPM <- predict(BioMass.Mod, estimator="HPM")
BioMass.Mod$namesx[HPM$bestmodel+1][-1]

####################################################
####################################################
MedianBASModel_log <- lm(logSTBIOMSha ~ Elev_P30+
                         poly(Elev_P60,2)+
                         Elev_P90+
                         Elev_MAD_median+
                         Pct_all_returns_above_ht+
                         P90_pctAllOver3m+
                         elevation+
                         slope+
                         NDVI_Amp,  
                             data = data.mod)

MedianBASModel <- lm(STBIOMSha ~ poly(Elev_P60,2) +
                     Elev_MAD_median +
                     all_returns_above_ht_div_Total_first_returns_x_100 +
                     P30_pctAllOver3m +
                     P60_pctAllOver3m,  
                         data = data.mod)

#################################
## summary stats
summary(MedianBASModel_log)
AIC(MedianBASModel_log)

summary(MedianBASModel)
AIC(MedianBASModel)

########################################################
########################################################
## predict data
########################################################
########################################################
## BMA log
data.mod$lnBMAEstimates <- exp(predict(object = BioMass.Mod.log, top='10000')$fit)
data.val$lnBMAEstimates <- exp(predict(object = BioMass.Mod.log, newdata = data.val, top='10000')$fit)
data.val.ind$lnBMAEstimates <- exp(predict(object = BioMass.Mod.log, newdata = data.val.ind, top='10000')$fit)

data.mod$lnBMAResidual <- data.mod$lnBMAEstimates - data.mod$STBIOMSha
data.val$lnBMAResidual <- data.val$lnBMAEstimates - data.val$STBIOMSha
data.val.ind$lnBMAResidual <- data.val.ind$lnBMAEstimates - data.val.ind$STBIOMSha

data.mod$lnBMASqResidual <- (data.mod$lnBMAResidual)^2
data.val$lnBMASqResidual <- (data.val$lnBMAResidual)^2
data.val.ind$lnBMASqResidual <- (data.val.ind$lnBMAResidual)^2

############################
## BMA no log
data.mod$BMAEstimates <- predict(BioMass.Mod, top='10000')$fit
data.val$BMAEstimates <- predict(BioMass.Mod, newdata = data.val, top='10000')$fit
data.val.ind$BMAEstimates <- predict(BioMass.Mod, newdata = data.val.ind, top='10000')$fit

data.mod$BMAResidual <- data.mod$BMAEstimates - data.mod$STBIOMSha
data.val$BMAResidual <- data.val$BMAEstimates - data.val$STBIOMSha
data.val.ind$BMAResidual <- data.val.ind$BMAEstimates - data.val.ind$STBIOMSha

data.mod$BMASqResidual <- (data.mod$BMAResidual)^2
data.val$BMASqResidual <- (data.val$BMAResidual)^2
data.val.ind$BMASqResidual <- (data.val.ind$BMAResidual)^2

############################
## MPM with no transform
data.mod$MPMEstimates <- predict(MedianBASModel, se.fit = F)
data.val$MPMEstimates <- predict(MedianBASModel, newdata = data.val, se.fit = F)
data.val.ind$MPMEstimates <- predict(MedianBASModel, newdata = data.val.ind, se.fit = F)

data.mod$MPMResidual <- data.mod$MPMEstimates - data.mod$STBIOMSha
data.val$MPMResidual <- data.val$MPMEstimates - data.val$STBIOMSha
data.val.ind$MPMResidual <- data.val.ind$MPMEstimates - data.val.ind$STBIOMSha

data.mod$MPMSqResidual <- (data.mod$MPMResidual)^2
data.val$MPMSqResidual <- (data.val$MPMResidual)^2
data.val.ind$MPMSqResidual <- (data.val.ind$MPMResidual)^2

########################################################
########################################################
## export data and clear memory
########################################################
########################################################

#write.csv(data.mod, 'Data\\datamodNoZeroModelFits.csv', row.names = F)
#write.csv(data.val, 'Data\\datavalNoZeroModelFits.csv', row.names = F)
#write.csv(data.val.ind, 'Data\\datavalindNoZeroModelFits.csv', row.names = F)

########################################################
########################################################
## run performance calculations with this script:
## 04_ModelAssessment.R
########################################################
########################################################