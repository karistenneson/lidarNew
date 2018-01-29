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
#options(survey.lonely.psu="certainty")
options(survey.lonely.psu="average")
library(tidyverse)
library(dplyr)
#library(robustbase)
library(olsrr)
library(car)

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
# ? add P05? -- same with or without.
# swap L2 for SD?

# log log
BioMass.Mod.log.log <- bas.lm(logSTBIOMSha ~ log(Elev_mode) + log(Elev_P01) + log(Elev_P10) + log(Elev_P30) + log(Elev_P60) + log(Elev_P90) +  Elev_stddev +  Elev_kurtosis + Elev_skewness + Elev_MAD_median + Elev_MAD_mode + Elev_L3 + Elev_L4 + Elev_LCV + Elev_Lskewness + Elev_Lkurtosis + Pct_all_returns_above_ht + all_returns_above_ht_div_Total_first_returns_x_100 + pct_all_returns_above_mean + All_returns_above_mode_div_Total_first_returns_x_100 + mode_pctAllOver3m + P01_pctAllOver3m + P10_pctAllOver3m + P30_pctAllOver3m + P60_pctAllOver3m + P90_pctAllOver3m + elevation + aspect + slope + NDVI_Amp + R3ERUlabel, data = data.mod, prior="hyper-g", alpha = 3, modelprior=tr.poisson(10,30), method="MCMC+BAS")

# log
# BioMass.Mod.log <- bas.lm(logSTBIOMSha ~ poly(Elev_mode, 2) + poly(Elev_P01, 2) + poly(Elev_P10, 2) + poly(Elev_P30, 2) + poly(Elev_P60, 2) + poly(Elev_P90, 2) +  Elev_stddev +  Elev_kurtosis + Elev_skewness + Elev_MAD_median + Elev_MAD_mode + Elev_L3 + Elev_L4 + Elev_LCV + Elev_Lskewness + Elev_Lkurtosis + Pct_all_returns_above_ht + all_returns_above_ht_div_Total_first_returns_x_100 + pct_all_returns_above_mean + All_returns_above_mode_div_Total_first_returns_x_100 + mode_pctAllOver3m + P01_pctAllOver3m + P10_pctAllOver3m + P30_pctAllOver3m + P60_pctAllOver3m + P90_pctAllOver3m + elevation + aspect + slope + NDVI_Amp + R3ERUlabel, data = data.mod, prior="hyper-g", alpha = 3, modelprior=tr.poisson(10,30), method="MCMC+BAS")

# no transform
# BioMass.Mod <- bas.lm(STBIOMSha ~ poly(Elev_mode, 2) + poly(Elev_P01, 2) + poly(Elev_P10, 2) + poly(Elev_P30, 2) + poly(Elev_P60, 2) + poly(Elev_P90, 2) +   Elev_stddev +  Elev_skewness +Elev_kurtosis + Elev_MAD_median + Elev_MAD_mode + Elev_L3 + Elev_L4 + Elev_LCV + Elev_Lskewness + Elev_Lkurtosis + Pct_all_returns_above_ht + all_returns_above_ht_div_Total_first_returns_x_100 + pct_all_returns_above_mean + All_returns_above_mode_div_Total_first_returns_x_100 + mode_pctAllOver3m + P01_pctAllOver3m + P10_pctAllOver3m + P30_pctAllOver3m + P60_pctAllOver3m + P90_pctAllOver3m + elevation + aspect + slope + NDVI_Amp + R3ERUlabel,  data = data.mod, prior="hyper-g", alpha = 3, modelprior=tr.poisson(10,30), method="MCMC+BAS")

# correct specification of interaction is colinear with correct specification of quadratic; so that BMA model set up can't be solved. So I took it out - Elev_mode*Pct_all_returns_above_ht + Elev_P01*Pct_all_returns_above_ht+ Elev_P10*Pct_all_returns_above_ht+ Elev_P30*Pct_all_returns_above_ht+ Elev_P60*Pct_all_returns_above_ht+ Elev_P90*Pct_all_returns_above_ht + 

#################################################################### 
## Median Models:
BioMass.Mod.log.log$namesx[which(BioMass.Mod.log.log$probne0>0.5)][-1]
BioMass.Mod.log$namesx[which(BioMass.Mod.log$probne0>0.5)][-1]
BioMass.Mod$namesx[which(BioMass.Mod$probne0>0.5)][-1]

# Highest Probability Model
HPM <- predict(BioMass.Mod.log.log, estimator="HPM")
BioMass.Mod.log.log$namesx[HPM$bestmodel+1][-1]

HPM <- predict(BioMass.Mod.log, estimator="HPM")
BioMass.Mod.log$namesx[HPM$bestmodel+1][-1]

HPM <- predict(BioMass.Mod, estimator="HPM")
BioMass.Mod$namesx[HPM$bestmodel+1][-1]

#################################################################### 
## step test comparison
#StepModel <- lm(STBIOMSha ~ poly(Elev_mode, 2) + poly(Elev_P01, 2) + poly(Elev_P10, 2) + poly(Elev_P30, 2) + poly(Elev_P60, 2) + poly(Elev_P90, 2) +   Elev_stddev +  Elev_skewness +Elev_kurtosis + Elev_MAD_median + Elev_MAD_mode + Elev_L3 + Elev_L4 + Elev_LCV + Elev_Lskewness + Elev_Lkurtosis + Pct_all_returns_above_ht + all_returns_above_ht_div_Total_first_returns_x_100 + pct_all_returns_above_mean + All_returns_above_mode_div_Total_first_returns_x_100 + mode_pctAllOver3m + mode_pctAlloverModeDiv1st + P01_pctAllOver3m + P10_pctAllOver3m + Elev_P30:Pct_all_returns_above_ht + P60_pctAllOver3m + P90_pctAllOver3m + elevation + aspect + slope + NDVI_Amp + R3ERUlabel,  data = data.mod)

#null <- lm(STBIOMSha ~ 1, data = data.mod)
#step(null, scope = list(upper = StepModel), direction ='both')

####################################################
####################################################
HPMBASModel_log_log <- lm(logSTBIOMSha ~ log(Elev_P30)+
                           log(Elev_P60)+
                           log(Elev_P90)+
                           Elev_MAD_median+
                           Pct_all_returns_above_ht+
                           Pct_all_returns_above_ht*log(Elev_P60) + #P60_pctAllOver3m+
                            Pct_all_returns_above_ht*log(Elev_P90) + #P90_pctAllOver3m+
                           elevation+
                           slope,  
                         data = data.mod)

outlierTest(HPMBASModel_log_log)
qqPlot(HPMBASModel_log_log, main="QQ Plot")
leveragePlots(HPMBASModel_log_log)
sqrt(vif(HPMBASModel_log_log)) >2 # problem
test<-summary(HPMBASModel_log_log)
PRSE<-100*(test$coefficients[,2]/test$coefficients[,1])

####################################################################
MPMBASModel_log_log <- lm(logSTBIOMSha ~ log(Elev_P30)+
                            log(Elev_P60)+
                            log(Elev_P90)+
                            Elev_MAD_median+
                            Pct_all_returns_above_ht+
                            Pct_all_returns_above_ht*log(Elev_P90) + #P90_pctAllOver3m+
                            elevation,  
                          data = data.mod)

outlierTest(MPMBASModel_log_log)
qqPlot(MPMBASModel_log_log, main="QQ Plot")
leveragePlots(MPMBASModel_log_log)
vif(MPMBASModel_log_log) # problem
test<-summary(MPMBASModel_log_log)
PRSE<-100*(test$coefficients[,2]/test$coefficients[,1])

####################################################################
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

##
#MedianBASModel <- lm(STBIOMSha ~ poly(Elev_P60,2) + Elev_MAD_median + all_returns_above_ht_div_Total_first_returns_x_100 + P30_pctAllOver3m + P60_pctAllOver3m, data = data.mod)

MedianBASModel <- lm(STBIOMSha ~ poly(Elev_P60,2) +
                       Elev_MAD_median +
                       all_returns_above_ht_div_Total_first_returns_x_100 +
                       Elev_P30*Pct_all_returns_above_ht +
                       P60_pctAllOver3m,  
                     data = data.mod)

outlierTest(MedianBASModel)
qqPlot(MedianBASModel, main="QQ Plot")
leveragePlots(MedianBASModel)
av.Plots(MedianBASModel)
sqrt(vif(MedianBASModel)) >2 # problem
test<-summary(MedianBASModel)
PRSE<-100*(test$coefficients[,2]/test$coefficients[,1])

MedianBASModel_nop60 <- lm(STBIOMSha ~ poly(Elev_P60,2) +
                       Elev_MAD_median +
                       all_returns_above_ht_div_Total_first_returns_x_100 +
                       Elev_P30*Pct_all_returns_above_ht,  
                     data = data.mod)

test<-summary(MedianBASModel_nop60)
sqrt(vif(MedianBASModel_nop60)) >2 # problem

PRSE<-100*(test$coefficients[,2]/test$coefficients[,1])

##########
MedianBASModel_nop60nofirst <- lm(STBIOMSha ~ poly(Elev_P60,2) +
                             Elev_MAD_median +
                             Elev_P30*Pct_all_returns_above_ht,  
                           data = data.mod)

test<-summary(MedianBASModel_nop60nofirst)
sqrt(vif(MedianBASModel_nop60nofirst)) >2 # problem

PRSE<-100*(test$coefficients[,2]/test$coefficients[,1])


outlierTest(MedianBASModel_nop60nofirst)
qqPlot(MedianBASModel_nop60nofirst, main="QQ Plot")
leveragePlots(MedianBASModel_nop60nofirst)
av.Plots(MedianBASModel_nop60nofirst)
sqrt(vif(MedianBASModel_nop60nofirst)) >2 # problem
test<-summary(MedianBASModel_nop60nofirst)
PRSE<-100*(test$coefficients[,2]/test$coefficients[,1])

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

data.mod$lnBMAResidual <- data.mod$STBIOMSha-data.mod$lnBMAEstimates 
data.val$lnBMAResidual <- data.val$STBIOMSha-data.val$lnBMAEstimates 
data.val.ind$lnBMAResidual <- data.val.ind$STBIOMSha-data.val.ind$lnBMAEstimates 

data.mod$lnBMASqResidual <- (data.mod$lnBMAResidual)^2
data.val$lnBMASqResidual <- (data.val$lnBMAResidual)^2
data.val.ind$lnBMASqResidual <- (data.val.ind$lnBMAResidual)^2

############################
## BMA no log
data.mod$BMAEstimates <- predict(BioMass.Mod, top='10000')$fit
data.val$BMAEstimates <- predict(BioMass.Mod, newdata = data.val, top='10000')$fit
data.val.ind$BMAEstimates <- predict(BioMass.Mod, newdata = data.val.ind, top='10000')$fit

data.mod$BMAResidual <- data.mod$STBIOMSha - data.mod$BMAEstimates 
data.val$BMAResidual <- data.val$STBIOMSha - data.val$BMAEstimates 
data.val.ind$BMAResidual <- data.val.ind$STBIOMSha - data.val.ind$BMAEstimates 

data.mod$BMASqResidual <- (data.mod$BMAResidual)^2
data.val$BMASqResidual <- (data.val$BMAResidual)^2
data.val.ind$BMASqResidual <- (data.val.ind$BMAResidual)^2

############################
## MPM with log transform
data.mod$lnMPMEstimates <- exp(predict(MedianBASModel_log, se.fit = F))
data.val$lnMPMEstimates <- exp(predict(MedianBASModel_log, newdata = data.val, se.fit = F))
data.val.ind$lnMPMEstimates <- exp(predict(MedianBASModel_log, newdata = data.val.ind, se.fit = F))

data.mod$lnMPMResidual <- data.mod$STBIOMSha - data.mod$MPMEstimates 
data.val$lnMPMResidual <- data.val$STBIOMSha - data.val$MPMEstimates 
data.val.ind$lnMPMResidual <- data.val.ind$STBIOMSha - data.val.ind$MPMEstimates 

data.mod$lnMPMSqResidual <- (data.mod$MPMResidual)^2
data.val$lnMPMSqResidual <- (data.val$MPMResidual)^2
data.val.ind$lnMPMSqResidual <- (data.val.ind$MPMResidual)^2

############################
data.mod$MPMEstimates <- predict(MedianBASModel_nop60nofirst, se.fit = F)
data.val$MPMEstimates <- predict(MedianBASModel_nop60nofirst, newdata = data.val, se.fit = F)
data.val.ind$MPMEstimates <- predict(MedianBASModel_nop60nofirst, newdata = data.val.ind, se.fit = F)

data.mod$MPMResidual <- data.mod$STBIOMSha - data.mod$MPMEstimates 
data.val$MPMResidual <- data.val$STBIOMSha - data.val$MPMEstimates 
data.val.ind$MPMResidual <- data.val.ind$STBIOMSha - data.val.ind$MPMEstimates 

data.mod$MPMSqResidual <- (data.mod$MPMResidual)^2
data.val$MPMSqResidual <- (data.val$MPMResidual)^2
data.val.ind$MPMSqResidual <- (data.val.ind$MPMResidual)^2

## MPM with no transform
data.mod$MPMOLDEstimates <- predict(MedianBASModel, se.fit = F)
data.val$MPMOLDEstimates <- predict(MedianBASModel, newdata = data.val, se.fit = F)
data.val.ind$MPMOLDEstimates <- predict(MedianBASModel, newdata = data.val.ind, se.fit = F)

data.mod$MPMOLDResidual <- data.mod$STBIOMSha - data.mod$MPMEOLDstimates 
data.val$MPMOLDResidual <- data.val$STBIOMSha - data.val$MPMOLDEstimates 
data.val.ind$MPMOLDResidual <- data.val.ind$STBIOMSha - data.val.ind$MPMOLDEstimates 

data.mod$MPMOLDSqResidual <- (data.mod$MPMOLDResidual)^2
data.val$MPMOLDSqResidual <- (data.val$MPMOLDResidual)^2
data.val.ind$MPMOLDSqResidual <- (data.val.ind$MPMOLDResidual)^2


## MPM with log, log transform
data.mod$MPMlnlnEstimates <- exp(predict(MPMBASModel_log_log, se.fit = F))
data.val$MPMlnlnEstimates <- exp(predict(MPMBASModel_log_log, newdata = data.val, se.fit = F))
data.val.ind$MPMlnlnEstimates <- exp(predict(MPMBASModel_log_log, newdata = data.val.ind, se.fit = F))

data.mod$MPMlnlnResidual <- data.mod$STBIOMSha - data.mod$MPMlnlnEstimates 
data.val$MPMlnlnResidual <- data.val$STBIOMSha - data.val$MPMlnlnEstimates 
data.val.ind$MPMlnlnResidual <- data.val.ind$STBIOMSha - data.val.ind$MPMlnlnEstimates 

data.mod$MPMlnlnSqResidual <- (data.mod$MPMlnlnResidual)^2
data.val$MPMlnlnSqResidual <- (data.val$MPMlnlnResidual)^2
data.val.ind$MPMlnlnSqResidual <- (data.val.ind$MPMlnlnResidual)^2
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
## Table_7_RMSE_Bias.R
########################################################
########################################################
