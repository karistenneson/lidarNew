### This file is for assessing model performance
## Written by MS Patterson (maspatte@uw.edu) and Karis Tenneson (krtenneson@fs.fed.us)
# Last updated: Jan 17, 2018

### set working directory
#setwd('C:\\Users\\krtenneson\\Desktop\\lidarPaper\\lidarNew\\scripts')
#setwd('\\\\166.2.126.25\\rseat\\Programs\\Reimbursibles\\fy2016\\R3_lidar_equation_transferability\\Analysis\\VersionControl\\lidarNew\\scripts')
#setwd("~/Documents/R/lidarNew/scripts") #Mac
#setwd("~/R/lidarNew/scripts") #WinCampus
#source(file='02_DataPrep.R')

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

## load data
data.mod <- read.csv('Data\\datamodNoZeroModelFits.csv')
data.val <- read.csv('Data\\datavalNoZeroModelFits.csv')
data.val.ind <- read.csv('Data\\datavalindNoZeroModelFits.csv')

########################################################
########################################################
## run calculations on MPM
########################################################
########################################################
## model data

data.svy.mod.s <- svydesign(ids = ~1, data = data.mod)
data.svy.mod.p <- svydesign(ids = ~1, data = data.mod, fpc = data.mod$fpc, strata = data.mod$Stratum)

sites <- c('Kaibab Plateau, AZ', 'Coconino NF, 4FRI, AZ', 'Tonto NF, 4FRI, AZ', 'Apache-Sitgreaves NF, 4FRI, AZ, Phase 1', 'Southwest Jemez Mountains, NM')
MCD <- 'Model Construction Data'

# All sites
MCD2 <- c(MCD, '&', signif(sqrt(svymean(~MPMSqResidual, data.svy.mod.p))[1],4), #RMSE
          '&', signif(100*sqrt(svymean(~MPMSqResidual, data.svy.mod.p))/(svymean(~STBIOMSha, data.svy.mod.p))[1], 4), #RMSE
          '&', signif(svymean(~MPMResidual, data.svy.mod.p)[1], 4), #Bias
          '&', signif(svymean(~MPMResidual, data.svy.mod.p)[1]/(svymean(~STBIOMSha, data.svy.mod.p))[1],4), #%Bias
          '&', signif(sqrt(svymean(~MPMSqResidual, data.svy.mod.s))[1],4), #RMSE
          '&', signif(100*sqrt(svymean(~MPMSqResidual, data.svy.mod.s))/(svymean(~STBIOMSha, data.svy.mod.s))[1], 4), #RMSE
          '&', signif(svymean(~MPMResidual, data.svy.mod.s)[1], 4), #Bias
          '&', signif(svymean(~MPMResidual, data.svy.mod.s)[1]/(svymean(~STBIOMSha, data.svy.mod.s))[1],4) #%Bias
)

# Individual sites
frame <- cbind(sites, 
#RMSE
rep('&',length(sites)), signif(sqrt(svyby(~MPMSqResidual, ~Forest, data.svy.mod.p, svymean)[,2]),4), 
#%RMSE
rep('&',length(sites)), signif(signif(100 * (sqrt(svyby(~MPMSqResidual, ~Forest, data.svy.mod.p, svymean)[,2]) / svymean(~STBIOMSha, data.svy.mod.p)[1]), 4), 4), 
#Bias
rep('&',length(sites)), signif(svyby(~MPMResidual, ~Forest, data.svy.mod.p, svymean)[2],2), 
#%Bias
rep('&',length(sites)), signif(100*svyby(~MPMResidual, ~Forest, data.svy.mod.p, svymean)[2]/svymean(~STBIOMSha, data.svy.mod.p)[1], 4),
### pop vs sample splitter column
rep('& ',length(sites)),
#RMSE
rep('&',length(sites)), signif(sqrt(svyby(~MPMSqResidual, ~Forest, data.svy.mod.s, svymean)[,2]),4), 
#%RMSE
rep('&',length(sites)), signif(signif(100 * (sqrt(svyby(~MPMSqResidual, ~Forest, data.svy.mod.s, svymean)[,2]) / svymean(~STBIOMSha, data.svy.mod.s)[1]), 4), 4), 
#Bias
rep('&',length(sites)), signif(svyby(~MPMResidual, ~Forest, data.svy.mod.s, svymean)[2],2), 
#%Bias
rep('&',length(sites)), signif(100*svyby(~MPMResidual, ~Forest, data.svy.mod.s, svymean)[2]/svymean(~STBIOMSha, data.svy.mod.s)[1], 4), rep('\\\\', length(sites)))
colnames(frame)<-c('site', 'div', 'rmsepop', 'div','Pctrmsepop','div', 'biaspop','div', 'Pctbiaspop','div','rmsesmp','div', 'Pctrmsesmp', 'div','biassmp','div', 'Pctbiassmp', 'rowEnd')
write.csv(frame, file = 'Manuscript_tables\\modelErrors.csv', row.names = F)

########################################################
##############################################################################################
########################################################
## validation data
data.svy.val.s <- svydesign(ids = ~1, data = data.val)
data.svy.val.p <- svydesign(ids = ~1, data = data.val, fpc = data.val$fpc, strata = data.val$Stratum)

sites <- c('Kaibab Plateau, AZ', 'Coconino NF, 4FRI, AZ', 'Tonto NF, 4FRI, AZ', 'Apache-Sitgreaves NF, 4FRI, AZ, Phase 1', 'Southwest Jemez Mountains, NM')
MVD <- 'Model Development Validation Data'

# All sites
MVD2 <- c(MVD, '&', '-', #RMSE
          '&', '-', #RMSE
          '&', '-', #Bias
          '&', '-', #%Bias
          '&_','&', signif(sqrt(svymean(~MPMSqResidual, data.svy.val.s))[1],4), #RMSE
          '&', signif(100*sqrt(svymean(~MPMSqResidual, data.svy.val.s))/(svymean(~STBIOMSha, data.svy.val.s))[1], 4), #RMSE
          '&', signif(svymean(~MPMResidual, data.svy.val.s)[1], 4), #Bias
          '&', signif(svymean(~MPMResidual, data.svy.val.s)[1]/(svymean(~STBIOMSha, data.svy.val.s))[1],4), '\\\\' #%Bias
)

MVD2
write.csv(MVD2, file = 'Manuscript_tables\\modelErrorsvalPopAll.csv', row.names = F)
# Individual sites
frame <- cbind(sites, 
               #RMSE
               rep('&',length(sites)), signif(sqrt(svyby(~MPMSqResidual, ~Forest, data.svy.val.p, svymean)[,2]),4), 
               #%RMSE
               rep('&',length(sites)), signif(signif(100 * (sqrt(svyby(~MPMSqResidual, ~Forest, data.svy.val.p, svymean)[,2]) / svymean(~STBIOMSha, data.svy.val.p)[1]), 4), 4), 
               #Bias
               rep('&',length(sites)), signif(svyby(~MPMResidual, ~Forest, data.svy.val.p, svymean)[2],2), 
               #%Bias
               rep('&',length(sites)), signif(100*svyby(~MPMResidual, ~Forest, data.svy.val.p, svymean)[2]/svymean(~STBIOMSha, data.svy.val.p)[1], 4),
               ### pop vs sample splitter column
               rep('& ',length(sites)),
               #RMSE
               rep('&',length(sites)), signif(sqrt(svyby(~MPMSqResidual, ~Forest, data.svy.val.s, svymean)[,2]),4), 
               #%RMSE
               rep('&',length(sites)), signif(signif(100 * (sqrt(svyby(~MPMSqResidual, ~Forest, data.svy.val.s, svymean)[,2]) / svymean(~STBIOMSha, data.svy.val.s)[1]), 4), 4), 
               #Bias
               rep('&',length(sites)), signif(svyby(~MPMResidual, ~Forest, data.svy.val.s, svymean)[2],2), 
               #%Bias
               rep('&',length(sites)), signif(100*svyby(~MPMResidual, ~Forest, data.svy.val.s, svymean)[2]/svymean(~STBIOMSha, data.svy.val.s)[1], 4), rep('\\\\', length(sites)))
colnames(frame)<-c('site', 'div', 'rmsepop', 'div','Pctrmsepop','div', 'biaspop','div', 'Pctbiaspop','div','rmsesmp','div', 'Pctrmsesmp', 'div','biassmp','div', 'Pctbiassmp', 'rowEnd')
frame
write.csv(frame, file = 'Manuscript_tables\\modelErrorsval.csv', row.names = F)

########################################################
##############################################################################################
########################################################
## Transferability validation data
data.svy.val.ind.s <- svydesign(ids = ~1, data = data.val.ind)
data.svy.val.ind.p <- svydesign(ids = ~1, data = data.val.ind, fpc = data.val.ind$fpc, strata = data.val.ind$Stratum)

sites <- c('Apache-Sitgreaves NF, 4FRI, AZ, Phase 2', 'Apache-Sitgreaves NF, 4FRI, AZ, Phase 3')

# Individual sites
frame <- cbind(sites, 
               #RMSE
               rep('&',length(sites)), signif(sqrt(svyby(~MPMSqResidual, ~Forest, data.svy.val.ind.p, svymean)[,2]),4), 
               #%RMSE
               rep('&',length(sites)), signif(signif(100 * (sqrt(svyby(~MPMSqResidual, ~Forest, data.svy.val.ind.p, svymean)[,2]) / svymean(~STBIOMSha, data.svy.val.ind.p)[1]), 4), 4), 
               #Bias
               rep('&',length(sites)), signif(svyby(~MPMResidual, ~Forest, data.svy.val.ind.p, svymean)[2],2), 
               #%Bias
               rep('&',length(sites)), signif(100*svyby(~MPMResidual, ~Forest, data.svy.val.ind.p, svymean)[2]/svymean(~STBIOMSha, data.svy.val.ind.p)[1], 4),
               ### pop vs sample splitter column
               rep('& ',length(sites)),
               #RMSE
               rep('&',length(sites)), signif(sqrt(svyby(~MPMSqResidual, ~Forest, data.svy.val.ind.s, svymean)[,2]),4), 
               #%RMSE
               rep('&',length(sites)), signif(signif(100 * (sqrt(svyby(~MPMSqResidual, ~Forest, data.svy.val.ind.s, svymean)[,2]) / svymean(~STBIOMSha, data.svy.val.ind.s)[1]), 4), 4), 
               #Bias
               rep('&',length(sites)), signif(svyby(~MPMResidual, ~Forest, data.svy.val.ind.s, svymean)[2],2), 
               #%Bias
               rep('&',length(sites)), signif(100*svyby(~MPMResidual, ~Forest, data.svy.val.ind.s, svymean)[2]/svymean(~STBIOMSha, data.svy.val.ind.s)[1], 4), rep('\\\\', length(sites)))
colnames(frame)<-c('site', 'div', 'rmsepop', 'div','Pctrmsepop','div', 'biaspop','div', 'Pctbiaspop','div','rmsesmp','div', 'Pctrmsesmp', 'div','biassmp','div', 'Pctbiassmp', 'rowEnd')
frame
write.csv(frame, file = 'Manuscript_tables\\modelErrorsvalind.csv', row.names = F)

########################################################
########################################################
## run calculations on BMA object
########################################################
########################################################
## model data
data.svy.mod <- svydesign(ids = ~1, data = data.mod)
data.svy.mod <- svydesign(ids = ~1, data = data.mod, fpc = data.mod$fpc, strata = data.mod$Stratum)

## RMSE
sqrt(svymean(~BMASqResidual, data.svy.mod))
sqrt(svyby(~BMASqResidual, ~Forest, data.svy.mod, svymean))
## % RMSE
sqrt(svymean(~BMASqResidual, data.svy.mod))/(svymean(~STBIOMSha, data.svy.mod))
sqrt(svyby(~BMASqResidual, ~Forest, data.svy.mod, svymean))/(svymean(~STBIOMSha, data.svy.mod))

## Bias
svymean(~BMAResidual, data.svy.mod)
svyby(~BMAResidual, ~Forest, data.svy.mod, svymean)
## % Bias
svymean(~BMAResidual, data.svy.mod)/(svymean(~STBIOMSha, data.svy.mod))
svyby(~BMAResidual, ~Forest, data.svy.mod, svymean)/(svymean(~STBIOMSha, data.svy.mod))



########################################################
## validation data
data.svy.val <- svydesign(ids = ~1, data = data.val)
data.svy.val <- svydesign(ids = ~1, data = data.val, fpc = data.val$fpc, strata = data.val$Stratum)

## Bias
svymean(~BMAResidual, data.svy.val)
svyby(~BMAResidual, ~Forest, data.svy.val, svymean)
## % Bias
svymean(~BMAResidual, data.svy.val)/(svymean(~STBIOMSha, data.svy.val))
svyby(~BMAResidual, ~Forest, data.svy.val, svymean)/(svymean(~STBIOMSha, data.svy.val))

## RMSE
sqrt(svymean(~BMASqResidual, data.svy.val))
sqrt(svyby(~BMASqResidual, ~Forest, data.svy.val, svymean))
## % RMSE
sqrt(svymean(~BMASqResidual, data.svy.val))/(svymean(~STBIOMSha, data.svy.val))
sqrt(svyby(~BMASqResidual, ~Forest, data.svy.val, svymean))/(svymean(~STBIOMSha, data.svy.val))

########################################################
## Transferability validation data
data.svy.val.ind <- svydesign(ids = ~1, data = data.val.ind)
data.svy.val.ind <- svydesign(ids = ~1, data = data.val.ind, fpc = data.val.ind$fpc, strata = data.val.ind$Stratum)

## Bias
svymean(~BMAResidual, data.svy.val.ind)
svyby(~BMAResidual, ~Forest, data.svy.val.ind, svymean)
## % Bias
svymean(~BMAResidual, data.svy.val.ind)/(svymean(~STBIOMSha, data.svy.val.ind))
svyby(~BMAResidual, ~Forest, data.svy.val.ind, svymean)/(svymean(~STBIOMSha, data.svy.val.ind))

## RMSE
sqrt(svymean(~BMASqResidual, data.svy.val.ind))
sqrt(svyby(~BMASqResidual, ~Forest, data.svy.val.ind, svymean))
## % RMSE
sqrt(svymean(~BMASqResidual, data.svy.val.ind))/(svymean(~STBIOMSha, data.svy.val.ind))
sqrt(svyby(~BMASqResidual, ~Forest, data.svy.val.ind, svymean))/(svymean(~STBIOMSha, data.svy.val.ind))

########################################################
########################################################
## run calculations on lnBMA object
########################################################
########################################################
## model data
data.svy.mod <- svydesign(ids = ~1, data = data.mod)
data.svy.mod <- svydesign(ids = ~1, data = data.mod, fpc = data.mod$fpc, strata = data.mod$Stratum)

## Bias
svymean(~lnBMAResidual, data.svy.mod)
svyby(~lnBMAResidual, ~Forest, data.svy.mod, svymean)
## % Bias
svymean(~lnBMAResidual, data.svy.mod)/(svymean(~STBIOMSha, data.svy.mod))
svyby(~lnBMAResidual, ~Forest, data.svy.mod, svymean)/(svymean(~STBIOMSha, data.svy.mod))

## RMSE
sqrt(svymean(~lnBMASqResidual, data.svy.mod))
sqrt(svyby(~lnBMASqResidual, ~Forest, data.svy.mod, svymean))
## % RMSE
sqrt(svymean(~lnBMASqResidual, data.svy.mod))/(svymean(~STBIOMSha, data.svy.mod))
sqrt(svyby(~lnBMASqResidual, ~Forest, data.svy.mod, svymean))/(svymean(~STBIOMSha, data.svy.mod))

########################################################
## validation data
data.svy.val <- svydesign(ids = ~1, data = data.val)
data.svy.val <- svydesign(ids = ~1, data = data.val, fpc = data.val$fpc, strata = data.val$Stratum)

## Bias
svymean(~lnBMAResidual, data.svy.val)
svyby(~lnBMAResidual, ~Forest, data.svy.val, svymean)
## % Bias
svymean(~lnBMAResidual, data.svy.val)/(svymean(~STBIOMSha, data.svy.val))
svyby(~lnBMAResidual, ~Forest, data.svy.val, svymean)/(svymean(~STBIOMSha, data.svy.val))

## RMSE
sqrt(svymean(~lnBMASqResidual, data.svy.val))
sqrt(svyby(~lnBMASqResidual, ~Forest, data.svy.val, svymean))
## % RMSE
sqrt(svymean(~lnBMASqResidual, data.svy.val))/(svymean(~STBIOMSha, data.svy.val))
sqrt(svyby(~lnBMASqResidual, ~Forest, data.svy.val, svymean))/(svymean(~STBIOMSha, data.svy.val))

########################################################
## Transferability validation data
data.svy.val.ind <- svydesign(ids = ~1, data = data.val.ind)
data.svy.val.ind <- svydesign(ids = ~1, data = data.val.ind, fpc = data.val.ind$fpc, strata = data.val.ind$Stratum)

## Bias
svymean(~lnBMAResidual, data.svy.val.ind)
svyby(~lnBMAResidual, ~Forest, data.svy.val.ind, svymean)
## % Bias
svymean(~lnBMAResidual, data.svy.val.ind)/(svymean(~STBIOMSha, data.svy.val.ind))
svyby(~lnBMAResidual, ~Forest, data.svy.val.ind, svymean)/(svymean(~STBIOMSha, data.svy.val.ind))

## RMSE
sqrt(svymean(~lnBMASqResidual, data.svy.val.ind))
sqrt(svyby(~lnBMASqResidual, ~Forest, data.svy.val.ind, svymean))
## % RMSE
sqrt(svymean(~lnBMASqResidual, data.svy.val.ind))/(svymean(~STBIOMSha, data.svy.val.ind))
sqrt(svyby(~lnBMASqResidual, ~Forest, data.svy.val.ind, svymean))/(svymean(~STBIOMSha, data.svy.val.ind))

########################################################
########################################################
## Make some plots, in development
########################################################
########################################################

## plots
Prednotrans <- predict(MedianBASModel)
plot(Prednotrans ~ data.mod$STBIOMSha, xlim = c(0,600), ylim = c(0,600))
points(Prednotrans[data.mod$PlotSizeAcres >= .1] ~ data.mod$STBIOMSha[data.mod$PlotSizeAcres >= .1], col ='gold')
points(Prednotrans[data.mod$PlotSizeAcres < 0.05] ~ data.mod$STBIOMSha[data.mod$PlotSizeAcres < 0.05], col ='red')
abline(a = 0, b = 1 , col = 'black', lwd = 2)

PrednotransVal <- predict(MedianBASModel, newdata = data.val)
plot(PrednotransVal~data.val$STBIOMSha, xlim = c(0,600), ylim = c(0,600))
plot(PrednotransVal ~ data.val$STBIOMSha, xlim = c(0,600), ylim = c(0,600))
points(PrednotransVal[data.val$PlotSizeAcres >= .1] ~ data.val$STBIOMSha[data.val$PlotSizeAcres >= .1], col ='gold')
points(PrednotransVal[data.val$PlotSizeAcres < 0.05] ~ data.val$STBIOMSha[data.val$PlotSizeAcres < 0.05], col ='red', pch = 15)
abline(a = 0, b = 1 , col = 'black', lwd = 2)

PrednotransValind <- predict(MedianBASModel, newdata = data.val.ind)

########################################################
########################################################
## Summary statistics fro table 4.
########################################################
########################################################
## model building data
data.svy <- svydesign(ids = ~1, data = data.mod)
data.svy <- svydesign(ids = ~1, data = data.mod, fpc = data.mod$fpc, strata = data.mod$Stratum)

svymean(~STBIOMSha, data.svy)
svyby(~STBIOMSha, ~Forest, data.svy, svymean)

svymean(~elevation, data.svy)
svyby(~elevation, ~Forest, data.svy, svymean)

table(data.mod$R3ERUlabelName)

## validation data
data.val$Stratum <- as.character(data.val$Stratum)
data.svy.val <- svydesign(ids = ~1, data = data.val)
data.svy.val <- svydesign(ids = ~1, data = data.val, fpc = data.val$fpc, strata = data.val$Stratum)

svymean(~STBIOMSha, data.svy.val)
svyby(~STBIOMSha, ~Forest, data.svy.val, svymean)

svymean(~elevation, data.svy.val)
svyby(~elevation, ~Forest, data.svy.val, svymean)

## transfer data
data.svy.val.ind <- svydesign(ids = ~1, data = data.val.ind, fpc = data.val.ind$fpc, strata = data.val.ind$Stratum)
data.svy.val.ind <- svydesign(ids = ~1, data = data.val.ind)

#svymean(~STBIOMSha, data.svy.val.ind)
svyby(~STBIOMSha, ~Forest, data.svy.val.ind, svymean)

#svymean(~elevation, data.svy.val.ind)
svyby(~elevation, ~Forest, data.svy.val.ind, svymean)


########################################################
########################################################
## ERU Dummy variables
########################################################
########################################################
### add in dummy variables for the important ERU units
data.mod$R3ERUlabelB <- data.mod$R3ERUlabelE <- data.mod$R3ERUlabelF <- data.mod$R3ERUlabelG <-   rep(0, length(data.mod[ , 1]))

data.mod$R3ERUlabelB[data.mod$R3ERUlabel == 'B'] <- 1
data.mod$R3ERUlabelE[data.mod$R3ERUlabel == 'E'] <- 1
data.mod$R3ERUlabelF[data.mod$R3ERUlabel == 'F'] <- 1
data.mod$R3ERUlabelG[data.mod$R3ERUlabel == 'G'] <- 1

data.val$R3ERUlabelB <- data.val$R3ERUlabelE <- data.val$R3ERUlabelF <- data.val$R3ERUlabelG <- 
  rep(0, length(data.val[ , 1]))
data.val$R3ERUlabelB[data.val$R3ERUlabel == 'B'] <- 1
data.val$R3ERUlabelE[data.val$R3ERUlabel == 'E'] <- 1
data.val$R3ERUlabelF[data.val$R3ERUlabel == 'F'] <- 1
data.val$R3ERUlabelG[data.val$R3ERUlabel == 'G'] <- 1
#################################################
##################################################################################################
##################################################################################################
##################################################################################################
#################################################
## Old Models 
#################################################
##################################################################################################
##################################################################################################
##################################################################################################
#################################################

MedianBASModel_log <- svyglm(logSTBIOMSha ~ Elev_P30 +
                               Elev_MAD_median +
                               Elev_LCV +
                               Pct_all_returns_above_ht +
                               P90_pctAllOver3m +
                               elevation +
                               slope +
                               NDVI_Amp,  
                             design = data.svy)

HPMlm.log <- svyglm(logSTBIOMSha ~ Elev_P30 +
                      Elev_MAD_median +
                      Elev_LCV +
                      Pct_all_returns_above_ht +
                      P90_pctAllOver3m +
                      elevation +
                      slope,  
                    design = data.svy)

MedianBASModel <- svyglm(STBIOMSha ~ Elev_P60 +
                           Elev_MAD_median +
                           Elev_L3 +
                           Elev_Lskewness +
                           Elev_Lkurtosis +
                           all_returns_above_ht_div_Total_first_returns_x_100 +
                           P30_pctAllOver3m +
                           P60_pctAllOver3m +
                           elevation,
                         design = data.svy)

HPMlm <- svyglm(STBIOMSha ~ Elev_P30 +  
                  Elev_L3 + 
                  Elev_Lskewness + 
                  Elev_Lkurtosis + 
                  all_returns_above_ht_div_Total_first_returns_x_100 + 
                  P30_pctAllOver3m + 
                  P90_pctAllOver3m, 
                design = data.svy)

MedianBASModel.sqrt <- svyglm(sqrt(STBIOMSha) ~ Elev_MAD_median +
                                Pct_all_returns_above_ht +
                                P30_pctAllOver3m +
                                P60_pctAllOver3m +
                                elevation + 
                                slope,
                              design = data.svy)

#################################################
#################################################
##################################
## add in forest level...
data.svy <- svydesign(ids = ~1, data = data.mod, fpc = data.mod$fpc, strata = data.mod$Stratum)

data.val$Stratum <- as.character(data.val$Stratum)
data.svy.val <- svydesign(ids = ~1, data = data.val, fpc = data.val$fpc, strata = data.val$Stratum)

data.svy.val.ind <- svydesign(ids = ~1, data = data.val.ind, fpc = data.val.ind$fpc, strata = data.val.ind$Stratum)

#################################################
## #1
## log transformed models
summary(MedianBASModel_log); 
summary(MedianBASModel_log)$aic

# residual and error data frame
predictedoutput <- exp(predict(MedianBASModel_log, newdata=data.val))
Resoutput <- predictedoutput - (data.val$STBIOMSha)

frame <- cbind(predictedoutput, Resoutput, Resoutput^2,  
               data.val [ , c('STBIOMSha', 'Forest','PlotSizeAcres', 'fpc', 'Stratum', 
                              'R3ERUlabelB', 'R3ERUlabelE', 'R3ERUlabelF', 'R3ERUlabelG')])
colnames(frame)<- c('Prediction', 'PredictionSE', "Residual", "ResidualSE", "SqResidual", "SqResidualSE",  
                    'STBIOMSha', 'Forest','PlotSizeAcres', "fpc", "Stratum",
                    'B, PJ woodland', 'E, spruce fir', 'F, mix con', 'G, PIPO')
CrossValidation <- svydesign(ids = ~1, data = frame, fpc = frame$fpc, strata = frame$Stratum)

#Bias
svymean(frame$Residual, design = CrossValidation)
# % Bias
100*(svymean(frame$Residual, design = CrossValidation)/
       svymean(frame$Prediction, design = CrossValidation)) # remember to 

#Root Mean Square Error
sqrt(svymean(frame$SqResidual, design = CrossValidation)) # remember to Square the Standard Error
#% RMSE
100*(sqrt(svymean(frame$SqResidual, design = CrossValidation))/
       svymean(frame$Prediction, design = CrossValidation)) # remember to Square the Standard Error

ggplot(frame, aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'NorthKaibab', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'SWJM', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Sitgreaves', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Tonto', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Coconino', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()

#################################################################
## no transformation 
## #2
summary(MedianBASModel); 
summary(MedianBASModel)$aic

# residual and error data frame
predictedoutput <- predict(MedianBASModel, newdata=data.val)
Resoutput <- predictedoutput - (data.val$STBIOMSha)

frame <- cbind(predictedoutput, Resoutput, Resoutput^2, 
               data.val [ , c('STBIOMSha', 'Forest','PlotSizeAcres', 'fpc', 'Stratum', 
                              'R3ERUlabelB', 'R3ERUlabelE', 'R3ERUlabelF', 'R3ERUlabelG')])
colnames(frame)<- c('Prediction', 'PredictionSE', "Residual", "ResidualSE", "SqResidual", "SqResidualSE",  
                    'STBIOMSha', 'Forest','PlotSizeAcres', "fpc", "Stratum",
                    'B, PJ woodland', 'E, spruce fir', 'F, mix con', 'G, PIPO')
CrossValidation <- svydesign(ids = ~1, data = frame, fpc = frame$fpc, strata = frame$Stratum)

#Bias
svymean(frame$Residual, design = CrossValidation)
# % Bias
100*(svymean(frame$Residual, design = CrossValidation)/
       svymean(frame$Prediction, design = CrossValidation)) # remember to 

#Root Mean Square Error
sqrt(svymean(frame$SqResidual, design = CrossValidation)) # remember to Square the Standard Error
#% RMSE
100*(sqrt(svymean(frame$SqResidual, design = CrossValidation))/
       svymean(frame$Prediction, design = CrossValidation)) # remember to Square the Standard Error

ggplot(frame, aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()

ggplot(frame[frame$Forest == 'NorthKaibab', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'SWJM', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Sitgreaves', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Tonto', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Coconino', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()

####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
## selected model
########################################
#################################################################
## no transformation 
## #3
summary(MedianBASModel); 
summary(MedianBASModel)$aic

# residual and error data frame
predictedoutput <- predict(MedianBASModel, newdata=data.val)
Resoutput <- predictedoutput - (data.val$STBIOMSha)
PctRes <- ((predictedoutput+1) / (data.val$STBIOMSha))*100

frame <- cbind(predictedoutput, Resoutput, Resoutput^2, PctRes, 
               data.val [ , c('STBIOMSha', 'Forest','PlotSizeAcres', 'fpc', 'Stratum', 
                              'R3ERUlabelB', 'R3ERUlabelE', 'R3ERUlabelF', 'R3ERUlabelG')])
colnames(frame)<- c('Prediction', 'PredictionSE', "Residual", "ResidualSE", "SqResidual", "SqResidualSE", "PctResidual", "PctResidualSE", 
                    'STBIOMSha', 'Forest','PlotSizeAcres', "fpc", "Stratum",
                    'B, PJ woodland', 'E, spruce fir', 'F, mix con', 'G, PIPO')
CrossValidation <- svydesign(ids = ~1, data = frame, fpc = frame$fpc, strata = frame$Stratum)

#Bias
svymean(frame$Residual, design = CrossValidation)
# % Bias
100*(svymean(frame$Residual, design = CrossValidation)/
       svymean(frame$Prediction, design = CrossValidation)) # remember to 

#Root Mean Square Error
sqrt(svymean(frame$SqResidual, design = CrossValidation)) # remember to Square the Standard Error
#% RMSE
100*(sqrt(svymean(frame$SqResidual, design = CrossValidation))/
       svymean(frame$Prediction, design = CrossValidation)) # remember to Square the Standard Error

ggplot(frame, aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()

ggplot(frame[frame$Forest == 'NorthKaibab', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'SWJM', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Sitgreaves', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Tonto', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Coconino', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()

### Hex plots, weighted by sample weights
svyplot(frame$Residual~frame$STBIOMSha, design = CrossValidation, style = 'trans', xlab = 'Observed', ylab = 'Residual')
abline(h = 0, col = 'red')

svyplot(frame$Residual ~ frame$STBIOMSha, design = CrossValidation, style = 'trans', xbins = 20, xlab = 'Observed', ylab = 'Percent agreement')
abline(h = 0, col = 'black')
smth <- svysmooth(frame$Residual ~ frame$STBIOMSha, design = CrossValidation)
lines(smth, col = 'red')


unique(data.val$PlotSizeAcres)

#############################################################
#############################################################
## unfit models or poorly performing...
#############################################################
#############################################################
# sqrt -- looks bad!
BioMass.Mod.sqrt <- bas.lm(sqrt(STBIOMSha) ~ Elev_mode + Elev_P01 + Elev_P10 + Elev_P30 + Elev_P60 + Elev_P90 +   
                             Elev_stddev +  Elev_kurtosis + Elev_MAD_median + Elev_MAD_mode + Elev_L3 + Elev_L4 + Elev_LCV + Elev_Lskewness + Elev_Lkurtosis + 
                             Pct_all_returns_above_ht + all_returns_above_ht_div_Total_first_returns_x_100 + pct_all_returns_above_mean + All_returns_above_mode_div_Total_first_returns_x_100 + 
                             mode_pctAllOver3m + mode_pctAlloverModeDiv1st + P01_pctAllOver3m + P10_pctAllOver3m + P30_pctAllOver3m + P60_pctAllOver3m + P90_pctAllOver3m +
                             elevation + aspect + slope + NDVI_Amp + R3ERUlabel, 
                           data = data.mod, 
                           prior="hyper-g",
                           alpha = 3,
                           modelprior=tr.poisson(10,30),
                           method="MCMC+BAS")


# log log -- no solution
BioMass.Mod.loglog <- bas.lm(logSTBIOMSha ~   logmode + logP01 + logP10 + logP30 + logP60 + logP90 +
                               Elev_stddev +  Elev_kurtosis + Elev_MAD_median + Elev_MAD_mode + Elev_L3 + Elev_L4 + Elev_LCV + Elev_Lskewness + Elev_Lkurtosis + 
                               log_Pct_all_returns_above_ht + log_pct_all_returns_above_mean + log_all_returns_above_ht_div_Total_first_returns_x_100 + 
                               logmode_pctAllOver3m + logmode_pctAlloverModeDiv1st + logP01_pctAllOver3m + logP10_pctAllOver3m + logP30_pctAllOver3m + logP60_pctAllOver3m + logP90_pctAllOver3m + 
                               elevation + aspect + slope + NDVI_Amp + R3ERUlabel,
                             data = data.mod, 
                             prior="hyper-g",
                             alpha = 3,
                             modelprior=tr.poisson(10,30),
                             method="MCMC+BAS")
#sq rt, log -- no solution
BioMass.Mod.sqlog <- bas.lm(sqrt(STBIOMSha) ~   logmode + logP01 + logP10 + logP30 + logP60 + logP90 +
                              Elev_stddev +  Elev_kurtosis + Elev_MAD_median + Elev_MAD_mode + Elev_L3 + Elev_L4 + Elev_LCV + Elev_Lskewness + Elev_Lkurtosis + 
                              log_Pct_all_returns_above_ht + log_pct_all_returns_above_mean + log_all_returns_above_ht_div_Total_first_returns_x_100 + 
                              logmode_pctAllOver3m + logmode_pctAlloverModeDiv1st + logP01_pctAllOver3m + logP10_pctAllOver3m + logP30_pctAllOver3m + logP60_pctAllOver3m + logP90_pctAllOver3m + 
                              elevation + aspect + slope + NDVI_Amp + R3ERUlabel,
                            data = data.mod, 
                            prior="hyper-g",
                            alpha = 3,
                            modelprior=tr.poisson(10,30),
                            method="MCMC+BAS")

#############################################################
#MPM
BioMass.Mod.loglog$namesx[which(BioMass.Mod.loglog$probne0>0.5)][-1]
BioMass.Mod.sqlog$namesx[which(BioMass.Mod.sqlog$probne0>0.5)][-1]

# HPM
HPM <- predict(BioMass.Mod.loglog, estimator="HPM")
BioMass.Mod.loglog$namesx[HPM$bestmodel+1][-1]

HPM <- predict(BioMass.Mod.sqlog, estimator="HPM")
BioMass.Mod.sqlog$namesx[HPM$bestmodel+1][-1]


##################################
## #1
## log log transformed models
# Median same as HPM
summary(MedianBASModel_loglog); 
summary(MedianBASModel_loglog)$aic
# residual and error data frame
predictedoutput <- exp(predict(MedianBASModel_loglog, newdata=data.val))-1
Resoutput <- predictedoutput - (data.val$STBIOMSha)
PctRes <- ((predictedoutput+1) / (data.val$STBIOMSha))*100

frame <- cbind(predictedoutput, Resoutput, Resoutput^2, PctRes, 
               data.val [ , c('STBIOMSha', 'Forest','PlotSizeAcres', 'fpc', 'Stratum', 
                              'R3ERUlabelB', 'R3ERUlabelE', 'R3ERUlabelF', 'R3ERUlabelG')])
colnames(frame)<- c('Prediction', 'PredictionSE', "Residual", "ResidualSE", "SqResidual", "SqResidualSE", "PctResidual", "PctResidualSE", 
                    'STBIOMSha', 'Forest','PlotSizeAcres', "fpc", "Stratum",
                    'B, PJ woodland', 'E, spruce fir', 'F, mix con', 'G, PIPO')
CrossValidation <- svydesign(ids = ~1, data = frame, fpc = frame$fpc, strata = frame$Stratum)

#Bias
svymean(frame$Residual, design = CrossValidation)
# % Bias
100*(svymean(frame$Residual, design = CrossValidation)/
       svymean(frame$Prediction, design = CrossValidation)) # remember to 

#Root Mean Square Error
sqrt(svymean(frame$SqResidual, design = CrossValidation)) # remember to Square the Standard Error
#% RMSE
100*(sqrt(svymean(frame$SqResidual, design = CrossValidation))/
       svymean(frame$Prediction, design = CrossValidation)) # remember to Square the Standard Error

ggplot(frame, aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'NorthKaibab', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'SWJM', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Sitgreaves', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Tonto', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Coconino', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()
