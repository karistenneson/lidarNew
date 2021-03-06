### This file is for running the BMA components of the analysis
## Written by MS Patterson (maspatte@uw.edu) and Karis Tenneson (krtenneson@fs.fed.us)
# Last updated: Oct 20, 2017

### Bring in data
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
library(tidyverse)
library(dplyr)

#library(robustbase)

### load data
data.mod <- read.csv('Data//datamod.csv')
data.val <- read.csv('Data//dataval.csv')
data.val.ind <- read.csv('Data//datavalind.csv')

head(data.mod); dim(data.mod)
#data.mod <- data.mod[data.mod$STBIOMS != 0 , ]
#data.val <- data.val[data.val$STBIOMS != 0 , ]

DATA.mod <- data.mod[ c(-591, -1958), predictorSubset]
DATA.mod$SmplWts<- data.mod$SmplWts[c(-591, -1958)]
DATA.val <- data.val[ c(-622, -736), predictorSubset]

######################################################
#sq rt, log
BioMass.Mod.sqrtlog <- bas.lm(sqrt(STBIOMSha) ~   logmode + logP01 + logP10 + logP30 + logP60 + logP90 +
                               Elev_stddev +  Elev_kurtosis + Elev_MAD_median + Elev_MAD_mode + Elev_L3 + Elev_L4 + Elev_LCV + Elev_Lskewness + Elev_Lkurtosis + 
                               log_Pct_all_returns_above_ht + log_pct_all_returns_above_mean + log_all_returns_above_ht_div_Total_first_returns_x_100 + 
                               logmode_pctAllOver3m + logmode_pctAlloverModeDiv1st + logP01_pctAllOver3m + logP10_pctAllOver3m + logP30_pctAllOver3m + logP60_pctAllOver3m + logP90_pctAllOver3m + 
                               elevation + aspect + slope + NDVI_Amp + R3ERUlabel, 
                             weights = DATA.mod$SmplWts,
                             data = DATA.mod, 
                             prior="hyper-g",
                             alpha = 3,
                             modelprior=tr.poisson(10,30),
                             method="MCMC+BAS")

# sqrt
BioMass.Mod.sqrt <- bas.lm(sqrt(STBIOMSha) ~ Elev_mode + Elev_P01 + Elev_P10 + Elev_P30 + Elev_P60 + Elev_P90 +   
                            Elev_stddev +  Elev_kurtosis + Elev_MAD_median + Elev_MAD_mode + Elev_L3 + Elev_L4 + Elev_LCV + Elev_Lskewness + Elev_Lkurtosis + 
                            Pct_all_returns_above_ht + all_returns_above_ht_div_Total_first_returns_x_100 + pct_all_returns_above_mean + All_returns_above_mode_div_Total_first_returns_x_100 + 
                            mode_pctAllOver3m + mode_pctAlloverModeDiv1st + P01_pctAllOver3m + P10_pctAllOver3m + P30_pctAllOver3m + P60_pctAllOver3m + P90_pctAllOver3m +
                            elevation + aspect + slope + NDVI_Amp + R3ERUlabel, 
                          weights = DATA.mod$SmplWts, 
                          data = DATA.mod, 
                          prior="hyper-g",
                          alpha = 3,
                          modelprior=tr.poisson(10,30),
                          method="MCMC+BAS")

# log log
BioMass.Mod.loglog <- bas.lm(logSTBIOMSha ~   logmode + logP01 + logP10 + logP30 + logP60 + logP90 +
  Elev_stddev +  Elev_kurtosis + Elev_MAD_median + Elev_MAD_mode + Elev_L3 + Elev_L4 + Elev_LCV + Elev_Lskewness + Elev_Lkurtosis + 
  log_Pct_all_returns_above_ht + log_pct_all_returns_above_mean + log_all_returns_above_ht_div_Total_first_returns_x_100 + 
  logmode_pctAllOver3m + logmode_pctAlloverModeDiv1st + logP01_pctAllOver3m + logP10_pctAllOver3m + logP30_pctAllOver3m + logP60_pctAllOver3m + logP90_pctAllOver3m + 
  elevation + aspect + slope + NDVI_Amp + R3ERUlabel, 
  weights = DATA.mod$SmplWts,
  data = DATA.mod, 
  prior="hyper-g",
  alpha = 3,
  modelprior=tr.poisson(10,30),
  method="MCMC+BAS")

# log
BioMass.Mod.log <- bas.lm(logSTBIOMSha ~ Elev_mode + Elev_P01 + Elev_P10 + Elev_P30 + Elev_P60 + Elev_P90 +   
      Elev_stddev +  Elev_kurtosis + Elev_MAD_median + Elev_MAD_mode + Elev_L3 + Elev_L4 + Elev_LCV + Elev_Lskewness + Elev_Lkurtosis + 
      Pct_all_returns_above_ht + all_returns_above_ht_div_Total_first_returns_x_100 + pct_all_returns_above_mean + All_returns_above_mode_div_Total_first_returns_x_100 + 
      mode_pctAllOver3m + mode_pctAlloverModeDiv1st + P01_pctAllOver3m + P10_pctAllOver3m + P30_pctAllOver3m + P60_pctAllOver3m + P90_pctAllOver3m +
      elevation + aspect + slope + NDVI_Amp + R3ERUlabel, 
      weights = DATA.mod$SmplWts, 
      data = DATA.mod, 
      prior="hyper-g",
      alpha = 3,
      modelprior=tr.poisson(10,30),
      method="MCMC+BAS")

# Full variable pool, truncated poisson prior, hyper-g
BioMass.Mod <- bas.lm(STBIOMSha ~ Elev_mode + Elev_P01 + Elev_P10 + Elev_P30 + Elev_P60 + Elev_P90 +   
                            Elev_stddev +  Elev_kurtosis + Elev_MAD_median + Elev_MAD_mode + Elev_L3 + Elev_L4 + Elev_LCV + Elev_Lskewness + Elev_Lkurtosis + 
                            Pct_all_returns_above_ht + all_returns_above_ht_div_Total_first_returns_x_100 + pct_all_returns_above_mean + All_returns_above_mode_div_Total_first_returns_x_100 + 
                            mode_pctAllOver3m + mode_pctAlloverModeDiv1st + P01_pctAllOver3m + P10_pctAllOver3m + P30_pctAllOver3m + P60_pctAllOver3m + P90_pctAllOver3m +
                            elevation + aspect + slope + NDVI_Amp + R3ERUlabel, 
                          weights = DATA.mod$SmplWts, 
                          data = DATA.mod, 
                          prior="hyper-g",
                          alpha = 3,
                          modelprior=tr.poisson(10,30),
                          method="MCMC+BAS")

####################################################
####################################################

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


DATA.mod <- data.mod[ c(-591, -1958), c(predictorSubset, 'R3ERUlabelB', 'R3ERUlabelE', 'R3ERUlabelF', 'R3ERUlabelG', 'Forest','PlotSizeAcres', 'fpc', 'Stratum')]

DATA.val <- data.val[ c(-622, -736), c(predictorSubset, 'R3ERUlabelB', 'R3ERUlabelE', 
                                       'R3ERUlabelF', 'R3ERUlabelG', 'Forest','PlotSizeAcres', 'fpc', 'Stratum')]

data.svy <- svydesign(ids = ~1, data = DATA.mod, fpc = DATA.mod$fpc, strata = DATA.mod$Stratum)

data.svy.val <- svydesign(ids = ~1, data = DATA.val, fpc = DATA.val$fpc, strata = DATA.val$Stratum)


####################################################
####################################################
## look up covariates in median models:
BioMass.Mod.loglog$namesx[which(BioMass.Mod.loglog$probne0>0.5)][-1]
BioMass.Mod.log$namesx[which(BioMass.Mod.log$probne0>0.5)][-1]
BioMass.Mod$namesx[which(BioMass.Mod$probne0>0.5)][-1]

# Can also call the Median Probability Model like this:
#MPM <- predict(BioMass.Mod.no, estimator="MPM")
#BioMass.Mod.no$namesx[MPM$bestmodel+1][-1]

########################################
## #1
## log log transformed model
MedianBASModel_loglog <- svyglm(logSTBIOMSha ~ logP60 +
                                Elev_stddev +
                                Elev_L3 +
                                log_Pct_all_returns_above_ht +
                                logP10_pctAllOver3m +
                                logP60_pctAllOver3m,
                                design = data.svy)

summary(lm(logSTBIOMSha ~ logP60 +
                                  Elev_stddev +
                                  Elev_L3 +
                                  log_Pct_all_returns_above_ht +
                                  logP10_pctAllOver3m +
                                  logP60_pctAllOver3m,
                                data = DATA.mod))

MedianBASModel_log <- svyglm(logSTBIOMSha ~ Elev_P01 +
                             Elev_P90 +
                             Elev_stddev +
                             Pct_all_returns_above_ht +
                             P01_pctAllOver3m +
                             P10_pctAllOver3m +
                             P60_pctAllOver3m +
                             P90_pctAllOver3m +
                             elevation +
                             slope,  
                             design = data.svy)

summary(lm(logSTBIOMSha ~ Elev_P01 +
                               Elev_P90 +
                               Elev_stddev +
                               Pct_all_returns_above_ht +
                               P01_pctAllOver3m +
                               P10_pctAllOver3m +
                               P60_pctAllOver3m +
                               P90_pctAllOver3m +
                               elevation +
                               slope,  
                             data = DATA.mod))

MedianBASModel <- svyglm(STBIOMSha ~ Elev_P60 +
                         Elev_L3 +
                         Elev_Lskewness +
                         Elev_Lkurtosis +
                         all_returns_above_ht_div_Total_first_returns_x_100 +
                         P30_pctAllOver3m +
                         P60_pctAllOver3m +
                         P90_pctAllOver3m +
                         elevation,
                         design = data.svy)

summary(lm(STBIOMSha ~ Elev_P60 +
                           Elev_L3 +
                           Elev_Lskewness +
                           Elev_Lkurtosis +
                           all_returns_above_ht_div_Total_first_returns_x_100 +
                           P30_pctAllOver3m +
                           P60_pctAllOver3m +
                           P90_pctAllOver3m +
                           elevation,
                         data = DATA.mod))
#################################################################### 

# Highest Probability Model
HPM <- predict(BioMass.Mod.loglog, estimator="HPM")
BioMass.Mod.loglog$namesx[HPM$bestmodel+1][-1]

HPM <- predict(BioMass.Mod.log, estimator="HPM")
BioMass.Mod.log$namesx[HPM$bestmodel+1][-1]

HPM <- predict(BioMass.Mod, estimator="HPM")
BioMass.Mod$namesx[HPM$bestmodel+1][-1]


#HPMlm.loglog <- svyglm(logSTBIOMSha ~ # same as median model, design = data.svy)

HPMlm.log <- svyglm(logSTBIOMSha ~ Elev_P01 + 
                      Elev_P90 + 
                      Elev_stddev + 
                      Elev_Lskewness + 
                      Pct_all_returns_above_ht + 
                      P01_pctAllOver3m + 
                      P10_pctAllOver3m + 
                      P60_pctAllOver3m + 
                      P90_pctAllOver3m + 
                      elevation + 
                      aspect + 
                      slope, 
                    design = data.svy)

summary(lm(logSTBIOMSha ~ Elev_P01 + 
                      Elev_P90 + 
                      Elev_stddev + 
                      Elev_Lskewness + 
                      Pct_all_returns_above_ht + 
                      P01_pctAllOver3m + 
                      P10_pctAllOver3m + 
                      P60_pctAllOver3m + 
                      P90_pctAllOver3m + 
                      elevation + 
                      aspect + 
                      slope, data = DATA.mod))

HPMlm <- svyglm(STBIOMSha ~ Elev_P30 +  
                  Elev_L3 + 
                  Elev_Lskewness + 
                  Elev_Lkurtosis + 
                  all_returns_above_ht_div_Total_first_returns_x_100 + 
                  P30_pctAllOver3m + 
                  P90_pctAllOver3m, 
                design = data.svy)
summary(lm(STBIOMSha ~ Elev_P30 +  
        Elev_L3 + 
        Elev_Lskewness + 
        Elev_Lkurtosis + 
        all_returns_above_ht_div_Total_first_returns_x_100 + 
        P30_pctAllOver3m + 
        P90_pctAllOver3m, 
      data = DATA.mod))
##################################
##################################
#################################################################################
####################################################################################
####################################################################################
#################################################################################
##################################

##################################
## #1
## log log transformed models
# Median same as HPM
summary(MedianBASModel_loglog); 
summary(MedianBASModel_loglog)$aic
# residual and error data frame
predictedoutput <- exp(predict(MedianBASModel_loglog, newdata=DATA.val))-1
Resoutput <- predictedoutput - (DATA.val$STBIOMSha)
PctRes <- ((predictedoutput+1) / (DATA.val$STBIOMSha))*100

frame <- cbind(predictedoutput, Resoutput, Resoutput^2, PctRes, 
               DATA.val [ , c('STBIOMSha', 'Forest','PlotSizeAcres', 'fpc', 'Stratum', 
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

####################################################################
## #2
## log transformed models
summary(MedianBASModel_log); 
summary(MedianBASModel_log)$aic

# residual and error data frame
predictedoutput <- exp(predict(MedianBASModel_log, newdata=DATA.val))-1
Resoutput <- predictedoutput - (DATA.val$STBIOMSha)
PctRes <- ((predictedoutput+1) / (DATA.val$STBIOMSha))*100

frame <- cbind(predictedoutput, Resoutput, Resoutput^2, PctRes, 
               DATA.val [ , c('STBIOMSha', 'Forest','PlotSizeAcres', 'fpc', 'Stratum', 
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

#################################################################
## no transformation 
## #3
summary(MedianBASModel); 
summary(MedianBASModel)$aic

# residual and error data frame
predictedoutput <- predict(MedianBASModel, newdata=DATA.val)
Resoutput <- predictedoutput - (DATA.val$STBIOMSha)
PctRes <- ((predictedoutput+1) / (DATA.val$STBIOMSha))*100

frame <- cbind(predictedoutput, Resoutput, Resoutput^2, PctRes, 
               DATA.val [ , c('STBIOMSha', 'Forest','PlotSizeAcres', 'fpc', 'Stratum', 
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

##################################
##################################
#################################################################################
####################################################################################
####################################################################################
#################################################################################
##################################
## log log transformed models
## #1
summary(HPMlm.loglog); 
summary(HPMlm.loglog)$aic
# residual and error data frame
predictedoutput <- exp(predict(HPMlm.loglog, newdata=DATA.val))-1
Resoutput <- predictedoutput - (DATA.val$STBIOMSha)
PctRes <- ((predictedoutput+1) / (DATA.val$STBIOMSha))*100

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
ggplot(frame[frame$Forest == 'NorthKaibab', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'SWJM', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Sitgreaves', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Tonto', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Coconino', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()

#################################################################
## log transformed models
## #2
summary(HPMlm.log); 
summary(HPMlm.log)$aic
# residual and error data frame
predictedoutput <- exp(predict(HPMlm.log, newdata=DATA.val))-1
Resoutput <- predictedoutput - (DATA.val$STBIOMSha)
PctRes <- ((predictedoutput+1) / (DATA.val$STBIOMSha))*100

frame <- cbind(predictedoutput, Resoutput, Resoutput^2, PctRes, 
               DATA.val [ , c('STBIOMSha', 'Forest','PlotSizeAcres', 'fpc', 'Stratum', 
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

################################################################
## no transformation
## #3
summary(HPMlm); 
summary(HPMlm)$aic

# residual and error data frame
predictedoutput <- predict(HPMlm, newdata=DATA.val)
Resoutput <- predictedoutput - (DATA.val$STBIOMSha)
PctRes <- ((predictedoutput+1) / (DATA.val$STBIOMSha))*100

frame <- cbind(predictedoutput, Resoutput, Resoutput^2, PctRes, 
               DATA.val [ , c('STBIOMSha', 'Forest','PlotSizeAcres', 'fpc', 'Stratum', 
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
predictedoutput <- predict(MedianBASModel, newdata=DATA.val)
Resoutput <- predictedoutput - (DATA.val$STBIOMSha)
PctRes <- ((predictedoutput+1) / (DATA.val$STBIOMSha))*100

frame <- cbind(predictedoutput, Resoutput, Resoutput^2, PctRes, 
               DATA.val [ , c('STBIOMSha', 'Forest','PlotSizeAcres', 'fpc', 'Stratum', 
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

