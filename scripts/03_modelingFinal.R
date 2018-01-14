### This file is for running the BMA components of the analysis
## Written by MS Patterson (maspatte@uw.edu) and Karis Tenneson (krtenneson@fs.fed.us)
# Last updated: Oct 20, 2017

### Bring in data
#setwd('C:\\Users\\krtenneson\\Desktop\\lidarPaper\\lidarNew\\scripts')
#setwd('\\\\166.2.126.25\\rseat\\Programs\\Reimbursibles\\fy2016\\R3_lidar_equation_transferability\\Analysis\\VersionControl\\lidarNew\\scripts')
#setwd("~/Documents/R/lidarNew/scripts") #Mac
#setwd("~/R/lidarNew/scripts") #WinCampus

### source data
source(file="DataPrepwithZeroesv2.R")
### source functions
source(file="functions.R")

data.mod$R3ERUlabel <- as.factor(data.mod$R3ERUlabel)
data.val$R3ERUlabel <- as.factor(data.val$R3ERUlabel)
data.val.ind$R3ERUlabel <- as.factor(data.val.ind$R3ERUlabel)

### Load required packages
library(cvTools)
library(BAS)
library(corrgram)
#library(robustbase)

head(data.mod); dim(data.mod)
#data.mod <- data.mod[data.mod$STBIOMS != 0 , ]
#data.val <- data.val[data.val$STBIOMS != 0 , ]

#Total area of model construction data set is  713168
data.mod$PercentArea <- (data.mod$fpc / 713168)*100
data.mod$SmplSize <- data.mod$SmplWts <- rep(0, length(data.mod$fpc))

strata<-unique(data.mod$Stratum)

for (i in 1:length(strata)){
  SampleSize <- length(data.mod$Stratum[data.mod$Stratum == strata[i]])
  data.mod$SmplSize[data.mod$Stratum == strata[i]] <- SampleSize
  data.mod$SmplWts[data.mod$Stratum == strata[i]] <- data.mod$PercentArea[data.mod$Stratum == strata[i]]/SampleSize* 100
}

## remove these columns for the models:
## 'Site','Forest', 
## "PlotSizeAcres", "fpc", "Stratum",
## R3ERUcodeFull, 'R3ERUlabelName'
## > 95% corr with P60: "Elev_ave", "Elev_P40", "Elev_P50","Elev_P70", "Elev_P75", "Elev_P80", 
## > 95% corr with P90: "Elev_P95", "Elev_P99", 
## > 95% corr with P30: "Elev_P20", "Elev_P25", 
## > 95% corr with stddev: "Elev_variance", "Elev_IQ", "Elev_AAD", "Elev_L2", 
## > 95% corr with Elev_LCV: "Elev_CV",
## > 95% corr with Elev_Lskewness: "Elev_skewness",
## > 95% corr with pct_all_returns_above_mean: "Pct_first_returns_above_mean", "All_returns_above_mean_div_Total_first_returns_x_100"
## > 95% corr with Pct_all_returns_above_ht: "Pct_first_returns_above_ht", 
## > 95% corr with All_returns_above_mode_div_Total_first_returns_x_100: "pct_all_returns_above_mode", "Pct_first_returns_above_mode", 

#corrgram(DATA.mod[ , c(1, 3, 13:18)], type="data", lower.panel=panel.shadeNtext, upper.panel=panel.signif, main="height")

#corrgram(DATA.mod[ , c(1, 4:12)], type="data", lower.panel=panel.shadeNtext, upper.panel=panel.signif, main="shape")

#corrgram(DATA.mod[ , c(1, 19:22)], type="data", lower.panel=panel.shadeNtext, upper.panel=panel.signif, main="density")

predictorSubset <- c("STBIOMSha", 'logSTBIOMSha', "TCUmha", 
  "Elev_stddev",  "Elev_kurtosis", "Elev_MAD_median", "Elev_MAD_mode", "Elev_L3", "Elev_L4", "Elev_LCV", "Elev_Lskewness", "Elev_Lkurtosis",   
  "Elev_mode", "Elev_P01", "Elev_P10", "Elev_P30",  "Elev_P60", "Elev_P90",
  "Pct_all_returns_above_ht", "all_returns_above_ht_div_Total_first_returns_x_100",  "pct_all_returns_above_mean", "All_returns_above_mode_div_Total_first_returns_x_100",  
  "mode_pctAllOver3m", "mode_pctAlloverModeDiv1st","P01_pctAllOver3m", "P10_pctAllOver3m", "P30_pctAllOver3m", "P60_pctAllOver3m",  "P90_pctAllOver3m", 
  "logmode", "logP01", "logP10", "logP30", "logP60", "logP90", 
  "log_Pct_all_returns_above_ht", "log_pct_all_returns_above_mean",  "log_all_returns_above_ht_div_Total_first_returns_x_100", 
  "logmode_pctAllOver3m", "logmode_pctAlloverModeDiv1st", "logP01_pctAllOver3m", "logP10_pctAllOver3m", "logP30_pctAllOver3m", "logP60_pctAllOver3m","logP90_pctAllOver3m",
  "elevation", "aspect", "slope", "NDVI_Amp", "R3ERUlabel")

DATA.mod <- data.mod[ , predictorSubset]
DATA.val <- data.val[ , predictorSubset]

######################################################
#weights = data.mod$SmplWts,
BioMass.Mod.loglog <- bas.lm(logSTBIOMSha ~   logmode + logP01 + logP10 + logP30 + logP60 + logP90 +
  Elev_stddev +  Elev_kurtosis + Elev_MAD_median + Elev_MAD_mode + Elev_L3 + Elev_L4 + Elev_LCV + Elev_Lskewness + Elev_Lkurtosis + 
  log_Pct_all_returns_above_ht + log_pct_all_returns_above_mean + log_all_returns_above_ht_div_Total_first_returns_x_100 + 
  logmode_pctAllOver3m + logmode_pctAlloverModeDiv1st + logP01_pctAllOver3m + logP10_pctAllOver3m + logP30_pctAllOver3m + logP60_pctAllOver3m + logP90_pctAllOver3m + 
  aspect + slope + NDVI_Amp + R3ERUlabel, 
  weights = data.mod$SmplWts,
  data = DATA.mod, 
  prior="hyper-g",
  alpha = 3,
  modelprior=tr.poisson(10,30),
  method="MCMC+BAS")

# Full variable pool, truncated poisson prior, hyper-g
BioMass.Mod.log <- bas.lm(logSTBIOMSha ~ Elev_mode + Elev_P01 + Elev_P10 + Elev_P30 + Elev_P60 + Elev_P90 +   
      Elev_stddev +  Elev_kurtosis + Elev_MAD_median + Elev_MAD_mode + Elev_L3 + Elev_L4 + Elev_LCV + Elev_Lskewness + Elev_Lkurtosis + 
      Pct_all_returns_above_ht + all_returns_above_ht_div_Total_first_returns_x_100 + pct_all_returns_above_mean + All_returns_above_mode_div_Total_first_returns_x_100 + 
      mode_pctAllOver3m + mode_pctAlloverModeDiv1st + P01_pctAllOver3m + P10_pctAllOver3m + P30_pctAllOver3m + P60_pctAllOver3m + P90_pctAllOver3m +
      elevation + aspect + slope + NDVI_Amp + R3ERUlabel, 
      weights = data.mod$SmplWts, 
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
                          weights = data.mod$SmplWts, 
                          data = DATA.mod, 
                          prior="hyper-g",
                          alpha = 3,
                          modelprior=tr.poisson(10,30),
                          method="MCMC+BAS")

####################################################
####################################################
## look up covariates in median models:
BioMass.Mod.loglog$namesx[which(BioMass.Mod.loglog$probne0>0.5)][-1]
BioMass.Mod.log$namesx[which(BioMass.Mod.log$probne0>0.5)][-1]
BioMass.Mod$namesx[which(BioMass.Mod$probne0>0.5)][-1]

# Can also call the Median Probability Model like this:
#MPM <- predict(BioMass.Mod.no, estimator="MPM")
#BioMass.Mod.no$namesx[MPM$bestmodel+1][-1]

####################################################
####################################################

data.mod$R3ERUlabelB <- data.mod$R3ERUlabelE <- data.mod$R3ERUlabelF <- data.mod$R3ERUlabelG <-   rep(0, length(data.mod[ , 1]))

data.mod$R3ERUlabelB[data.mod$R3ERUlabel == 'B'] <- 1
data.mod$R3ERUlabelE[data.mod$R3ERUlabel == 'E'] <- 1
data.mod$R3ERUlabelF[data.mod$R3ERUlabel == 'F'] <- 1
data.mod$R3ERUlabelG[data.mod$R3ERUlabel == 'G'] <- 1

data.mod$Ln_Elev_mode <- log(data.mod$Elev_mode)

data.val$R3ERUlabelB <- data.val$R3ERUlabelE <- data.val$R3ERUlabelF <- data.val$R3ERUlabelG <- 
  rep(0, length(data.val[ , 1]))
data.val$R3ERUlabelB[data.val$R3ERUlabel == 'B'] <- 1
data.val$R3ERUlabelE[data.val$R3ERUlabel == 'E'] <- 1
data.val$R3ERUlabelF[data.val$R3ERUlabel == 'F'] <- 1
data.val$R3ERUlabelG[data.val$R3ERUlabel == 'G'] <- 1


DATA.mod <- data.mod[ , c(predictorSubset, 'R3ERUlabelB', 'R3ERUlabelE', 'R3ERUlabelF', 'R3ERUlabelG', 'fpc', 'Stratum')]

DATA.val <- data.val[ , c(predictorSubset, 'R3ERUlabelB', 'R3ERUlabelE', 'R3ERUlabelF', 'R3ERUlabelG', 'fpc', 'Stratum')]

data.svy <- svydesign(ids = ~1, data = DATA.mod, fpc = DATA.mod$fpc, strata = DATA.mod$Stratum)

data.svy.val <- svydesign(ids = ~1, data = DATA.val, fpc = DATA.val$fpc, strata = DATA.val$Stratum)

########################################
## No transformation (with Zeroes)
## log transformed models
MedianBASModel_loglog <- svyglm(logSTBIOMSha ~ Elev_L4 + Elev_LCV + Elev_Lskewness + Elev_Lkurtosis  + log(Elev_mode) + log(Elev_P01) + log(Elev_P30) + log(Elev_P60) + log(Elev_P90) + log(Pct_all_returns_above_ht) + log(Elev_P01)*log(pct_all_returns_above_mean) + log(Elev_P01)*log(All_returns_above_mode_div_Total_first_returns_x_100) + log(Elev_P05)*log(Pct_all_returns_above_ht) + log(Elev_P05)*log(All_returns_above_mode_div_Total_first_returns_x_100) + log(Elev_P30)*log(Pct_all_returns_above_ht) + log(Elev_P60)*log(Pct_all_returns_above_ht) + log(Elev_P60)*log(pct_all_returns_above_mean) + log(Elev_P90)*log(Pct_all_returns_above_ht) + log(Elev_P90)*log(pct_all_returns_above_mean) + elevation + aspect + NDVI_Amp + R3ERUlabelB + R3ERUlabelE + R3ERUlabelF + R3ERUlabelG, 
                                      design = data.svy)

MedianBASModel_log <- svyglm(logSTBIOMSha ~ updateMe, 
                                      design = data.svy)

MedianBASModel <- svyglm(STBIOMSha ~ updateMe , 
                                    design = data.svy)

##################################
## #1
## log log transformed models
summary(MedianBASModel_loglog); 
summary(MedianBASModel_loglog)$aic
#Root Mean Square Error
predictedoutput <- exp(predict(MedianBASModel_loglog, newdata=DATA.val))-1
plot(predictedoutput~(DATA.val$STBIOMSha), xlim = c(0,650), ylim = c(0,650))
abline(a = 0, b = 1, col = 'red')
Resoutput <- predictedoutput - (DATA.val$STBIOMSha)
frame <- cbind(Resoutput, Resoutput^2, data.val[ , c('fpc', 'Stratum')])
colnames(frame)<- c("Resoutput", "ResoutputSE", "SqResoutput", "SqResoutputSE", "fpc", "Stratum")
ResSqVec <- svydesign(ids = ~1, data = frame, fpc = frame$fpc, strata = frame$Stratum)
sqrt(svymean(frame$SqResoutput, design = ResSqVec)) # remember to Square the Standard Error

## #2
## log transformed models
summary(MedianBASModel_log); 
summary(MedianBASModel_log)$aic
#Root Mean Square Error
predictedoutput <- exp(predict(MedianBASModel_log, newdata=DATA.val))-1
plot(predictedoutput~(DATA.val$STBIOMSha), xlim = c(0,650), ylim = c(0,650))
abline(a = 0, b = 1, col = 'red')
Resoutput <- predictedoutput - (DATA.val$STBIOMSha)
frame <- cbind(Resoutput, Resoutput^2, data.val[ , c('fpc', 'Stratum')])
colnames(frame)<- c("Resoutput", "ResoutputSE", "SqResoutput", "SqResoutputSE", "fpc", "Stratum")
ResSqVec <- svydesign(ids = ~1, data = frame, fpc = frame$fpc, strata = frame$Stratum)
sqrt(svymean(frame$SqResoutput, design = ResSqVec)) # remember to Square the Standard Error

## no transformation 
## #3
summary(MedianBASModel); 
summary(MedianBASModel)$aic
#Root Mean Square Error
predictedoutput <- predict(MedianBASModel_transNoInt, newdata=DATA.val.trans)
plot(predictedoutput~(DATA.val$STBIOMSha), xlim = c(0,650), ylim = c(0,650))
abline(a = 0, b = 1, col = 'red')
Resoutput <- predictedoutput - (DATA.val$STBIOMSha)
frame <- cbind(Resoutput, Resoutput^2, data.val [ , c('fpc', 'Stratum')])
colnames(frame)<- c("Resoutput", "ResoutputSE", "SqResoutput", "SqResoutputSE", "fpc", "Stratum")
ResSqVec <- svydesign(ids = ~1, data = frame, fpc = frame$fpc, strata = frame$Stratum)
sqrt(svymean(frame$SqResoutput, design = ResSqVec)) # remember to Square the Standard Error

# Model diagnostics
#plot(BioMass.Mod, ask=F)
#plot(BioMass.Mod.trans, ask=F)

#pull some models out using predict functions

##################################
##################################
# Highest Probability Model
HPM <- predict(BioMass.Mod.loglog, estimator="HPM")
BioMass.Mod.loglog$namesx[HPM$bestmodel+1][-1]

HPM <- predict(BioMass.Mod.log, estimator="HPM")
BioMass.Mod.log$namesx[HPM$bestmodel+1][-1]

HPM <- predict(BioMass.Mod, estimator="HPM")
BioMass.Mod$namesx[HPM$bestmodel+1][-1]


HPMlm.loglog <- svyglm(logSTBIOMSha ~ update, 
               design = data.svy)

HPMlm.log <- svyglm(logSTBIOMSha ~ update, 
               design = data.svy)

HPMlm <- svyglm(STBIOMSha ~ update, 
                           design = data.svy)

##################################
## log log transformed models
## #1
summary(HPMlm.loglog); 
summary(HPMlm.loglog)$aic
#Root Mean Square Error
predictedoutput <- exp(predict(HPMlm.loglog, newdata=DATA.val))-1
plot(predictedoutput~(DATA.val$STBIOMSha), xlim = c(0,650), ylim = c(0,650))
abline(a = 0, b = 1, col = 'red')
Resoutput <- predictedoutput - (DATA.val$STBIOMSha)
frame <- cbind(Resoutput, Resoutput^2, data.val[ , c('fpc', 'Stratum')])
colnames(frame)<- c("Resoutput", "ResoutputSE", "SqResoutput", "SqResoutputSE", "fpc", "Stratum")
ResSqVec <- svydesign(ids = ~1, data = frame, fpc = frame$fpc, strata = frame$Stratum)
sqrt(svymean(frame$SqResoutput, design = ResSqVec)) # remember to Square the Standard Error

## log transformed models
## #2
summary(HPMlm.log); 
summary(HPMlm.log)$aic
#Root Mean Square Error
predictedoutput <- exp(predict(HPMlm.log, newdata=DATA.val))-1
plot(predictedoutput~(DATA.val$STBIOMSha), xlim = c(0,650), ylim = c(0,650))
abline(a = 0, b = 1, col = 'red')
Resoutput <- predictedoutput - (DATA.val$STBIOMS)
frame <- cbind(Resoutput, Resoutput^2, data.val[ , c('fpc', 'Stratum')])
colnames(frame)<- c("Resoutput", "ResoutputSE", "SqResoutput", "SqResoutputSE", "fpc", "Stratum")
ResSqVec <- svydesign(ids = ~1, data = frame, fpc = frame$fpc, strata = frame$Stratum)
sqrt(svymean(frame$SqResoutput, design = ResSqVec)) # remember to Square the Standard Error

## no transformation
## #3
summary(HPMlm); 
summary(HPMlm)$aic

#Root Mean Square Error
predictedoutput <- predict(HPMlm, newdata=DATA.val.trans)
plot(predictedoutput~(DATA.val$STBIOMSha), xlim = c(0,650), ylim = c(0,650))
abline(a = 0, b = 1, col = 'red')
Resoutput <- predictedoutput - (DATA.val$STBIOMSha)
frame <- cbind(Resoutput, Resoutput^2, data.val[ , c('fpc', 'Stratum')])
colnames(frame)<- c("Resoutput", "ResoutputSE", "SqResoutput", "SqResoutputSE", "fpc", "Stratum")
ResSqVec <- svydesign(ids = ~1, data = frame, fpc = frame$fpc, strata = frame$Stratum)
sqrt(svymean(frame$SqResoutput, design = ResSqVec)) # remember to Square the Standard Error


