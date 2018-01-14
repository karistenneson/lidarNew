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
BioMass.Mod.trans$namesx[which(BioMass.Mod.trans$probne0>0.5)][-1]
BioMass.Mod.trans.no$namesx[which(BioMass.Mod.trans.no$probne0>0.5)][-1]
BioMass.Mod.trans.noInt$namesx[which(BioMass.Mod.trans.noInt$probne0>0.5)][-1]

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
DATA.mod.trans<-DATA.mod
DATA.mod.trans$STBIOMSha <- DATA.mod.trans$STBIOMSha + 1
DATA.mod.trans$Elev_mode<- DATA.mod.trans$Elev_mode + 1
DATA.mod.trans$Elev_P01 <- DATA.mod.trans$Elev_P01 + 1
DATA.mod.trans$Elev_P05 <- DATA.mod.trans$Elev_P05 + 1
DATA.mod.trans$Elev_P10 <- DATA.mod.trans$Elev_P10 + 1
DATA.mod.trans$Elev_P30 <- DATA.mod.trans$Elev_P30 + 1
DATA.mod.trans$Elev_P60 <- DATA.mod.trans$Elev_P60 + 1
DATA.mod.trans$Elev_P90 <- DATA.mod.trans$Elev_P90 + 1
DATA.mod.trans$Pct_all_returns_above_ht <- DATA.mod.trans$Pct_all_returns_above_ht + 1           
DATA.mod.trans$all_returns_above_ht_div_Total_first_returns_x_100 <- DATA.mod.trans$all_returns_above_ht_div_Total_first_returns_x_100 + 1 
DATA.mod.trans$pct_all_returns_above_mean<- DATA.mod.trans$pct_all_returns_above_mean + 1
DATA.mod.trans$All_returns_above_mode_div_Total_first_returns_x_100 <- DATA.mod.trans$All_returns_above_mode_div_Total_first_returns_x_100 + 1

DATA.val <- data.val[ , c(predictorSubset, 'R3ERUlabelB', 'R3ERUlabelE', 'R3ERUlabelF', 'R3ERUlabelG', 'fpc', 'Stratum')]
DATA.val.trans<-DATA.val
DATA.val.trans$STBIOMSha <- DATA.val.trans$STBIOMSha + 1
DATA.val.trans$Elev_mode <- DATA.val.trans$Elev_mode + 1
DATA.val.trans$Elev_P01 <- DATA.val.trans$Elev_P01 + 1
DATA.val.trans$Elev_P05 <- DATA.val.trans$Elev_P05 + 1
DATA.val.trans$Elev_P10 <- DATA.val.trans$Elev_P10 + 1
DATA.val.trans$Elev_P30 <- DATA.val.trans$Elev_P30 + 1
DATA.val.trans$Elev_P60 <- DATA.val.trans$Elev_P60 + 1
DATA.val.trans$Elev_P90 <- DATA.val.trans$Elev_P90 + 1
DATA.val.trans$Pct_all_returns_above_ht <- DATA.val.trans$Pct_all_returns_above_ht + 1           
DATA.val.trans$all_returns_above_ht_div_Total_first_returns_x_100 <- DATA.val.trans$all_returns_above_ht_div_Total_first_returns_x_100 + 1 
DATA.val.trans$pct_all_returns_above_mean<- DATA.val.trans$pct_all_returns_above_mean + 1
DATA.val.trans$All_returns_above_mode_div_Total_first_returns_x_100 <- DATA.val.trans$All_returns_above_mode_div_Total_first_returns_x_100 + 1

data.svy <- svydesign(ids = ~1, data = DATA.mod, fpc = DATA.mod$fpc, strata = DATA.mod$Stratum)
data.svy.trans <- svydesign(ids = ~1, data = DATA.mod.trans, fpc = DATA.mod.trans$fpc, strata = DATA.mod.trans$Stratum) 

data.svy.val <- svydesign(ids = ~1, data = DATA.val, fpc = DATA.val$fpc, strata = DATA.val$Stratum)
data.svy.val.trans <- svydesign(ids = ~1, data = DATA.val.trans, fpc = DATA.val.trans$fpc, strata = DATA.val.trans$Stratum) 

########################################
## No transformation (with Zeroes)
## log transformed models
MedianBASModel_transWeights <- svyglm(log(STBIOMSha) ~ Elev_L4 + Elev_LCV + Elev_Lskewness + Elev_Lkurtosis  + log(Elev_mode) + log(Elev_P01) + log(Elev_P30) + log(Elev_P60) + log(Elev_P90) + log(Pct_all_returns_above_ht) + log(Elev_P01)*log(pct_all_returns_above_mean) + log(Elev_P01)*log(All_returns_above_mode_div_Total_first_returns_x_100) + log(Elev_P05)*log(Pct_all_returns_above_ht) + log(Elev_P05)*log(All_returns_above_mode_div_Total_first_returns_x_100) + log(Elev_P30)*log(Pct_all_returns_above_ht) + log(Elev_P60)*log(Pct_all_returns_above_ht) + log(Elev_P60)*log(pct_all_returns_above_mean) + log(Elev_P90)*log(Pct_all_returns_above_ht) + log(Elev_P90)*log(pct_all_returns_above_mean) + elevation + aspect + NDVI_Amp + R3ERUlabelB + R3ERUlabelE + R3ERUlabelF + R3ERUlabelG, 
                                      design = data.svy.trans)

MedianBASModel_transNoWeight <- svyglm(log(STBIOMSha) ~ updateMe, 
                                      design = data.svy.trans)

MedianBASModel_transNoInt <- svyglm(log(STBIOMSha) ~ updateMe , 
                                    design = data.svy.trans)

##################################
## log transformed models
## #3
summary(MedianBASModel_transWeights); 
summary(MedianBASModel_transWeights)$aic
#Root Mean Square Error
predictedoutput <- exp(predict(MedianBASModel_transWeights, newdata=DATA.val.trans))-1
plot(predictedoutput~(DATA.val.trans$STBIOMSha-1), xlim = c(0,650), ylim = c(0,650))
abline(a = 0, b = 1, col = 'red')
Resoutput <- predictedoutput - (DATA.val.trans$STBIOMSha-1)
frame <- cbind(Resoutput, Resoutput^2, data.val [ , c('fpc', 'Stratum')])
colnames(frame)<- c("Resoutput", "ResoutputSE", "SqResoutput", "SqResoutputSE", "fpc", "Stratum")
ResSqVec <- svydesign(ids = ~1, data = frame, fpc = frame$fpc, strata = frame$Stratum)
sqrt(svymean(frame$SqResoutput, design = ResSqVec)) # remember to Square the Standard Error


## #4
summary(MedianBASModel_transNoWeight); 
summary(MedianBASModel_transNoWeight)$aic
#Root Mean Square Error
predictedoutput <- exp(predict(MedianBASModel_transNoWeight, newdata=DATA.val.trans))-1
plot(predictedoutput~(DATA.val.trans$STBIOMSha-1), xlim = c(0,650), ylim = c(0,650))
abline(a = 0, b = 1, col = 'red')
Resoutput <- predictedoutput - (DATA.val.trans$STBIOMSha-1)
frame <- cbind(Resoutput, Resoutput^2, data.val [ , c('fpc', 'Stratum')])
colnames(frame)<- c("Resoutput", "ResoutputSE", "SqResoutput", "SqResoutputSE", "fpc", "Stratum")
ResSqVec <- svydesign(ids = ~1, data = frame, fpc = frame$fpc, strata = frame$Stratum)
sqrt(svymean(frame$SqResoutput, design = ResSqVec)) # remember to Square the Standard Error


## #5
summary(MedianBASModel_transNoInt); 
summary(MedianBASModel_transNoInt)$aic
#Root Mean Square Error
predictedoutput <- exp(predict(MedianBASModel_transNoInt, newdata=DATA.val.trans))-1
plot(predictedoutput~(DATA.val.trans$STBIOMSha-1), xlim = c(0,650), ylim = c(0,650))
abline(a = 0, b = 1, col = 'red')
Resoutput <- predictedoutput - (DATA.val.trans$STBIOMSha-1)
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
HPM <- predict(BioMass.Mod.trans, estimator="HPM")
BioMass.Mod.trans$namesx[HPM$bestmodel+1][-1]

HPM <- predict(BioMass.Mod.trans.no, estimator="HPM")
BioMass.Mod.trans.no$namesx[HPM$bestmodel+1][-1]

HPM <- predict(BioMass.Mod.trans.noInt, estimator="HPM")
BioMass.Mod.trans.noInt$namesx[HPM$bestmodel+1][-1]


HPMlm.trans <- svyglm(log(STBIOMSha)~ Elev_L4  + Elev_LCV + Elev_Lskewness + Elev_Lkurtosis + log(Elev_mode) + log(Elev_P01) + log(Elev_P30) + log(Elev_P60) + log(Elev_P90) + log(Pct_all_returns_above_ht) + elevation + NDVI_Amp + R3ERUlabelB + R3ERUlabelE + R3ERUlabelF + R3ERUlabelG + log(Elev_P01)*log(Pct_all_returns_above_ht) + log(Elev_P01)*log(All_returns_above_mode_div_Total_first_returns_x_100)+ log(Elev_P05)*log(Pct_all_returns_above_ht)+ log(Elev_P05)*log(pct_all_returns_above_mean)+ log(Elev_P05)*log(All_returns_above_mode_div_Total_first_returns_x_100) + log(Elev_P10)*log(Pct_all_returns_above_ht) + log(Elev_P30)*log(Pct_all_returns_above_ht) + log(Elev_P60)*log(Pct_all_returns_above_ht) + log(Elev_P60)*log(pct_all_returns_above_mean) + log(Elev_P90)*log(Pct_all_returns_above_ht) + log(Elev_P90)*log(pct_all_returns_above_mean), 
               design = data.svy.trans)

HPMlm.trans.no <- svyglm(log(STBIOMSha)~ update, 
               design = data.svy.trans)

HPMlm.trans.noInt <- svyglm(log(STBIOMSha)~ update, 
                           design = data.svy.trans)

##################################
## log transformed models
## #3
summary(HPMlm.trans); 
summary(HPMlm.trans)$aic
#Root Mean Square Error
predictedoutput <- exp(predict(HPMlm.trans, newdata=DATA.val.trans))-1
plot(predictedoutput~(DATA.val.trans$STBIOMSha-1), xlim = c(0,650), ylim = c(0,650))
abline(a = 0, b = 1, col = 'red')
Resoutput <- predictedoutput - (DATA.val.trans$STBIOMSha-1)
frame <- cbind(Resoutput, Resoutput^2, data.val[ , c('fpc', 'Stratum')])
colnames(frame)<- c("Resoutput", "ResoutputSE", "SqResoutput", "SqResoutputSE", "fpc", "Stratum")
ResSqVec <- svydesign(ids = ~1, data = frame, fpc = frame$fpc, strata = frame$Stratum)
sqrt(svymean(frame$SqResoutput, design = ResSqVec)) # remember to Square the Standard Error


## #4
summary(HPMlm.trans.no); 
summary(HPMlm.trans.no)$aic
HPMlm_TransNoWts.res <- predict(HPMlm.trans.no, newdata=DATA.val.trans)
#Root Mean Square Error
predictedoutput <- exp(predict(HPMlm.trans.no, newdata=DATA.val.trans))-1
plot(predictedoutput~(DATA.val.trans$STBIOMSha-1), xlim = c(0,650), ylim = c(0,650))
abline(a = 0, b = 1, col = 'red')
Resoutput <- predictedoutput - (DATA.val.trans$STBIOMSha-1)
frame <- cbind(Resoutput, Resoutput^2, data.val[ , c('fpc', 'Stratum')])
colnames(frame)<- c("Resoutput", "ResoutputSE", "SqResoutput", "SqResoutputSE", "fpc", "Stratum")
ResSqVec <- svydesign(ids = ~1, data = frame, fpc = frame$fpc, strata = frame$Stratum)
sqrt(svymean(frame$SqResoutput, design = ResSqVec)) # remember to Square the Standard Error


## #5
summary(HPMlm.trans.noInt); 
summary(HPMlm.trans.noInt)$aic

#Root Mean Square Error
predictedoutput <- exp(predict(HPMlm.trans.noInt, newdata=DATA.val.trans))-1
plot(predictedoutput~(DATA.val.trans$STBIOMSha-1), xlim = c(0,650), ylim = c(0,650))
abline(a = 0, b = 1, col = 'red')
Resoutput <- predictedoutput - (DATA.val.trans$STBIOMSha-1)
frame <- cbind(Resoutput, Resoutput^2, data.val[ , c('fpc', 'Stratum')])
colnames(frame)<- c("Resoutput", "ResoutputSE", "SqResoutput", "SqResoutputSE", "fpc", "Stratum")
ResSqVec <- svydesign(ids = ~1, data = frame, fpc = frame$fpc, strata = frame$Stratum)
sqrt(svymean(frame$SqResoutput, design = ResSqVec)) # remember to Square the Standard Error


##################################
######################################################################################################
##################################
########################################
########################################
## No transformation (After Removing Zeroes)
MedianBASModel_Weights.no0 <- svyglm(STBIOMSha ~ Elev_L4 + Elev_LCV +
                                       Elev_P01 + Elev_P05 + Elev_P10 + Elev_P30 + Elev_P90 + 
                                       all_returns_above_ht_div_Total_first_returns_x_100 + All_returns_above_mode_div_Total_first_returns_x_100 + 
                                       aspect + NDVI_Amp + 
                                       Elev_mode*Pct_all_returns_above_ht + 
                                       Elev_mode*pct_all_returns_above_mean + Elev_mode*All_returns_above_mode_div_Total_first_returns_x_100 + 
                                       Elev_P01*Pct_all_returns_above_ht + Elev_P01*pct_all_returns_above_mean + 
                                       Elev_P01*all_returns_above_ht_div_Total_first_returns_x_100 + Elev_P01*All_returns_above_mode_div_Total_first_returns_x_100 + Elev_P05:Pct_all_returns_above_ht + Elev_P05*pct_all_returns_above_mean + Elev_P05*All_returns_above_mode_div_Total_first_returns_x_100 + Elev_P10*Pct_all_returns_above_ht + Elev_P10*All_returns_above_mode_div_Total_first_returns_x_100 + Elev_P30*pct_all_returns_above_mean + Elev_P30*All_returns_above_mode_div_Total_first_returns_x_100 + Elev_P60*all_returns_above_ht_div_Total_first_returns_x_100 + R3ERUlabelE + R3ERUlabelF + R3ERUlabelG 
                                     , 
                                     design = data.svy)

MedianBASModel_NoWeight.no0 <- svyglm(STBIOMSha ~ Elev_MAD_median + Elev_L3 + Elev_Lskewness + Elev_P60 + Elev_P30*pct_all_returns_above_mean + Elev_P90*Pct_all_returns_above_ht, 
                                      design = data.svy)

## log transformed models
MedianBASModel_transWeights.no0 <- svyglm(log(STBIOMSha) ~ Elev_kurtosis + Elev_LCV + Elev_L3 + Elev_Lskewness + Elev_P05 + Elev_P90 + Pct_all_returns_above_ht + all_returns_above_ht_div_Total_first_returns_x_100 + slope + NDVI_Amp + Elev_mode*All_returns_above_mode_div_Total_first_returns_x_100 + Elev_P01*pct_all_returns_above_mean + Elev_P01*All_returns_above_mode_div_Total_first_returns_x_100 + Elev_P05*Pct_all_returns_above_ht + Elev_P05*all_returns_above_ht_div_Total_first_returns_x_100 + Elev_P10*all_returns_above_ht_div_Total_first_returns_x_100 + Elev_P10*All_returns_above_mode_div_Total_first_returns_x_100 + Elev_P30*all_returns_above_ht_div_Total_first_returns_x_100 + Elev_P30*All_returns_above_mode_div_Total_first_returns_x_100 + Elev_P60*Pct_all_returns_above_ht + Elev_P60*All_returns_above_mode_div_Total_first_returns_x_100 + Elev_P90*All_returns_above_mode_div_Total_first_returns_x_100 + R3ERUlabelB + R3ERUlabelE + R3ERUlabelF + R3ERUlabelG, 
                                          design = data.svy.trans)

MedianBASModel_transNoWeight.no0 <- svyglm(log(STBIOMSha) ~ Elev_stddev + Elev_P05 + Elev_P90 + Pct_all_returns_above_ht + all_returns_above_ht_div_Total_first_returns_x_100 + elevation + slope + Elev_P90*All_returns_above_mode_div_Total_first_returns_x_100 + R3ERUlabelG, 
                                           design = data.svy.trans)

MedianBASModel_transNoInt.no0 <- svyglm(log(STBIOMSha) ~ Elev_stddev + Elev_MAD_median  + Elev_LCV + Elev_P05 + Elev_P90 + Pct_all_returns_above_ht + elevation + slope, 
                                        design = data.svy.trans)

##################################
## No transformation
summary(MedianBASModel_Weights.no0); 
summary(MedianBASModel_Weights.no0)$aic
#Root Mean Square Error
predictedoutput <- predict(MedianBASModel_Weights.no0, newdata=DATA.val)
plot(predictedoutput~(DATA.val$STBIOMSha), xlim = c(0,650), ylim = c(0,650))
abline(a = 0, b = 1, col = 'red')
Resoutput <- predictedoutput - (DATA.val.trans$STBIOMSha)
frame <- cbind(Resoutput, Resoutput^2, data.val[ , c('fpc', 'Stratum')])
colnames(frame)<- c("Resoutput", "ResoutputSE", "SqResoutput", "SqResoutputSE", "fpc", "Stratum")
ResSqVec <- svydesign(ids = ~1, data = frame, fpc = frame$fpc, strata = frame$Stratum)
sqrt(svymean(frame$SqResoutput, design = ResSqVec))# remember to Square the Standard Error


summary(MedianBASModel_NoWeight.no0); 
summary(MedianBASModel_NoWeight.no0)$aic
#Root Mean Square Error
predictedoutput <- predict(MedianBASModel_NoWeight.no0, newdata=DATA.val)
plot(predictedoutput~(DATA.val$STBIOMSha), xlim = c(0,650), ylim = c(0,650))
abline(a = 0, b = 1, col = 'red')
Resoutput <- predictedoutput - (DATA.val.trans$STBIOMSha)
frame <- cbind(Resoutput, Resoutput^2, data.val[ , c('fpc', 'Stratum')])
colnames(frame)<- c("Resoutput", "ResoutputSE", "SqResoutput", "SqResoutputSE", "fpc", "Stratum")
ResSqVec <- svydesign(ids = ~1, data = frame, fpc = frame$fpc, strata = frame$Stratum)
sqrt(svymean(frame$SqResoutput, design = ResSqVec))# remember to Square the Standard Error


## log transformed models
summary(MedianBASModel_transWeights.no0); 
summary(MedianBASModel_transWeights.no0)$aic
#Root Mean Square Error
predictedoutput <- exp(predict(MedianBASModel_transWeights.no0, newdata=DATA.val.trans))-1
plot(predictedoutput~(DATA.val.trans$STBIOMSha-1), xlim = c(0,650), ylim = c(0,650))
abline(a = 0, b = 1, col = 'red')
Resoutput <- predictedoutput - (DATA.val.trans$STBIOMSha-1)
frame <- cbind(Resoutput, Resoutput^2, data.val[ , c('fpc', 'Stratum')])
colnames(frame)<- c("Resoutput", "ResoutputSE", "SqResoutput", "SqResoutputSE", "fpc", "Stratum")
ResSqVec <- svydesign(ids = ~1, data = frame, fpc = frame$fpc, strata = frame$Stratum)
sqrt(svymean(frame$SqResoutput, design = ResSqVec))# remember to Square the Standard Error


summary(MedianBASModel_transNoWeight.no0); 
summary(MedianBASModel_transNoWeight.no0)$aic
#Root Mean Square Error
predictedoutput <- exp(predict(MedianBASModel_transNoWeight.no0, newdata=DATA.val.trans))-1
plot(predictedoutput~(DATA.val.trans$STBIOMSha-1), xlim = c(0,650), ylim = c(0,650))
abline(a = 0, b = 1, col = 'red')
Resoutput <- predictedoutput - (DATA.val.trans$STBIOMSha-1)
frame <- cbind(Resoutput, Resoutput^2, data.val[ , c('fpc', 'Stratum')])
colnames(frame)<- c("Resoutput", "ResoutputSE", "SqResoutput", "SqResoutputSE", "fpc", "Stratum")
ResSqVec <- svydesign(ids = ~1, data = frame, fpc = frame$fpc, strata = frame$Stratum)
sqrt(svymean(frame$SqResoutput, design = ResSqVec))# remember to Square the Standard Error


summary(MedianBASModel_transNoInt.no0); 
summary(MedianBASModel_transNoInt.no0)$aic
#Root Mean Square Error
predictedoutput <- exp(predict(MedianBASModel_transNoInt.no0, newdata=DATA.val.trans))-1
plot(predictedoutput~(DATA.val.trans$STBIOMSha-1), xlim = c(0,650), ylim = c(0,650))
abline(a = 0, b = 1, col = 'red')
Resoutput <- predictedoutput - (DATA.val.trans$STBIOMSha-1)
frame <- cbind(Resoutput, Resoutput^2, data.val[ , c('fpc', 'Stratum')])
colnames(frame)<- c("Resoutput", "ResoutputSE", "SqResoutput", "SqResoutputSE", "fpc", "Stratum")
ResSqVec <- svydesign(ids = ~1, data = frame, fpc = frame$fpc, strata = frame$Stratum)
sqrt(svymean(frame$SqResoutput, design = ResSqVec))# remember to Square the Standard Error



### models built on data without zeroes.

HPMlm.no0 <- svyglm(STBIOMSha ~ Elev_L4 + Elev_LCV + Elev_P01 + Elev_P05 + Elev_P10 + Elev_P30 + Elev_P90 + all_returns_above_ht_div_Total_first_returns_x_100 + All_returns_above_mode_div_Total_first_returns_x_100 + aspect + NDVI_Amp + Elev_mode*Pct_all_returns_above_ht + Elev_mode*pct_all_returns_above_mean + Elev_mode*All_returns_above_mode_div_Total_first_returns_x_100 + Elev_P01*Pct_all_returns_above_ht + Elev_P01*pct_all_returns_above_mean + Elev_P01*all_returns_above_ht_div_Total_first_returns_x_100 + Elev_P01*All_returns_above_mode_div_Total_first_returns_x_100 + Elev_P05*Pct_all_returns_above_ht + Elev_P05*pct_all_returns_above_mean + Elev_P05*All_returns_above_mode_div_Total_first_returns_x_100 + Elev_P10*Pct_all_returns_above_ht + Elev_P10*All_returns_above_mode_div_Total_first_returns_x_100 + Elev_P30*pct_all_returns_above_mean + Elev_P30*All_returns_above_mode_div_Total_first_returns_x_100 + Elev_P60*all_returns_above_ht_div_Total_first_returns_x_100 + R3ERUlabelE + R3ERUlabelF + R3ERUlabelG, 
                design = data.svy)

HPMlm.no.no0 <- svyglm(STBIOMSha ~ Elev_MAD_median + Elev_L3 + Elev_Lskewness + Elev_Lkurtosis + Elev_P60 + Elev_P10*all_returns_above_ht_div_Total_first_returns_x_100 + Elev_P30*pct_all_returns_above_mean + Elev_P90*Pct_all_returns_above_ht, 
                   design = data.svy)

HPMlm.trans.no0 <- svyglm(log(STBIOMSha)~ Elev_kurtosis + Elev_L3 + Elev_LCV + Elev_Lskewness + Elev_P05 + Elev_P90 + Pct_all_returns_above_ht + all_returns_above_ht_div_Total_first_returns_x_100 + slope + NDVI_Amp + Elev_mode*All_returns_above_mode_div_Total_first_returns_x_100 + Elev_P01*pct_all_returns_above_mean + Elev_P01*All_returns_above_mode_div_Total_first_returns_x_100 + Elev_P05*Pct_all_returns_above_ht + Elev_P05*all_returns_above_ht_div_Total_first_returns_x_100 + Elev_P10*all_returns_above_ht_div_Total_first_returns_x_100 + Elev_P10*All_returns_above_mode_div_Total_first_returns_x_100 + Elev_P30*all_returns_above_ht_div_Total_first_returns_x_100 + Elev_P30*All_returns_above_mode_div_Total_first_returns_x_100 + Elev_P60*Pct_all_returns_above_ht + Elev_P60*pct_all_returns_above_mean + Elev_P60*All_returns_above_mode_div_Total_first_returns_x_100 + Elev_P90*All_returns_above_mode_div_Total_first_returns_x_100 + R3ERUlabelB + R3ERUlabelE + R3ERUlabelF + R3ERUlabelG, 
                      design = data.svy.trans)

HPMlm.trans.no.no0 <- svyglm(log(STBIOMSha)~ Elev_stddev + Elev_MAD_median + Elev_P05 + Elev_P90 + Pct_all_returns_above_ht + all_returns_above_ht_div_Total_first_returns_x_100 + elevation + slope + Elev_P60*all_returns_above_ht_div_Total_first_returns_x_100 + Elev_P90*Pct_all_returns_above_ht, 
                         design = data.svy.trans)

HPMlm.trans.noInt.no0 <- svyglm(log(STBIOMSha)~ Elev_stddev + Elev_LCV + Elev_P05 + Elev_P60 + Elev_P90 + Pct_all_returns_above_ht + elevation + slope, 
                            design = data.svy.trans)

##################################
## No transformation
## Model #1
summary(HPMlm.no0); 
summary(HPMlm.no0)$aic
#Root Mean Square Error
predictedoutput <- predict(HPMlm.no0, newdata=DATA.val)
plot(predictedoutput~(DATA.val$STBIOMSha), xlim = c(0,650), ylim = c(0,650))
abline(a = 0, b = 1, col = 'red')
Resoutput <- predictedoutput - (DATA.val.trans$STBIOMSha)
frame <- cbind(Resoutput, Resoutput^2, data.val[ , c('fpc', 'Stratum')])
colnames(frame)<- c("Resoutput", "ResoutputSE", "SqResoutput", "SqResoutputSE", "fpc", "Stratum")
ResSqVec <- svydesign(ids = ~1, data = frame, fpc = frame$fpc, strata = frame$Stratum)
sqrt(svymean(frame$SqResoutput, design = ResSqVec))# remember to Square the Standard Error

## Model #2
summary(HPMlm.no.no0); 
summary(HPMlm.no.no0)$aic
#Root Mean Square Error
predictedoutput <- predict(HPMlm.no.no0, newdata=DATA.val)
plot(predictedoutput~(DATA.val.trans$STBIOMSha), xlim = c(0,650), ylim = c(0,650))
abline(a = 0, b = 1, col = 'red')
Resoutput <- predictedoutput - (DATA.val.trans$STBIOMSha)
frame <- cbind(Resoutput, Resoutput^2, data.val[ , c('fpc', 'Stratum')])
colnames(frame)<- c("Resoutput", "ResoutputSE", "SqResoutput", "SqResoutputSE", "fpc", "Stratum")
ResSqVec <- svydesign(ids = ~1, data = frame, fpc = frame$fpc, strata = frame$Stratum)
sqrt(svymean(frame$SqResoutput, design = ResSqVec))# remember to Square the Standard Error

## log transformed models
## Model #3
summary(HPMlm.trans.no0); 
summary(HPMlm.trans.no0)$aic
#Root Mean Square Error
predictedoutput <- exp(predict(HPMlm.trans.no0, newdata=DATA.val.trans))-1
plot(predictedoutput~(DATA.val.trans$STBIOMSha-1), xlim = c(0,650), ylim = c(0,650))
abline(a = 0, b = 1, col = 'red')
Resoutput <- predictedoutput - (DATA.val.trans$STBIOMSha-1)
frame <- cbind(Resoutput, Resoutput^2, data.val[ , c('fpc', 'Stratum')])
colnames(frame)<- c("Resoutput", "ResoutputSE", "SqResoutput", "SqResoutputSE", "fpc", "Stratum")
ResSqVec <- svydesign(ids = ~1, data = frame, fpc = frame$fpc, strata = frame$Stratum)
sqrt(svymean(frame$SqResoutput, design = ResSqVec))# remember to Square the Standard Error

## Model #4
summary(HPMlm.trans.no.no0); 
summary(HPMlm.trans.no.no0)$aic
#Root Mean Square Error
predictedoutput <- exp(predict(HPMlm.trans.no.no0, newdata=DATA.val.trans))-1
plot(predictedoutput~(DATA.val.trans$STBIOMSha-1), xlim = c(0,650), ylim = c(0,650))
abline(a = 0, b = 1, col = 'red')
Resoutput <- predictedoutput - (DATA.val.trans$STBIOMSha-1)
frame <- cbind(Resoutput, Resoutput^2, data.val[ , c('fpc', 'Stratum')])
colnames(frame)<- c("Resoutput", "ResoutputSE", "SqResoutput", "SqResoutputSE", "fpc", "Stratum")
ResSqVec <- svydesign(ids = ~1, data = frame, fpc = frame$fpc, strata = frame$Stratum)
sqrt(svymean(frame$SqResoutput, design = ResSqVec))# remember to Square the Standard Error

## Model #5
summary(HPMlm.trans.noInt.no0); 
summary(HPMlm.trans.noInt.no0)$aic
#########################
#Root Mean Square Error
predictedoutput <- exp(predict(HPMlm.trans.noInt.no0, newdata=DATA.val.trans))-1
plot(predictedoutput~(DATA.val.trans$STBIOMSha-1), xlim = c(0,650), ylim = c(0,650))
abline(a = 0, b = 1, col = 'red')
Resoutput <- predictedoutput - (DATA.val.trans$STBIOMSha-1)
frame <- cbind(Resoutput, Resoutput^2, data.val[ , c('fpc', 'Stratum')])
colnames(frame)<- c("Resoutput", "ResoutputSE", "SqResoutput", "SqResoutputSE", "fpc", "Stratum")
ResSqVec <- svydesign(ids = ~1, data = frame, fpc = frame$fpc, strata = frame$Stratum)
sqrt(svymean(frame$SqResoutput, design = ResSqVec)) # remember to Square the Standard Error


#Mean Summed Error
mape(DATA.val.trans$STBIOMSha-1, exp(HPMlm_TransNoWtsInt.no0.res), includeSE=T)
#Mean Bias Error
sum((DATA.val.trans$STBIOMSha-1) - HPMlm_TransNoWtsInt.no0.res)/length(HPMlm_TransNoWtsInt.no0.res)

####################################################
####################################################
# Best Predictive Model, closest to BMA in terms of squared error loss, takes a pretty long time to find. 

BPM <- predict(BioMass.Mod, estimator="BPM") #not running, grrrr
BioMass.Mod$namesx[BPM$bestmodel+1][-1]

##################################
## Diagnostics
plot(MPMlm, ask=F) # This model is pretty solid, better redisdual distribution than HPM, and better adjusted R^2 


########################################
################################################################################
############################################################################################
############################################################################################
#################################################################################################
######################################################################################################
#################################################################################################
############################################################################################
############################################################################################
########################################

### Same business, but for Total Cubit Feet of wood.

# Full variable pool, truncated poisson prior, hyper-g
TCUFT.Mod <- bas.lm(log(TCUFT)~ . -STBIOMSha, 
                      data=DATA.mod, 
                      prior="hyper-g",
                      alpha = 3,
                      modelprior=tr.poisson(10,30),
                      method="MCMC+BAS")

summary(TCUFT.Mod)

# What are the median model variables
TCUFT.Mod$namesx[which(TCUFT.Mod$probne0>0.5)][-1]

# Highest Probability Model
HPM <- predict(TCUFT.Mod, estimator="HPM")
TCUFT.Mod$namesx[HPM$bestmodel+1][-1]

HPMlm <- lm(log(TCUFT)~ Elev_skewness + Elev_P80 + Pct_first_returns_above_ht + Pct_all_returns_above_ht +
            All_returns_above_ht + Pct_first_returns_above_mean + All_returns_above_mean_div_Total_first_returns_x_100 + 
              Total_first_returns + Total_all_returns + R3ERUCODE + elevation + slope, data=DATA.mod)

summary(HPMlm)
plot(HPMlm, ask=F)

# Median Probability Model 
MPM <- predict(TCUFT.Mod, estimator="MPM")
TCUFT.Mod$namesx[MPM$bestmodel+1][-1]

MPMlm <- lm(log(TCUFT)~ Elev_skewness + Elev_P80 + Pct_first_returns_above_ht + Pct_all_returns_above_ht +
              All_returns_above_ht + Pct_first_returns_above_mean + All_returns_above_mean_div_Total_first_returns_x_100 + 
              Total_first_returns + Total_all_returns + R3ERUCODE + Forest + elevation + slope, data=DATA.mod)

summary(MPMlm)
plot(MPMlm, ask=F)

# Best Predictive Model, closest to BMA in terms of squared error loss, takes a pretty long time to find. 

BPM <- predict(BioMass.Mod, estimator="BPM") #not running, grrrr

### Combined final model?
cor.test(DATA.mod$STBIOMSha, DATA.mod$TCUFT)
# Standing biomass and volume are 0.98 correlated. 

FinModVarFull <- select(DATA.mod, Elev_skewness, Elev_CV, Elev_LCV, Elev_P80, Pct_first_returns_above_ht, 
                    Pct_all_returns_above_ht, All_returns_above_ht, Pct_first_returns_above_mean, 
                    All_returns_above_mean_div_Total_first_returns_x_100, Total_first_returns,
                    Total_all_returns, R3ERUCODE, elevation, slope, PlotSizeAcres, Forest)

corrgram(FinModVarFull, order=T, lower.panel=panel.ellipse,
         upper.panel=panel.cor, text.panel=panel.txt,
         main="Lidar Predictor Data in PC2/PC1 Order") # Trim until no correlations above ~0.5

FinModVarTrim <- select(DATA.mod, Elev_skewness, Elev_LCV, Elev_P80, Total_first_returns, 
                        Pct_first_returns_above_ht, R3ERUCODE, elevation, slope, PlotSizeAcres, Forest)

corrgram(FinModVarTrim, order=T, lower.panel=panel.ellipse,
         upper.panel=panel.cor, text.panel=panel.txt,
         main="Lidar Predictor Data in PC2/PC1 Order") # Trim until no correlations above ~0.5

# Final Model Performance

FinModB <- lm(log(STBIOMSha)~ Elev_skewness + Elev_LCV + Elev_P80 + Total_first_returns + 
                Pct_all_returns_above_ht + elevation + slope + PlotSizeAcres + Forest + R3ERUCODE , 
              data=DATA.mod)
summary(FinModB)
plot(FinModB, ask=F)

FinModT <- lm(log(TCUFT)~ Elev_skewness + Elev_LCV + Elev_P80 + Total_first_returns + 
                Pct_all_returns_above_ht + elevation + slope + PlotSizeAcres + Forest + R3ERUCODE , 
              data=DATA.mod)
summary(FinModT)
plot(FinModT, ask=F)

# Possible Outliers
Outliers <- c("53","1148","1238","1242","2145", "2651")
Outliers <- which(rownames(DATA.mod) %in% Outliers)
DATA.mod[Outliers, c(1,2)] #most low values, though 1238 has a fair bit of Biomass. 
DATA.mod.t <- DATA.mod[-Outliers,]

# Final Model Performance

FinModBt <- lm(log(STBIOMSha)~ Elev_skewness + Elev_LCV + Elev_P80 + Total_first_returns + 
                Pct_all_returns_above_ht + elevation + slope + PlotSizeAcres + Forest + R3ERUCODE , 
              data=DATA.mod.t)
summary(FinModBt)
plot(FinModBt, ask=F)

FinModTt <- lm(log(TCUFT)~ Elev_skewness + Elev_LCV + Elev_P80 + Total_first_returns + 
                Pct_all_returns_above_ht + elevation + slope + PlotSizeAcres + Forest + R3ERUCODE , 
              data=DATA.mod.t)
summary(FinModTt)
plot(FinModTt, ask=F)


# ### RObust versions of final model.
# 
# # Standing biomass and volume are 0.98 correlated. 
# 
# FinModB.r <- lmrob(log(STBIOMSha)~ Elev_LCV + Elev_skewness + Elev_P80 + Pct_all_returns_above_ht +
#                 Total_first_returns + R3ERUCODE + elevation + slope + PlotSizeAcres, data=DATA.mod, 
#                 setting = "KS2014", fast.s.large.n = Inf)
# summary(FinModB.r)
# plot(FinModB.r, ask=F)
# 
# FinModT.r <- lmrob(log(TCUFT)~ Elev_LCV + Elev_skewness + Elev_P80 + Pct_all_returns_above_ht +
#                 Total_first_returns + R3ERUCODE + elevation + slope + PlotSizeAcres, data=DATA.mod, 
#                 setting = "KS2014", fast.s.large.n = Inf)
# summary(FinModT.r)
# plot(FinModT.r, ask=F)
