### This file is for developing the BMA components of the analysis
## Written by MS Patterson (maspatte@uw.edu)
# Last updated: July 11, 2017

setwd('C:\\Users\\krtenneson\\Desktop\\lidarModel\\VersionControl\\lidarNew')
setwd('\\\\166.2.126.25\\rseat\\Programs\\Reimbursibles\\fy2016\\R3_lidar_equation_transferability\\Analysis\\VersionControl\\lidarNew')
setwd("~/Documents/R/lidarNew/scripts") #Mac
setwd("~/R/lidarNew/scripts") #WinCampus
setwd("~/R/projects/lidarNew/scripts") #WinHome

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

#Bring in Environmental Data
Aux <- read_csv("../Data/Auxillary/Merged_10172017.csv")
AuxTrim <- select(Aux, PLOT_ID, R3ERUCODE, elevation, aspect, slope)
AllData <- merge(AllData, AuxTrim, by="PLOT_ID", all=F)
AllData$R3ERUCODE <- as.factor(AllData$R3ERUCODE)

NDVI <- read_csv("../Data/Auxillary/NDVIamplitude.csv")

AllData <- merge(AllData, select(NDVI, PLOT_ID, NDVI_Sample_NDVI_amplitude_1), by="PLOT_ID", all=F)
AllData <- rename(AllData, NDVI_Amp=NDVI_Sample_NDVI_amplitude_1)

### Partion data
Variables <- colnames(AllData) #pull variable names for use with select()
LidarNames <- c(Variables[23:75]) #subset of lidar metrics
FieldNames <- Variables[1:20] #subset of field variables
AuxNames <- Variables[72:76] #subset of additional environmental variables
RandUnif <- Variables[11] #The Random Uniform Variable

### Need to add in bit here for selecting rows using the Random Uniform, 
### or alternately add that to the Independent variables

STBIOMS <- AllData$STBIOMS #Standing biomass
TCUFT <- AllData$TCUFT #Total timber volume

PRED <- select(AllData, LidarNames, AuxNames, FieldNames[9], RandUnif) #Select data and fields to work with. 

### Exploring relationships between predictors
corrgram(PRED, order=T, lower.panel=panel.shade,
         upper.panel=panel.ellipse, text.panel=panel.txt,
         main="Lidar Predictor Data in PC2/PC1 Order")
# A lot of positive correlation, but some things relatively uncorrelated with
# everything else, like Kurtosis. Total returns also only weakly correlated with
# most everything else. 


### Model development
### Filter data down for development, package development
DATA.test <- AllData[AllData$RandomUniform<0.4,]

STBIOMS.test <- DATA.test$STBIOMS #Standing biomass
TCUFT.test <- DATA.test$TCUFT #Total timber volume

PRED.test <- select(DATA.test, LidarNames, AuxNames, FieldNames[9]) #Select lidar metric data

BMod <- cbind(STBIOMS.test, PRED.test)

# Centering data
BModC <- cbind(STBIOMS.test, center(PRED.test[,-50]), PRED.test[50])

# # Full variable pool
# BioBLM <- bas.lm(log(STBIOMS.test+.00001)~ ., 
#                  data=BMod, 
#                  prior="g-prior", 
#                  modelprior=uniform())
# 
# summary(BioBLM)
# PredNames[which(BioBLM$probne0>0.5)-1]
# 
# plot(BioBLM, ask=F)
# 
# #Plot CI of all parameters
# plot(confint(coef(BioBLM), parm=2:50))
# 
# #Confidence Intervals on median/highest probability model
# plot(confint(coef(BioBLM, estimator="MPM")))
# plot(confint(coef(BioBLM, estimator="HPM")))
# 
# # Full variable pool, centered
# BioBLMc <- bas.lm(log(STBIOMS.test+.00001)~ ., 
#                  data=BModC, 
#                  prior="g-prior", 
#                  modelprior=uniform())
# 
# summary(BioBLMc)
# PredNames[which(BioBLMc$probne0>0.5)-1]
# 
# plot(BioBLMc, ask=F)
# 
# #Plot CI of all parameters
# plot(confint(coef(BioBLMc), parm=2:50))
# 
# #Confidence Intervals on median/highest probability model
# plot(confint(coef(BioBLMc, estimator="MPM")))
# plot(confint(coef(BioBLMc, estimator="HPM")))
# 
# # Full variable pool, centered, truncated poisson prior, g = N (unit information prior)
# BioBLMp <- bas.lm(log(STBIOMS.test+.00001)~ ., 
#                   data=BModC, 
#                   prior="g-prior",
#                   alpha = nrow(BModC),
#                   modelprior=tr.poisson(9,30))
# 
# summary(BioBLMp)
# PredNames[which(BioBLMp$probne0>0.5)-1]
# 
# plot(BioBLMc, ask=F)
# 
# Full variable pool, centered, truncated poisson prior, hyper-g
BioBLMhc <- bas.lm(log(STBIOMS.test+.00001)~ .,
                   data=BMod,
                   prior="hyper-g",
                   alpha = 3,
                   modelprior=tr.poisson(10,30),
                   method="MCMC")

summary(BioBLMhc)
BioBLMhc$namesx[which(BioBLMhc$probne0>0.5)][-1]

# Full variable pool, truncated poisson prior, hyper-g
BioBLMh <- bas.lm(log(STBIOMS.test+.00001)~ ., 
                  data=BMod, 
                  prior="hyper-g",
                  alpha = 3,
                  modelprior=tr.poisson(10,30),
                  method="MCMC")

summary(BioBLMh)
BioBLMh$namesx[which(BioBLMh$probne0>0.5)][-1]


# Correlation in variables in the median model
MedModVar <- select(BMod, BioBLMh$namesx[which(BioBLMh$probne0>0.5)][-1])

corrgram(MedModVar, order=T, lower.panel=panel.ellipse,
         upper.panel=panel.cor, text.panel=panel.txt,
         main="Lidar Predictor Data in PC2/PC1 Order")

#Fairly strong correlation between Elev_L2 and Elev_variance, as well as pct_all... and All_returns_above... 
#some odd relationships!

# Model diagnostics
plot(BioBLMh, ask=F)
plot(BioBLMhc, ask=F)

#pull some models out using predict functions

# Highest Probability Model
HPM <- predict(BioBLMh, estimator="HPM")
BioBLMh$namesx[HPM$bestmodel+1][-1]

HPMlm <- lm(log(STBIOMS.test +0.00001)~ , data=Bmod)

summary(HPMlm)
plot(HPMlm, ask=F)

# Median Probability Model (same as Highest Probability Model)
MPM <- predict(BioBLMh, estimator="MPM")
BioBLMh$namesx[MPM$bestmodel+1][-1]

MPMlm <- lm(log(STBIOMS.test +0.00001)~ Elev_ave + Elev_variance + Elev_P05 +
              Elev_P50 + Pct_all_returns_above_ht + Pct_first_returns_above_mean +
              pct_all_returns_above_mean + R3ERUCODE + elevation + slope, data=BMod)

summary(MPMlm)
plot(MPMlm, ask=F)

# Best Predictive Model, closest to BMA in terms of squared error loss, takes a pretty long time to find. 
BPM <- predict(BioBLMh, estimator="BPM")
BioBLMh$namesx[BPM$bestmodel+1][-1] # Best Predictive model is quite large, has 19 variables. 

BPMlm <- lm(log(STBIOMS.test +0.00001)~ Elev_variance + Elev_CV + Elev_MAD_mode + Elev_L2 + Elev_Lskewness + Elev_Lkurtosis +
Elev_P40 + Elev_P50 + Elev_P75 + Elev_P80 + Pct_all_returns_above_ht + pct_all_returns_above_mean +
All_returns_above_mean_div_Total_first_returns_x_100 + First_returns_above_mode + 
All_returns_above_mean + Total_all_returns + elevation + slope + NDVI_Amp, data=BMod)

summary(BPMlm)
plot(BPMlm, ask=F) #Slightly better fit with residuals than model below. 

#A possible final model? 
BMmodel <- lm(log(STBIOMS.test +0.00001)~ Elev_CV + Elev_L2 + Elev_Lskewness + Elev_Lkurtosis + pct_all_returns_above_mean +
                elevation + slope, data=BMod)
summary(BMmodel)
plot(BMmodel) # Some slight non-linearities in residuals and QQ. 

### Generate Predictions

# This uses the Bayesian model object
ValPred <- predict(BioBLMh, newdata= , estimator= )

### Validate Predictions against Validation set
