library(ggplot2)

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
#library(robustbase)

### load data
data.mod <- read.csv('Data//datamod.csv')
data.val <- read.csv('Data//dataval.csv')
data.val.ind <- read.csv('Data//datavalind.csv')

par(mfrow = c(2,2))

# height metrics
max(data.mod$STBIOMSha);max(data.val$STBIOMSha)
data.mod$ID[data.mod$STBIOMSha >600]
data.val$ID[data.val$STBIOMSha >600]
data.val$ID[data.val.ind$STBIOMSha >381]
data.val$Forest[data.val.ind$STBIOMSha >381]
plot(data.mod$STBIOMSha~data.mod$Elev_mode)
plot(data.val$STBIOMSha~data.val$Elev_mode)
plot(data.val.ind$STBIOMSha~data.val.ind$Elev_mode)
plot(0,0)

plot(data.mod$STBIOMSha~data.mod$Elev_P01)
plot(data.val$STBIOMSha~data.val$Elev_P01)
plot(data.val.ind$STBIOMSha~data.val.ind$Elev_P01)
plot(0,0)

plot(data.mod$STBIOMSha~data.mod$Elev_P10)
plot(data.val$STBIOMSha~data.val$Elev_P10)
plot(data.val.ind$STBIOMSha~data.val.ind$Elev_P10)
points(data.val.ind$STBIOMSha[data.val.ind$STBIOMSha>380]~data.val.ind$Elev_P10[data.val.ind$STBIOMSha>380], col = 'red')
plot(0,0)

plot(data.mod$STBIOMSha~data.mod$Elev_P30)
plot(data.val$STBIOMSha~data.val$Elev_P30)
plot(data.val.ind$STBIOMSha~data.val.ind$Elev_P30)
plot(0,0)

plot(data.mod$STBIOMSha~data.mod$Elev_P60)
plot(data.val$STBIOMSha~data.val$Elev_P60)
plot(data.val.ind$STBIOMSha~data.val.ind$Elev_P60)
plot(0,0)

plot(data.mod$STBIOMSha~data.mod$Elev_P90)
plot(data.val$STBIOMSha~data.val$Elev_P90)
plot(data.val.ind$STBIOMSha~data.val.ind$Elev_P90)
plot(0,0)

# shape
plot(data.mod$STBIOMSha~data.mod$Elev_stddev)
plot(data.val$STBIOMSha~data.val$Elev_stddev)
points(data.val$STBIOMSha[622]~data.val$Elev_stddev[622], col = 'red')
plot(data.val.ind$STBIOMSha~data.val.ind$Elev_stddev)
plot(0,0)

data.mod$ID[data.mod$Elev_kurtosis >15]
data.val$ID[data.val$Elev_kurtosis >150]
plot(data.mod$STBIOMSha~data.mod$Elev_kurtosis)
plot(data.val$STBIOMSha~data.val$Elev_kurtosis)
points(data.val$STBIOMSha[622]~data.val$Elev_kurtosis[622], col = 'red')
plot(data.val.ind$STBIOMSha~data.val.ind$Elev_kurtosis)

ggplot(data.mod[data.mod$Elev_kurtosis <25, ], aes(Elev_kurtosis, STBIOMSha)) + geom_point() + geom_smooth()
ggplot(data.val[data.val$Elev_kurtosis <25, ], aes(Elev_kurtosis, STBIOMSha)) + geom_point() + geom_smooth()
ggplot(data.val.ind, aes(Elev_kurtosis, STBIOMSha)) + geom_point() + geom_smooth()

par(mfrow = c(2,2))
data.mod$ID[data.mod$Elev_skewness >5]
data.val$ID[data.val$Elev_skewness >10]
plot(data.mod$STBIOMSha~data.mod$Elev_skewness)
plot(data.val$STBIOMSha~data.val$Elev_skewness)
points(data.val$STBIOMSha[622]~data.val$Elev_skewness[622], col = 'red')
plot(data.val.ind$STBIOMSha~data.val.ind$Elev_skewness)
plot(0,0)

ggplot(data.mod[data.mod$Elev_skewness <5, ], aes(Elev_skewness, STBIOMSha)) + geom_point() + geom_smooth()
ggplot(data.val[data.val$Elev_skewness <5, ], aes(Elev_skewness, STBIOMSha)) + geom_point() + geom_smooth()
ggplot(data.val.ind, aes(Elev_skewness, STBIOMSha)) + geom_point() + geom_smooth()

par(mfrow = c(2,2))
plot(data.mod$STBIOMSha~data.mod$Elev_MAD_median)
plot(data.val$STBIOMSha~data.val$Elev_MAD_median)
points(data.val$STBIOMSha[622]~data.val$Elev_MAD_median[622], col = 'red')
plot(data.val.ind$STBIOMSha~data.val.ind$Elev_MAD_median)
plot(0,0)

plot(data.mod$STBIOMSha~data.mod$Elev_MAD_mode)
plot(data.val$STBIOMSha~data.val$Elev_MAD_mode)
points(data.val$STBIOMSha[622]~data.val$Elev_MAD_mode[622], col = 'red')
plot(data.val.ind$STBIOMSha~data.val.ind$Elev_MAD_mode)
plot(0,0)

plot(data.mod$STBIOMSha~data.mod$Elev_L3)
plot(data.val$STBIOMSha~data.val$Elev_L3)
points(data.val$STBIOMSha[622]~data.val$Elev_L3[622], col = 'red')
plot(data.val.ind$STBIOMSha~data.val.ind$Elev_L3)
plot(0,0)

plot(data.mod$STBIOMSha~data.mod$Elev_L4)
plot(data.val$STBIOMSha~data.val$Elev_L4)
points(data.val$STBIOMSha[622]~data.val$Elev_L4[622], col = 'red')
plot(data.val.ind$STBIOMSha~data.val.ind$Elev_L4)
plot(0,0)

plot(data.mod$STBIOMSha~data.mod$Elev_LCV)
plot(data.val$STBIOMSha~data.val$Elev_LCV)
points(data.val$STBIOMSha[622]~data.val$Elev_LCV[622], col = 'red')
plot(data.val.ind$STBIOMSha~data.val.ind$Elev_LCV)
plot(0,0)

plot(data.mod$STBIOMSha~data.mod$Elev_Lkurtosis)
plot(data.val$STBIOMSha~data.val$Elev_Lkurtosis)
points(data.val$STBIOMSha[622]~data.val$Elev_Lkurtosis[622], col = 'red')
plot(data.val.ind$STBIOMSha~data.val.ind$Elev_Lkurtosis)
plot(0,0)

######## density
plot(data.mod$STBIOMSha~data.mod$Pct_all_returns_above_ht)
plot(data.val$STBIOMSha~data.val$Pct_all_returns_above_ht)
points(data.val$STBIOMSha[622]~data.val$Pct_all_returns_above_ht[622], col = 'red')
plot(data.val.ind$STBIOMSha~data.val.ind$Pct_all_returns_above_ht)
plot(0,0)

plot(data.mod$STBIOMSha~data.mod$all_returns_above_ht_div_Total_first_returns_x_100)
plot(data.val$STBIOMSha~data.val$all_returns_above_ht_div_Total_first_returns_x_100)
points(data.val$STBIOMSha[622]~data.val$all_returns_above_ht_div_Total_first_returns_x_100[622], col = 'red')
plot(data.val.ind$STBIOMSha~data.val.ind$all_returns_above_ht_div_Total_first_returns_x_100)
plot(0,0)

plot(data.mod$STBIOMSha~data.mod$pct_all_returns_above_mean)
plot(data.val$STBIOMSha~data.val$pct_all_returns_above_mean)
points(data.val$STBIOMSha[622]~data.val$pct_all_returns_above_mean[622], col = 'red')
plot(data.val.ind$STBIOMSha~data.val.ind$pct_all_returns_above_mean)
plot(0,0)

plot(data.mod$STBIOMSha~data.mod$All_returns_above_mode_div_Total_first_returns_x_100)
plot(data.val$STBIOMSha~data.val$All_returns_above_mode_div_Total_first_returns_x_100)
points(data.val$STBIOMSha[622]~data.val$All_returns_above_mode_div_Total_first_returns_x_100[622], col = 'red')
plot(data.val.ind$STBIOMSha~data.val.ind$All_returns_above_mode_div_Total_first_returns_x_100)
data.val.ind$ID[data.val.ind$All_returns_above_mode_div_Total_first_returns_x_100>100]
plot(0,0)

## interaction
plot(data.mod$STBIOMSha~data.mod$mode_pctAlloverModeDiv1st)
plot(data.val$STBIOMSha~data.val$mode_pctAlloverModeDiv1st)
plot(data.val.ind$STBIOMSha~data.val.ind$mode_pctAlloverModeDiv1st)
points(data.val$STBIOMSha[622]~data.val$mode_pctAlloverModeDiv1st[622], col = 'red')
plot(0,0)

plot(data.mod$STBIOMSha~data.mod$mode_pctAllOver3m)
plot(data.val$STBIOMSha~data.val$mode_pctAllOver3m)
plot(data.val.ind$STBIOMSha~data.val.ind$mode_pctAllOver3m)
points(data.val$STBIOMSha[622]~data.val$mode_pctAllOver3m[622], col = 'red')
plot(0,0)

plot(data.mod$STBIOMSha~data.mod$P01_pctAllOver3m)
plot(data.val$STBIOMSha~data.val$P01_pctAllOver3m)
plot(data.val.ind$STBIOMSha~data.val.ind$P01_pctAllOver3m)
points(data.val$STBIOMSha[622]~data.val$P01_pctAllOver3m[622], col = 'red')
plot(0,0)

plot(data.mod$STBIOMSha~data.mod$P10_pctAllOver3m)
plot(data.val$STBIOMSha~data.val$P10_pctAllOver3m)
plot(data.val.ind$STBIOMSha~data.val.ind$P10_pctAllOver3m)
points(data.val$STBIOMSha[622]~data.val$P10_pctAllOver3m[622], col = 'red')
plot(0,0)

plot(data.mod$STBIOMSha~data.mod$P30_pctAllOver3m)
plot(data.val$STBIOMSha~data.val$P30_pctAllOver3m)
plot(data.val.ind$STBIOMSha~data.val.ind$P30_pctAllOver3m)
points(data.val$STBIOMSha[622]~data.val$P30_pctAllOver3m[622], col = 'red')
plot(0,0)

plot(data.mod$STBIOMSha~data.mod$P60_pctAllOver3m)
plot(data.val$STBIOMSha~data.val$P60_pctAllOver3m)
plot(data.val.ind$STBIOMSha~data.val.ind$P60_pctAllOver3m)
points(data.val$STBIOMSha[622]~data.val$P60_pctAllOver3m[622], col = 'red')
plot(0,0)

plot(data.mod$STBIOMSha~data.mod$P90_pctAllOver3m)
plot(data.val$STBIOMSha~data.val$P90_pctAllOver3m)
plot(data.val.ind$STBIOMSha~data.val.ind$P90_pctAllOver3m)
points(data.val$STBIOMSha[622]~data.val$P90_pctAllOver3m[622], col = 'red')
plot(0,0)

# environmental
plot(data.mod$STBIOMSha~data.mod$elevation)
plot(data.val$STBIOMSha~data.val$elevation)
plot(data.val.ind$STBIOMSha~data.val.ind$elevation)
points(data.val$STBIOMSha[622]~data.val$elevation[622], col = 'red')
plot(0,0)

plot(data.mod$STBIOMSha~data.mod$aspect)
plot(data.val$STBIOMSha~data.val$aspect)
plot(data.val.ind$STBIOMSha~data.val.ind$aspect)
points(data.val$STBIOMSha[622]~data.val$aspect[622], col = 'red')
plot(0,0)

plot(data.mod$STBIOMSha~data.mod$slope)
plot(data.val$STBIOMSha~data.val$slope)
plot(data.val.ind$STBIOMSha~data.val.ind$slope)
points(data.val$STBIOMSha[622]~data.val$slope[622], col = 'red')
plot(0,0)

plot(data.mod$STBIOMSha~data.mod$NDVI_Amp)
plot(data.val$STBIOMSha~data.val$NDVI_Amp)
plot(data.val.ind$STBIOMSha~data.val.ind$NDVI_Amp)
points(data.val$STBIOMSha[622]~data.val$NDVI_Amp[622], col = 'red')
plot(0,0)

plot(data.mod$STBIOMSha~data.mod$R3ERUlabel)
plot(data.val$STBIOMSha~data.val$R3ERUlabel)
plot(data.val.ind$STBIOMSha~data.val.ind$R3ERUlabel)
points(data.val$STBIOMSha[622]~data.val$R3ERUlabel[622], col = 'red')
plot(0,0)

plot(data.mod$STBIOMSha~data.mod$PlotSizeAcres)
plot(data.val$STBIOMSha~data.val$PlotSizeAcres)
plot(data.val.ind$STBIOMSha~data.val.ind$PlotSizeAcres)
points(data.val$STBIOMSha[622]~data.val$PlotSizeAcres[622], col = 'red')
plot(0,0)


## add loess smoothers
ggplot(data.mod, aes(Elev_stddev, STBIOMSha)) + geom_point() + geom_smooth()
ggplot(data.val, aes(Elev_stddev, STBIOMSha)) + geom_point() + geom_smooth()
ggplot(data.val.ind, aes(Elev_stddev, STBIOMSha)) + geom_point() + geom_smooth()

ggplot(data.mod[data.mod$Elev_kurtosis <25, ], aes(Elev_kurtosis, STBIOMSha)) + geom_point() + geom_smooth()
ggplot(data.val[data.val$Elev_kurtosis <25, ], aes(Elev_kurtosis, STBIOMSha)) + geom_point() + geom_smooth()
ggplot(data.val.ind, aes(Elev_kurtosis, STBIOMSha)) + geom_point() + geom_smooth()

ggplot(data.mod, aes(Elev_MAD_median, STBIOMSha)) + geom_point() + geom_smooth()
ggplot(data.val, aes(Elev_MAD_median, STBIOMSha)) + geom_point() + geom_smooth()
ggplot(data.val.ind, aes(Elev_MAD_median, STBIOMSha)) + geom_point() + geom_smooth()

ggplot(data.mod, aes(Elev_MAD_mode, STBIOMSha)) + geom_point() + geom_smooth()
ggplot(data.val, aes(Elev_MAD_mode, STBIOMSha)) + geom_point() + geom_smooth()
ggplot(data.val.ind, aes(Elev_MAD_mode, STBIOMSha)) + geom_point() + geom_smooth()

ggplot(data.mod, aes(Elev_L3, STBIOMSha)) + geom_point() + geom_smooth()
ggplot(data.val, aes(Elev_L3, STBIOMSha)) + geom_point() + geom_smooth()
ggplot(data.val.ind, aes(Elev_L3, STBIOMSha)) + geom_point() + geom_smooth()

ggplot(data.mod, aes(Elev_L4, STBIOMSha)) + geom_point() + geom_smooth()
ggplot(data.val, aes(Elev_L4, STBIOMSha)) + geom_point() + geom_smooth()
ggplot(data.val.ind, aes(Elev_L4, STBIOMSha)) + geom_point() + geom_smooth()

ggplot(data.mod, aes(Elev_LCV, STBIOMSha)) + geom_point() + geom_smooth()
ggplot(data.val, aes(Elev_LCV, STBIOMSha)) + geom_point() + geom_smooth()
ggplot(data.val.ind, aes(Elev_LCV, STBIOMSha)) + geom_point() + geom_smooth()

ggplot(data.mod, aes(Elev_Lskewness, STBIOMSha)) + geom_point() + geom_smooth()
ggplot(data.val, aes(Elev_Lskewness, STBIOMSha)) + geom_point() + geom_smooth()
ggplot(data.val.ind, aes(Elev_Lskewness, STBIOMSha)) + geom_point() + geom_smooth()

ggplot(data.mod, aes(Elev_Lkurtosis, STBIOMSha)) + geom_point() + geom_smooth()
ggplot(data.val, aes(Elev_Lkurtosis, STBIOMSha)) + geom_point() + geom_smooth()
ggplot(data.val.ind, aes(Elev_Lkurtosis, STBIOMSha)) + geom_point() + geom_smooth()

summary(lm(data.mod$STBIOMSha~data.mod$Elev_MAD_mode))
summary(lm(data.mod$STBIOMSha~ poly(data.mod$Elev_MAD_mode,2)))
summary(lm(data.mod$STBIOMSha~ poly(data.mod$Elev_MAD_mode,3)))

ggplot(data.mod, aes(Elev_MAD_mode, STBIOMSha)) + geom_point() + geom_smooth()
ggplot(data.val, aes(Elev_MAD_mode, STBIOMSha)) + geom_point() + geom_smooth()
ggplot(data.val.ind, aes(Elev_MAD_mode, STBIOMSha)) + geom_point() + geom_smooth()

ggplot(data.mod, aes(Elev_MAD_mode, STBIOMSha)) + geom_point() + geom_smooth()
ggplot(data.val, aes(Elev_MAD_mode, logSTBIOMSha)) + geom_point() + geom_smooth()
ggplot(data.val.ind, aes(Elev_MAD_mode, logSTBIOMSha)) + geom_point() + geom_smooth()



####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
## log log transformed models
# Median same as HPM
summary(MedianBASModel_loglog); 
summary(MedianBASModel_loglog)$aic
# residual and error data frame
predictedoutput <- exp(predict(MedianBASModel_loglog, newdata = data.mod))-1
Resoutput <- predictedoutput - (data.mod$STBIOMSha)
PctRes <- ((predictedoutput+1) / (data.mod$STBIOMSha))*100

frame <- cbind(predictedoutput, Resoutput, Resoutput^2, PctRes, 
               data.mod[ , c('STBIOMSha', 'Forest','PlotSizeAcres', 'fpc', 'Stratum', 
                              'logmode', 'logP01', 'logP10', 'logP30', 'logP60', 'logP90',
                              'Elev_stddev', 'Elev_kurtosis', 'Elev_MAD_median', 'Elev_MAD_mode', 'Elev_L3', 'Elev_L4', 'Elev_LCV', 'Elev_Lskewness', 'Elev_Lkurtosis', 
                              'log_Pct_all_returns_above_ht', 'log_pct_all_returns_above_mean', 'log_all_returns_above_ht_div_Total_first_returns_x_100', 
                              'logmode_pctAllOver3m', 'logmode_pctAlloverModeDiv1st', 'logP01_pctAllOver3m', 'logP10_pctAllOver3m', 'logP30_pctAllOver3m', 'logP60_pctAllOver3m', 'logP90_pctAllOver3m', 
                              'elevation', 'aspect', 'slope', 'NDVI_Amp', 
                              'R3ERUlabel')]
               )
colnames(frame)<- c('Prediction', 'PredictionSE', "Residual", "ResidualSE", "SqResidual", "SqResidualSE", "PctResidual", "PctResidualSE", 
                    'STBIOMSha', 'Forest','PlotSizeAcres', "fpc", "Stratum",
                    'logmode', 'logP01', 'logP10', 'logP30', 'logP60', 'logP90',
                    'Elev_stddev', ' Elev_kurtosis', 'Elev_MAD_median', 'Elev_MAD_mode', 'Elev_L3', 'Elev_L4', 'Elev_LCV', 'Elev_Lskewness', 'Elev_Lkurtosis', 
                    'log_Pct_all_returns_above_ht', 'log_pct_all_returns_above_mean', 'log_all_returns_above_ht_div_Total_first_returns_x_100', 
                    'logmode_pctAllOver3m', 'logmode_pctAlloverModeDiv1st', 'logP01_pctAllOver3m', 'logP10_pctAllOver3m', 'logP30_pctAllOver3m', 'logP60_pctAllOver3m', 'logP90_pctAllOver3m', 
                    'elevation', 'aspect', 'slope', 'NDVI_Amp', 'ERU')
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


########################################
ggplot(frame, aes(STBIOMSha, Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'NorthKaibab', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'SWJM', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Sitgreaves', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Tonto', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Coconino', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()

########################################
########################################

### Hex plots, weighted by sample weights
svyplot(frame$Residual~frame$STBIOMSha, design = CrossValidation, style = 'trans', xlab = 'Observed', ylab = 'Residual')
abline(h = 0, col = 'red')

svyplot(frame$Residual ~ frame$STBIOMSha, design = CrossValidation, style = 'trans', xbins = 20, xlab = 'Observed', ylab = 'Percent agreement')
abline(h = 0, col = 'black')
smth <- svysmooth(frame$Residual ~ frame$STBIOMSha, design = CrossValidation)
lines(smth, col = 'red')


unique(data.val$PlotSizeAcres)

