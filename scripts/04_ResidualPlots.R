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

########################################
########################################
ggplot(frame, aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'NorthKaibab', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'SWJM', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Sitgreaves', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Tonto', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Coconino', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()


########################################
########################################
ggplot(frame, aes(logmode, Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'NorthKaibab', ], aes(logmode,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'SWJM', ], aes(logmode,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Sitgreaves', ], aes(logmode,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Tonto', ], aes(logmode,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Coconino', ], aes(logmode,Residual)) + geom_point() + geom_smooth()

########################################
########################################

ggplot(frame, aes(logP01, Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'NorthKaibab', ], aes(logP01,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'SWJM', ], aes(logP01,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Sitgreaves', ], aes(logP01,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Tonto', ], aes(logP01,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Coconino', ], aes(logP01,Residual)) + geom_point() + geom_smooth()

########################################
########################################
ggplot(frame, aes(logP10, Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'NorthKaibab', ], aes(logP10,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'SWJM', ], aes(logP10,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Sitgreaves', ], aes(logP10,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Tonto', ], aes(logP10,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Coconino', ], aes(logP10,Residual)) + geom_point() + geom_smooth()

########################################
########################################
ggplot(frame, aes(logP30, Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'NorthKaibab', ], aes(logP30,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'SWJM', ], aes(logP30,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Sitgreaves', ], aes(logP30,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Tonto', ], aes(logP30,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Coconino', ], aes(logP30,Residual)) + geom_point() + geom_smooth()

########################################
########################################
ggplot(frame, aes(logP60, Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'NorthKaibab', ], aes(logP60,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'SWJM', ], aes(logP60,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Sitgreaves', ], aes(logP60,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Tonto', ], aes(logP60,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Coconino', ], aes(logP60,Residual)) + geom_point() + geom_smooth()

########################################
########################################
ggplot(frame, aes(logP90, Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'NorthKaibab', ], aes(logP90,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'SWJM', ], aes(logP90,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Sitgreaves', ], aes(logP90,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Tonto', ], aes(logP90,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Coconino', ], aes(logP90,Residual)) + geom_point() + geom_smooth()

########################################
########################################
ggplot(frame, aes(Elev_stddev, Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'NorthKaibab', ], aes(Elev_stddev,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'SWJM', ], aes(Elev_stddev,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Sitgreaves', ], aes(Elev_stddev,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Tonto', ], aes(Elev_stddev,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Coconino', ], aes(Elev_stddev,Residual)) + geom_point() + geom_smooth()

########################################
########################################
ggplot(frame, aes(Elev_Lkurtosis, Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'NorthKaibab', ], aes(Elev_Lkurtosis,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'SWJM', ], aes(Elev_Lkurtosis,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Sitgreaves', ], aes(Elev_Lkurtosis,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Tonto', ], aes(Elev_Lkurtosis,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Coconino', ], aes(Elev_Lkurtosis,Residual)) + geom_point() + geom_smooth()

########################################
########################################
ggplot(frame, aes(Elev_MAD_median, Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'NorthKaibab', ], aes(Elev_MAD_median,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'SWJM', ], aes(Elev_MAD_median,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Sitgreaves', ], aes(Elev_MAD_median,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Tonto', ], aes(Elev_MAD_median,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Coconino', ], aes(Elev_MAD_median,Residual)) + geom_point() + geom_smooth()

########################################
########################################
ggplot(frame, aes(Elev_MAD_mode, Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'NorthKaibab', ], aes(Elev_MAD_mode,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'SWJM', ], aes(Elev_MAD_mode,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Sitgreaves', ], aes(Elev_MAD_mode,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Tonto', ], aes(Elev_MAD_mode,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Coconino', ], aes(Elev_MAD_mode,Residual)) + geom_point() + geom_smooth()

########################################
########################################
ggplot(frame, aes(Elev_L3, Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'NorthKaibab', ], aes(Elev_L3,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'SWJM', ], aes(Elev_L3,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Sitgreaves', ], aes(Elev_L3,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Tonto', ], aes(Elev_L3,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Coconino', ], aes(Elev_L3,Residual)) + geom_point() + geom_smooth()

########################################
########################################
ggplot(frame, aes(Elev_L4, Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'NorthKaibab', ], aes(Elev_L4,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'SWJM', ], aes(Elev_L4,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Sitgreaves', ], aes(Elev_L4,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Tonto', ], aes(Elev_L4,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Coconino', ], aes(Elev_L4,Residual)) + geom_point() + geom_smooth()

########################################
########################################
ggplot(frame, aes(Elev_LCV, Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'NorthKaibab', ], aes(Elev_LCV,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'SWJM', ], aes(Elev_LCV,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Sitgreaves', ], aes(Elev_LCV,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Tonto', ], aes(Elev_LCV,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Coconino', ], aes(Elev_LCV,Residual)) + geom_point() + geom_smooth()

########################################
########################################
ggplot(frame, aes(Elev_Lskewness, Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'NorthKaibab', ], aes(Elev_Lskewness,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'SWJM', ], aes(Elev_Lskewness,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Sitgreaves', ], aes(Elev_Lskewness,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Tonto', ], aes(Elev_Lskewness,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Coconino', ], aes(Elev_Lskewness,Residual)) + geom_point() + geom_smooth()

########################################
########################################
ggplot(frame, aes(Elev_Lkurtosis, Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'NorthKaibab', ], aes(Elev_Lkurtosis,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'SWJM', ], aes(Elev_Lkurtosis,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Sitgreaves', ], aes(Elev_Lkurtosis,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Tonto', ], aes(Elev_Lkurtosis,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Coconino', ], aes(Elev_Lkurtosis,Residual)) + geom_point() + geom_smooth()

########################################
########################################
ggplot(frame, aes(log_Pct_all_returns_above_ht, Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'NorthKaibab', ], aes(log_Pct_all_returns_above_ht,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'SWJM', ], aes(log_Pct_all_returns_above_ht,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Sitgreaves', ], aes(log_Pct_all_returns_above_ht,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Tonto', ], aes(log_Pct_all_returns_above_ht,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Coconino', ], aes(log_Pct_all_returns_above_ht,Residual)) + geom_point() + geom_smooth()

########################################
########################################
ggplot(frame, aes(log_pct_all_returns_above_mean, Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'NorthKaibab', ], aes(log_pct_all_returns_above_mean,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'SWJM', ], aes(log_pct_all_returns_above_mean,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Sitgreaves', ], aes(log_pct_all_returns_above_mean,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Tonto', ], aes(log_pct_all_returns_above_mean,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Coconino', ], aes(log_pct_all_returns_above_mean,Residual)) + geom_point() + geom_smooth()

########################################
########################################
ggplot(frame, aes(log_all_returns_above_ht_div_Total_first_returns_x_100, Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'NorthKaibab', ], aes(log_all_returns_above_ht_div_Total_first_returns_x_100,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'SWJM', ], aes(log_all_returns_above_ht_div_Total_first_returns_x_100,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Sitgreaves', ], aes(log_all_returns_above_ht_div_Total_first_returns_x_100,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Tonto', ], aes(log_all_returns_above_ht_div_Total_first_returns_x_100,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Coconino', ], aes(log_all_returns_above_ht_div_Total_first_returns_x_100,Residual)) + geom_point() + geom_smooth()

########################################
########################################
ggplot(frame, aes(logmode_pctAllOver3m, Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'NorthKaibab', ], aes(logmode_pctAllOver3m,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'SWJM', ], aes(logmode_pctAllOver3m,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Sitgreaves', ], aes(logmode_pctAllOver3m,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Tonto', ], aes(logmode_pctAllOver3m,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Coconino', ], aes(logmode_pctAllOver3m,Residual)) + geom_point() + geom_smooth()

########################################
########################################


########################################
########################################

'logmode_pctAlloverModeDiv1st', 'logP01_pctAllOver3m', 'logP10_pctAllOver3m', 'logP30_pctAllOver3m', 'logP60_pctAllOver3m', 'logP90_pctAllOver3m', 'aspect', 'slope', 'NDVI_Amp', 'R3ERUlabel'
