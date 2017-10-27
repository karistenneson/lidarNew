### This file is for producing predicted values and verifying against remaining data
## Written by MS Patterson (maspatte@uw.edu)
# Last updated: Oct 20, 2017

library(cvTools)

# Make the New Data Object from the remaining data
DATA.new <- AllData[AllData$RandomUniform>=0.75,]

#Select predictors, chuck unused variables
DATA.new <- select(DATA.new, STBIOMS, TCUFT, LidarNames, AuxNames, FieldNames[9])

# Centering data
DATA.newC <- cbind(DATA.new[, c(1,2)], center(DATA.new[,c(-1,-2,-52)]), DATA.new[52])

### Generate Predictions
NewBiomass <- predict(object=FinModB, newdata=DATA.new)
NewTCUFT <- predict(object=FinModT, newdata=DATA.new)

#Compare to real data

# plots
qqplot(exp(NewBiomass), DATA.new$STBIOMS)
qqplot(exp(NewTCUFT), DATA.new$TCUFT)

qplot(exp(NewBiomass), DATA.new$STBIOMS) +geom_smooth(method=lm, se=T)
qplot(exp(NewTCUFT), DATA.new$TCUFT) +geom_smooth(method=lm, se=T)


# metrics, Root Mean Square Error, Mean Summed Error, Mean Bias Error
BioRMSPE <- rmspe(DATA.new$STBIOMS, exp(NewBiomass), includeSE=T)
BioMAPE <- mape(DATA.new$STBIOMS, exp(NewBiomass), includeSE=T)
BioMBE <- sum((exp(NewBiomass) - DATA.new$STBIOMS))/length(NewBiomass)

CufRMSPE <- rmspe(DATA.new$TCUFT, exp(NewTCUFT), includeSE=T)
CufMAPE <- mape(DATA.new$TCUFT, exp(NewTCUFT), includeSE=T)
CufMBE <- sum((exp(NewTCUFT) - DATA.new$TCUFT))/length(NewBiomass)

# Robust metrics, drops outliers from data to calculate metrics 
BioRMSPE.r <- rtmspe(DATA.new$STBIOMS, exp(NewBiomass), includeSE=T)

CufRMSPE.r <- rtmspe(DATA.new$TCUFT, exp(NewTCUFT), includeSE=T)

### Validate Predictions against Validation set
