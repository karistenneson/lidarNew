### This file is for producing predicted values and verifying against remaining data
## Written by MS Patterson (maspatte@uw.edu)
# Last updated: Oct 20, 2017

library(cvTools)

# Make the New Data Object from the remaining data
DATA.new <- AllData[AllData$RandomUniform>=0.75,]

#Select predictors, chuck unused variables
DATA.new <- select(DATA.new, STBIOMS, TCUFT, Predictors)

# Centering data
DATA.newC <- cbind(DATA.new[, c(1,2)], center(DATA.new[,c(-1,-2,-52,-57)]), DATA.new[52, 57])

### Generate Predictions
NewBiomass <- predict(object=FinModBt, newdata=DATA.new)
NewTCUFT <- predict(object=FinModTt, newdata=DATA.new)

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


### Predictions from BAS object itself.

BMABio <- predict(BioMass.Mod, DATA.new, top='10000')

BmaBioRMSPE <- rmspe(DATA.new$STBIOMS, exp(BMABio$fit), includeSE=T)
BmaBioMBE <- sum((exp(BMABio$fit) - DATA.new$STBIOMS))/length(NewBiomass)


BMAWood <- predict(TCUFT.Mod, DATA.new, top='10000')

BmaWoodRMSPE <- rmspe(DATA.new$TCUFT, exp(BMAWood$fit), includeSE=T)
BmaWoodMBE <- sum((exp(BMAWood$fit) - DATA.new$TCUFT))/length(NewBiomass)
