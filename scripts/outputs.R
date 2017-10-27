### This file is for producing outputs from the analysis
## Written by MS Patterson (maspatte@uw.edu)
# Last updated: Oct 27, 2017

### Should run through the DatPrep and modeling processes before doing anything here. 

library(broom)
library(cvTools)

### Produce some clean model outputs for paper

# Tables of predictors
BioMass.tab <- tidy(FinModB)
Volume.tab <- tidy(FinModT)

# "Table" of model significance
Sig.tab <- rbind(glance(FinModB), glance(FinModT))
rownames(Sig.tab) <- c("STBIOMS", "TCUFT")

### Internal performance metrics
# metrics, Root Mean Square Error, Mean Bias Error
BioRMSPE <- rmspe(DATA.new$STBIOMS, exp(NewBiomass), includeSE=T)
BioMBE <- sum((exp(NewBiomass) - DATA.new$STBIOMS))/length(NewBiomass)

CufRMSPE <- rmspe(DATA.new$TCUFT, exp(NewTCUFT), includeSE=T)
CufMBE <- sum((exp(NewTCUFT) - DATA.new$TCUFT))/length(NewBiomass)

# Robust metrics, drops outliers from data to calculate metrics 
BioRMSPE.r <- rtmspe(DATA.new$STBIOMS, exp(NewBiomass), includeSE=T)

CufRMSPE.r <- rtmspe(DATA.new$TCUFT, exp(NewTCUFT), includeSE=T)

### External performance metrics

# normal

# robust
