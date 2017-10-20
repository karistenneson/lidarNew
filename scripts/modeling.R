### This file is for running the BMA components of the analysis
## Written by MS Patterson (maspatte@uw.edu)
# Last updated: Cot 20, 2017

### Bring in data
source(file="DataPrep.R")

### Load required packages
library(BAS)
library(stats)
library(tidyverse)
library(corrgram)

# Full variable pool, centered, truncated poisson prior, hyper-g
BioMass.Modc <- bas.lm(log(STBIOMS)~ .,
                   data=DATA.modC,
                   prior="hyper-g",
                   alpha = 3,
                   modelprior=tr.poisson(10,30),
                   method="MCMC+BAS")

summary(BioMass.Modc)
BioMass.Modc$namesx[which(BioMass.Modc$probne0>0.5)][-1]

# Full variable pool, truncated poisson prior, hyper-g
BioMass.Mod <- bas.lm(log(STBIOMS)~ ., 
                  data=DATA.mod, 
                  prior="hyper-g",
                  alpha = 3,
                  modelprior=tr.poisson(10,30),
                  method="MCMC+BAS")

summary(BioMass.Mod)
BioMass.Mod$namesx[which(BioMass.Mod$probne0>0.5)][-1]


# Correlation in variables in the median model
MedModVar <- select(BioMass.Mod, BioMass.Mod$namesx[which(BioMass.Mod$probne0>0.5)][-1])

corrgram(MedModVar, order=T, lower.panel=panel.ellipse,
         upper.panel=panel.cor, text.panel=panel.txt,
         main="Lidar Predictor Data in PC2/PC1 Order")

#Fairly strong correlations between many variables, several clear pairs that should probably be removed. 

# Model diagnostics
plot(BioMass.Mod, ask=F)
plot(BioMass.Modc, ask=F)

#pull some models out using predict functions

# Highest Probability Model
HPM <- predict(BioMass.Mod, estimator="HPM")
BioMass.Mod$namesx[HPM$bestmodel+1][-1]

HPMlm <- lm(log(STBIOMS)~ , data=DATA.mod)

summary(HPMlm)
plot(HPMlm, ask=F)

# Median Probability Model 
MPM <- predict(BioMass.Mod, estimator="MPM")
BioMass.Mod$namesx[MPM$bestmodel+1][-1]

MPMlm <- lm(log(STBIOMS)~ , data=BioMass.Mod)

summary(MPMlm)
plot(MPMlm, ask=F)

# Best Predictive Model, closest to BMA in terms of squared error loss, takes a pretty long time to find. 
BPM <- predict(BioMass.Mod, estimator="BPM") #not running, grrrr
# BioBLMh$namesx[BPM$bestmodel+1][-1] # Best Predictive model is  
# 
# BPMlm <- lm(log(STBIOMS.test)~ , data=BMod)
# 
# summary(BPMlm)
# plot(BPMlm, ask=F) #Slightly better fit with residuals than model below. 


#A possible final model? 
BioMass.ModelF <- lm(log(STBIOMS)~ , data=BioMass.Mod)
summary(BioMass.ModelF)
plot(BioMass.ModelF) # Some slight non-linearities in residuals and QQ. 
