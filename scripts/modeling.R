### This file is for running the BMA components of the analysis
## Written by MS Patterson (maspatte@uw.edu)
# Last updated: Cot 20, 2017

### Bring in data
source(file="DataPrep.R")

### Load required packages
library(BAS)
library(corrgram)

# Full variable pool, centered, truncated poisson prior, hyper-g
BioMass.Modc <- bas.lm(log(STBIOMS)~ . -TCUFT,
                   data=DATA.modC,
                   prior="hyper-g",
                   alpha = 3,
                   modelprior=tr.poisson(10,30),
                   method="MCMC+BAS")

summary(BioMass.Modc)

# Full variable pool, truncated poisson prior, hyper-g
BioMass.Mod <- bas.lm(log(STBIOMS)~ . -TCUFT, 
                  data=DATA.mod, 
                  prior="hyper-g",
                  alpha = 3,
                  modelprior=tr.poisson(10,30),
                  method="MCMC+BAS")

summary(BioMass.Mod)

# What are the median model variables
BioMass.Mod$namesx[which(BioMass.Mod$probne0>0.5)][-1]
BioMass.Modc$namesx[which(BioMass.Modc$probne0>0.5)][-1]

# Correlation in variables in the median model
MedModVar <- select(DATA.mod, BioMass.Mod$namesx[which(BioMass.Mod$probne0>0.5)][-1])

corrgram(MedModVar, order=T, lower.panel=panel.ellipse,
         upper.panel=panel.cor, text.panel=panel.txt,
         main="Lidar Predictor Data in PC2/PC1 Order")

#Fairly strong correlations between many variables, several clear pairs that should probably be removed.
# Variable marked with * seems best to remove
# Total_all_returns & Total_first_returns*
# Elev_CV* & Elev_LCV
# Pct_all_returns_above...* & Pct_first_returns_above...
# All_returns_above_mean_div_Total_first_returns_x_100 & Pct_first_returns_above_mean*

# Model diagnostics
plot(BioMass.Mod, ask=F)
plot(BioMass.Modc, ask=F)

#pull some models out using predict functions

# Highest Probability Model
HPM <- predict(BioMass.Mod, estimator="HPM")
BioMass.Mod$namesx[HPM$bestmodel+1][-1]

HPMlm <- lm(log(STBIOMS)~ Elev_skewness + Elev_P80 + Pct_first_returns_above_ht + Pct_all_returns_above_ht +
              all_returns_above_ht_div_Total_first_returns_x_100 + Pct_first_returns_above_mean +
              All_returns_above_mean_div_Total_first_returns_x_100 + Total_first_returns +
              Total_all_returns + R3ERUCODE, data=DATA.mod)

summary(HPMlm)
plot(HPMlm, ask=F)

# Median Probability Model 
MPM <- predict(BioMass.Mod, estimator="MPM")
BioMass.Mod$namesx[MPM$bestmodel+1][-1]

MPMlm <- lm(log(STBIOMS)~ Elev_CV + Elev_LCV + Elev_P80+ Pct_first_returns_above_ht + 
              Pct_all_returns_above_ht + Pct_first_returns_above_mean + 
              All_returns_above_mean_div_Total_first_returns_x_100 + Total_first_returns + 
              Total_all_returns + elevation + NDVI_Amp, data=DATA.mod)

summary(MPMlm)
plot(MPMlm, ask=F) # This model is pretty solid, better redisdual distribution than HPM, and better adjusted R^2 

# Best Predictive Model, closest to BMA in terms of squared error loss, takes a pretty long time to find. 

BPM <- predict(BioMass.Mod, estimator="BPM") #not running, grrrr

#A possible final model? 
FinModVar <- select(DATA.mod, Elev_LCV, Elev_P80, Pct_all_returns_above_ht, Total_all_returns,
                    elevation, NDVI_Amp)
corrgram(FinModVar, order=T, lower.panel=panel.ellipse,
         upper.panel=panel.cor, text.panel=panel.txt,
         main="Lidar Predictor Data in PC2/PC1 Order") # No correlations above ~0.5

BioMass.ModelF <- lm(log(STBIOMS)~ Elev_LCV + Elev_P80+ Pct_all_returns_above_ht + 
                       Total_all_returns + elevation + NDVI_Amp, data=DATA.mod)
summary(BioMass.ModelF)
plot(BioMass.ModelF) # Some slight non-linearities in residuals and QQ. 
