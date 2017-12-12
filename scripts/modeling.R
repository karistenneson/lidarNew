### This file is for running the BMA components of the analysis
## Written by MS Patterson (maspatte@uw.edu)
# Last updated: Oct 20, 2017

### Bring in data
#setwd('C:\\Users\\krtenneson\\Desktop\\lidarPaper\\lidarNew\\scripts')
#setwd('\\\\166.2.126.25\\rseat\\Programs\\Reimbursibles\\fy2016\\R3_lidar_equation_transferability\\Analysis\\VersionControl\\lidarNew\\scripts')
#setwd("~/Documents/R/lidarNew/scripts") #Mac
#setwd("~/R/lidarNew/scripts") #WinCampus
source(file="DataPrep.R")

### Load required packages
library(BAS)
library(corrgram)
#library(robustbase)

data.svy <- svydesign(ids = ~1, data = fulldata[(fulldata$Forest != forestname[6] & fulldata$Forest != forestname[7]),], strata = fulldata$Stratum[(fulldata$Forest != forestname[6] & fulldata$Forest != forestname[7])], fpc = fulldata$fpc[(fulldata$Forest != forestname[6] & fulldata$Forest != forestname[7])]) 

## Update data set to just 
## the bottom 75% of teh random uniform distribution.

## remove these columns for the models:
## 'R3ERUCODE', 'R3ERUlabelFull', 'Site','Forest', 
## "PlotSizeAcres", "fpc", "Stratum"

DATA.modC <- fulldata[(fulldata$Forest != forestname[6] & fulldata$Forest != forestname[7] & fulldata$RandomUniform <0.75), c(
  "STBIOMSha", "TCUmha", 
  "Elev_ave", "Elev_mode", "Elev_stddev")] #, "Elev_variance", "Elev_CV", "Elev_IQ", "Elev_skewness", "Elev_kurtosis", "Elev_AAD", "Elev_MAD_median", "Elev_MAD_mode", "Elev_L2", "Elev_L3", "Elev_L4", "Elev_LCV", "Elev_Lskewness", "Elev_Lkurtosis", "Elev_P01", "Elev_P05", "Elev_P10", "Elev_P20", "Elev_P25", "Elev_P30", "Elev_P40", "Elev_P50", "Elev_P60", "Elev_P70", "Elev_P75", "Elev_P80", "Elev_P90", "Elev_P95", "Elev_P99", 
#  "Pct_first_returns_above_ht", "Pct_all_returns_above_ht", "all_returns_above_ht_div_Total_first_returns_x_100", "Pct_first_returns_above_mean", "Pct_first_returns_above_mode", "pct_all_returns_above_mean", "pct_all_returns_above_mode", "All_returns_above_mean_div_Total_first_returns_x_100", "All_returns_above_mode_div_Total_first_returns_x_100", 
#  "elevation", "aspect", "slope", "NDVI_Amp", "R3ERUlabel")]

DATA.modC$R3ERUlabel <- as.factor(DATA.modC$R3ERUlabel)
DATA.modC$STBIOMSha <- DATA.modC$STBIOMSha+0.05
DATA.modC$TCUmha <- DATA.modC$TCUmha+0.05

# Full variable pool, centered, truncated poisson prior, hyper-g
BioMass.Modc <- bas.lm(log(STBIOMSha)~ . -TCUmha,
                   data=DATA.modC,
                   prior="hyper-g",
                   alpha = 3,
                   modelprior=tr.poisson(10,30),
                   method="MCMC+BAS")

summary(BioMass.Modc)

# Full variable pool, truncated poisson prior, hyper-g
BioMass.Mod <- bas.lm(log(STBIOMSha)~ . -TCUFT, 
                  data=DATA.mod, 
                  prior="hyper-g",
                  alpha = 3,
                  modelprior=tr.poisson(10,30),
                  method="MCMC+BAS")

summary(BioMass.Mod)

# What are the median model variables
BioMass.Mod$namesx[which(BioMass.Mod$probne0>0.5)][-1]
BioMass.Modc$namesx[which(BioMass.Modc$probne0>0.5)][-1]

# Model diagnostics
plot(BioMass.Mod, ask=F)
plot(BioMass.Modc, ask=F)

#pull some models out using predict functions

# Highest Probability Model
HPM <- predict(BioMass.Mod, estimator="HPM")
BioMass.Mod$namesx[HPM$bestmodel+1][-1]

HPMlm <- lm(log(STBIOMSha)~ Elev_ave + Elev_CV + Elev_LCV + Pct_first_returns_above_ht + 
              Pct_all_returns_above_ht + all_returns_above_ht_div_Total_first_returns_x_100 + 
              Pct_first_returns_above_mean + All_returns_above_mean_div_Total_first_returns_x_100 + 
              Total_first_returns + Total_all_returns + R3ERUCODE + Forest + PlotSizeAcres, data=DATA.mod)

summary(HPMlm)
plot(HPMlm, ask=F)

# Median Probability Model 
MPM <- predict(BioMass.Mod, estimator="MPM")
BioMass.Mod$namesx[MPM$bestmodel+1][-1]

MPMlm <- lm(log(STBIOMSha)~ Elev_CV + Elev_LCV + Elev_P80+ Pct_first_returns_above_ht + 
              Pct_all_returns_above_ht + All_returns_above_ht + Pct_first_returns_above_mean + 
              All_returns_above_mean_div_Total_first_returns_x_100 + Total_first_returns + 
              Total_all_returns + R3ERUCODE + Forest + PlotSizeAcres, data=DATA.mod)

summary(MPMlm)
plot(MPMlm, ask=F) # This model is pretty solid, better redisdual distribution than HPM, and better adjusted R^2 

# Best Predictive Model, closest to BMA in terms of squared error loss, takes a pretty long time to find. 

BPM <- predict(BioMass.Mod, estimator="BPM") #not running, grrrr

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
