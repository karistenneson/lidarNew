### This file is for running the BMA components of the analysis
## Written by MS Patterson (maspatte@uw.edu)
# Last updated: Oct 20, 2017

### Bring in data
#setwd('C:\\Users\\krtenneson\\Desktop\\lidarPaper\\lidarNew\\scripts')
#setwd('\\\\166.2.126.25\\rseat\\Programs\\Reimbursibles\\fy2016\\R3_lidar_equation_transferability\\Analysis\\VersionControl\\lidarNew\\scripts')
#setwd("~/Documents/R/lidarNew/scripts") #Mac
#setwd("~/R/lidarNew/scripts") #WinCampus
source(file="DataPrepwithZeroesv2.R")

### Load required packages
library(survey)
options(survey.lonely.psu="certainty")
options(survey.lonely.psu="adjust")

##################################################################################################################################
## OLD CODE
##################################################################################################################################

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

##################################################################################################################################
##################################################################################################################################

######### Validation Data
# average biomass, volume and elevation
Subset<- data.val.ind
data.mod.svy <- svydesign(ids = ~1, data = Subset, strata = Subset$Stratum, fpc = Subset$fpc) 
svymean(~STBIOMSha, data.mod.svy)
svymean(~TCUmha, data.mod.svy)
svymean(~elevation, data.mod.svy)

svyby(~STBIOMSha, ~Forest, data.mod.svy, svymean)
svyby(~TCUmha, ~Forest, data.mod.svy, svymean)
svyby(~elevation, ~Forest, data.mod.svy, svymean)

######### Model Construct Data
# average biomass, volume and elevation
Subset<- data.mod
data.mod.svy <- svydesign(ids = ~1, data = Subset, strata = Subset$Stratum, fpc = Subset$fpc) 
svymean(~STBIOMSha, data.mod.svy)
svymean(~TCUmha, data.mod.svy)
svymean(~elevation, data.mod.svy)

svyby(~STBIOMSha, ~Forest, data.mod.svy, svymean)
svyby(~TCUmha, ~Forest, data.mod.svy, svymean)
svyby(~elevation, ~Forest, data.mod.svy, svymean)

######### Model Construct Data (for validation)
# average biomass, volume and elevation
Subset<- data.val
data.mod.svy <- svydesign(ids = ~1, data = Subset, strata = Subset$Stratum, fpc = Subset$fpc) 
svymean(~STBIOMSha, data.mod.svy)
svymean(~TCUmha, data.mod.svy)
svymean(~elevation, data.mod.svy)

svyby(~STBIOMSha, ~Forest, data.mod.svy, svymean)
svyby(~TCUmha, ~Forest, data.mod.svy, svymean)
svyby(~elevation, ~Forest, data.mod.svy, svymean)

############################

## Look up plot size distribution
table(Coco$PlotSizeAcres)
table(Tonto$PlotSizeAcres)
table(Sit$PlotSizeAcres)
table(SWJM$PlotSizeAcres)

############################

## ERU Composition 
unique(data.mod$R3ERUlabel)
unique(data.val$R3ERUlabel)
unique(data.val.ind$R3ERUlabel)

## model data
Subset<- data.mod
data.svy <- svydesign(ids = ~1, data = Subset, strata = Subset$Stratum, fpc = Subset$fpc) 

table <- svytotal(~R3ERUlabel, data.svy)

# grasslands
(table[1]/sum(table))*100
(16796/sum(table))*100

# PJ
100*(table[2]/sum(table))
100*(4636/sum(table))

#** with shrub
100*table[3]/sum(table)
100*25/sum(table)

# D. decid
100*table[4]/sum(table)
100*13/sum(table)

# spruce
100*table[5]/sum(table)
100*20309/sum(table)

# mixed conifer
100*table[6]/sum(table)
100*28016/sum(table)

# PP
100*table[7]/sum(table)
100*34476/sum(table)


## validation data
Subset<- data.val
data.svy <- svydesign(ids = ~1, data = Subset, strata = Subset$Stratum, fpc = Subset$fpc) 

table <- svytotal(~R3ERUlabel, data.svy)

# grasslands
(table[1]/sum(table))*100
(39.9/sum(table))*100

# PJ
100*(table[2]/sum(table))
100*(10244.65/sum(table))

#** with shrub
100*table[3]/sum(table)
100*0/sum(table)

# D. decid
#100*table[4]/sum(table)
#100*19104/sum(table)

# spruce
100*table[4]/sum(table)
100*50584/sum(table)

# mixed conifer
100*table[5]/sum(table)
100*49352/sum(table)

# PP
100*table[6]/sum(table)
100*48647/sum(table)

## Total indp. validation data
Subset<- data.val.ind
data.svy <- svydesign(ids = ~1, data = Subset, strata = Subset$Stratum, fpc = Subset$fpc) 

table <- svytotal(~R3ERUlabel, data.svy)

# grasslands
(table[1]/sum(table))*100
(3801.5/sum(table))*100

# PJ
#100*(table[2]/sum(table))
#100*(10244.65/sum(table))

#** with shrub
100*table[2]/sum(table)
100*1779.7/sum(table)

# D. decid
#100*table[4]/sum(table)
#100*19104/sum(table)

# spruce
100*table[3]/sum(table)
100*9859.2/sum(table)

# mixed conifer
100*table[4]/sum(table)
100*15867.2/sum(table)

# PP
100*table[5]/sum(table)
100*17528.5/sum(table)

##############################################################
colVecFull <- c('yellow','yellow4',
                'royalblue1','royalblue4','royalblue3',
                'plum1','violetred2','violetred4',
                'olivedrab1','limegreen','seagreen4')

colVec <- c('yellow', 'orange', 'red', 
            'royalblue3',
            'plum1', 'violetred4', 'seagreen4')

#################################
par(mfrow = c(2, 3))
forestname <- unique(AllData$Forest)
forestname <- forestname[c(4, 1, 3, 2, 5, 7, 6)]
labels <- c("b) Kaibab Plateau", 'c) Coconino N.F.', 'd) Tonto N.F.', 'e) Apache-Sitgreaves N.F., Phase 1', 'f) Santa Fe N.F.',  'h) Apache-Sitgreaves N.F., Phase 2',  'i) Apache-Sitgreaves N.F., Phase 3')

table(AllData$R3ERUlabel[(AllData$Forest != forestname[6] & AllData$Forest != forestname[7])])
data.svy <- svydesign(ids = ~1, 
                      data = AllData[(AllData$Forest != forestname[6] & AllData$Forest != forestname[7] & AllData$R3ERUlabel != 'k'),], 
                      strata = AllData$Stratum[(AllData$Forest != forestname[6] & AllData$Forest != forestname[7] & AllData$R3ERUlabel != 'k')], 
                      fpc = AllData$fpc[(AllData$Forest != forestname[6] & AllData$Forest != forestname[7] & AllData$R3ERUlabel != 'k')]) 

svyboxplot(STBIOMSha ~ R3ERUlabel, data.svy, varwidth = T, col = colVec, main = "a) All Plots used for Model Development", ylim = c(0,700), ylab = 'AGB (tons/ha)')
abline(h = 0)

#################

#################

## Kaibab  
i = 1
Forest = forestname[i]
Forest

table(AllData$R3ERUlabel[(AllData$Forest == Forest)])
data.svy <- svydesign(ids = ~1, 
                      data = AllData[(AllData$Forest == Forest & AllData$R3ERUlabel != 'A'& AllData$R3ERUlabel != 'B'),], 
                      strata = AllData$Stratum[(AllData$Forest == Forest & AllData$R3ERUlabel != 'A'& AllData$R3ERUlabel != 'B')], 
                      fpc = AllData$fpc[(AllData$Forest == Forest & AllData$R3ERUlabel != 'A'& AllData$R3ERUlabel != 'B')]) 

svyboxplot(STBIOMSha ~ R3ERUlabel, data.svy, varwidth = T, col = colVec[c(5, 6, 7)], main = labels[i], ylim = c(0,700))
abline(h = 0)


## Coco
i = i +1
Forest = forestname[i]
Forest
table(AllData$R3ERUlabel[(AllData$Forest == Forest)])
data.svy <- svydesign(ids = ~1, 
                      data = AllData[(AllData$Forest == Forest & AllData$R3ERUlabel != 'k'),], 
                      strata = AllData$Stratum[(AllData$Forest == Forest & AllData$R3ERUlabel != 'k')], 
                      fpc = AllData$fpc[(AllData$Forest == Forest& AllData$R3ERUlabel != 'k')]) 

svyboxplot(STBIOMSha ~ R3ERUlabel, data.svy, varwidth = T, col = colVec[c(1, 6, 7)], main = labels[i], ylim = c(0,700))
abline(h = 0)

## Tonto
i = i +1
Forest = forestname[i]
Forest
table(AllData$R3ERUlabel[(AllData$Forest == Forest)])
data.svy <- svydesign(ids = ~1, 
                      data = AllData[(AllData$Forest == Forest),], 
                      strata = AllData$Stratum[(AllData$Forest == Forest)], 
                      fpc = AllData$fpc[(AllData$Forest == Forest)]) 

svyboxplot(STBIOMSha ~ R3ERUlabel, data.svy, varwidth = T, col = colVec[c(2:4, 6, 7)], main = labels[i], ylim = c(0,700), ylab = 'AGB (tons/ha)')
abline(h = 0)

## Sitgreaves
i = i +1
Forest = forestname[i]
Forest
table(AllData$R3ERUlabel[(AllData$Forest == Forest)])
data.svy <- svydesign(ids = ~1, 
                      data = AllData[(AllData$Forest == Forest & AllData$R3ERUlabel != 'g'),], 
                      strata = AllData$Stratum[(AllData$Forest == Forest& AllData$R3ERUlabel != 'g')], 
                      fpc = AllData$fpc[(AllData$Forest == Forest& AllData$R3ERUlabel != 'g')]) 

svyboxplot(STBIOMSha ~ R3ERUlabel, data.svy, varwidth = T, col = colVec[c(1, 2, 6, 7)], main = labels[i], ylim = c(0,700))
abline(h = 0)

## SJWM
i = i +1
Forest = forestname[i]
Forest
table(AllData$R3ERUlabel[(AllData$Forest == Forest)])
data.svy <- svydesign(ids = ~1, 
                      data = AllData[(AllData$Forest == Forest & AllData$R3ERUlabel != 'g'),], 
                      strata = AllData$Stratum[(AllData$Forest == Forest & AllData$R3ERUlabel != 'g')], 
                      fpc = AllData$fpc[(AllData$Forest == Forest & AllData$R3ERUlabel != 'g')]) 

svyboxplot(STBIOMSha ~ R3ERUlabel, data.svy, varwidth = T, col = colVec[c(2, 5, 6, 7)], main = labels[i], ylim = c(0,700))
abline(h = 0)

#################################
##################################################################
#################################

par(mfrow = c(2, 3))
table(AllData$R3ERUlabel[(AllData$Forest == forestname[6] | AllData$Forest == forestname[7])])

data.svy <- svydesign(ids = ~1, 
                      data = AllData[((AllData$Forest == forestname[6] | AllData$Forest == forestname[7]) & AllData$R3ERUlabel != 'A'),], 
                      strata = AllData$Stratum[((AllData$Forest == forestname[6] | AllData$Forest == forestname[7]) & AllData$R3ERUlabel != 'A')], 
                      fpc = AllData$fpc[((AllData$Forest == forestname[6] | AllData$Forest == forestname[7]) & AllData$R3ERUlabel != 'A')]) 

svyboxplot(STBIOMSha ~ R3ERUlabel, data.svy, varwidth = T, col = colVec[c(3, 5, 6, 7)], main = "g) Plots for Model Assessment", ylim = c(0,700), ylab = 'AGB (tons/ha)')
abline(h = 0)

#################
table <- svytotal(~R3ERUlabel, data.svy)

# decid and shrub
table[1]/sum(table)

# spruce
table[2]/sum(table)


# mixed conifer
table[3]/sum(table)

# PP
table[4]/sum(table)

#################

## Sit, P2
i = 6
Forest = forestname[i]
Forest
table(AllData$R3ERUlabel[(AllData$Forest == Forest)])

data.svy <- svydesign(ids = ~1, 
                      data = AllData[(AllData$Forest == Forest),], 
                      strata = AllData$Stratum[(AllData$Forest == Forest)], 
                      fpc = AllData$fpc[(AllData$Forest == Forest)]) 

svyboxplot(STBIOMSha ~ R3ERUlabel, data.svy, varwidth = T, col = colVec[c(3, 6, 7)], main = labels[i], ylim = c(0,700))
abline(h = 0)


## Sit, P3
i = 7
Forest = forestname[i]
Forest
table(AllData$R3ERUlabel[(AllData$Forest == Forest)])

data.svy <- svydesign(ids = ~1, 
                      data = AllData[(AllData$Forest == Forest & AllData$R3ERUlabel != 'A'),], 
                      strata = AllData$Stratum[(AllData$Forest == Forest & AllData$R3ERUlabel != 'A')], 
                      fpc = AllData$fpc[(AllData$Forest == Forest& AllData$R3ERUlabel != 'A')]) 

svyboxplot(STBIOMSha ~ R3ERUlabel, data.svy, varwidth = T, col = colVec[c(5, 6, 7)], main = labels[i], ylim = c(0,700))
abline(h = 0)


#################################
## create legend

ERU_legend_labels <- c(
  'A. herbaceous and grasslands',
  'B. Pinyon-juniper woodland', 
  'C. narrowleaf cottonwood, shrub',
  'D. alder and willow', 
  'E. spruce-fir', 
  'F. mixed conifer', 
  'G. Ponderosa pine')

par(mfrow = c(1, 1))
plot(1,1)
legend('bottomright', ERU_legend_labels, fill = colVec[-5])

#################################

par(mfrow = c(1, 1))

boxplot(AllData$STBIOMSha ~ AllData$R3ERUCODE, varwidth = T)
abline(h = 0)
################################################################
################################################################
## Area calculations
################################################################
################################################################

sum(unique(data.val$fpc[data.val$Forest == 'Coconino']))
sum(unique(data.val$fpc[data.val$Forest == 'NorthKaibab']))
sum(unique(data.val$fpc[data.val$Forest == 'Sitgreaves']))
sum(unique(data.val$fpc[data.val$Forest == 'Tonto']))
sum(unique(data.val$fpc[data.val$Forest == 'SWJM']))

sum(unique(Sit2$fpc))
sum(unique(Sit3$fpc))
