### This file is for processing the raw data into working data frames. 
## Written by MS Patterson (maspatte@uw.edu) and Karis Tenneson (karistenneson@gmail.com)
# Last updated: Dec 9 2017

### Set working environment, necessary for any next steps. 

#setwd('C:\\Users\\krtenneson\\Desktop\\lidarPaper\\lidarNew\\scripts')
#setwd('\\\\166.2.126.25\\rseat\\Programs\\Reimbursibles\\fy2016\\R3_lidar_equation_transferability\\Analysis\\VersionControl\\lidarNew\\scripts')
#setwd("~/Documents/R/lidarNew/scripts") #Mac
#setwd("~/R/lidarNew/scripts") #WinCampus
#setwd("~/R/projects/lidarNew/scripts") #WinHome

### Load required packages
library(tidyverse)

library(survey)
options(survey.lonely.psu="certainty")
options(survey.lonely.psu="adjust")

### Source functions
source(file="functions.R")

### Bring in data
Kaibab <- read.csv("../Data/NKaibab07192017.csv", stringsAsFactors=FALSE)
Kaibab$STBIOMSha <- Kaibab$STBIOMS*2.47105
Kaibab$TCUmha <- Kaibab$TCUFT*2.47105*0.0283168

DataSub<-Kaibab[Kaibab$RandomUniform <0.75,]
data.svy <- svydesign(ids = ~1, data = DataSub, strata = DataSub$Stratum, fpc = DataSub$fpc) 
svymean(~STBIOMSha,design=data.svy)
svymean(~TCUmha,design=data.svy)

DataSub<-Kaibab[Kaibab$RandomUniform >=0.75,]
data.svy <- svydesign(ids = ~1, data = DataSub, strata = DataSub$Stratum, fpc = DataSub$fpc) 
svymean(~STBIOMSha,design=data.svy)
svymean(~TCUmha,design=data.svy)

##
Coco <- read.csv("../Data/Coconino07162017.csv", stringsAsFactors=FALSE)
Coco$STBIOMSha <- Coco$STBIOMS*2.47105
Coco$TCUmha <- Coco$TCUFT*2.47105*0.0283168

CocoSub<-Coco[Coco$RandomUniform <0.75,]
data.svy <- svydesign(ids = ~1, data = CocoSub, strata = CocoSub$Stratum, fpc = CocoSub$fpc) 
svymean(~STBIOMSha,design=data.svy)
svymean(~TCUmha,design=data.svy)

CocoSub<-Coco[Coco$RandomUniform >=0.75,]
data.svy <- svydesign(ids = ~1, data = CocoSub, strata = CocoSub$Stratum, fpc = CocoSub$fpc) 
svymean(~STBIOMSha,design=data.svy)
svymean(~TCUmha,design=data.svy)

table(Coco$PlotSizeAcres)
table(Tonto$PlotSizeAcres)
table(Sit$PlotSizeAcres)
table(SWJM$PlotSizeAcres)
##


Tonto <- read.csv("../Data/Tonto07162017.csv", stringsAsFactors=FALSE)
Tonto$STBIOMSha <- Tonto$STBIOMS*2.47105
Tonto$TCUmha <- Tonto$TCUFT*2.47105*0.0283168

DataSub<-Tonto[Tonto$RandomUniform <0.75,]
data.svy <- svydesign(ids = ~1, data = DataSub, strata = DataSub$Stratum, fpc = DataSub$fpc) 
svymean(~STBIOMSha,design=data.svy)
svymean(~TCUmha,design=data.svy)

DataSub<-Tonto[Tonto$RandomUniform >=0.75,]
data.svy <- svydesign(ids = ~1, data = DataSub, strata = DataSub$Stratum, fpc = DataSub$fpc) 
svymean(~STBIOMSha,design=data.svy)
svymean(~TCUmha,design=data.svy)
##

Sit <- read.csv("../Data/Sitgreaves07162017.csv", stringsAsFactors=FALSE)
Sit$STBIOMSha <- Sit$STBIOMS*2.47105
Sit$TCUmha <- Sit$TCUFT*2.47105*0.0283168

DataSub<-Sit[Sit$RandomUniform <0.75,]
data.svy <- svydesign(ids = ~1, data = DataSub, strata = DataSub$Stratum, fpc = DataSub$fpc) 
svymean(~STBIOMSha,design=data.svy)
svymean(~TCUmha,design=data.svy)

DataSub<-Sit[Sit$RandomUniform >=0.75,]
data.svy <- svydesign(ids = ~1, data = DataSub, strata = DataSub$Stratum, fpc = DataSub$fpc) 
svymean(~STBIOMSha,design=data.svy)
svymean(~TCUmha,design=data.svy)
##

SWJM <- read.csv("../Data/SWJM07162017.csv", stringsAsFactors=FALSE)
SWJM$STBIOMSha <- SWJM$STBIOMS*2.47105
SWJM$TCUmha <- SWJM$TCUFT*2.47105*0.0283168

DataSub<-SWJM[SWJM$RandomUniform <0.75,]
data.svy <- svydesign(ids = ~1, data = DataSub, strata = DataSub$Stratum, fpc = DataSub$fpc) 
svymean(~STBIOMSha,design=data.svy)
svymean(~TCUmha,design=data.svy)

DataSub<-SWJM[SWJM$RandomUniform >=0.75,]
data.svy <- svydesign(ids = ~1, data = DataSub, strata = DataSub$Stratum, fpc = DataSub$fpc) 
svymean(~STBIOMSha,design=data.svy)
svymean(~TCUmha,design=data.svy)
##

AllData <- rbind(Coco, Sit, SWJM, Tonto, Kaibab)

#Bring in Environmental Data
Aux <- read.csv("../Data/Auxillary/Merged_10172017.csv", stringsAsFactors=FALSE)
AuxTrim <- select(Aux, PLOT_ID, R3ERUCODE, elevation, aspect, slope)
AllData <- merge(AllData, AuxTrim, by="PLOT_ID", all=F)
AllData$R3ERUCODE <- as.factor(AllData$R3ERUCODE)
AllData$Forest <- as.factor(AllData$Forest)
AllData$Site <- as.factor(AllData$Site)

NDVI <- read.csv("../Data/Auxillary/NDVIamplitude.csv")

AllData <- merge(AllData, select(NDVI, PLOT_ID, NDVI_Sample_NDVI_amplitude_1), by="PLOT_ID", all=F)
AllData <- rename(AllData, NDVI_Amp=NDVI_Sample_NDVI_amplitude_1)

#Remove 'zero' biomass outliers
#AllData <- AllData[AllData$STBIOMS>0,]
#AllData <- AllData[AllData$TCUFT>0,]

### Partion data
Variables <- colnames(AllData) #pull variable names for use with select()
## exclude total count lidar metrics, use only the relative count measures (e.g., percent returns)
LidarNames <- Variables[c(23:57, 60:65)] #subset of lidar metrics
FieldNames <- Variables[1:20] #subset of field variables
AuxNames <- Variables[74:78] #subset of additional environmental variables
RandUnif <- Variables[11] #The Random Uniform Variable
#head(select(AllData, LidarNames))

Predictors <- c(LidarNames, AuxNames, FieldNames[c(6, 7,9, 12, 15)])

### Filter data down for modeling
DATA.mod <- AllData[AllData$RandomUniform<0.75,]

#Select predictors, chuck unused variables
data.mod <- select(DATA.mod, STBIOMSha, TCUmha, Predictors)

# Centering data
#DATA.modC <- cbind(data.mod[, c(1,2)], center(data.mod[,c(-1, -2, -53, -58, -59)]), data.mod[c(53, 58, 59)])

#########################################################
### Filter data down for modeling
DATA.val <- AllData[AllData$RandomUniform>=0.75,]

#Select predictors, chuck unused variables
data.val <- select(DATA.val, STBIOMSha, TCUmha, Predictors)

# Centering data
#DATA.valC <- cbind(data.val[, c(1,2)], center(data.val[,c(-1, -2, -53, -58, -59)]), data.val[c(53, 58, 59)])

#########################################################

# average biomass, volume, elevation
Subset<- data.mod
data.mod.svy <- svydesign(ids = ~1, data = Subset, strata = Subset$Stratum, fpc = Subset$fpc) 
svymean(~STBIOMSha, data.mod.svy)
svymean(~TCUmha, data.mod.svy)
svymean(~elevation, data.mod.svy)

Subsetv<- data.val
data.val.svy <- svydesign(ids = ~1, data = Subsetv, strata = Subsetv$Stratum, fpc = Subsetv$fpc) 
svymean(~STBIOMSha, data.val.svy)
svymean(~TCUmha, data.val.svy)
svymean(~elevation, data.val.svy)

# average elevation by project
i =  0
forestname <- unique(data.mod$Forest)

i = i +1
Forest = forestname[i]
Forest
Subset<- data.mod[data.mod$Forest == Forest,]
data.mod.svy <- svydesign(ids = ~1, data = Subset, strata = Subset$Stratum, fpc = Subset$fpc) 
svymean(~elevation, data.mod.svy)

Subsetv<- data.val[data.val$Forest == Forest,]
data.val.svy <- svydesign(ids = ~1, data = Subsetv, strata = Subsetv$Stratum, fpc = Subsetv$fpc) 
svymean(~elevation, data.val.svy)


################################################################
################################################################
## Independent Data
################################################################
################################################################

Sit2 <- read.csv("../Data/SitgreavesPhase2_09252017.csv", stringsAsFactors=FALSE)
Sit2$STBIOMSha <- Sit2$STBIOMS*2.47105
Sit2$TCUmha <- Sit2$TCUFT*2.47105*0.0283168

DataSub<-Sit2#[Sit2$STBIOMSha > 0,]
data.svy <- svydesign(ids = ~1, data = DataSub, strata = DataSub$Stratum, fpc = DataSub$fpc) 
svymean(~STBIOMSha,design=data.svy)
svymean(~TCUmha,design=data.svy)
##

Sit3 <- read.csv("../Data/Apache_09252017.csv", stringsAsFactors=FALSE)
Sit3$STBIOMSha <- Sit3$STBIOMS*2.47105
Sit3$TCUmha <- Sit3$TCUFT*2.47105*0.0283168

DataSub<-Sit3#[Sit3$STBIOMSha > 0,]
data.svy <- svydesign(ids = ~1, data = DataSub, strata = DataSub$Stratum, fpc = DataSub$fpc) 
svymean(~STBIOMSha,design=data.svy)
svymean(~TCUmha,design=data.svy)
##

ValData <- rbind(Sit2, Sit3)

#Bring in Environmental Data
ValData <- merge(ValData, AuxTrim, by="PLOT_ID", all=F)
ValData$R3ERUCODE <- as.factor(ValData$R3ERUCODE)
ValData$Forest <- as.factor(ValData$Forest)
ValData$Site <- as.factor(ValData$Site)

ValData <- merge(ValData, select(NDVI, PLOT_ID, NDVI_Sample_NDVI_amplitude_1), by="PLOT_ID", all=F)
ValData <- rename(ValData, NDVI_Amp=NDVI_Sample_NDVI_amplitude_1)

#Remove 'zero' biomass outliers
#ValData <- ValData[ValData$STBIOMS>0,]
#ValData <- ValData[ValData$TCUFT>0,]

#Select predictors, chuck unused variables
data.ind.val <- select(ValData, STBIOMSha, TCUmha, Predictors)

# average biomass and volume
Subset<- data.ind.val
data.mod.svy <- svydesign(ids = ~1, data = Subset, strata = Subset$Stratum, fpc = Subset$fpc) 
svymean(~STBIOMSha, data.mod.svy)
svymean(~TCUmha, data.mod.svy)
svymean(~elevation, data.mod.svy)

# average elevation by project
forestname <- unique(data.ind.val$Forest)
i = 0
i = i +1
Forest = forestname[i]
Forest
Subset<- data.ind.val[data.ind.val$Forest == Forest,]
data.mod.svy <- svydesign(ids = ~1, data = Subset, strata = Subset$Stratum, fpc = Subset$fpc) 
svymean(~elevation, data.mod.svy)


################################################################
################################################################
## Simplify the ERU units
################################################################
################################################################
fulldata <- rbind(data.mod, data.val, data.ind.val)
unique(fulldata$R3ERUCODE)
fulldata$R3ERUlabel <- 'update'
fulldata$R3ERUlabel[fulldata$R3ERUCODE == 'CPGB'] <- 'A'
fulldata$R3ERUlabel[fulldata$R3ERUCODE == 'MSG'] <- 'A'

fulldata$R3ERUlabel[fulldata$R3ERUCODE == 'PJO'] <- 'B'

fulldata$R3ERUlabel[fulldata$R3ERUCODE == '230'] <- 'C'
fulldata$R3ERUlabel[fulldata$R3ERUCODE == '110'] <- 'D'

fulldata$R3ERUlabel[fulldata$R3ERUCODE == 'SFF'] <- 'E'
fulldata$R3ERUlabel[fulldata$R3ERUCODE == 'MCD'] <- 'F'
fulldata$R3ERUlabel[fulldata$R3ERUCODE == 'MCW'] <- 'F'
fulldata$R3ERUlabel[fulldata$R3ERUCODE == '350'] <- 'G'
fulldata$R3ERUlabel[fulldata$R3ERUCODE == 'PPE'] <- 'G'
fulldata$R3ERUlabel[fulldata$R3ERUCODE == 'PPF'] <- 'G'

fulldata$R3ERUlabel[fulldata$R3ERUCODE == '290'] <- 'k'

unique(fulldata$R3ERUlabel)
#fulldata <- fulldata[fulldata$R3ERUlabel != 'k', ]
fulldata$R3ERUlabel <- as.factor(fulldata$R3ERUlabel)
fulldata$R3ERUlabel <- as.character(fulldata$R3ERUlabel)

fulldata$R3ERUlabelFull[fulldata$R3ERUCODE == 'CPGB'] <- 'Colorado Plateau, Great Basin grassland' # a
fulldata$R3ERUlabelFull[fulldata$R3ERUCODE == 'MSG'] <- 'Montane, subalpine grassland' # A

fulldata$R3ERUlabelFull[fulldata$R3ERUCODE == 'PJO'] <- 'Pinyon-juniper woodland' # B

fulldata$R3ERUlabelFull[fulldata$R3ERUCODE == '230'] <- 'Narrowleaf cottonwood, shrub' # C
fulldata$R3ERUlabelFull[fulldata$R3ERUCODE == '110'] <- 'Arizona alder, willow' # D

fulldata$R3ERUlabelFull[fulldata$R3ERUCODE == 'SFF'] <- 'Spruce-fir'
# E

fulldata$R3ERUlabelFull[fulldata$R3ERUCODE == 'MCD'] <- 'Mixed conifer, freq. fire' # F
fulldata$R3ERUlabelFull[fulldata$R3ERUCODE == 'MCW'] <- 'Mixed conifer with aspen' # F

fulldata$R3ERUlabelFull[fulldata$R3ERUCODE == '350'] <- 'Ponderosa pine, willow' # G
fulldata$R3ERUlabelFull[fulldata$R3ERUCODE == 'PPE'] <- 'Ponderosa pine, evergreen oak' # G
fulldata$R3ERUlabelFull[fulldata$R3ERUCODE == 'PPF'] <- 'Ponderosa pine' # G

fulldata$R3ERUlabelFull[fulldata$R3ERUCODE == '290'] <- 'Willow, thinleaf alder' # k

unique(fulldata$R3ERUlabel)

colVecFull <- c('yellow','yellow4',
            'royalblue1','royalblue4','royalblue3',
            'plum1','violetred2','violetred4',
            'olivedrab1','limegreen','seagreen4')

colVec <- c('yellow', 'orange', 'red', 
            'royalblue3',
            'plum1', 'violetred4', 'seagreen4')

#################################
par(mfrow = c(2, 3))
forestname <- unique(fulldata$Forest)
forestname <- forestname[c(4, 1, 3, 2, 5, 7, 6)]
labels <- c("b) Kaibab Plateau", 'c) Coconino N.F.', 'd) Tonto N.F.', 'e) Apache-Sitgreaves N.F., Phase 1', 'f) Santa Fe N.F.',  'h) Apache-Sitgreaves N.F., Phase 2',  'i) Apache-Sitgreaves N.F., Phase 3')

table(fulldata$R3ERUlabel[(fulldata$Forest != forestname[6] & fulldata$Forest != forestname[7])])
data.svy <- svydesign(ids = ~1, 
                      data = fulldata[(fulldata$Forest != forestname[6] & fulldata$Forest != forestname[7] & fulldata$R3ERUlabel != 'k'),], 
                      strata = fulldata$Stratum[(fulldata$Forest != forestname[6] & fulldata$Forest != forestname[7] & fulldata$R3ERUlabel != 'k')], 
                      fpc = fulldata$fpc[(fulldata$Forest != forestname[6] & fulldata$Forest != forestname[7] & fulldata$R3ERUlabel != 'k')]) 

svyboxplot(STBIOMSha ~ R3ERUlabel, data.svy, varwidth = T, col = colVec, main = "a) All Plots used for Model Development", ylim = c(0,700), ylab = 'AGB (tons/ha)')
abline(h = 0)

#################

table <- svytotal(~R3ERUlabel, data.svy)

# grasslands
(table[1]/sum(table))*100
(10.5/sum(table))*100

# PJ
100*(table[2]/sum(table))
100*(1598.4/sum(table))

#** with shrub
100*table[3]/sum(table)
100*695.9/sum(table)

# decid
100*table[4]/sum(table)
100*19104/sum(table)

# spruce
100*table[5]/sum(table)
100*20/sum(table)

# mixed conifer
100*table[6]/sum(table)
100*3401/sum(table)

# PP
100*table[7]/sum(table)
100*24187.3/sum(table)

#################

## Kaibab  
i = 1
Forest = forestname[i]
Forest

table(fulldata$R3ERUlabel[(fulldata$Forest == Forest)])
data.svy <- svydesign(ids = ~1, 
                      data = fulldata[(fulldata$Forest == Forest & fulldata$R3ERUlabel != 'A'& fulldata$R3ERUlabel != 'B'),], 
                      strata = fulldata$Stratum[(fulldata$Forest == Forest & fulldata$R3ERUlabel != 'A'& fulldata$R3ERUlabel != 'B')], 
                      fpc = fulldata$fpc[(fulldata$Forest == Forest & fulldata$R3ERUlabel != 'A'& fulldata$R3ERUlabel != 'B')]) 

svyboxplot(STBIOMSha ~ R3ERUlabel, data.svy, varwidth = T, col = colVec[c(5, 6, 7)], main = labels[i], ylim = c(0,700))
abline(h = 0)


## Coco
i = i +1
Forest = forestname[i]
Forest
table(fulldata$R3ERUlabel[(fulldata$Forest == Forest)])
data.svy <- svydesign(ids = ~1, 
                      data = fulldata[(fulldata$Forest == Forest & fulldata$R3ERUlabel != 'k'),], 
                      strata = fulldata$Stratum[(fulldata$Forest == Forest & fulldata$R3ERUlabel != 'k')], 
                      fpc = fulldata$fpc[(fulldata$Forest == Forest& fulldata$R3ERUlabel != 'k')]) 

svyboxplot(STBIOMSha ~ R3ERUlabel, data.svy, varwidth = T, col = colVec[c(1, 6, 7)], main = labels[i], ylim = c(0,700))
abline(h = 0)

## Tonto
i = i +1
Forest = forestname[i]
Forest
table(fulldata$R3ERUlabel[(fulldata$Forest == Forest)])
data.svy <- svydesign(ids = ~1, 
                      data = fulldata[(fulldata$Forest == Forest),], 
                      strata = fulldata$Stratum[(fulldata$Forest == Forest)], 
                      fpc = fulldata$fpc[(fulldata$Forest == Forest)]) 

svyboxplot(STBIOMSha ~ R3ERUlabel, data.svy, varwidth = T, col = colVec[c(2:4, 6, 7)], main = labels[i], ylim = c(0,700), ylab = 'AGB (tons/ha)')
abline(h = 0)

## Sitgreaves
i = i +1
Forest = forestname[i]
Forest
table(fulldata$R3ERUlabel[(fulldata$Forest == Forest)])
data.svy <- svydesign(ids = ~1, 
                      data = fulldata[(fulldata$Forest == Forest & fulldata$R3ERUlabel != 'g'),], 
                      strata = fulldata$Stratum[(fulldata$Forest == Forest& fulldata$R3ERUlabel != 'g')], 
                      fpc = fulldata$fpc[(fulldata$Forest == Forest& fulldata$R3ERUlabel != 'g')]) 

svyboxplot(STBIOMSha ~ R3ERUlabel, data.svy, varwidth = T, col = colVec[c(1, 2, 6, 7)], main = labels[i], ylim = c(0,700))
abline(h = 0)

## SJWM
i = i +1
Forest = forestname[i]
Forest
table(fulldata$R3ERUlabel[(fulldata$Forest == Forest)])
data.svy <- svydesign(ids = ~1, 
                      data = fulldata[(fulldata$Forest == Forest & fulldata$R3ERUlabel != 'g'),], 
                      strata = fulldata$Stratum[(fulldata$Forest == Forest & fulldata$R3ERUlabel != 'g')], 
                      fpc = fulldata$fpc[(fulldata$Forest == Forest & fulldata$R3ERUlabel != 'g')]) 

svyboxplot(STBIOMSha ~ R3ERUlabel, data.svy, varwidth = T, col = colVec[c(2, 5, 6, 7)], main = labels[i], ylim = c(0,700))
abline(h = 0)

#################################
##################################################################
#################################

par(mfrow = c(2, 3))
table(fulldata$R3ERUlabel[(fulldata$Forest == forestname[6] | fulldata$Forest == forestname[7])])

data.svy <- svydesign(ids = ~1, 
                      data = fulldata[((fulldata$Forest == forestname[6] | fulldata$Forest == forestname[7]) & fulldata$R3ERUlabel != 'A'),], 
                      strata = fulldata$Stratum[((fulldata$Forest == forestname[6] | fulldata$Forest == forestname[7]) & fulldata$R3ERUlabel != 'A')], 
                      fpc = fulldata$fpc[((fulldata$Forest == forestname[6] | fulldata$Forest == forestname[7]) & fulldata$R3ERUlabel != 'A')]) 

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
table(fulldata$R3ERUlabel[(fulldata$Forest == Forest)])

data.svy <- svydesign(ids = ~1, 
                      data = fulldata[(fulldata$Forest == Forest),], 
                      strata = fulldata$Stratum[(fulldata$Forest == Forest)], 
                      fpc = fulldata$fpc[(fulldata$Forest == Forest)]) 

svyboxplot(STBIOMSha ~ R3ERUlabel, data.svy, varwidth = T, col = colVec[c(3, 6, 7)], main = labels[i], ylim = c(0,700))
abline(h = 0)


## Sit, P3
i = 7
Forest = forestname[i]
Forest
table(fulldata$R3ERUlabel[(fulldata$Forest == Forest)])

data.svy <- svydesign(ids = ~1, 
                      data = fulldata[(fulldata$Forest == Forest & fulldata$R3ERUlabel != 'A'),], 
                      strata = fulldata$Stratum[(fulldata$Forest == Forest & fulldata$R3ERUlabel != 'A')], 
                      fpc = fulldata$fpc[(fulldata$Forest == Forest& fulldata$R3ERUlabel != 'A')]) 

svyboxplot(STBIOMSha ~ R3ERUlabel, data.svy, varwidth = T, col = colVec[c(5, 6, 7)], main = labels[i], ylim = c(0,700))
abline(h = 0)


#################################
## create legend

ERU_legend_labels <- c(
  'A. grasslands',
  'B. Pinyon-juniper woodland', 
  'C. narrowleaf cottonwood, shrub',
  'D. Arizona alder, willow', 
  'E. spruce-fir', 
  'F. mixed conifer', 
  'G. Ponderosa pine')

par(mfrow = c(1, 1))
plot(1,1)
legend('bottomright', ERU_legend_labels, fill = colVec[-5])

#################################

par(mfrow = c(1, 1))

boxplot(fulldata$STBIOMSha ~ fulldata$R3ERUCODE, varwidth = T)
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
