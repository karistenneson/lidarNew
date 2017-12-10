### This file is for processing the raw data into working data frames. 
## Written by MS Patterson (maspatte@uw.edu) and modified by Karis Tenneson (karistenneson@gmail.com)
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
Coco <- read.csv("../Data/Coconino07162017.csv", stringsAsFactors=FALSE)
Coco$STBIOMSha <- Coco$STBIOMS*2.47105

CocoSub<-Coco[Coco$RandomUniform <0.75 & Coco$STBIOMSha > 0,]
data.svy <- svydesign(ids = ~1, data = CocoSub, strata = CocoSub$Stratum, fpc = CocoSub$fpc) 
svymean(~STBIOMSha,design=data.svy)
svymean(~TCUFT,design=data.svy)

CocoSub<-Coco[Coco$RandomUniform >=0.75 & Coco$STBIOMSha > 0,]
data.svy <- svydesign(ids = ~1, data = CocoSub, strata = CocoSub$Stratum, fpc = CocoSub$fpc) 
svymean(~STBIOMSha,design=data.svy)
svymean(~TCUFT,design=data.svy)
##

Sit <- read.csv("../Data/Sitgreaves07162017.csv", stringsAsFactors=FALSE)
Sit$STBIOMSha <- Sit$STBIOMS*2.47105
DataSub<-Sit[Sit$RandomUniform <0.75 & Sit$STBIOMSha > 0,]
data.svy <- svydesign(ids = ~1, data = DataSub, strata = DataSub$Stratum, fpc = DataSub$fpc) 
svymean(~STBIOMSha,design=data.svy)
svymean(~TCUFT,design=data.svy)

DataSub<-Sit[Sit$RandomUniform >=0.75 & Sit$STBIOMSha > 0,]
data.svy <- svydesign(ids = ~1, data = DataSub, strata = DataSub$Stratum, fpc = DataSub$fpc) 
svymean(~STBIOMSha,design=data.svy)
svymean(~TCUFT,design=data.svy)
##

SWJM <- read.csv("../Data/SWJM07162017.csv", stringsAsFactors=FALSE)
SWJM$STBIOMSha <- SWJM$STBIOMS*2.47105
DataSub<-SWJM[SWJM$RandomUniform <0.75 & SWJM$STBIOMSha > 0,]
data.svy <- svydesign(ids = ~1, data = DataSub, strata = DataSub$Stratum, fpc = DataSub$fpc) 
svymean(~STBIOMSha,design=data.svy)
svymean(~TCUFT,design=data.svy)

DataSub<-SWJM[SWJM$RandomUniform >=0.75 & SWJM$STBIOMSha > 0,]
data.svy <- svydesign(ids = ~1, data = DataSub, strata = DataSub$Stratum, fpc = DataSub$fpc) 
svymean(~STBIOMSha,design=data.svy)
svymean(~TCUFT,design=data.svy)
##

Tonto <- read.csv("../Data/Tonto07162017.csv", stringsAsFactors=FALSE)
Tonto$STBIOMSha <- Tonto$STBIOMS*2.47105
DataSub<-Tonto[Tonto$RandomUniform <0.75 & Tonto$STBIOMSha > 0,]
data.svy <- svydesign(ids = ~1, data = DataSub, strata = DataSub$Stratum, fpc = DataSub$fpc) 
svymean(~STBIOMSha,design=data.svy)
svymean(~TCUFT,design=data.svy)

DataSub<-Tonto[Tonto$RandomUniform >=0.75 & Tonto$STBIOMSha > 0,]
data.svy <- svydesign(ids = ~1, data = DataSub, strata = DataSub$Stratum, fpc = DataSub$fpc) 
svymean(~STBIOMSha,design=data.svy)
svymean(~TCUFT,design=data.svy)
##

Kaibab <- read.csv("../Data/NKaibab07192017.csv", stringsAsFactors=FALSE)
Kaibab$STBIOMSha <- Kaibab$STBIOMS*2.47105
DataSub<-Kaibab[Kaibab$RandomUniform <0.75 & Kaibab$STBIOMSha > 0,]
data.svy <- svydesign(ids = ~1, data = DataSub, strata = DataSub$Stratum, fpc = DataSub$fpc) 
svymean(~STBIOMSha,design=data.svy)
svymean(~TCUFT,design=data.svy)

DataSub<-Kaibab[Kaibab$RandomUniform >=0.75 & Kaibab$STBIOMSha > 0,]
data.svy <- svydesign(ids = ~1, data = DataSub, strata = DataSub$Stratum, fpc = DataSub$fpc) 
svymean(~STBIOMSha,design=data.svy)
svymean(~TCUFT,design=data.svy)

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
AllData <- AllData[AllData$STBIOMS>0,]
AllData <- AllData[AllData$TCUFT>0,]

### Partion data
Variables <- colnames(AllData) #pull variable names for use with select()
## exclude total count lidar metrics, use only the relative count measures (e.g., percent returns)
LidarNames <- Variables[c(23:57, 60:65)] #subset of lidar metrics
FieldNames <- Variables[1:20] #subset of field variables
AuxNames <- Variables[72:76] #subset of additional environmental variables
RandUnif <- Variables[11] #The Random Uniform Variable
#head(select(AllData, LidarNames))

Predictors <- c(LidarNames, AuxNames, FieldNames[c(6, 7,9, 12, 15)])

### Filter data down for modeling
DATA.mod <- AllData[AllData$RandomUniform<0.75,]

#Select predictors, chuck unused variables
data.mod <- select(DATA.mod, STBIOMSha, TCUFT, Predictors)

# Centering data
#DATA.modC <- cbind(data.mod[, c(1,2)], center(data.mod[,c(-1, -2, -53, -58, -59)]), data.mod[c(53, 58, 59)])

#########################################################
### Filter data down for modeling
DATA.val <- AllData[AllData$RandomUniform>=0.75,]

#Select predictors, chuck unused variables
data.val <- select(DATA.val, STBIOMSha, TCUFT, Predictors)

# Centering data
#DATA.valC <- cbind(data.val[, c(1,2)], center(data.val[,c(-1, -2, -53, -58, -59)]), data.val[c(53, 58, 59)])

#########################################################
## Simplify the ERU units

#########################################################
library(survey)

# average biomass, volume, elevation
Subset<- data.mod
data.mod.svy <- svydesign(ids = ~1, data = Subset, strata = Subset$Stratum, fpc = Subset$fpc) 
svymean(~STBIOMSha, data.mod.svy)
svymean(~TCUFT, data.mod.svy)
svymean(~elevation, data.mod.svy)

Subsetv<- data.val
data.val.svy <- svydesign(ids = ~1, data = Subsetv, strata = Subsetv$Stratum, fpc = Subsetv$fpc) 
svymean(~STBIOMSha, data.val.svy)
svymean(~TCUFT, data.val.svy)
svymean(~elevation, data.val.svy)

# average elevation by project
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

## ERU composition
boxplot(data.modC$STBIOMS ~ data.modC$R3ERUCODE, varwidth = T)
boxplot(data.valC$STBIOMS ~ data.valC$R3ERUCODE, varwidth = T)

boxplot(data.modC$STBIOMS ~ data.modC$Forest, varwidth = T)
boxplot(data.valC$STBIOMS ~ data.valC$Forest, varwidth = T)

################################################################
################################################################
## Independent Data
################################################################
################################################################

Sit2 <- read.csv("../Data/SitgreavesPhase2_09252017.csv", stringsAsFactors=FALSE)
Sit2$STBIOMSha <- Sit2$STBIOMS*2.47105
DataSub<-Sit2[Sit2$STBIOMSha > 0,]
data.svy <- svydesign(ids = ~1, data = DataSub, strata = DataSub$Stratum, fpc = DataSub$fpc) 
svymean(~STBIOMSha,design=data.svy)
svymean(~TCUFT,design=data.svy)
##

Sit3 <- read.csv("../Data/Apache_09252017.csv", stringsAsFactors=FALSE)
Sit3$STBIOMSha <- Sit3$STBIOMS*2.47105
DataSub<-Sit3[Sit3$STBIOMSha > 0,]
data.svy <- svydesign(ids = ~1, data = DataSub, strata = DataSub$Stratum, fpc = DataSub$fpc) 
svymean(~STBIOMSha,design=data.svy)
svymean(~TCUFT,design=data.svy)
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
ValData <- ValData[ValData$STBIOMS>0,]
ValData <- ValData[ValData$TCUFT>0,]

#Select predictors, chuck unused variables
data.ind.val <- select(ValData, STBIOMSha, TCUFT, Predictors)

# average biomass and volume
Subset<- data.ind.val
data.mod.svy <- svydesign(ids = ~1, data = Subset, strata = Subset$Stratum, fpc = Subset$fpc) 
svymean(~STBIOMSha, data.mod.svy)
svymean(~TCUFT, data.mod.svy)
svymean(~elevation, data.mod.svy)

# average elevation by project
forestname <- unique(data.ind.val$Forest)

i = i +1
Forest = forestname[i]
Forest
Subset<- data.ind.val[data.ind.val$Forest == Forest,]
data.mod.svy <- svydesign(ids = ~1, data = Subset, strata = Subset$Stratum, fpc = Subset$fpc) 
svymean(~elevation, data.mod.svy)


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
