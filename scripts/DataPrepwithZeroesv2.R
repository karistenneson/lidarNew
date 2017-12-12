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
Coco <- read.csv("../Data/Coconino07162017.csv", stringsAsFactors=FALSE)
Tonto <- read.csv("../Data/Tonto07162017.csv", stringsAsFactors=FALSE)
Sit <- read.csv("../Data/Sitgreaves07162017.csv", stringsAsFactors=FALSE)
SWJM <- read.csv("../Data/SWJM07162017.csv", stringsAsFactors=FALSE)

### Independent Data
Sit2 <- read.csv("../Data/SitgreavesPhase2_09252017.csv", stringsAsFactors=FALSE)
Sit3 <- read.csv("../Data/Apache_09252017.csv", stringsAsFactors=FALSE)

## merge data from projects
AllData <- rbind(Coco, Sit, SWJM, Tonto, Kaibab, Sit2, Sit3)
remove(Coco, Sit, SWJM, Tonto, Kaibab, Sit2, Sit3)

# Change to metric units
AllData$STBIOMSha <- AllData$STBIOMS*2.47105
AllData$TCUmha <- AllData$TCUFT*2.47105*0.0283168

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

remove(AuxTrim, Aux, NDVI)

## Simplify the ERU units
AllData$R3ERUcodeFull[AllData$R3ERUCODE == 'CPGB'] <- 'Colorado Plateau, Great Basin grassland' # A
AllData$R3ERUcodeFull[AllData$R3ERUCODE == 'MSG'] <- 'Montane, subalpine grassland' # A
AllData$R3ERUcodeFull[AllData$R3ERUCODE == 'PJO'] <- 'Pinyon-juniper woodland' # B
AllData$R3ERUcodeFull[AllData$R3ERUCODE == '230'] <- 'Narrowleaf cottonwood, shrub' # C
AllData$R3ERUcodeFull[AllData$R3ERUCODE == '110'] <- 'Arizona alder, willow' # D
AllData$R3ERUcodeFull[AllData$R3ERUCODE == '290'] <- 'Willow, thinleaf alder' # D
AllData$R3ERUcodeFull[AllData$R3ERUCODE == 'SFF'] <- 'Spruce-fir' # E
AllData$R3ERUcodeFull[AllData$R3ERUCODE == 'MCD'] <- 'Mixed conifer, freq. fire' # F
AllData$R3ERUcodeFull[AllData$R3ERUCODE == 'MCW'] <- 'Mixed conifer with aspen' # F
AllData$R3ERUcodeFull[AllData$R3ERUCODE == '350'] <- 'Ponderosa pine, willow' # G
AllData$R3ERUcodeFull[AllData$R3ERUCODE == 'PPE'] <- 'Ponderosa pine, evergreen oak' # G
AllData$R3ERUcodeFull[AllData$R3ERUCODE == 'PPF'] <- 'Ponderosa pine' # G

AllData$R3ERUlabel[AllData$R3ERUCODE == 'CPGB'] <- 'A'
AllData$R3ERUlabel[AllData$R3ERUCODE == 'MSG'] <- 'A'
AllData$R3ERUlabel[AllData$R3ERUCODE == 'PJO'] <- 'B'
AllData$R3ERUlabel[AllData$R3ERUCODE == '230'] <- 'C'
AllData$R3ERUlabel[AllData$R3ERUCODE == '110'] <- 'D'
AllData$R3ERUlabel[AllData$R3ERUCODE == '290'] <- 'D'
AllData$R3ERUlabel[AllData$R3ERUCODE == 'SFF'] <- 'E'
AllData$R3ERUlabel[AllData$R3ERUCODE == 'MCD'] <- 'F'
AllData$R3ERUlabel[AllData$R3ERUCODE == 'MCW'] <- 'F'
AllData$R3ERUlabel[AllData$R3ERUCODE == '350'] <- 'G'
AllData$R3ERUlabel[AllData$R3ERUCODE == 'PPE'] <- 'G'
AllData$R3ERUlabel[AllData$R3ERUCODE == 'PPF'] <- 'G'

AllData$R3ERUlabelName[AllData$R3ERUCODE == 'CPGB'] <- 'A. herbaceous and grasslands'
AllData$R3ERUlabelName[AllData$R3ERUCODE == 'MSG'] <- 'A. herbaceous and grasslands'
AllData$R3ERUlabelName[AllData$R3ERUCODE == 'PJO'] <- 'B. Pinyon-juniper woodland'
AllData$R3ERUlabelName[AllData$R3ERUCODE == '230'] <- 'C. narrowleaf cottonwood, shrub'
AllData$R3ERUlabelName[AllData$R3ERUCODE == '110'] <- 'D. alder and willow'
AllData$R3ERUlabelName[AllData$R3ERUCODE == '290'] <- 'D. alder and willow'
AllData$R3ERUlabelName[AllData$R3ERUCODE == 'SFF'] <- 'E. spruce-fir'
AllData$R3ERUlabelName[AllData$R3ERUCODE == 'MCD'] <- 'F. mixed conifer'
AllData$R3ERUlabelName[AllData$R3ERUCODE == 'MCW'] <- 'F. mixed conifer'
AllData$R3ERUlabelName[AllData$R3ERUCODE == '350'] <- 'G. Ponderosa pine'
AllData$R3ERUlabelName[AllData$R3ERUCODE == 'PPE'] <- 'G. Ponderosa pine'
AllData$R3ERUlabelName[AllData$R3ERUCODE == 'PPF'] <- 'G. Ponderosa pine'

##########################################
### Partion data
Variables <- colnames(AllData) #pull variable names for use with select()
## exclude total count lidar metrics, use only the relative count measures (e.g., percent returns)
LidarNames <- Variables[c(23:57, 60:65)] #subset of lidar metrics
FieldNames <- Variables[c(1:20, 74, 79, 81)] #subset of field variables
AuxNames <- Variables[c(75:78, 80)] #subset of additional environmental variables
RandUnif <- Variables[11] #The Random Uniform Variable
#head(select(AllData, LidarNames))

Predictors <- c(LidarNames, AuxNames, FieldNames[c(6, 7,9, 12, 15, 22:23)])

### Filter data down for modeling
DATA.mod <- AllData[(AllData$RandomUniform < 0.75 & AllData$Forest != 'Sitgreaves, P2' & AllData$Forest != 'Apache'),]
DATA.val <- AllData[(AllData$RandomUniform>=0.75 & AllData$Forest != 'Sitgreaves, P2' & AllData$Forest != 'Apache'),]
DATA.val.ind <- AllData[(AllData$Forest == 'Sitgreaves, P2' | AllData$Forest == 'Apache'),]

#Select predictors, chuck unused variables
data.mod <- select(DATA.mod, STBIOMSha, TCUmha, Predictors)
data.val <- select(DATA.val, STBIOMSha, TCUmha, Predictors)
data.val.ind <- select(DATA.val.ind, STBIOMSha, TCUmha, Predictors)

remove(AllData, DATA.mod, DATA.val, DATA.val.ind)
remove(AuxNames, FieldNames, LidarNames, RandUnif, Variables, Predictors)

# Centering data
#DATA.modC <- cbind(data.mod[, c(1,2)], center(data.mod[,c(-1, -2, -53, -58, -59)]), data.mod[c(53, 58, 59)])
#DATA.valC <- cbind(data.val[, c(1,2)], center(data.val[,c(-1, -2, -53, -58, -59)]), data.val[c(53, 58, 59)])

