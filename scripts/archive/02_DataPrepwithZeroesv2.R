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
library(dplyr)
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
AuxTrim <- Aux[, c('PLOT_ID', 'R3ERUCODE', 'elevation', 'aspect', 'slope')]
AllData <- merge(AllData, AuxTrim, by="PLOT_ID", all=F)
AllData$R3ERUCODE <- as.factor(AllData$R3ERUCODE)
AllData$Forest <- as.factor(AllData$Forest)
AllData$Site <- as.factor(AllData$Site)

NDVI <- read.csv("../Data/Auxillary/NDVIamplitude.csv")

AllData <- merge(AllData, NDVI[, c('PLOT_ID', 'NDVI_Sample_NDVI_amplitude_1')], by="PLOT_ID", all=F)
AllData <- rename(AllData, NDVI_Amp=NDVI_Sample_NDVI_amplitude_1)

remove(AuxTrim, Aux, NDVI)

## Simplify the ERU units
AllData$R3ERUcodeFull[AllData$R3ERUCODE == 'CPGB'] <- 'Colorado Plateau, Great Basin grassland' # A
AllData$R3ERUcodeFull[AllData$R3ERUCODE == 'MSG'] <- 'Montane, subalpine grassland' # A
AllData$R3ERUcodeFull[AllData$R3ERUCODE == 'PJO'] <- 'Pinyon-juniper woodland' # B
AllData$R3ERUcodeFull[AllData$R3ERUCODE == '230'] <- 'Narrowleaf cottonwood, shrub' # C
AllData$R3ERUcodeFull[AllData$R3ERUCODE == '110'] <- 'Arizona alder, willow' # C
AllData$R3ERUcodeFull[AllData$R3ERUCODE == '290'] <- 'Willow, thinleaf alder' # C
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
AllData$R3ERUlabel[AllData$R3ERUCODE == '110'] <- 'C'
AllData$R3ERUlabel[AllData$R3ERUCODE == '290'] <- 'C'
AllData$R3ERUlabel[AllData$R3ERUCODE == 'SFF'] <- 'E'
AllData$R3ERUlabel[AllData$R3ERUCODE == 'MCD'] <- 'F'
AllData$R3ERUlabel[AllData$R3ERUCODE == 'MCW'] <- 'F'
AllData$R3ERUlabel[AllData$R3ERUCODE == '350'] <- 'G'
AllData$R3ERUlabel[AllData$R3ERUCODE == 'PPE'] <- 'G'
AllData$R3ERUlabel[AllData$R3ERUCODE == 'PPF'] <- 'G'

AllData$R3ERUlabelName[AllData$R3ERUCODE == 'CPGB'] <- 'A. herbaceous and grasslands'
AllData$R3ERUlabelName[AllData$R3ERUCODE == 'MSG'] <- 'A. herbaceous and grasslands'
AllData$R3ERUlabelName[AllData$R3ERUCODE == 'PJO'] <- 'B. Pinyon-juniper woodland'
AllData$R3ERUlabelName[AllData$R3ERUCODE == '230'] <- 'C. decid'
AllData$R3ERUlabelName[AllData$R3ERUCODE == '110'] <- 'C. decid'
AllData$R3ERUlabelName[AllData$R3ERUCODE == '290'] <- 'C. decid'
AllData$R3ERUlabelName[AllData$R3ERUCODE == 'SFF'] <- 'E. spruce-fir'
AllData$R3ERUlabelName[AllData$R3ERUCODE == 'MCD'] <- 'F. mixed conifer'
AllData$R3ERUlabelName[AllData$R3ERUCODE == 'MCW'] <- 'F. mixed conifer'
AllData$R3ERUlabelName[AllData$R3ERUCODE == '350'] <- 'G. Ponderosa pine'
AllData$R3ERUlabelName[AllData$R3ERUCODE == 'PPE'] <- 'G. Ponderosa pine'
AllData$R3ERUlabelName[AllData$R3ERUCODE == 'PPF'] <- 'G. Ponderosa pine'

##########################################
### Partion data
AllData$logSTBIOMSha <- log(AllData$STBIOMSha +1)

######################################################
## test volume correlations
## test log-log volume correlations

AllData$mode_pctAllOver3m <- AllData$Elev_mode * AllData$Pct_all_returns_above_ht 
#AllData$mode_pctAllOverMean <- AllData$Elev_mode * AllData$pct_all_returns_above_mean 
#AllData$logmode_pctAllOverMean <- log(AllData$Elev_mode+1) * log(AllData$pct_all_returns_above_mean+1) 
#AllData$mode_pctAllover3mDiv1st <- AllData$Elev_mode * AllData$all_returns_above_ht_div_Total_first_returns_x_100 
#AllData$logmode_pctAllover3mDiv1st <- log(AllData$Elev_mode+1) * log(AllData$all_returns_above_ht_div_Total_first_returns_x_100+1) 
AllData$mode_pctAlloverModeDiv1st <- AllData$Elev_mode * AllData$All_returns_above_mode_div_Total_first_returns_x_100 

#####################
AllData$P01_pctAllOver3m <- AllData$Elev_P01 * AllData$Pct_all_returns_above_ht 
#AllData$P01_pctAllOverMean <- AllData$Elev_P01 * AllData$pct_all_returns_above_mean 
#AllData$logP01_pctAllOverMean <- log(AllData$Elev_P01+ 1) * log(AllData$pct_all_returns_above_mean+ 1) 
#AllData$P01_pctAllover3mDiv1st <- AllData$Elev_P01 * AllData$all_returns_above_ht_div_Total_first_returns_x_100
#AllData$logP01_pctAllover3mDiv1st <- log(AllData$Elev_P01+ 1) * log(AllData$all_returns_above_ht_div_Total_first_returns_x_100+ 1) 
#AllData$P01_pctAlloverModeDiv1st <- AllData$Elev_P01 * AllData$All_returns_above_mode_div_Total_first_returns_x_100 
#AllData$logP01_pctAlloverModeDiv1st <- log(AllData$Elev_P01+ 1) * log(AllData$All_returns_above_mode_div_Total_first_returns_x_100+ 1) 

#####################
AllData$P10_pctAllOver3m <- AllData$Elev_P10 * AllData$Pct_all_returns_above_ht 
#AllData$P10_pctAllOverMean <- AllData$Elev_P10 * AllData$pct_all_returns_above_mean 
#AllData$logP10_pctAllOverMean <- log(AllData$Elev_P10+ 1) * log(AllData$pct_all_returns_above_mean+ 1) 
#AllData$P10_pctAllover3mDiv1st <- AllData$Elev_P10 * AllData$all_returns_above_ht_div_Total_first_returns_x_100 
#AllData$logP10_pctAllover3mDiv1st <- log(AllData$Elev_P10+ 1) * log(AllData$all_returns_above_ht_div_Total_first_returns_x_100+ 1) 
#AllData$P10_pctAlloverModeDiv1st <- AllData$Elev_P10 * AllData$All_returns_above_mode_div_Total_first_returns_x_100 
#AllData$logP10_pctAlloverModeDiv1st <- log(AllData$Elev_P10+ 1) * log(AllData$All_returns_above_mode_div_Total_first_returns_x_100+ 1) 

AllData$P30_pctAllOver3m <- AllData$Elev_P30 * AllData$Pct_all_returns_above_ht 
#AllData$P30_pctAllOverMean <- AllData$Elev_P30 * AllData$pct_all_returns_above_mean 
#AllData$logP30_pctAllOverMean <- log(AllData$Elev_P30+ 1) * log(AllData$pct_all_returns_above_mean+ 1) 
#AllData$P30_pctAllover3mDiv1st <- AllData$Elev_P30 * AllData$all_returns_above_ht_div_Total_first_returns_x_100 
#AllData$logP30_pctAllover3mDiv1st <- log(AllData$Elev_P30+ 1) * log(AllData$all_returns_above_ht_div_Total_first_returns_x_100+ 1) 
#AllData$P30_pctAlloverModeDiv1st <- AllData$Elev_P30 * AllData$All_returns_above_mode_div_Total_first_returns_x_100 
#AllData$logP30_pctAlloverModeDiv1st <- log(AllData$Elev_P30+ 1) * log(AllData$All_returns_above_mode_div_Total_first_returns_x_100+ 1) 

AllData$P60_pctAllOver3m <- AllData$Elev_P60 * AllData$Pct_all_returns_above_ht 
#AllData$P60_pctAllOverMean <- AllData$Elev_P60 * AllData$pct_all_returns_above_mean 
#AllData$logP60_pctAllOverMean <- log(AllData$Elev_P60+ 1) * log(AllData$pct_all_returns_above_mean+ 1) 
#AllData$P60_pctAllover3mDiv1st <- AllData$Elev_P60 * AllData$all_returns_above_ht_div_Total_first_returns_x_100 
#AllData$logP60_pctAllover3mDiv1st <- log(AllData$Elev_P60+ 1) * log(AllData$all_returns_above_ht_div_Total_first_returns_x_100+ 1) 
#AllData$P60_pctAlloverModeDiv1st <- AllData$Elev_P60 * AllData$All_returns_above_mode_div_Total_first_returns_x_100 
#AllData$logP60_pctAlloverModeDiv1st <- log(AllData$Elev_P60+ 1) * log(AllData$All_returns_above_mode_div_Total_first_returns_x_100+ 1) 

AllData$P90_pctAllOver3m <- AllData$Elev_P90 * AllData$Pct_all_returns_above_ht 
#AllData$P90_pctAllOverMean <- AllData$Elev_P90 * AllData$pct_all_returns_above_mean 
#AllData$logP90_pctAllOverMean <- log(AllData$Elev_P90+ 1) * log(AllData$pct_all_returns_above_mean+ 1) 
#AllData$P90_pctAllover3mDiv1st <- AllData$Elev_P90 * AllData$all_returns_above_ht_div_Total_first_returns_x_100
#AllData$logP90_pctAllover3mDiv1st <- log(AllData$Elev_P90+ 1) * log(AllData$all_returns_above_ht_div_Total_first_returns_x_100+ 1) 
#AllData$P90_pctAlloverModeDiv1st <- AllData$Elev_P90 * AllData$All_returns_above_mode_div_Total_first_returns_x_100 
#AllData$logP90_pctAlloverModeDiv1st <- log(AllData$Elev_P90+ 1) * log(AllData$All_returns_above_mode_div_Total_first_returns_x_100+ 1) 

AllData$logmode <- log(AllData$Elev_mode+1) 
AllData$logP01 <- log(AllData$Elev_P01+ 1) 
AllData$logP10 <- log(AllData$Elev_P10+ 1) 
AllData$logP30 <- log(AllData$Elev_P30+ 1) 
AllData$logP60 <- log(AllData$Elev_P60+ 1) 
AllData$logP90 <- log(AllData$Elev_P90+ 1)

AllData$log_Pct_all_returns_above_ht <- log(AllData$Pct_all_returns_above_ht+1)
AllData$log_pct_all_returns_above_mean <- log(AllData$pct_all_returns_above_mean + 1) 
AllData$log_all_returns_above_ht_div_Total_first_returns_x_100 <- log(AllData$all_returns_above_ht_div_Total_first_returns_x_100+ 1) 
AllData$log_All_returns_above_mode_div_Total_first_returns_x_100 <- log(AllData$All_returns_above_mode_div_Total_first_returns_x_100 + 1) 

AllData$logmode_pctAllOver3m <- log(AllData$Elev_mode+1) * log(AllData$Pct_all_returns_above_ht+1)
AllData$logmode_pctAlloverModeDiv1st <- log(AllData$Elev_mode+1) * log(AllData$All_returns_above_mode_div_Total_first_returns_x_100+1) 
AllData$logP01_pctAllOver3m <- log(AllData$Elev_P01+ 1) * log(AllData$Pct_all_returns_above_ht+ 1) 
AllData$logP10_pctAllOver3m <- log(AllData$Elev_P10+ 1) * log(AllData$Pct_all_returns_above_ht+ 1) 
AllData$logP30_pctAllOver3m <- log(AllData$Elev_P30+ 1) * log(AllData$Pct_all_returns_above_ht+ 1) 
AllData$logP60_pctAllOver3m <- log(AllData$Elev_P60+ 1) * log(AllData$Pct_all_returns_above_ht+ 1) 
AllData$logP90_pctAllOver3m <- log(AllData$Elev_P90+ 1) * log(AllData$Pct_all_returns_above_ht+ 1) 

#dim(AllData)
#colnames(AllData)[83:96]

#corrgram(AllData[(AllData$Forest != 'Sitgreaves, P2' & AllData$Forest != 'Apache'), c(22,82, 83:96)], type="data", lower.panel=panel.shadeNtext, upper.panel=panel.signif, main="interaction terms")

######################################################################
######################################################################
######################################################################
######################################################################

#############################################################
## organize columns
Variables <- colnames(AllData) #pull variable names for use with select()
## exclude total count lidar metrics, use only the relative count measures (e.g., percent returns)
LidarNames <- Variables[c(23:40, 41, 42, 43, 45:49, 51, 52, 54, 55:57, 60:65, 83:106)] #subset of lidar metrics
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
data.mod <- select(DATA.mod, STBIOMSha, logSTBIOMSha, TCUmha, Predictors)
data.val <- select(DATA.val, STBIOMSha, logSTBIOMSha, TCUmha, Predictors)
data.val.ind <- select(DATA.val.ind, STBIOMSha, logSTBIOMSha, TCUmha, Predictors)

remove(AllData, DATA.mod, DATA.val, DATA.val.ind)
remove(AuxNames, FieldNames, LidarNames, RandUnif, Variables, Predictors)

# Centering data
#DATA.modC <- cbind(data.mod[, c(1,2)], center(data.mod[,c(-1, -2, -53, -58, -59)]), data.mod[c(53, 58, 59)])
#DATA.valC <- cbind(data.val[, c(1,2)], center(data.val[,c(-1, -2, -53, -58, -59)]), data.val[c(53, 58, 59)])

