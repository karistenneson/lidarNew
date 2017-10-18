### This file is for developing the BMA components of the analysis
## Written by Tenneson
# Last updated: 10/17/2017

setwd('C:\\Users\\krtenneson\\Desktop\\lidarModel\\VersionControl\\lidarNew')
setwd('\\\\166.2.126.25\\rseat\\Programs\\Reimbursibles\\fy2016\\R3_lidar_equation_transferability\\Analysis\\VersionControl\\lidarNew')
setwd("~/Documents/R/lidar/")

### Load required packages
library(BAS)
library(stats)
library(tidyverse)
library(corrgram)

### Bring in data
Apache <- read.csv("./Data/Apache_09252017.csv")
Apache$PLOT_ID<-as.character(Apache$PLOT_ID)
dim(Apache)
Coco <- read.csv("./Data/Coconino07162017.csv")
Coco$PLOT_ID<-as.character(Coco$PLOT_ID)
dim(Coco)
Kaibab <- read.csv("./Data/NKaibab07192017.csv")
Kaibab$PLOT_ID<-as.character(Kaibab$PLOT_ID)
dim(Kaibab)
Sit <- read.csv("./Data/Sitgreaves07162017.csv")
Sit$PLOT_ID<-as.character(Sit$PLOT_ID)
dim(Sit)
SitP2 <- read.csv("./Data/SitgreavesPhase2_09252017.csv")
SitP2$PLOT_ID<-as.character(SitP2$PLOT_ID)
dim(SitP2)
SWJM <- read.csv("./Data/SWJM07162017.csv")
SWJM$PLOT_ID<-as.character(SWJM$PLOT_ID)
dim(SWJM)
Tonto <- read.csv("./Data/Tonto07162017.csv")
Tonto$PLOT_ID<-as.character(Tonto$PLOT_ID)
dim(Tonto)

AllData <- rbind(Apache, Coco, Kaibab, Sit, SitP2, SWJM, Tonto)
tail(AllData)
dim(AllData)

##################################################
## export data to be sampled with auxilary data
## terrain sampled here: https://code.earthengine.google.com/ba1c04889e01e0d96aa14542477e3bd3
## ERU data is located here: \\166.2.126.25\rseat\Programs\Reimbursibles\fy2016\R3_lidar_equation_transferability\Data\ERU

write.csv(AllData, file = "./Data/PointFiles/Merged_10172017.csv", row.names = F)

##################################################
## merge terrain and ERU data
##################################################

ERU <- read.csv("./Data/Auxillary/ERU/ERU_units.csv")
dim(ERU)
colnames(ERU)
Terrain <- read.csv("./Data/Auxillary/Terrain_EE/DatawithTerrain.csv")
dim(Terrain)
colnames(Terrain)

AllAuxData <- merge(ERU[, c(1:13)], Terrain[,c(1, 9:12)], by.x = 'PLOT_ID', by.y = 'PLOT_ID')
dim(AllAuxData)
head(AllAuxData)

##################################################
## export data to be merged with field and lidar metrics

write.csv(AllAuxData, file = "./Data/Auxillary/Merged_10172017.csv", row.names = F)
