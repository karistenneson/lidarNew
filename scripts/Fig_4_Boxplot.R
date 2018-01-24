### This file is for assessing model performance
## Written Karis Tenneson (krtenneson@fs.fed.us)
# Last updated: Jan 20, 2018

### set working directory
#setwd('C:\\Users\\krtenneson\\Desktop\\lidarPaper\\lidarNew\\scripts')
#setwd('\\\\166.2.126.25\\rseat\\Programs\\Reimbursibles\\fy2016\\R3_lidar_equation_transferability\\Analysis\\VersionControl\\lidarNew\\scripts')
#setwd("~/Documents/R/lidarNew/scripts") #Mac
#setwd("~/R/lidarNew/scripts") #WinCampus
#source(file='02_DataPrep.R')

### Load required packages
library(cvTools)
library(BAS)
library(corrgram)
library(survey)
options(survey.lonely.psu="certainty")
options(survey.lonely.psu="adjust")
library(tidyverse)
library(dplyr)
#library(robustbase)

## load data
data.mod <- read.csv('Data\\datamodNoZeroModelFits.csv')
data.val <- read.csv('Data\\datavalNoZeroModelFits.csv')
data.val.ind <- read.csv('Data\\datavalindNoZeroModelFits.csv')

#######################################################
#######################################################
## merge data and run summary statistics
#######################################################
#######################################################
dataall<-rbind(data.val,data.val.ind, data.mod)

aggregate(dataall[, 'STBIOMSha'], list(dataall$PlotSizeha), mean)
aggregate(dataall[, 'MPMResidual'], list(dataall$PlotSizeha), sd)

aggregate(dataall[, 'MPMResidual'], list(dataall$PlotSizeha), mean)
aggregate(dataall[, 'MPMResidual'], list(dataall$PlotSizeha), sd)
#######################################################
#######################################################

boxplot(dataall[, 'MPMResidual'] ~ dataall$PlotSizeha, notch = T, ylim = c(-320,320), xlab = "Plot Size (ha)", 
        ylab = "Field AGB - Lidar AGB (Mg/ha)", varwidth = T)

abline(h = seq(from = -300, to = 300, by = 100), col = 'gray')
abline(h= 0)

boxplot(dataall[, 'MPMResidual'] ~ dataall$PlotSizeha, notch = T, add = T, col = 'light gray', varwidth = T)

percent<-signif(c(13, 322, 1122,329,1412,112)/3310*100,3)


t.test(dataall$MPMEstimates, dataall$STBIOMSha, paired = T)
t.test(dataall$MPMEstimates[dataall$PlotSizeha == 0.008], dataall$STBIOMSha[dataall$PlotSizeha == 0.008], paired = T)
