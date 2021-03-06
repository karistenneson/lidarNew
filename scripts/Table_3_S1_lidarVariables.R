### This file is for calculating correlation coefficients. 
## Written by Karis Tenneson (karistenneson@gmail.com)
# Last updated: Jan 19 2018

### Set working environment, necessary for any next steps. 

#setwd('C:\\Users\\krtenneson\\Desktop\\lidarPaper\\lidarNew\\scripts')
#setwd('\\\\166.2.126.25\\rseat\\Programs\\Reimbursibles\\fy2016\\R3_lidar_equation_transferability\\Analysis\\VersionControl\\lidarNew\\scripts')
#setwd("~/Documents/R/lidarNew/scripts") #Mac
#setwd("~/R/lidarNew/scripts") #WinCampus
#setwd("~/R/projects/lidarNew/scripts") #WinHome

### Load required packages
#library(tidyverse)
#library(dplyr)
#library(cvTools)
#library(BAS)
library(corrgram)
#library(survey)
#options(survey.lonely.psu="certainty")
#options(survey.lonely.psu="adjust")
#library(robustbase)
#library(olsrr)

### Source functions
source(file="archive//01_functions.R")

### Bring in data
data.corr.ht <- read.csv("Data//data4CorrelationsNoZeroHt.csv", stringsAsFactors=FALSE)
data.corr.cd <- read.csv("Data//data4CorrelationsNoZeroCanDen.csv", stringsAsFactors=FALSE)
######################################################################
######################################################################
######################################################################
## remove these columns for the models:
## 'Site','Forest', 
## "PlotSizeAcres", "fpc", "Stratum",
## R3ERUcodeFull, 'R3ERUlabelName'
## > 95% corr with P60: "Elev_ave", "Elev_P40", "Elev_P50","Elev_P70", "Elev_P75", "Elev_P80", 
## > 95% corr with P90: "Elev_P95", "Elev_P99", 
## > 95% corr with P30: "Elev_P20", "Elev_P25", 
## > 95% corr with stddev: "Elev_variance", "Elev_IQ", "Elev_AAD", "Elev_L2", 
## > 95% corr with Elev_LCV: "Elev_CV",
## > 95% corr with Elev_Lskewness: "Elev_skewness",
## > 95% corr with pct_all_returns_above_mean: "Pct_first_returns_above_mean", "All_returns_above_mean_div_Total_first_returns_x_100"
## > 95% corr with Pct_all_returns_above_ht: "Pct_first_returns_above_ht", 
## > 95% corr with All_returns_above_mode_div_Total_first_returns_x_100: "pct_all_returns_above_mode", "Pct_first_returns_above_mode", 

#corrgram(data.corr[ , c(1, 3, 13:18)], type="data", lower.panel=panel.shadeNtext, upper.panel=panel.signif, main="height")

#corrgram(data.corr[ , c(1, 4:12)], type="data", lower.panel=panel.shadeNtext, upper.panel=panel.signif, main="shape")

#corrgram(data.corr[ , c(1, 19:22)], type="data", lower.panel=panel.shadeNtext, upper.panel=panel.signif, main="density")

#1 == st bioms
#2 == 2:6,31 is height

corrgram(data.corr.ht[ , c(2:6,31)], type="data", lower.panel=panel.shadeNtext, upper.panel=panel.signif, main="interaction terms")

corrgram(data.corr.ht[ , c(2:6, 31, 16, 23:30,32,33)], type="data", lower.panel=panel.shadeNtext, upper.panel=panel.signif, main="interaction terms")

#3 == 7:15 is skew
corrgram(data.corr.ht[ , c(7:15, 17:22)], type="data", lower.panel=panel.shadeNtext, upper.panel=panel.signif, main="interaction terms")

#4 == density
corrgram(data.corr.cd[2:10], type="data", lower.panel=panel.shadeNtext, upper.panel=panel.signif, main="interaction terms")
