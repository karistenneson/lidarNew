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
library(lmodel2)
#library(robustbase)

## load data
data.mod <- read.csv('Data\\datamodNoZeroModelFits.csv')
data.val <- read.csv('Data\\datavalNoZeroModelFits.csv')
data.val.ind <- read.csv('Data\\datavalindNoZeroModelFits.csv')

########################################################
########################################################
## Make some plots, in development
## see https://gist.github.com/richfitz/5963583
## and here https://nicercode.github.io/guides/plotting/ 
## https://stackoverflow.com/questions/37897984/r-plot-displaying-both-point-type-and-line-type-in-legend
## for overview of settings that are available
########################################################
########################################################
ForIndex <- unique(data.val$Forest)
startSeq<- 0
FinSeq <- 700
plotInt <- 200

labelstart <- 10
labelstartR <- 10
labelend <- 745
labelendR <- 695

OneOnelty <- 1
OneOnelwd <- 1
OneOneCol <- 'black'

trendlwd <- 2
trendlty <- 5
trendCol <- 'red'

trendlwdPop <- 1
trendltyPop <- 1
trendColPop <- 'orange'

## plots
#png(file="scatterplotObPred.png",width=800,height=1200,res=120)
#pdf(file="scatterplotObPred.pdf", width=6, height=12, family = "Helvetica")
#dev.new(width=7, height=16)
testmamod<-lmodel2(STBIOMSha ~ MPMEstimates, data = data.mod, nperm=1000)
testmaval<-lmodel2(STBIOMSha ~ MPMEstimates, data = data.val, nperm=1000)

dev.new(width=8, height=4)
par(mfrow=c(1, 2), mar=c(4.1, 2.1, 1.5, 0.5),
    oma=c(0, 2, 0, 0), mgp=c(2.25, 1, 0))

par(mfrow=c(1, 2), mar=c(4.1, 0.6, 1.5, 0.5),
    oma=c(0, 3, 0, 0), mgp=c(2.25, 1, 0))
#all 
plot(testmamod, 'MA', main = '', xlim = c(0,755), ylim = c(0,755), pch = 19, xlab = '', ylab = '', lwd = 2, las = 1)
mtext("Field AGB (Mg/ha)", 2, outer=FALSE, line=2.5, xpd=NA)
mtext("Predicted AGB (Mg/ha)", 1, outer=FALSE, line=2)

abline(h = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
abline(v = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
abline(a=0, b=1, col = OneOneCol, lwd = OneOnelwd, lty = OneOnelty)
text(labelstart, labelend, c('A. All Model Const. Development'), adj=c(0,0.5))
text(labelstartR, labelendR, c('MA Reg.: Field = 1.22 * Pred - 0.27'), adj=c(0,0.5))

plot(testmaval, 'MA', main = '', xlim = c(0,755), ylim = c(0,755), pch = 19, xlab = ' ', ylab = ' ', las = 1, yaxt="n")
mtext("Predicted AGB (Mg/ha)", 1, outer=FALSE, line=2)
axis(2, labels=FALSE)

abline(h = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
abline(v = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
abline(a=0, b=1, col = OneOneCol, lwd = OneOnelwd, lty = OneOnelty)
text(labelstart, labelend, c('B. All Model Const. Validation'), adj=c(0,0.5))
text(labelstartR, labelendR, c('MA Reg.: Field = 1.2 * Pred - 25.29'), adj=c(0,0.5))


####################################
####################################
