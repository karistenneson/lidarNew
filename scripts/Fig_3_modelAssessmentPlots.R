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
FinSeq <- 1000
plotInt <- 200

labelstart <- 10
labelstartR <- 10
labelend <- 745
labelendR <- 695

OneOnelty <- 1
OneOnelwd <- 1
OneOneCol <- 'black'

trendlty <- 1
trendlwd <- 2
trendCol <- 'red'

CIlty <- 1
CIlwd <- 2
CICol <- 'orange'

## plots
#png(file="scatterplotObPred.png",width=800,height=1200,res=120)
#pdf(file="scatterplotObPred.pdf", width=6, height=12, family = "Helvetica")
#dev.new(width=7, height=16)
dev.new(width=16, height=16)

#par(mfrow=c(3, 3), mar=c(0.6, 0.6, 1.5, 0.5), oma=c(3.3, 3.3, 0, 2), mgp=c(2.25, 1, 0))
par(mfrow=c(3, 3), mar=c(0.6, 0.6, 0.6, 0.5),
    oma=c(3.3, 3.3, 0, 2), mgp=c(2.25, 1, 0))
#par(mar=c(5,3,2,2)+0.1)

#############################
#all 
plot(STBIOMSha ~ MPMEstimates, data = data.mod, xlim = c(0,755), ylim = c(0,755), col = 'white', las=1, xaxt="n")
abline(h = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
abline(v = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
points(STBIOMSha ~ MPMEstimates, data = data.mod, pch = 19, cex = .75, col = 'gray')
points(STBIOMSha ~ MPMEstimates, data = data.val, pch = 19, cex = .75, col = 'black', bg= 'black')
abline(a=0, b=1, col = OneOneCol, lwd = OneOnelwd, lty = OneOnelty)

modeldata<-data.val
testmodel<-lm(STBIOMSha ~ MPMEstimates, data = modeldata)
test<-summary(testmodel)
df.new <- data.frame(MPMEstimates=seq(min(modeldata$MPMEstimates)-10, max(modeldata$MPMEstimates),length.out = 100), STBIOMSha=seq(min(modeldata$STBIOMSha)-10, max(modeldata$STBIOMSha),length.out = 100))
tail(df.new)
pred2 <- predict(testmodel, df.new, se.fit=TRUE)
lines(pred2$fit+1.96*pred2$se.fit ~ df.new$MPMEstimates, col=CICol, lty=CIlty, lwd=CIlwd)
lines(pred2$fit-1.96*pred2$se.fit ~ df.new$MPMEstimates, col=CICol, lty=CIlty, lwd=CIlwd)
lines(pred2$fit~df.new$MPMEstimates, col = trendCol, lwd = trendlwd, lty = trendlty)

mtext("Field AGB (Mg/ha)", 2, outer=FALSE, line=2.75, xpd=NA, cex = 0.75)
text(labelstart, labelend, c('A. All Model Const.'), adj=c(0,0.5))
text(labelstartR, labelendR, paste("Field =",signif(test$coefficients[2],3),"* Pred.",'+',signif(test$coefficients[1],3)), adj=c(0,0.5))
text(labelstart, labelendR-50, c('RMSE = 41.15'), adj=c(0,0.5))
text(labelstart, labelendR-100, c('Bias = -2.26'), adj=c(0,0.5))

#############################
# Kaibab
plot(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$Forest==ForIndex[4], ], xlim = c(0,755), ylim = c(0,755), col = 'white', las=1, yaxt="n", xaxt="n")
abline(h = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
abline(v = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
points(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$Forest==ForIndex[4], ], pch = 19, cex = .75, col = 'gray')
points(STBIOMSha ~ MPMEstimates, data = data.val[data.val$Forest==ForIndex[4], ], pch = 19, cex = .75, col = 'black', bg= 'black')
abline(a=0, b=1, col = OneOneCol, lwd = OneOnelwd, lty = OneOnelty)

modeldata<-data.val[data.val$Forest==ForIndex[4], ]
testmodel<-lm(STBIOMSha ~ MPMEstimates, data = modeldata)
test<-summary(testmodel)
df.new <- data.frame(MPMEstimates=seq(min(modeldata$MPMEstimates)-10, max(modeldata$MPMEstimates),length.out = 100), STBIOMSha=seq(min(modeldata$STBIOMSha)-10, max(modeldata$STBIOMSha),length.out = 100))
tail(df.new)
pred2 <- predict(testmodel, df.new, se.fit=TRUE)
lines(pred2$fit+1.96*pred2$se.fit ~ df.new$MPMEstimates, col=CICol, lty=CIlty, lwd=CIlwd)
lines(pred2$fit-1.96*pred2$se.fit ~ df.new$MPMEstimates, col=CICol, lty=CIlty, lwd=CIlwd)
lines(pred2$fit~df.new$MPMEstimates, col = trendCol, lwd = trendlwd, lty = trendlty)

text(labelstartR, labelendR, paste("Field =",signif(test$coefficients[2],3),"* Pred.",'+',signif(test$coefficients[1],3)), adj=c(0,0.5))
svySlopeNK <- summary(svyglm(STBIOMSha ~ MPMEstimates, design = data.svy.val.NK))
abline(a = svySlopeNK$coefficients[1], b = svySlopeNK$coefficients[2], col = trendColPop, lwd = trendlwdPop, lty = trendlwdPop)
text(labelstart, labelend, c('B. Kaibab Plateau'), adj=c(0,0.5))
text(labelstart, labelendR-50, c('RMSE = 43.7'), adj=c(0,0.5))
text(labelstart, labelendR-100, c('Bias = 6.7'), adj=c(0,0.5))

#############################
# Coco
plot(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$Forest==ForIndex[1], ], xlim = c(0,755), ylim = c(0,755), col = 'white', las=1, xaxt="n", yaxt="n")
abline(h = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
abline(v = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
points(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$Forest==ForIndex[1], ], pch = 19, cex = .75, col = 'gray')
points(STBIOMSha ~ MPMEstimates, data = data.val[data.val$Forest==ForIndex[1], ], pch = 19, cex = .75, col = 'black', bg= 'black')
axis(4, las = 1)
abline(a=0, b=1, col = OneOneCol, lwd = OneOnelwd, lty = OneOnelty)

modeldata<-data.val[data.val$Forest==ForIndex[1], ]
testmodel<-lm(STBIOMSha ~ MPMEstimates, data = modeldata)
test<-summary(testmodel)
df.new <- data.frame(MPMEstimates=seq(min(modeldata$MPMEstimates)-10, max(modeldata$MPMEstimates),length.out = 100), STBIOMSha=seq(min(modeldata$STBIOMSha)-10, max(modeldata$STBIOMSha),length.out = 100))
tail(df.new)
pred2 <- predict(testmodel, df.new, se.fit=TRUE)
lines(pred2$fit+1.96*pred2$se.fit ~ df.new$MPMEstimates, col=CICol, lty=CIlty, lwd=CIlwd)
lines(pred2$fit-1.96*pred2$se.fit ~ df.new$MPMEstimates, col=CICol, lty=CIlty, lwd=CIlwd)
lines(pred2$fit~df.new$MPMEstimates, col = trendCol, lwd = trendlwd, lty = trendlty)

text(labelstartR, labelendR, paste("Field =",signif(test$coefficients[2],3),"* Pred.",'+',signif(test$coefficients[1],3)
), adj=c(0,0.5))
svySlopeCoco <- summary(svyglm(STBIOMSha ~ MPMEstimates, design = data.svy.val.Coco))
abline(a = svySlopeCoco$coefficients[1], b = svySlopeCoco$coefficients[2], col = trendColPop, lwd = trendlwdPop, lty = trendlwdPop)
text(labelstart, labelend, c('C. Coconino NF'), adj=c(0,0.5))
text(labelstart, labelendR-50, c('RMSE = 41.74'), adj=c(0,0.5))
text(labelstart, labelendR-100, c('Bias = -0.23'), adj=c(0,0.5))

#############################
# Tonto
plot(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$Forest==ForIndex[3], ], xlim = c(0,755), ylim = c(0,755), col = 'white', las=1, xaxt="n")
#axis(2, labels=FALSE)
abline(h = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
abline(v = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
#axis(1, labels=FALSE)
points(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$Forest==ForIndex[3], ], pch = 19, cex = .75, col = 'gray')
points(STBIOMSha ~ MPMEstimates, data = data.val[data.val$Forest==ForIndex[3], ], pch = 19, cex = .75, col = 'black', bg= 'black')
abline(a=0, b=1, col = OneOneCol, lwd = OneOnelwd, lty = OneOnelty)

modeldata<-data.val[data.val$Forest==ForIndex[3], ]
testmodel<-lm(STBIOMSha ~ MPMEstimates, data = modeldata)
test<-summary(testmodel)
df.new <- data.frame(MPMEstimates=seq(min(modeldata$MPMEstimates)-10, max(modeldata$MPMEstimates),length.out = 100), STBIOMSha=seq(min(modeldata$STBIOMSha)-10, max(modeldata$STBIOMSha),length.out = 100))
tail(df.new)
pred2 <- predict(testmodel, df.new, se.fit=TRUE)
lines(pred2$fit+1.96*pred2$se.fit ~ df.new$MPMEstimates, col=CICol, lty=CIlty, lwd=CIlwd)
lines(pred2$fit-1.96*pred2$se.fit ~ df.new$MPMEstimates, col=CICol, lty=CIlty, lwd=CIlwd)
lines(pred2$fit~df.new$MPMEstimates, col = trendCol, lwd = trendlwd, lty = trendlty)

text(labelstartR, labelendR, paste("Field =",signif(test$coefficients[2],3),"* Pred.",'+',signif(test$coefficients[1],3)), adj=c(0,0.5))
svySlopeTonto <- summary(svyglm(STBIOMSha ~ MPMEstimates, design = data.svy.val.Tonto))
abline(a = svySlopeTonto$coefficients[1], b = svySlopeTonto$coefficients[2], col = trendColPop, lwd = trendlwdPop, lty = trendlwdPop)
mtext("Field AGB (Mg/ha)", 2, outer=FALSE, line=2.75, xpd=NA, cex = 0.75)
text(labelstart, labelend, c('D. Tonto NF'), adj=c(0,0.5))
text(labelstart, labelendR-50, c('RMSE = 41.73'), adj=c(0,0.5))
text(labelstart, labelendR-100, c('Bias = -5.2'), adj=c(0,0.5))

############################
## Sit
plot(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$Forest==ForIndex[2], ], xlim = c(0,755), ylim = c(0,755), col = 'white',  las=1, xaxt="n", yaxt="n")
abline(h = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
abline(v = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
points(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$Forest==ForIndex[2], ], pch = 19, cex = .75, col = 'gray')
points(STBIOMSha ~ MPMEstimates, data = data.val[data.val$Forest==ForIndex[2], ], pch = 19, cex = .75, col = 'black', bg= 'black')
#axis(1, labels=FALSE)
abline(a=0, b=1, col = OneOneCol, lwd = OneOnelwd, lty = OneOnelty)

modeldata<-data.val[data.val$Forest==ForIndex[2], ]
testmodel<-lm(STBIOMSha ~ MPMEstimates, data = modeldata)
test<-summary(testmodel)
df.new <- data.frame(MPMEstimates=seq(min(modeldata$MPMEstimates)-10, max(modeldata$MPMEstimates),length.out = 100), STBIOMSha=seq(min(modeldata$STBIOMSha)-10, max(modeldata$STBIOMSha),length.out = 100))
tail(df.new)
pred2 <- predict(testmodel, df.new, se.fit=TRUE)
lines(pred2$fit+1.96*pred2$se.fit ~ df.new$MPMEstimates, col=CICol, lty=CIlty, lwd=CIlwd)
lines(pred2$fit-1.96*pred2$se.fit ~ df.new$MPMEstimates, col=CICol, lty=CIlty, lwd=CIlwd)
lines(pred2$fit~df.new$MPMEstimates, col = trendCol, lwd = trendlwd, lty = trendlty)

text(labelstartR, labelendR, paste("Field =",signif(test$coefficients[2],3),"* Pred.",'+',signif(test$coefficients[1],3)), adj=c(0,0.5))
svySlopeSit <- summary(svyglm(STBIOMSha ~ MPMEstimates, design = data.svy.val.Sit))
abline(a = svySlopeSit$coefficients[1], b = svySlopeSit$coefficients[2], col = trendColPop, lwd = trendlwdPop, lty = trendlwdPop)
text(labelstart, labelend, c('E. AS NF, Stage 1'), adj=c(0,0.5))
text(labelstart, labelendR-50, c('RMSE = 28.38'), adj=c(0,0.5))
text(labelstart, labelendR-100, c('Bias = -4.2'), adj=c(0,0.5))

#############################
## SWJM
plot(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$Forest==ForIndex[5], ], xlim = c(0,755), ylim = c(0,755), col = 'white', 
     las=1, yaxt="n")
#axis(2, labels=FALSE)
abline(h = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
abline(v = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
axis(1, labels=FALSE)
axis(4, las = 1)
points(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$Forest==ForIndex[5], ], pch = 19, cex = .75, col = 'gray')
points(STBIOMSha ~ MPMEstimates, data = data.val[data.val$Forest==ForIndex[5], ], pch = 19, cex = .75, col = 'black', bg= 'black')
abline(a=0, b=1, col = OneOneCol, lwd = OneOnelwd, lty = OneOnelty)

modeldata<-data.val[data.val$Forest==ForIndex[5], ]
testmodel<-lm(STBIOMSha ~ MPMEstimates, data = modeldata)
test<-summary(testmodel)
df.new <- data.frame(MPMEstimates=seq(min(modeldata$MPMEstimates)-10, max(modeldata$MPMEstimates),length.out = 100), STBIOMSha=seq(min(modeldata$STBIOMSha)-10, max(modeldata$STBIOMSha),length.out = 100))
tail(df.new)
pred2 <- predict(testmodel, df.new, se.fit=TRUE)
lines(pred2$fit+1.96*pred2$se.fit ~ df.new$MPMEstimates, col=CICol, lty=CIlty, lwd=CIlwd)
lines(pred2$fit-1.96*pred2$se.fit ~ df.new$MPMEstimates, col=CICol, lty=CIlty, lwd=CIlwd)
lines(pred2$fit~df.new$MPMEstimates, col = trendCol, lwd = trendlwd, lty = trendlty)

text(labelstartR, labelendR, paste("Field =",signif(test$coefficients[2],3),"* Pred.",'+',signif(test$coefficients[1],3)), adj=c(0,0.5))
svySlopeSWJM <- summary(svyglm(STBIOMSha ~ MPMEstimates, design = data.svy.val.SWJM))
abline(a = svySlopeSWJM$coefficients[1], b = svySlopeSWJM$coefficients[2], col = trendColPop, lwd = trendlwdPop, lty = trendlwdPop)
mtext("Predicted AGB (Mg/ha)", 1, outer=FALSE, line=2.75, xpd=NA, cex = 0.75)
text(labelstart, labelend, c('F. SW Jemez Mountains'), adj=c(0,0.5))

text(labelstart, labelendR-50, c('RMSE = 30.36'), adj=c(0,0.5))
text(labelstart, labelendR-100, c('Bias = -15.41'), adj=c(0,0.5))

#############################
#############################
ForIndexVal <- unique(data.val.ind$Forest)

#############################
## Sit2
plot(STBIOMSha ~ MPMEstimates, data = data.val.ind[data.val.ind$Forest==ForIndexVal[2], ], xlim = c(0,755), ylim = c(0,755), col = 'white', ylab = '', las=1)
abline(h = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
abline(v = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
points(STBIOMSha ~ MPMEstimates, data = data.val.ind[data.val.ind$Forest==ForIndexVal[2], ], pch = 19, cex = .75, col = 'black')
abline(a=0, b=1, col = OneOneCol, lwd = OneOnelwd, lty = OneOnelty)

modeldata<-data.val.ind[data.val.ind$Forest==ForIndexVal[2], ]
testmodel<-lm(STBIOMSha ~ MPMEstimates, data = modeldata)
test<-summary(testmodel)
df.new <- data.frame(MPMEstimates=seq(min(modeldata$MPMEstimates)-10, max(modeldata$MPMEstimates),length.out = 100), STBIOMSha=seq(min(modeldata$STBIOMSha)-10, max(modeldata$STBIOMSha),length.out = 100))
tail(df.new)
pred2 <- predict(testmodel, df.new, se.fit=TRUE)
lines(pred2$fit+1.96*pred2$se.fit ~ df.new$MPMEstimates, col=CICol, lty=CIlty, lwd=CIlwd)
lines(pred2$fit-1.96*pred2$se.fit ~ df.new$MPMEstimates, col=CICol, lty=CIlty, lwd=CIlwd)
lines(pred2$fit~df.new$MPMEstimates, col = trendCol, lwd = trendlwd, lty = trendlty)

text(labelstartR, labelendR, paste("Field =",signif(test$coefficients[2],3),"* Pred.",'+',signif(test$coefficients[1],3)), adj=c(0,0.5))
svySlopeSit2 <- summary(svyglm(STBIOMSha ~ MPMEstimates, design = data.svy.val.Sit2))
abline(a = svySlopeSit2$coefficients[1], b = svySlopeSit2$coefficients[2], col = trendColPop, lwd = trendlwdPop, lty = trendlwdPop)
mtext("Field AGB (Mg/ha)", 2, outer=FALSE, line=2.75, xpd=NA, cex = 0.75)
mtext("Predicted AGB (Mg/ha)", 1, outer=FALSE, line=2.75, xpd=NA, cex = 0.75)
text(labelstart, labelend, c('G. AS NF, Stage 2'), adj=c(0,0.5))

text(labelstart, labelendR-50, c('RMSE = 23.25'), adj=c(0,0.5))
text(labelstart, labelendR-100, c('Bias = -3.5'), adj=c(0,0.5))

#############################
## Sit3
plot(STBIOMSha ~ MPMEstimates, data = data.val.ind[data.val.ind$Forest==ForIndexVal[1], ], xlim = c(0,755), ylim = c(0,755), col = 'white', las=1, yaxt="n")
#axis(2, labels=FALSE)
abline(h = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
abline(v = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
axis(4, las = 1)
points(STBIOMSha ~ MPMEstimates, data = data.val.ind[data.val.ind$Forest==ForIndexVal[1], ], pch = 19, cex = .75, col = 'black', bg= 'black')
abline(a=0, b=1, col = OneOneCol, lwd = OneOnelwd, lty = OneOnelty)

modeldata<-data.val.ind[data.val.ind$Forest==ForIndexVal[1], ]
testmodel<-lm(STBIOMSha ~ MPMEstimates, data = modeldata)
test<-summary(testmodel)
df.new <- data.frame(MPMEstimates=seq(min(modeldata$MPMEstimates)-10, max(modeldata$MPMEstimates),length.out = 100), STBIOMSha=seq(min(modeldata$STBIOMSha)-10, max(modeldata$STBIOMSha),length.out = 100))
tail(df.new)
pred2 <- predict(testmodel, df.new, se.fit=TRUE)
lines(pred2$fit+1.96*pred2$se.fit ~ df.new$MPMEstimates, col=CICol, lty=CIlty, lwd=CIlwd)
lines(pred2$fit-1.96*pred2$se.fit ~ df.new$MPMEstimates, col=CICol, lty=CIlty, lwd=CIlwd)
lines(pred2$fit~df.new$MPMEstimates, col = trendCol, lwd = trendlwd, lty = trendlty)

text(labelstartR, labelendR, paste("Field =",signif(test$coefficients[2],3),"* Pred.",signif(test$coefficients[1],3)), adj=c(0,0.5))
svySlopeSit3 <- summary(svyglm(STBIOMSha ~ MPMEstimates, design = data.svy.val.Sit3))
abline(a = svySlopeSit3$coefficients[1], b = svySlopeSit3$coefficients[2], col = trendColPop, lwd = trendlwdPop, lty = trendlwdPop)
mtext("Predicted AGB (Mg/ha)", 1, outer=FALSE, line=2.75, xpd=NA, cex = 0.75)
text(labelstart, labelend, c('H. AS NF, Stage 3'), adj=c(0,0.5))

text(labelstart, labelendR-50, c('RMSE = 32.82'), adj=c(0,0.5))
text(labelstart, labelendR-100, c('Bias = -10.94'), adj=c(0,0.5))

#############################
## Legend
plot(STBIOMSha ~ MPMEstimates, data = data.mod, xlim = c(0,755), ylim = c(0,755), col = 'white', xlab = '', las=1,yaxt="n", xaxt="n", frame.plot = F)

legend('bottomright', c('model calibration','validation', 'trendline', '95% CI', '1:1'), 
       col = c('gray', 'black', trendCol,CICol, OneOneCol), 
       pch = c(19, 19, NA, NA, NA),  pt.bg = c('black', 'gray'), bty = 'n', 
       lty = c(NA, NA, trendlty, CIlty, OneOnelty), lwd = c(NA, NA, trendlwd, CIlwd, OneOnelwd), 
       cex = 1.1)


#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################














#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
########################################################
## set up survey design
data.val$Stratum <- as.character(data.val$Stratum)
data.val.ind$Stratum <- as.character(data.val.ind$Stratum)

data.svy.val.all <- svydesign(ids = ~1, data = data.val, fpc = data.val$fpc, strata = data.val$Stratum)
data.svy.val.NK <- svydesign(ids = ~1, data = data.val[data.val$Forest == 'NorthKaibab', ], fpc = data.val$fpc[data.val$Forest == 'NorthKaibab'], strata = data.val$Stratum[data.val$Forest == 'NorthKaibab'])
data.svy.val.Coco <- svydesign(ids = ~1, data = data.val[data.val$Forest == 'Coconino', ], fpc = data.val$fpc[data.val$Forest == 'Coconino'], strata = data.val$Stratum[data.val$Forest == 'Coconino'])
data.svy.val.Tonto <- svydesign(ids = ~1, data = data.val[data.val$Forest == 'Tonto', ], fpc = data.val$fpc[data.val$Forest == 'Tonto'], strata = data.val$Stratum[data.val$Forest == 'Tonto'])
data.svy.val.Sit <- svydesign(ids = ~1, data = data.val[data.val$Forest == 'Sitgreaves', ], fpc = data.val$fpc[data.val$Forest == 'Sitgreaves'], strata = data.val$Stratum[data.val$Forest == 'Sitgreaves'])
data.svy.val.SWJM <- svydesign(ids = ~1, data = data.val[data.val$Forest == 'SWJM', ], fpc = data.val$fpc[data.val$Forest == 'SWJM'], strata = data.val$Stratum[data.val$Forest == 'SWJM'])

data.svy.val.Sit2 <- svydesign(ids = ~1, data = data.val.ind[data.val.ind$Forest == 'Sitgreaves, P2', ], fpc = data.val.ind$fpc[data.val.ind$Forest == 'Sitgreaves, P2'], strata = data.val.ind$Stratum[data.val.ind$Forest == 'Sitgreaves, P2'])
data.svy.val.Sit3 <- svydesign(ids = ~1, data = data.val.ind[data.val.ind$Forest == 'Apache', ], fpc = data.val.ind$fpc[data.val.ind$Forest == 'Apache'], strata = data.val.ind$Stratum[data.val.ind$Forest == 'Apache'])
#########################################################################
#########################################################################
#########################################################################
#########################################################################

#dev.off()
summary(lm(STBIOMSha ~ MPMEstimates, data = data.val[data.val$Forest==ForIndex[4], ]))
summary(lm(STBIOMSha ~ MPMEstimates, data = data.val[data.val$Forest==ForIndex[1], ]))
summary(lm(STBIOMSha ~ MPMEstimates, data = data.val[data.val$Forest==ForIndex[3], ]))
summary(lm(STBIOMSha ~ MPMEstimates, data = data.val[data.val$Forest==ForIndex[2], ]))
summary(lm(STBIOMSha ~ MPMEstimates, data = data.val[data.val$Forest==ForIndex[5], ]))

summary(lm(STBIOMSha ~ MPMEstimates, data = data.val.ind[data.val.ind$Forest==ForIndexVal[1], ]))
summary(lm(STBIOMSha ~ MPMEstimates, data = data.val.ind[data.val.ind$Forest==ForIndexVal[2], ]))
#########################################################################
summary(lm(STBIOMSha ~ 0 + MPMEstimates, data = data.val[data.val$Forest==ForIndex[4], ]))
summary(lm(STBIOMSha ~ 0 + MPMEstimates, data = data.val[data.val$Forest==ForIndex[1], ]))
summary(lm(STBIOMSha ~ 0 + MPMEstimates, data = data.val[data.val$Forest==ForIndex[3], ]))
summary(lm(STBIOMSha ~ 0 + MPMEstimates, data = data.val[data.val$Forest==ForIndex[2], ]))
summary(lm(STBIOMSha ~ 0 + MPMEstimates, data = data.val[data.val$Forest==ForIndex[5], ]))

test<-summary(lm(STBIOMSha ~ 0 + MPMEstimates, data = data.val[data.val$Forest==ForIndex[5], ]))
test$coefficients
paste("Field AGB =",signif(test$coefficients[1],3),"* Pred. AGB")
#########################################################################
