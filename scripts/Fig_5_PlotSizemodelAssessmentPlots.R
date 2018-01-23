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
data.mod$PlotSizeha <- signif((data.mod$PlotSizeAcres*0.404686), 1)
data.val$PlotSizeha <- signif((data.val$PlotSizeAcres*0.404686), 1)
data.val.ind$PlotSizeha <- signif((data.val.ind$PlotSizeAcres*0.404686), 1)

PlotIndex <- sort(unique(data.mod$PlotSizeha))
lengthVec <- rep(0,6)

for(i in 1:6){
lengthVec[i] <- length(data.mod$PlotSizeha[data.mod$PlotSizeha == PlotIndex [i]]) + 
  length(data.val$PlotSizeha[data.val$PlotSizeha == PlotIndex [i]]) + 
  length(data.val.ind$PlotSizeha[data.val.ind$PlotSizeha == PlotIndex [i]])
}

########################################################
startSeq<- 0
FinSeq <- 1000
plotInt <- 200

labelstart <- 10
labelstartR <- 10
labelstartR2 <- 10
labelend <- 745
labelendR <- 695
labelendR2 <- 645

OneOnelty <- 1
OneOnelwd <- 1
OneOneCol <- 'black'

trendlty <- 1
trendlwd <- 2
trendCol <- 'red'

CIlty <- 1
CIlwd <- 2
CICol <- 'orange'

transferCol <- 'cyan4'

modeldataFull<- rbind(data.mod,data.val,data.val.ind)
#modeldata <- rbind(data.mod)
#modeldata <- rbind(data.val,data.val.ind)
########################################################
#dev.new(width=16, height=16)

dev.new(width=7, height=16)
#par(mfrow=c(4, 2), mar=c(4.1, 2.1, 1.5, 0.5), oma=c(0, 2, 0, 0), mgp=c(2.25, 1, 0))
#par(mfrow=c(3, 2), mar=c(0.6, 0.6, 1.5, 0.5), oma=c(3.3, 3.3, 0, 0), mgp=c(2.25, 1, 0))
par(mfrow=c(3, 3), mar=c(0.6, 0.6, 1.5, 0.5), oma=c(3.3, 3.3, 0, 2), mgp=c(2.25, 1, 0))
#par(mar=c(5,3,2,2)+0.1)

#Smallest, .008
plot(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$PlotSizeha==PlotIndex[1], ], xlim = c(0,755), ylim = c(0,755), col = 'white', las=1, xaxt="n")#, main = 'a)')
abline(h = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
abline(v = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
points(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$PlotSizeha==PlotIndex[1], ], pch = 19, cex = .75, col = 'gray')
points(STBIOMSha ~ MPMEstimates, data = data.val[data.val$PlotSizeha==PlotIndex[1], ], pch = 19, cex = .75, col = 'black', bg= 'black')
points(STBIOMSha ~ MPMEstimates, data = data.val.ind[data.val.ind$PlotSizeha==PlotIndex[1], ], pch = 19, cex = .75, col = transferCol)
abline(a=0, b=1, col = OneOneCol, lwd = OneOnelwd, lty = OneOnelty)

modeldata<-modeldataFull[modeldataFull$PlotSizeha==PlotIndex[1], ]
testmodel<-lm(STBIOMSha ~ MPMEstimates, data = modeldata)
test<-summary(testmodel)
df.new <- data.frame(MPMEstimates=seq(min(modeldata$MPMEstimates)-10, max(modeldata$MPMEstimates),length.out = 100), STBIOMSha=seq(min(modeldata$STBIOMSha)-10, max(modeldata$STBIOMSha),length.out = 100))
tail(df.new)
pred2 <- predict(testmodel, df.new, se.fit=TRUE)
lines(pred2$fit+1.96*pred2$se.fit ~ df.new$MPMEstimates, col=CICol, lty=CIlty, lwd=CIlwd)
lines(pred2$fit-1.96*pred2$se.fit ~ df.new$MPMEstimates, col=CICol, lty=CIlty, lwd=CIlwd)
lines(pred2$fit~df.new$MPMEstimates, col = trendCol, lwd = trendlwd, lty = trendlty)

text(labelstartR, labelendR, paste("Field =",signif(test$coefficients[2],3),"* Pred.",'+',signif(test$coefficients[1],3)), adj=c(0,0.5))
mtext("Field AGB (Mg/ha)", 2, outer=FALSE, line=2.75, xpd=NA, cex = 0.75)
text(labelstart, labelend, paste('A. plot size =', PlotIndex[1], 'ha'), adj=c(0,0.5))
text(labelstartR2, labelendR2, paste('n =', lengthVec[1]), adj=c(0,0.5))

# .01
plot(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$PlotSizeha==PlotIndex[2], ], xlim = c(0,755), ylim = c(0,755), col = 'white', las=1, yaxt="n", xaxt="n")
abline(h = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
abline(v = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
#axis(2, labels=FALSE)
#axis(1, labels=FALSE)
points(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$PlotSizeha==PlotIndex[2], ], pch = 19, cex = .75, col = 'gray')
points(STBIOMSha ~ MPMEstimates, data = data.val[data.val$PlotSizeha==PlotIndex[2], ], pch = 19, cex = .75, col = 'black', bg= 'black')
points(STBIOMSha ~ MPMEstimates, data = data.val.ind[data.val.ind$PlotSizeha==PlotIndex[2], ], pch = 19, cex = .75, col = transferCol)
abline(a=0, b=1, col = OneOneCol, lwd = OneOnelwd, lty = OneOnelty)

modeldata<-modeldataFull[modeldataFull$PlotSizeha==PlotIndex[2], ]
testmodel<-lm(STBIOMSha ~ MPMEstimates, data = modeldata)
test<-summary(testmodel)
df.new <- data.frame(MPMEstimates=seq(min(modeldata$MPMEstimates)-10, max(modeldata$MPMEstimates),length.out = 100), STBIOMSha=seq(min(modeldata$STBIOMSha)-10, max(modeldata$STBIOMSha),length.out = 100))
tail(df.new)
pred2 <- predict(testmodel, df.new, se.fit=TRUE)
lines(pred2$fit+1.96*pred2$se.fit ~ df.new$MPMEstimates, col=CICol, lty=CIlty, lwd=CIlwd)
lines(pred2$fit-1.96*pred2$se.fit ~ df.new$MPMEstimates, col=CICol, lty=CIlty, lwd=CIlwd)
lines(pred2$fit~df.new$MPMEstimates, col = trendCol, lwd = trendlwd, lty = trendlty)

text(labelstartR, labelendR, paste("Field =",signif(test$coefficients[2],3),"* Pred.",'+',signif(test$coefficients[1],3)), adj=c(0,0.5))

text(labelstart, labelend, paste('B. plot size =', PlotIndex[2], 'ha'), adj=c(0,0.5))
text(labelstartR2, labelendR2, paste('n =', lengthVec[2]), adj=c(0,0.5))

# .02
plot(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$PlotSizeha==PlotIndex[3], ], xlim = c(0,755), ylim = c(0,755), col = 'white', las=1, xaxt="n", yaxt="n")
abline(h = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
abline(v = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
points(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$PlotSizeha==PlotIndex[3], ], pch = 19, cex = .75, col = 'gray')
points(STBIOMSha ~ MPMEstimates, data = data.val[data.val$PlotSizeha==PlotIndex[3], ], pch = 19, cex = .75, col = 'black', bg= 'black')
points(STBIOMSha ~ MPMEstimates, data = data.val.ind[data.val.ind$PlotSizeha==PlotIndex[3], ], pch = 19, cex = .75, col = transferCol)
axis(4, las = 1)
abline(a=0, b=1, col = OneOneCol, lwd = OneOnelwd, lty = OneOnelty)

modeldata<-modeldataFull[modeldataFull$PlotSizeha==PlotIndex[3], ]
testmodel<-lm(STBIOMSha ~ MPMEstimates, data = modeldata)
test<-summary(testmodel)
df.new <- data.frame(MPMEstimates=seq(min(modeldata$MPMEstimates)-10, max(modeldata$MPMEstimates),length.out = 100), STBIOMSha=seq(min(modeldata$STBIOMSha)-10, max(modeldata$STBIOMSha),length.out = 100))
tail(df.new)
pred2 <- predict(testmodel, df.new, se.fit=TRUE)
lines(pred2$fit+1.96*pred2$se.fit ~ df.new$MPMEstimates, col=CICol, lty=CIlty, lwd=CIlwd)
lines(pred2$fit-1.96*pred2$se.fit ~ df.new$MPMEstimates, col=CICol, lty=CIlty, lwd=CIlwd)
lines(pred2$fit~df.new$MPMEstimates, col = trendCol, lwd = trendlwd, lty = trendlty)

text(labelstartR, labelendR, paste("Field =",signif(test$coefficients[2],3),"* Pred.",'+',signif(test$coefficients[1],3)), adj=c(0,0.5))

text(labelstart, labelend, paste('C. plot size =', PlotIndex[3], 'ha'), adj=c(0,0.5))
text(labelstartR2, labelendR2, paste('n =', lengthVec[3]), adj=c(0,0.5))

# .03
plot(STBIOMSha ~ MPMEstimates, data = data.val[data.val$PlotSizeha==PlotIndex[4], ], xlim = c(0,755), ylim = c(0,755), col = 'white', las=1)
#axis(2, labels=FALSE)
abline(h = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
abline(v = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
#axis(4, las = 1)
points(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$PlotSizeha==PlotIndex[4], ], pch = 19, cex = .75, col = 'gray')
points(STBIOMSha ~ MPMEstimates, data = data.val[data.val$PlotSizeha==PlotIndex[4], ], pch = 19, cex = .75, col = 'black')
points(STBIOMSha ~ MPMEstimates, data = data.val.ind[data.val.ind$PlotSizeha==PlotIndex[4], ], pch = 19, cex = .75, col = transferCol)
abline(a=0, b=1, col = OneOneCol, lwd = OneOnelwd, lty = OneOnelty)

modeldata<-modeldataFull[modeldataFull$PlotSizeha==PlotIndex[4], ]
testmodel<-lm(STBIOMSha ~ MPMEstimates, data = modeldata)
test<-summary(testmodel)
df.new <- data.frame(MPMEstimates=seq(min(modeldata$MPMEstimates)-10, max(modeldata$MPMEstimates),length.out = 100), STBIOMSha=seq(min(modeldata$STBIOMSha)-10, max(modeldata$STBIOMSha),length.out = 100))
tail(df.new)
pred2 <- predict(testmodel, df.new, se.fit=TRUE)
lines(pred2$fit+1.96*pred2$se.fit ~ df.new$MPMEstimates, col=CICol, lty=CIlty, lwd=CIlwd)
lines(pred2$fit-1.96*pred2$se.fit ~ df.new$MPMEstimates, col=CICol, lty=CIlty, lwd=CIlwd)
lines(pred2$fit~df.new$MPMEstimates, col = trendCol, lwd = trendlwd, lty = trendlty)

text(labelstartR, labelendR, paste("Field =",signif(test$coefficients[2],3),"* Pred.",'+',signif(test$coefficients[1],3)), adj=c(0,0.5))

mtext("Predicted AGB (Mg/ha)", 1, outer=FALSE, line=2.75, xpd=NA, cex = 0.75)
mtext("Field AGB (Mg/ha)", 2, outer=FALSE, line=2.75, xpd=NA, cex = 0.75)
text(labelstart, labelend, paste('D. plot size =', PlotIndex[4], 'ha'), adj=c(0,0.5))
text(labelstartR2, labelendR2, paste('n =', lengthVec[4]), adj=c(0,0.5))

# .04
plot(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$PlotSizeha==PlotIndex[5], ], xlim = c(0,755), ylim = c(0,755), col = 'white', 
     las=1, yaxt="n")
#axis(2, labels=FALSE)
abline(h = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
abline(v = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
axis(1, labels=FALSE)
points(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$PlotSizeha==PlotIndex[5], ], pch = 19, cex = .75, col = 'gray')
points(STBIOMSha ~ MPMEstimates, data = data.val[data.val$PlotSizeha==PlotIndex[5], ], pch = 19, cex = .75, col = 'black', bg= 'black')
points(STBIOMSha ~ MPMEstimates, data = data.val.ind[data.val.ind$PlotSizeha==PlotIndex[5], ], pch = 19, cex = .75, col = transferCol)
abline(a=0, b=1, col = OneOneCol, lwd = OneOnelwd, lty = OneOnelty)

modeldata<-modeldataFull[modeldataFull$PlotSizeha==PlotIndex[5], ]
testmodel<-lm(STBIOMSha ~ MPMEstimates, data = modeldata)
test<-summary(testmodel)
df.new <- data.frame(MPMEstimates=seq(min(modeldata$MPMEstimates)-10, max(modeldata$MPMEstimates),length.out = 100), STBIOMSha=seq(min(modeldata$STBIOMSha)-10, max(modeldata$STBIOMSha),length.out = 100))
tail(df.new)
pred2 <- predict(testmodel, df.new, se.fit=TRUE)
lines(pred2$fit+1.96*pred2$se.fit ~ df.new$MPMEstimates, col=CICol, lty=CIlty, lwd=CIlwd)
lines(pred2$fit-1.96*pred2$se.fit ~ df.new$MPMEstimates, col=CICol, lty=CIlty, lwd=CIlwd)
lines(pred2$fit~df.new$MPMEstimates, col = trendCol, lwd = trendlwd, lty = trendlty)

text(labelstartR, labelendR, paste("Field =",signif(test$coefficients[2],3),"* Pred.",'+',signif(test$coefficients[1],3)), adj=c(0,0.5))

mtext("Predicted AGB (Mg/ha)", 1, outer=FALSE, line=2.75, xpd=NA, cex = 0.75)
text(labelstart, labelend, paste('E. plot size =', PlotIndex[5], 'ha'), adj=c(0,0.5))
text(labelstartR2, labelendR2, paste('n =', lengthVec[5]), adj=c(0,0.5))

# .08
plot(STBIOMSha ~ MPMEstimates, data = data.val[data.val$PlotSizeha==PlotIndex[6], ], xlim = c(0,755), ylim = c(0,755), col = 'white', ylab = '', las=1, yaxt = 'n')
abline(h = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
abline(v = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
points(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$PlotSizeha==PlotIndex[6], ], pch = 19, cex = .75, col = 'gray')
points(STBIOMSha ~ MPMEstimates, data = data.val[data.val$PlotSizeha==PlotIndex[6], ], pch = 19, cex = .75, col = 'black')
points(STBIOMSha ~ MPMEstimates, data = data.val.ind[data.val.ind$PlotSizeha==PlotIndex[6], ], pch = 19, cex = .75, col = transferCol)
abline(a=0, b=1, col = OneOneCol, lwd = OneOnelwd, lty = OneOnelty)

modeldata<-modeldataFull[modeldataFull$PlotSizeha==PlotIndex[6], ]
testmodel<-lm(STBIOMSha ~ MPMEstimates, data = modeldata)
test<-summary(testmodel)
df.new <- data.frame(MPMEstimates=seq(min(modeldata$MPMEstimates)-10, max(modeldata$MPMEstimates),length.out = 100), STBIOMSha=seq(min(modeldata$STBIOMSha)-10, max(modeldata$STBIOMSha),length.out = 100))
tail(df.new)
pred2 <- predict(testmodel, df.new, se.fit=TRUE)
lines(pred2$fit+1.96*pred2$se.fit ~ df.new$MPMEstimates, col=CICol, lty=CIlty, lwd=CIlwd)
lines(pred2$fit-1.96*pred2$se.fit ~ df.new$MPMEstimates, col=CICol, lty=CIlty, lwd=CIlwd)
lines(pred2$fit~df.new$MPMEstimates, col = trendCol, lwd = trendlwd, lty = trendlty)

text(labelstartR, labelendR, paste("Field =",signif(test$coefficients[2],3),"* Pred.",'+',signif(test$coefficients[1],3)), adj=c(0,0.5))
axis(4, las = 1)
mtext("Predicted AGB (Mg/ha)", 1, outer=FALSE, line=2.75, xpd=NA, cex = 0.75)
text(labelstart, labelend, paste('F. plot size =', PlotIndex[6], 'ha'), adj=c(0,0.5))
text(labelstartR2, labelendR2, paste('n =', lengthVec[6]), adj=c(0,0.5))

#all 
plot(STBIOMSha ~ MPMEstimates, data = data.mod, xlim = c(0,755), ylim = c(0,755), col = 'white', xlab = '', las=1,yaxt="n", xaxt="n", frame.plot = F)
legend('left', c('model construction','model validation', 'transfer validation', 'trendline', '95% CI', '1:1'), 
       col = c('gray', 'black', transferCol, trendCol, CICol, OneOneCol), 
       pch = c(19, 19, 19, NA, NA, NA),  pt.bg = c('black', 'gray'), bty = 'n', 
       lty = c(NA, NA, NA, trendlty, CIlty, OneOnelty), lwd = c(NA, NA, NA, trendlwd, CIlwd, OneOnelwd), 
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
#########################################################################
#########################################################################
#########################################################################
data.mod$Data <-  'data.mod'
data.val$Data <-  'data.val'
data.val.ind$Data <-  'data.val.ind'
modeldata <- rbind(data.val, data.mod, data.val.ind)

ggplot(modeldata[modeldata$PlotSizeha == PlotIndex[1],], aes(x=MPMEstimates, y=STBIOMSha)) +
  geom_point(aes(colour=factor(Data))) +
  geom_smooth(method="lm") + xlim(0,650) + ylim(0,650)

ggplot(modeldata[modeldata$PlotSizeha == PlotIndex[2],], aes(x=MPMEstimates, y=STBIOMSha)) +
  geom_point(aes(colour=factor(Data))) +
  geom_smooth(method="lm") + xlim(0,650) + ylim(0,650)

ggplot(modeldata[modeldata$PlotSizeha == PlotIndex[3],], aes(x=MPMEstimates, y=STBIOMSha)) +
  geom_point(aes(colour=factor(Data))) +
  geom_smooth(method="lm") + xlim(0,650) + ylim(0,650)

ggplot(modeldata[modeldata$PlotSizeha == PlotIndex[4],], aes(x=MPMEstimates, y=STBIOMSha)) +
  geom_point(aes(colour=factor(Data))) +
  geom_smooth(method="lm") + xlim(0,650) + ylim(0,650)

ggplot(modeldata[modeldata$PlotSizeha == PlotIndex[5],], aes(x=MPMEstimates, y=STBIOMSha)) +
  geom_point(aes(colour=factor(Data))) +
  geom_smooth(method="lm") + xlim(0,650) + ylim(0,650)

ggplot(modeldata[modeldata$PlotSizeha == PlotIndex[6],], aes(x=MPMEstimates, y=STBIOMSha)) +
  geom_point(aes(colour=factor(Data))) +
  geom_smooth(method="lm") + xlim(0,650) + ylim(0,650)



###################################################################

library(reshape2)
library(plotly)

sp <- ggplot(modeldata, aes(x=MPMEstimates, y=STBIOMSha)) + geom_point(alpha = 1/3, aes(colour=factor(Data)))+
  geom_smooth(method="lm") + xlim(0,650) + ylim(0,650) + scale_colour_manual(values = c('Black','Brown','Orange')) + 

# Divide by day, going horizontally and wrapping with 2 columns
sp + facet_wrap( ~ PlotSizeha, ncol=3)

ggplotly()

###################################################################
ggplot(data.mod[data.val$PlotSizeha==PlotIndex[1], ], aes(MPMEstimates, STBIOMSha)) + geom_point() + xlim(0,600)+ ylim(0,600) + geom_abline(slope = 1, intercept = 0, col = 'red', lty = 2, lwd = 1)

ggplot(data.mod[data.val$PlotSizeha==PlotIndex[2], ], aes(MPMEstimates, STBIOMSha)) + geom_point() + geom_smooth() + xlim(0,600)+ ylim(0,600) + geom_abline(slope = 1, intercept = 0, col = 'red', lty = 2, lwd = 1)

ggplot(data.mod[data.val$PlotSizeha==PlotIndex[3], ], aes(MPMEstimates, STBIOMSha)) + geom_point() + geom_smooth() + xlim(0,600)+ ylim(0,600) + geom_abline(slope = 1, intercept = 0, col = 'red', lty = 2, lwd = 1)

ggplot(data.mod[data.val$PlotSizeha==PlotIndex[4], ], aes(MPMEstimates, STBIOMSha)) + geom_point() + geom_smooth() + xlim(0,600)+ ylim(0,600) + geom_abline(slope = 1, intercept = 0, col = 'red', lty = 2, lwd = 1.5)

ggplot(data.mod[data.val$PlotSizeha==PlotIndex[5], ], aes(MPMEstimates, STBIOMSha)) + geom_point() + geom_smooth() + xlim(0,600)+ ylim(0,600) + geom_abline(slope = 1, intercept = 0, col = 'red', lty = 2, lwd = 1.5)

ggplot(data.mod[data.val$PlotSizeha==PlotIndex[6], ], aes(MPMEstimates, STBIOMSha)) + geom_point() + geom_smooth() + xlim(0,600)+ ylim(0,600) + geom_abline(slope = 1, intercept = 0, col = 'red', lty = 2, lwd = 1.5)

#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################

points(Prednotrans[data.mod$PlotSizeha >= .1] ~ data.mod$STBIOMSha[data.mod$PlotSizeha >= .1], col ='gold')
points(Prednotrans[data.mod$PlotSizeha < 0.05] ~ data.mod$STBIOMSha[data.mod$PlotSizeha < 0.05], col ='red')

ggplot(data.mod, aes(STBIOMSha,MPMEstimates)) + geom_point() + geom_smooth() + geom_smooth(method=lm, se=FALSE, col = 'grey')

ggplot(frame[frame$Forest == 'NorthKaibab', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'SWJM', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Sitgreaves', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Tonto', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Coconino', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()

