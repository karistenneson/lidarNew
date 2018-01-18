### This file is for assessing model performance
## Written by MS Patterson (maspatte@uw.edu) and Karis Tenneson (krtenneson@fs.fed.us)
# Last updated: Jan 17, 2018

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
## for overview of settings that are available
########################################################
########################################################
ForIndex <- unique(data.val$Forest)


## plots
#png(file="scatterplotObPred.png",width=800,height=1200,res=120)
#pdf(file="scatterplotObPred.pdf", width=6, height=12, family = "Helvetica")
dev.new(width=7, height=16)
#par(mfrow=c(4, 2), mar=c(4.1, 2.1, 1.5, 0.5), oma=c(0, 2, 0, 0), mgp=c(2.25, 1, 0))
par(mfrow=c(4, 2), mar=c(0.6, 0.6, 1.5, 0.5),
    oma=c(3.3, 3.3, 0, 2), mgp=c(2.25, 1, 0))
#par(mar=c(5,3,2,2)+0.1)
#all 
plot(STBIOMSha ~ MPMEstimates, data = data.mod, xlim = c(0,625), ylim = c(0,625), col = 'white', 
     ylab = 'Field AGB (Mg/ha)', xlab = '', las=1, xaxt="n")#, main = 'a)')
axis(1, labels=FALSE)
points(STBIOMSha ~ MPMEstimates, data = data.mod, pch = 19, cex = .75, col = 'gray')
points(STBIOMSha ~ MPMEstimates, data = data.val, pch = 19, cex = .75, col = 'black', bg= 'black')
abline(a=0, b=1, col = 'red', lwd = 2, lty = 5)
mtext("Field AGB (Mg/ha)", 2, outer=FALSE, line=2.75, xpd=NA, cex = 0.75)
text(10, 600, c('A. All Mod. Const. Data'), adj=c(0,0.5))
#legend('topleft', c('A'), col = c('black'), bty = 'n')
legend('bottomright', c('model construction','validation'), col = c('gray', 'black'), pch = c(19, 19),        pt.bg = c('black', 'gray'), bty = 'n')

# Kaibab
plot(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$Forest==ForIndex[4], ], xlim = c(0,625), ylim = c(0,625), col = 'white', 
     las=1, yaxt="n", xaxt="n")
#axis(2, labels=FALSE)
axis(1, labels=FALSE)
axis(4, las = 1)
points(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$Forest==ForIndex[4], ], pch = 19, cex = .75, col = 'gray')
points(STBIOMSha ~ MPMEstimates, data = data.val[data.val$Forest==ForIndex[4], ], pch = 19, cex = .75, col = 'black', bg= 'black')
abline(a=0, b=1, col = 'red', lwd = 2, lty = 5)
text(10, 600, c('B. Kaibab Plateau'), adj=c(0,0.5))
#legend('topleft', c('B'), col = c('black'), bty = 'n')

# Coco
plot(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$Forest==ForIndex[1], ], xlim = c(0,625), ylim = c(0,625), col = 'white', 
     ylab = 'Field AGB (Mg/ha)', las=1, xaxt="n")
points(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$Forest==ForIndex[1], ], pch = 19, cex = .75, col = 'gray')
points(STBIOMSha ~ MPMEstimates, data = data.val[data.val$Forest==ForIndex[1], ], pch = 19, cex = .75, col = 'black', bg= 'black')
axis(1, labels=FALSE)
mtext("Field AGB (Mg/ha)", 2, outer=FALSE, line=2.75, xpd=NA, cex = 0.75)
abline(a=0, b=1, col = 'red', lwd = 2, lty = 5)
text(10, 600, c('C. Coconino NF'), adj=c(0,0.5))
#legend('topleft', c('C'), col = c('black'), bty = 'n')

# Tonto
plot(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$Forest==ForIndex[3], ], xlim = c(0,625), ylim = c(0,625), col = 'white', 
     las=1, yaxt="n", xaxt="n")
#axis(2, labels=FALSE)
axis(1, labels=FALSE)
axis(4, las = 1)
points(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$Forest==ForIndex[3], ], pch = 19, cex = .75, col = 'gray')
points(STBIOMSha ~ MPMEstimates, data = data.val[data.val$Forest==ForIndex[3], ], pch = 19, cex = .75, col = 'black', bg= 'black')
abline(a=0, b=1, col = 'red', lwd = 2, lty = 5)
text(10, 600, c('D. Tonto NF'), adj=c(0,0.5))
#legend('topleft', c('D'), col = c('black'), bty = 'n')


# Sit
plot(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$Forest==ForIndex[2], ], xlim = c(0,625), ylim = c(0,625), col = 'white', 
     ylab = 'Field AGB (Mg/ha)', las=1, xaxt="n")
points(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$Forest==ForIndex[2], ], pch = 19, cex = .75, col = 'gray')
points(STBIOMSha ~ MPMEstimates, data = data.val[data.val$Forest==ForIndex[2], ], pch = 19, cex = .75, col = 'black', bg= 'black')
axis(1, labels=FALSE)
mtext("Field AGB (Mg/ha)", 2, outer=FALSE, line=2.75, xpd=NA, cex = 0.75)
abline(a=0, b=1, col = 'red', lwd = 2, lty = 5)
text(10, 600, c('E. Apache-Sitgreaves NF, Stage 1'), adj=c(0,0.5))
#legend('topleft', c('E'), col = c('black'), bty = 'n')

# SWJM
plot(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$Forest==ForIndex[5], ], xlim = c(0,625), ylim = c(0,625), col = 'white', 
     las=1, yaxt="n", xaxt="n")
#axis(2, labels=FALSE)
axis(1, labels=FALSE)
axis(4, las = 1)
points(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$Forest==ForIndex[5], ], pch = 19, cex = .75, col = 'gray')
points(STBIOMSha ~ MPMEstimates, data = data.val[data.val$Forest==ForIndex[5], ], pch = 19, cex = .75, col = 'black', bg= 'black')
abline(a=0, b=1, col = 'red', lwd = 2, lty = 5)
text(10, 600, c('F. Southwest Jemez Mountains'), adj=c(0,0.5))
#legend('topleft', c('F'), col = c('black'), bty = 'n')

ForIndexVal <- unique(data.val.ind$Forest)
# Sit2
plot(STBIOMSha ~ MPMEstimates, data = data.val.ind[data.val.ind$Forest==ForIndexVal[2], ], xlim = c(0,625), ylim = c(0,625), col = 'white', 
     xlab = 'Predicted AGB (Mg/ha)', ylab = 'Field AGB (Mg/ha)', las=1)
points(STBIOMSha ~ MPMEstimates, data = data.mod[data.val.ind$Forest==ForIndexVal[2], ], pch = 19, cex = .75, col = 'black')
abline(a=0, b=1, col = 'red', lwd = 2, lty = 5)
mtext("Field AGB (Mg/ha)", 2, outer=FALSE, line=2.75, xpd=NA, cex = 0.75)
mtext("Predicted AGB (Mg/ha)", 1, outer=FALSE, line=2.75, xpd=NA, cex = 0.75)
text(10, 600, c('G. Apache-Sitgreaves NF, Stage 2'), adj=c(0,0.5))
#legend('topleft', c('G'), col = c('black'), bty = 'n')

# Sit3
plot(STBIOMSha ~ MPMEstimates, data = data.val.ind[data.val.ind$Forest==ForIndexVal[1], ], xlim = c(0,625), ylim = c(0,625), col = 'white', 
     xlab = 'Predicted AGB (Mg/ha)', las=1, yaxt="n")
#axis(2, labels=FALSE)
axis(4, las = 1)
points(STBIOMSha ~ MPMEstimates, data = data.val.ind[data.val.ind$Forest==ForIndexVal[1], ], pch = 19, cex = .75, col = 'black', bg= 'black')
abline(a=0, b=1, col = 'red', lwd = 2, lty = 5)
mtext("Predicted AGB (Mg/ha)", 1, outer=FALSE, line=2.75, xpd=NA, cex = 0.75)
text(10, 600, c('H. Apache-Sitgreaves NF, Stage 3'), adj=c(0,0.5))
#legend('topleft', c('H'), col = c('black'), bty = 'n')

#dev.off()
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
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

#png(file="scatterplotObPred.png",width=800,height=1200,res=120)
#pdf(file="scatterplotObPred.pdf", width=6, height=12, family = "Helvetica")
dev.new(width=7, height=16)
#par(mfrow=c(4, 2), mar=c(4.1, 2.1, 1.5, 0.5), oma=c(0, 2, 0, 0), mgp=c(2.25, 1, 0))
#par(mfrow=c(3, 2), mar=c(0.6, 0.6, 1.5, 0.5), oma=c(3.3, 3.3, 0, 0), mgp=c(2.25, 1, 0))
par(mfrow=c(3, 2), mar=c(0.6, 0.6, 1.5, 0.5), oma=c(3.3, 3.3, 0, 2), mgp=c(2.25, 1, 0))
#par(mar=c(5,3,2,2)+0.1)
#.02 
plot(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$PlotSizeha==PlotIndex[1], ], xlim = c(0,625), ylim = c(0,625), col = 'white', 
     ylab = 'Field AGB (Mg/ha)', xlab = '', las=1, xaxt="n")#, main = 'a)')
axis(1, labels=FALSE)
points(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$PlotSizeha==PlotIndex[1], ], pch = 19, cex = .75, col = 'gray')
points(STBIOMSha ~ MPMEstimates, data = data.val[data.val$PlotSizeha==PlotIndex[1], ], pch = 19, cex = .75, col = 'black', bg= 'black')
points(STBIOMSha ~ MPMEstimates, data = data.val.ind[data.val.ind$PlotSizeha==PlotIndex[1], ], pch = 19, cex = .75, col = 'black', bg= 'black')
abline(a=0, b=1, col = 'red', lwd = 2, lty = 5)
mtext("Field AGB (Mg/ha)", 2, outer=FALSE, line=2.75, xpd=NA, cex = 0.75)
text(10, 600, paste('A. plot size =', PlotIndex[1], 'ha'), adj=c(0,0.5))
text(45, 550, paste('n =', lengthVec[1]), adj=c(0,0.5))
#legend('topleft', c('A'), col = c('black'), bty = 'n')
legend('bottomright', c('model construction','validation'), col = c('gray', 'black'), pch = c(19, 19),        pt.bg = c('black', 'gray'), bty = 'n')

# .03333
plot(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$PlotSizeha==PlotIndex[2], ], xlim = c(0,625), ylim = c(0,625), col = 'white', 
     las=1, yaxt="n", xaxt="n")
#axis(2, labels=FALSE)
axis(1, labels=FALSE)
axis(4, las = 1)
points(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$PlotSizeha==PlotIndex[2], ], pch = 19, cex = .75, col = 'gray')
points(STBIOMSha ~ MPMEstimates, data = data.val[data.val$PlotSizeha==PlotIndex[2], ], pch = 19, cex = .75, col = 'black', bg= 'black')
points(STBIOMSha ~ MPMEstimates, data = data.val.ind[data.val.ind$PlotSizeha==PlotIndex[2], ], pch = 19, cex = .75, col = 'black', bg= 'black')
abline(a=0, b=1, col = 'red', lwd = 2, lty = 5)
text(10, 600, paste('B. plot size =', PlotIndex[2], 'ha'), adj=c(0,0.5))
text(45, 550, paste('n =', lengthVec[2]), adj=c(0,0.5))
#legend('topleft', c('B'), col = c('black'), bty = 'n')

# .05
plot(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$PlotSizeha==PlotIndex[3], ], xlim = c(0,625), ylim = c(0,625), col = 'white', 
     ylab = 'Field AGB (Mg/ha)', las=1, xaxt="n")
points(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$PlotSizeha==PlotIndex[3], ], pch = 19, cex = .75, col = 'gray')
points(STBIOMSha ~ MPMEstimates, data = data.val[data.val$PlotSizeha==PlotIndex[3], ], pch = 19, cex = .75, col = 'black', bg= 'black')
points(STBIOMSha ~ MPMEstimates, data = data.val.ind[data.val.ind$PlotSizeha==PlotIndex[3], ], pch = 19, cex = .75, col = 'black', bg= 'black')
axis(1, labels=FALSE)
mtext("Field AGB (Mg/ha)", 2, outer=FALSE, line=2.75, xpd=NA, cex = 0.75)
abline(a=0, b=1, col = 'red', lwd = 2, lty = 5)
text(10, 600, paste('C. plot size =', PlotIndex[3], 'ha'), adj=c(0,0.5))
text(45, 550, paste('n =', lengthVec[3]), adj=c(0,0.5))
#legend('topleft', c('C'), col = c('black'), bty = 'n')

# .06666666
plot(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$PlotSizeha==PlotIndex[4], ], xlim = c(0,625), ylim = c(0,625), col = 'white', 
     las=1, yaxt="n", xaxt="n")
#axis(2, labels=FALSE)
axis(1, labels=FALSE)
axis(4, las = 1)
points(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$PlotSizeha==PlotIndex[4], ], pch = 19, cex = .75, col = 'gray')
points(STBIOMSha ~ MPMEstimates, data = data.val[data.val$PlotSizeha==PlotIndex[4], ], pch = 19, cex = .75, col = 'black', bg= 'black')
points(STBIOMSha ~ MPMEstimates, data = data.val.ind[data.val.ind$PlotSizeha==PlotIndex[4], ], pch = 19, cex = .75, col = 'black', bg= 'black')
abline(a=0, b=1, col = 'red', lwd = 2, lty = 5)
text(10, 600, paste('D. plot size =', PlotIndex[4], 'ha'), adj=c(0,0.5))
text(45, 550, paste('n =', lengthVec[4]), adj=c(0,0.5))
#legend('topleft', c('D'), col = c('black'), bty = 'n')

# 0.1
plot(STBIOMSha ~ MPMEstimates, data = data.val.ind[data.val.ind$PlotSizeha==PlotIndex[5], ], xlim = c(0,625), ylim = c(0,625), col = 'white', 
     xlab = 'Predicted AGB (Mg/ha)', ylab = 'Field AGB (Mg/ha)', las=1)
points(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$PlotSizeha==PlotIndex[5], ], pch = 19, cex = .75, col = 'gray')
points(STBIOMSha ~ MPMEstimates, data = data.val[data.val$PlotSizeha==PlotIndex[5], ], pch = 19, cex = .75, col = 'black', bg= 'black')
points(STBIOMSha ~ MPMEstimates, data = data.val.ind[data.val.ind$PlotSizeha==PlotIndex[5], ], pch = 19, cex = .75, col = 'black')
abline(a=0, b=1, col = 'red', lwd = 2, lty = 5)
mtext("Field AGB (Mg/ha)", 2, outer=FALSE, line=2.75, xpd=NA, cex = 0.75)
mtext("Predicted AGB (Mg/ha)", 1, outer=FALSE, line=2.75, xpd=NA, cex = 0.75)
text(10, 600, paste('E. plot size =', PlotIndex[5], 'ha'), adj=c(0,0.5))
text(45, 550, paste('n =', lengthVec[5]), adj=c(0,0.5))
#legend('topleft', c('E'), col = c('black'), bty = 'n')

# 0.2
plot(STBIOMSha ~ MPMEstimates, data = data.val.ind[data.val.ind$PlotSizeha==PlotIndex[6], ], xlim = c(0,625), ylim = c(0,625), col = 'white', 
     xlab = 'Predicted AGB (Mg/ha)', las=1, yaxt="n")
#axis(2, labels=FALSE)
axis(4, las = 1)
points(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$PlotSizeha==PlotIndex[6], ], pch = 19, cex = .75, col = 'gray')
points(STBIOMSha ~ MPMEstimates, data = data.val[data.val$PlotSizeha==PlotIndex[6], ], pch = 19, cex = .75, col = 'black', bg= 'black')
points(STBIOMSha ~ MPMEstimates, data = data.val.ind[data.val.ind$PlotSizeha==PlotIndex[6], ], pch = 19, cex = .75, col = 'black', bg= 'black')
abline(a=0, b=1, col = 'red', lwd = 2, lty = 5)
mtext("Predicted AGB (Mg/ha)", 1, outer=FALSE, line=2.75, xpd=NA, cex = 0.75)
text(10, 600, paste('F. plot size =', PlotIndex[6], 'ha'), adj=c(0,0.5))
text(45, 550, paste('n =', lengthVec[6]), adj=c(0,0.5))
#legend('topleft', c('F'), col = c('black'), bty = 'n')

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

points(Prednotrans[data.mod$PlotSizeha >= .1] ~ data.mod$STBIOMSha[data.mod$PlotSizeha >= .1], col ='gold')
points(Prednotrans[data.mod$PlotSizeha < 0.05] ~ data.mod$STBIOMSha[data.mod$PlotSizeha < 0.05], col ='red')

ggplot(data.mod, aes(STBIOMSha,MPMEstimates)) + geom_point() + geom_smooth() + geom_smooth(method=lm, se=FALSE, col = 'grey')

ggplot(frame[frame$Forest == 'NorthKaibab', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'SWJM', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Sitgreaves', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Tonto', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()
ggplot(frame[frame$Forest == 'Coconino', ], aes(STBIOMSha,Residual)) + geom_point() + geom_smooth()

