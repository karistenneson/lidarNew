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
## set up survey design
data.val$Stratum <- as.character(data.val$Stratum)
data.val.ind$Stratum <- as.character(data.val.ind$Stratum)

data.svy.val.NK <- svydesign(ids = ~1, data = data.val[data.val$Forest == 'NorthKaibab', ], fpc = data.val$fpc[data.val$Forest == 'NorthKaibab'], strata = data.val$Stratum[data.val$Forest == 'NorthKaibab'])
data.svy.val.Coco <- svydesign(ids = ~1, data = data.val[data.val$Forest == 'Coconino', ], fpc = data.val$fpc[data.val$Forest == 'Coconino'], strata = data.val$Stratum[data.val$Forest == 'Coconino'])
data.svy.val.Tonto <- svydesign(ids = ~1, data = data.val[data.val$Forest == 'Tonto', ], fpc = data.val$fpc[data.val$Forest == 'Tonto'], strata = data.val$Stratum[data.val$Forest == 'Tonto'])
data.svy.val.Sit <- svydesign(ids = ~1, data = data.val[data.val$Forest == 'Sitgreaves', ], fpc = data.val$fpc[data.val$Forest == 'Sitgreaves'], strata = data.val$Stratum[data.val$Forest == 'Sitgreaves'])
data.svy.val.SWJM <- svydesign(ids = ~1, data = data.val[data.val$Forest == 'SWJM', ], fpc = data.val$fpc[data.val$Forest == 'SWJM'], strata = data.val$Stratum[data.val$Forest == 'SWJM'])

data.svy.val.Sit2 <- svydesign(ids = ~1, data = data.val.ind[data.val.ind$Forest == 'Sitgreaves, P2', ], fpc = data.val.ind$fpc[data.val.ind$Forest == 'Sitgreaves, P2'], strata = data.val.ind$Stratum[data.val.ind$Forest == 'Sitgreaves, P2'])
data.svy.val.Sit3 <- svydesign(ids = ~1, data = data.val.ind[data.val.ind$Forest == 'Apache', ], fpc = data.val.ind$fpc[data.val.ind$Forest == 'Apache'], strata = data.val.ind$Stratum[data.val.ind$Forest == 'Apache'])

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
labelend <- 625
labelendR <- 575

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
dev.new(width=16, height=16)

#par(mfrow=c(3, 3), mar=c(0.6, 0.6, 1.5, 0.5), oma=c(3.3, 3.3, 0, 2), mgp=c(2.25, 1, 0))
par(mfrow=c(3, 3), mar=c(0.6, 0.6, 0.6, 0.5),
    oma=c(3.3, 3.3, 0, 2), mgp=c(2.25, 1, 0))
#par(mar=c(5,3,2,2)+0.1)

#all 
plot(STBIOMSha ~ MPMEstimates, data = data.mod, xlim = c(0,635), ylim = c(0,635), col = 'white', las=1, xaxt="n")#, main = 'a)')
abline(h = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
abline(v = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
points(STBIOMSha ~ MPMEstimates, data = data.mod, pch = 19, cex = .75, col = 'gray')
points(STBIOMSha ~ MPMEstimates, data = data.val, pch = 19, cex = .75, col = 'black', bg= 'black')
abline(a=0, b=1, col = OneOneCol, lwd = OneOnelwd, lty = OneOnelty)
abline(lm(STBIOMSha ~ 0 + MPMEstimates, data = data.val), col = trendCol, lwd = trendlwd, lty = trendlwd)
mtext("Field AGB (Mg/ha)", 2, outer=FALSE, line=2.75, xpd=NA, cex = 0.75)
text(labelstart, labelend, c('A. All Model Const.'), adj=c(0,0.5))

test<-summary(lm(STBIOMSha ~ MPMEstimates, data = data.val))
text(labelstartR, labelendR, paste("Field =",signif(test$coefficients[1],3),"* Pred."), adj=c(0,0.5))


# Kaibab
plot(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$Forest==ForIndex[4], ], xlim = c(0,635), ylim = c(0,635), col = 'white', las=1, yaxt="n", xaxt="n")
abline(h = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
abline(v = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
#axis(2, labels=FALSE)
#axis(1, labels=FALSE)
points(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$Forest==ForIndex[4], ], pch = 19, cex = .75, col = 'gray')
points(STBIOMSha ~ MPMEstimates, data = data.val[data.val$Forest==ForIndex[4], ], pch = 19, cex = .75, col = 'black', bg= 'black')
abline(a=0, b=1, col = OneOneCol, lwd = OneOnelwd, lty = OneOnelty)
abline(lm(STBIOMSha ~ 0 + MPMEstimates, data = data.val[data.val$Forest==ForIndex[4], ]), col = trendCol, lwd = trendlwd, lty = trendlwd)
svySlopeNK <- summary(svyglm(STBIOMSha ~ 0 + MPMEstimates, design = data.svy.val.NK))
abline(a = 0, b = svySlopeNK$coefficients[1], col = trendColPop, lwd = trendlwdPop, lty = trendlwdPop)
text(labelstart, labelend, c('B. Kaibab Plateau'), adj=c(0,0.5))
test<-summary(lm(STBIOMSha ~ 0 + MPMEstimates, data = data.val[data.val$Forest==ForIndex[4], ]))
text(labelstartR, labelendR, paste("Field =",signif(test$coefficients[1],3),"* Pred."), adj=c(0,0.5))
#legend('topleft', c('B'), col = c('black'), bty = 'n')

# Coco
plot(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$Forest==ForIndex[1], ], xlim = c(0,635), ylim = c(0,635), col = 'white', las=1, xaxt="n", yaxt="n")
abline(h = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
abline(v = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
points(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$Forest==ForIndex[1], ], pch = 19, cex = .75, col = 'gray')
points(STBIOMSha ~ MPMEstimates, data = data.val[data.val$Forest==ForIndex[1], ], pch = 19, cex = .75, col = 'black', bg= 'black')
#axis(1, labels=FALSE)
axis(4, las = 1)
abline(a=0, b=1, col = OneOneCol, lwd = OneOnelwd, lty = OneOnelty)
svySlopeCoco <- summary(svyglm(STBIOMSha ~ 0 + MPMEstimates, design = data.svy.val.Coco))
abline(a = 0, b = svySlopeCoco$coefficients[1], col = trendColPop, lwd = trendlwdPop, lty = trendlwdPop)
abline(lm(STBIOMSha ~ 0 + MPMEstimates, data = data.val[data.val$Forest==ForIndex[1], ]), col = trendCol, lwd = trendlwd, lty = trendlwd)
text(labelstart, labelend, c('C. Coconino NF'), adj=c(0,0.5))
test<-summary(lm(STBIOMSha ~ 0 + MPMEstimates, data = data.val[data.val$Forest==ForIndex[1], ]))
text(labelstartR, labelendR, paste("Field =",signif(test$coefficients[1],3),"* Pred."), adj=c(0,0.5))
#legend('topleft', c('C'), col = c('black'), bty = 'n')

# Tonto
plot(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$Forest==ForIndex[3], ], xlim = c(0,635), ylim = c(0,635), col = 'white', las=1, xaxt="n")
#axis(2, labels=FALSE)
abline(h = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
abline(v = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
#axis(1, labels=FALSE)
points(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$Forest==ForIndex[3], ], pch = 19, cex = .75, col = 'gray')
points(STBIOMSha ~ MPMEstimates, data = data.val[data.val$Forest==ForIndex[3], ], pch = 19, cex = .75, col = 'black', bg= 'black')
abline(a=0, b=1, col = OneOneCol, lwd = OneOnelwd, lty = OneOnelty)
svySlopeTonto <- summary(svyglm(STBIOMSha ~ 0 + MPMEstimates, design = data.svy.val.Tonto))
abline(a = 0, b = svySlopeTonto$coefficients[1], col = trendColPop, lwd = trendlwdPop, lty = trendlwdPop)
abline(lm(STBIOMSha ~ 0 + MPMEstimates, data = data.val[data.val$Forest==ForIndex[3], ]), col = trendCol, lwd = trendlwd, lty = trendlwd)
mtext("Field AGB (Mg/ha)", 2, outer=FALSE, line=2.75, xpd=NA, cex = 0.75)
text(labelstart, labelend, c('D. Tonto NF'), adj=c(0,0.5))
test<-summary(lm(STBIOMSha ~ 0 + MPMEstimates, data = data.val[data.val$Forest==ForIndex[3], ]))
text(labelstartR, labelendR, paste("Field =",signif(test$coefficients[1],3),"* Pred."), adj=c(0,0.5))

#legend('topleft', c('D'), col = c('black'), bty = 'n')


# Sit
plot(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$Forest==ForIndex[2], ], xlim = c(0,635), ylim = c(0,635), col = 'white',  las=1, xaxt="n", yaxt="n")
abline(h = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
abline(v = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
points(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$Forest==ForIndex[2], ], pch = 19, cex = .75, col = 'gray')
points(STBIOMSha ~ MPMEstimates, data = data.val[data.val$Forest==ForIndex[2], ], pch = 19, cex = .75, col = 'black', bg= 'black')
#axis(1, labels=FALSE)
abline(a=0, b=1, col = OneOneCol, lwd = OneOnelwd, lty = OneOnelty)
svySlopeSit <- summary(svyglm(STBIOMSha ~ 0 + MPMEstimates, design = data.svy.val.Sit))
abline(a = 0, b = svySlopeSit$coefficients[1], col = trendColPop, lwd = trendlwdPop, lty = trendlwdPop)
abline(lm(STBIOMSha ~ 0 + MPMEstimates, data = data.val[data.val$Forest==ForIndex[2], ]), col = trendCol, lwd = trendlwd, lty = trendlwd)
text(labelstart, labelend, c('E. AS NF, Stage 1'), adj=c(0,0.5))
test<-summary(lm(STBIOMSha ~ 0 + MPMEstimates, data = data.val[data.val$Forest==ForIndex[2], ]))
text(labelstartR, labelendR, paste("Field =",signif(test$coefficients[1],3),"* Pred."), adj=c(0,0.5))

#legend('topleft', c('E'), col = c('black'), bty = 'n')

# SWJM
plot(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$Forest==ForIndex[5], ], xlim = c(0,635), ylim = c(0,635), col = 'white', 
     las=1, yaxt="n")
#axis(2, labels=FALSE)
abline(h = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
abline(v = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
axis(1, labels=FALSE)
axis(4, las = 1)
points(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$Forest==ForIndex[5], ], pch = 19, cex = .75, col = 'gray')
points(STBIOMSha ~ MPMEstimates, data = data.val[data.val$Forest==ForIndex[5], ], pch = 19, cex = .75, col = 'black', bg= 'black')
abline(a=0, b=1, col = OneOneCol, lwd = OneOnelwd, lty = OneOnelty)
svySlopeSWJM <- summary(svyglm(STBIOMSha ~ 0 + MPMEstimates, design = data.svy.val.SWJM))
abline(a = 0, b = svySlopeSWJM$coefficients[1], col = trendColPop, lwd = trendlwdPop, lty = trendlwdPop)
abline(lm(STBIOMSha ~ 0 + MPMEstimates, data = data.val[data.val$Forest==ForIndex[5], ]), col = trendCol, lwd = trendlwd, lty = trendlwd)
mtext("Predicted AGB (Mg/ha)", 1, outer=FALSE, line=2.75, xpd=NA, cex = 0.75)
text(labelstart, labelend, c('F. SW Jemez Mountains'), adj=c(0,0.5))
test<-summary(lm(STBIOMSha ~ 0 + MPMEstimates, data = data.val[data.val$Forest==ForIndex[5], ]))
text(labelstartR, labelendR, paste("Field =",signif(test$coefficients[1],3),"* Pred."), adj=c(0,0.5))
#legend('topleft', c('F'), col = c('black'), bty = 'n')

ForIndexVal <- unique(data.val.ind$Forest)
# Sit2
plot(STBIOMSha ~ MPMEstimates, data = data.val.ind[data.val.ind$Forest==ForIndexVal[2], ], xlim = c(0,635), ylim = c(0,635), col = 'white', ylab = '', las=1)
abline(h = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
abline(v = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
points(STBIOMSha ~ MPMEstimates, data = data.mod[data.val.ind$Forest==ForIndexVal[2], ], pch = 19, cex = .75, col = 'black')
abline(a=0, b=1, col = OneOneCol, lwd = OneOnelwd, lty = OneOnelty)
svySlopeSit2 <- summary(svyglm(STBIOMSha ~ MPMEstimates, design = data.svy.val.Sit2))
abline(a = svySlopeSit2$coefficients[1], b = svySlopeSit2$coefficients[2], col = trendColPop, lwd = trendlwdPop, lty = trendlwdPop)
abline(lm(STBIOMSha ~ 0 + MPMEstimates, data = data.val.ind[data.val.ind$Forest==ForIndexVal[2], ]), col = trendCol, lwd = trendlwd, lty = trendlwd)
mtext("Field AGB (Mg/ha)", 2, outer=FALSE, line=2.75, xpd=NA, cex = 0.75)
mtext("Predicted AGB (Mg/ha)", 1, outer=FALSE, line=2.75, xpd=NA, cex = 0.75)
text(labelstart, labelend, c('G. AS NF, Stage 2'), adj=c(0,0.5))
test<-summary(lm(STBIOMSha ~ MPMEstimates, data = data.val.ind[data.val.ind$Forest==ForIndexVal[2], ]))
text(labelstartR, labelendR, paste("Field =",signif(test$coefficients[2],3),"* Pred.",signif(test$coefficients[1],3)), adj=c(0,0.5))
#legend('topleft', c('G'), col = c('black'), bty = 'n')

# Sit3
plot(STBIOMSha ~ MPMEstimates, data = data.val.ind[data.val.ind$Forest==ForIndexVal[1], ], xlim = c(0,635), ylim = c(0,635), col = 'white', las=1, yaxt="n")
#axis(2, labels=FALSE)
abline(h = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
abline(v = seq(from = startSeq, to =FinSeq, by = plotInt), col = 'light grey')
axis(4, las = 1)
points(STBIOMSha ~ MPMEstimates, data = data.val.ind[data.val.ind$Forest==ForIndexVal[1], ], pch = 19, cex = .75, col = 'black', bg= 'black')
abline(a=0, b=1, col = OneOneCol, lwd = OneOnelwd, lty = OneOnelty)
svySlopeSit3 <- summary(svyglm(STBIOMSha ~ MPMEstimates, design = data.svy.val.Sit3))
abline(a = svySlopeSit3$coefficients[1], b = svySlopeSit3$coefficients[2], col = trendColPop, lwd = trendlwdPop, lty = trendlwdPop)
abline(lm(STBIOMSha ~ MPMEstimates, data = data.val.ind[data.val.ind$Forest==ForIndexVal[1], ]), col = trendCol, lwd = trendlwd, lty = trendlwd)
mtext("Predicted AGB (Mg/ha)", 1, outer=FALSE, line=2.75, xpd=NA, cex = 0.75)
text(labelstart, labelend, c('H. AS NF, Stage 3'), adj=c(0,0.5))
test<-summary(lm(STBIOMSha ~ MPMEstimates, data = data.val.ind[data.val.ind$Forest==ForIndexVal[1], ]))
text(labelstartR, labelendR, paste("Field =",signif(test$coefficients[2],3),"* Pred.",signif(test$coefficients[1],3)), adj=c(0,0.5))
#legend('topleft', c('H'), col = c('black'), bty = 'n')

#all 
plot(STBIOMSha ~ MPMEstimates, data = data.mod, xlim = c(0,635), ylim = c(0,635), col = 'white', xlab = '', las=1,yaxt="n", xaxt="n", frame.plot = F)
legend('bottomright', c('model construction','validation', 'sample trendline', 'pop. trendline', '1:1'), 
       col = c('gray', 'black', trendCol,trendColPop, OneOneCol), 
       pch = c(19, 19, NA, NA, NA),  pt.bg = c('black', 'gray'), bty = 'n', 
       lty = c(NA, NA, trendlty,trendltyPop, OneOnelty), lwd = c(NA, NA, trendlwd,trendlwdPop, OneOnelwd), 
       cex = 1.1)


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
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################

x <- seq(1:10)
y <- x*4 + rnorm(length(x),0,1)
plot(y~x, ylim=c(0,max(y)),xlim = c(0,max(y)))
points(y~x)
abline(a=0, b=1, col = OneOneCol, lwd = OneOnelwd, lty = OneOnelty)
abline(lm(y~x), col = trendCol, lwd = trendlwd, lty = trendlwd)
summary(lm(y~x))
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
plot(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$PlotSizeha==PlotIndex[1], ], xlim = c(0,635), ylim = c(0,635), col = 'white', 
     ylab = 'Field AGB (Mg/ha)', xlab = '', las=1, xaxt="n")#, main = 'a)')
axis(1, labels=FALSE)
points(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$PlotSizeha==PlotIndex[1], ], pch = 19, cex = .75, col = 'gray')
points(STBIOMSha ~ MPMEstimates, data = data.val[data.val$PlotSizeha==PlotIndex[1], ], pch = 19, cex = .75, col = 'black', bg= 'black')
points(STBIOMSha ~ MPMEstimates, data = data.val.ind[data.val.ind$PlotSizeha==PlotIndex[1], ], pch = 19, cex = .75, col = 'black', bg= 'black')
abline(a=0, b=1, col = 'red', lwd = 2, lty = 5)
mtext("Field AGB (Mg/ha)", 2, outer=FALSE, line=2.75, xpd=NA, cex = 0.75)
text(labelstart, labelend, paste('A. plot size =', PlotIndex[1], 'ha'), adj=c(0,0.5))
text(45, 550, paste('n =', lengthVec[1]), adj=c(0,0.5))
#legend('topleft', c('A'), col = c('black'), bty = 'n')
legend('bottomright', c('model construction','validation'), col = c('gray', 'black'), pch = c(19, 19),        pt.bg = c('black', 'gray'), bty = 'n')

# .03333
plot(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$PlotSizeha==PlotIndex[2], ], xlim = c(0,635), ylim = c(0,635), col = 'white', 
     las=1, yaxt="n", xaxt="n")
#axis(2, labels=FALSE)
axis(1, labels=FALSE)
axis(4, las = 1)
points(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$PlotSizeha==PlotIndex[2], ], pch = 19, cex = .75, col = 'gray')
points(STBIOMSha ~ MPMEstimates, data = data.val[data.val$PlotSizeha==PlotIndex[2], ], pch = 19, cex = .75, col = 'black', bg= 'black')
points(STBIOMSha ~ MPMEstimates, data = data.val.ind[data.val.ind$PlotSizeha==PlotIndex[2], ], pch = 19, cex = .75, col = 'black', bg= 'black')
abline(a=0, b=1, col = 'red', lwd = 2, lty = 5)
text(labelstart, labelend, paste('B. plot size =', PlotIndex[2], 'ha'), adj=c(0,0.5))
text(45, 550, paste('n =', lengthVec[2]), adj=c(0,0.5))
#legend('topleft', c('B'), col = c('black'), bty = 'n')

# .05
plot(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$PlotSizeha==PlotIndex[3], ], xlim = c(0,635), ylim = c(0,635), col = 'white', 
     ylab = 'Field AGB (Mg/ha)', las=1, xaxt="n")
points(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$PlotSizeha==PlotIndex[3], ], pch = 19, cex = .75, col = 'gray')
points(STBIOMSha ~ MPMEstimates, data = data.val[data.val$PlotSizeha==PlotIndex[3], ], pch = 19, cex = .75, col = 'black', bg= 'black')
points(STBIOMSha ~ MPMEstimates, data = data.val.ind[data.val.ind$PlotSizeha==PlotIndex[3], ], pch = 19, cex = .75, col = 'black', bg= 'black')
axis(1, labels=FALSE)
mtext("Field AGB (Mg/ha)", 2, outer=FALSE, line=2.75, xpd=NA, cex = 0.75)
abline(a=0, b=1, col = 'red', lwd = 2, lty = 5)
text(labelstart, labelend, paste('C. plot size =', PlotIndex[3], 'ha'), adj=c(0,0.5))
text(45, 550, paste('n =', lengthVec[3]), adj=c(0,0.5))
#legend('topleft', c('C'), col = c('black'), bty = 'n')

# .06666666
plot(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$PlotSizeha==PlotIndex[4], ], xlim = c(0,635), ylim = c(0,635), col = 'white', 
     las=1, yaxt="n", xaxt="n")
#axis(2, labels=FALSE)
axis(1, labels=FALSE)
axis(4, las = 1)
points(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$PlotSizeha==PlotIndex[4], ], pch = 19, cex = .75, col = 'gray')
points(STBIOMSha ~ MPMEstimates, data = data.val[data.val$PlotSizeha==PlotIndex[4], ], pch = 19, cex = .75, col = 'black', bg= 'black')
points(STBIOMSha ~ MPMEstimates, data = data.val.ind[data.val.ind$PlotSizeha==PlotIndex[4], ], pch = 19, cex = .75, col = 'black', bg= 'black')
abline(a=0, b=1, col = 'red', lwd = 2, lty = 5)
text(labelstart, labelend, paste('D. plot size =', PlotIndex[4], 'ha'), adj=c(0,0.5))
text(45, 550, paste('n =', lengthVec[4]), adj=c(0,0.5))
#legend('topleft', c('D'), col = c('black'), bty = 'n')

# 0.1
plot(STBIOMSha ~ MPMEstimates, data = data.val.ind[data.val.ind$PlotSizeha==PlotIndex[5], ], xlim = c(0,635), ylim = c(0,635), col = 'white', 
     xlab = 'Predicted AGB (Mg/ha)', ylab = 'Field AGB (Mg/ha)', las=1)
points(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$PlotSizeha==PlotIndex[5], ], pch = 19, cex = .75, col = 'gray')
points(STBIOMSha ~ MPMEstimates, data = data.val[data.val$PlotSizeha==PlotIndex[5], ], pch = 19, cex = .75, col = 'black', bg= 'black')
points(STBIOMSha ~ MPMEstimates, data = data.val.ind[data.val.ind$PlotSizeha==PlotIndex[5], ], pch = 19, cex = .75, col = 'black')
abline(a=0, b=1, col = 'red', lwd = 2, lty = 5)
mtext("Field AGB (Mg/ha)", 2, outer=FALSE, line=2.75, xpd=NA, cex = 0.75)
mtext("Predicted AGB (Mg/ha)", 1, outer=FALSE, line=2.75, xpd=NA, cex = 0.75)
text(labelstart, labelend, paste('E. plot size =', PlotIndex[5], 'ha'), adj=c(0,0.5))
text(45, 550, paste('n =', lengthVec[5]), adj=c(0,0.5))
#legend('topleft', c('E'), col = c('black'), bty = 'n')

# 0.2
plot(STBIOMSha ~ MPMEstimates, data = data.val.ind[data.val.ind$PlotSizeha==PlotIndex[6], ], xlim = c(0,635), ylim = c(0,635), col = 'white', 
     xlab = 'Predicted AGB (Mg/ha)', las=1, yaxt="n")
#axis(2, labels=FALSE)
axis(4, las = 1)
points(STBIOMSha ~ MPMEstimates, data = data.mod[data.mod$PlotSizeha==PlotIndex[6], ], pch = 19, cex = .75, col = 'gray')
points(STBIOMSha ~ MPMEstimates, data = data.val[data.val$PlotSizeha==PlotIndex[6], ], pch = 19, cex = .75, col = 'black', bg= 'black')
points(STBIOMSha ~ MPMEstimates, data = data.val.ind[data.val.ind$PlotSizeha==PlotIndex[6], ], pch = 19, cex = .75, col = 'black', bg= 'black')
abline(a=0, b=1, col = 'red', lwd = 2, lty = 5)
mtext("Predicted AGB (Mg/ha)", 1, outer=FALSE, line=2.75, xpd=NA, cex = 0.75)
text(labelstart, labelend, paste('F. plot size =', PlotIndex[6], 'ha'), adj=c(0,0.5))
text(45, 550, paste('n =', lengthVec[6]), adj=c(0,0.5))
#legend('topleft', c('F'), col = c('black'), bty = 'n')

#########################################################################
#########################################################################
#########################################################################
ggplot(data.mod[data.val$PlotSizeha==PlotIndex[1], ], aes(MPMEstimates, STBIOMSha)) + geom_point() + geom_smooth() + xlim(0,600)+ ylim(0,600) + geom_abline(slope = 1, intercept = 0, col = 'red', lty = 2, lwd = 1)

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

