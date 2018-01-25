### This file is for assessing model performance
## Written Karis Tenneson (krtenneson@fs.fed.us)
# Last updated: Jan 20, 2018

### set working directory
#setwd('C:\\Users\\krtenneson\\Desktop\\lidarPaper\\lidarNew\\scripts')
#setwd('\\\\166.2.126.25\\rseat\\Programs\\Reimbursibles\\fy2016\\R3_lidar_equation_transferability\\Analysis\\VersionControl\\lidarNew\\scripts')
#setwd("~/Documents/R/lidarNew/scripts") #Mac
#setwd("~/R/lidarNew/scripts") #WinCampus
#source(file='02_DataPrep.R')

### Load requigrey packages
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
FinSeq <- 700
plotInt <- 200

labelstart <- 10
labelstartR <- 10
labelstartR2 <- 10
labelend <- 625
labelendR <- 575
labelendR2 <- 525

OneOnelty <- 1
OneOnelwd <- 1
OneOneCol <- 'black'

trendlwd <- 2
trendlty <- 5
trendCol <- 'firebrick'
SECol <- 'firebrick1'
transferCol <- 'blue'

trendColval <- 'green'
SEColval <- 'dark green'
transferCol <- 'blue'


dataall <- rbind(data.mod,data.val,data.val.ind)
########################################################
########################################################

#dev.new(width=16, height=16)
par(mfrow=c(1,2))
## heteroscedastic
plot(abs(MPMResidual) ~ STBIOMSha, data = data.mod, main = 'heteroscdastic errors')
abline(lm(abs(MPMResidual) ~ STBIOMSha, data = data.mod), col = 'grey', lwd = 2, lty = 1)
abline(h= 0, col = 'pink', lwd = 1, lty = 1)

## nonlinear
plot(MPMResidual ~ STBIOMSha, data = data.mod, main = 'errors not centegrey on 0')
abline(lm(MPMResidual ~ STBIOMSha, data = data.mod), col = 'grey', lwd = 2, lty = 1)
abline(h= 0, col = 'pink', lwd = 1, lty = 1)

hist(data.mod$MPMResidual)
hist(data.mod$MPMResidual, probability = 'freq')
?hist()


########################################################
########################################################
########################################################
library(dplyr) # With dplyr for example
dataall <- dataall %>% group_by(PlotSizeha) %>%
  mutate(med = median(STBIOMSha))

dataall <- dataall %>% group_by(PlotSizeha) %>%
  mutate(mean = mean(STBIOMSha))

dataall$HighAGB<-rep(0, length(dataall$ID))
dataall$HighAGB[abs(dataall$STBIOMSha)>400]<-1

dataall$pch<-dataall$PlotSizeha*100+14
## saturation plots
par(mfrow=c(3, 3))
#####################################
#####################################
# 30
plot(dataall$MPMResidual~dataall$Elev_P30, xlab = '30th percentile')
points(MPMResidual~Elev_P30, dataall[abs(dataall$MPMResidual)>100,], col = 'grey', pch = 15, cex = 3)
points(MPMResidual~Elev_P30, dataall[abs(dataall$STBIOMSha)>400,], col = 'purple', pch = dataall$pch[abs(dataall$STBIOMSha)>400], cex = 3)

points(MPMResidual~Elev_P30, dataall[dataall$STBIOMSha>400 & dataall$Elev_P60< 25, ], col = 'green', pch = dataall$pch[dataall$STBIOMSha>400 & dataall$Elev_P60< 25], cex = 3)
points(MPMResidual~Elev_P30, dataall[dataall$STBIOMSha>400 & dataall$Elev_P60< 20, ], col = 'orange', pch = dataall$pch[dataall$STBIOMSha>400 & dataall$Elev_P60< 20], cex = 3)
points(MPMResidual~Elev_P30, dataall[dataall$STBIOMSha>400 & dataall$Pct_all_returns_above_ht< 40,], col = 'blue', pch =dataall$pch[dataall$STBIOMSha>400 & dataall$Pct_all_returns_above_ht< 40], cex = 3)

# volume
plot(dataall$MPMResidual~dataall$P30_pctAllOver3m, xlab = '30th percentile * Can Cov')
points(MPMResidual~P30_pctAllOver3m, dataall[abs(dataall$MPMResidual)>100,], col = 'grey', pch = 15, cex = 3)
points(MPMResidual~P30_pctAllOver3m, dataall[abs(dataall$STBIOMSha)>400,], col = 'purple', pch = dataall$pch[abs(dataall$STBIOMSha)>400], cex = 3)

points(MPMResidual~P30_pctAllOver3m, dataall[dataall$STBIOMSha>400 & dataall$Elev_P60< 25, ], col = 'green', pch = dataall$pch[dataall$STBIOMSha>400 & dataall$Elev_P60< 25], cex = 3)
points(MPMResidual~P30_pctAllOver3m, dataall[dataall$STBIOMSha>400 & dataall$Elev_P60< 20, ], col = 'orange', pch = dataall$pch[dataall$STBIOMSha>400 & dataall$Elev_P60< 20], cex = 3)
points(MPMResidual~P30_pctAllOver3m, dataall[dataall$STBIOMSha>400 & dataall$Pct_all_returns_above_ht< 40,], col = 'blue', pch =dataall$pch[dataall$STBIOMSha>400 & dataall$Pct_all_returns_above_ht< 40], cex = 3)

# skew
plot(dataall$MPMResidual~dataall$Elev_MAD_median, xlab = 'MAD_median')
points(MPMResidual~Elev_MAD_median, dataall[abs(dataall$MPMResidual)>100,], col = 'grey', pch = 15, cex = 3)
points(MPMResidual~Elev_MAD_median, dataall[abs(dataall$STBIOMSha)>400,], col = 'purple', pch = dataall$pch[abs(dataall$STBIOMSha)>400], cex = 3)

points(MPMResidual~Elev_MAD_median, dataall[dataall$STBIOMSha>400 & dataall$Elev_P60< 25, ], col = 'green', pch = dataall$pch[dataall$STBIOMSha>400 & dataall$Elev_P60< 25], cex = 3)
points(MPMResidual~Elev_MAD_median, dataall[dataall$STBIOMSha>400 & dataall$Elev_P60< 20, ], col = 'orange', pch = dataall$pch[dataall$STBIOMSha>400 & dataall$Elev_P60< 20], cex = 3)
points(MPMResidual~Elev_MAD_median, dataall[dataall$STBIOMSha>400 & dataall$Pct_all_returns_above_ht< 40,], col = 'blue', pch =dataall$pch[dataall$STBIOMSha>400 & dataall$Pct_all_returns_above_ht< 40], cex = 3)

# 60
plot(dataall$MPMResidual~dataall$Elev_P60, xlab = '60th percentile')
points(MPMResidual~Elev_P60, dataall[abs(dataall$MPMResidual)>100,], col = 'grey', pch = 15, cex = 3)
points(MPMResidual~Elev_P60, dataall[abs(dataall$STBIOMSha)>400,], col = 'purple', pch = dataall$pch[abs(dataall$STBIOMSha)>400], cex = 3)

points(MPMResidual~Elev_P60, dataall[dataall$STBIOMSha>400 & dataall$Elev_P60< 25, ], col = 'green', pch = dataall$pch[dataall$STBIOMSha>400 & dataall$Elev_P60< 25], cex = 3)
points(MPMResidual~Elev_P60, dataall[dataall$STBIOMSha>400 & dataall$Elev_P60< 20, ], col = 'orange', pch = dataall$pch[dataall$STBIOMSha>400 & dataall$Elev_P60< 20], cex = 3)
points(MPMResidual~Elev_P60, dataall[dataall$STBIOMSha>400 & dataall$Pct_all_returns_above_ht< 40,], col = 'blue', pch =dataall$pch[dataall$STBIOMSha>400 & dataall$Pct_all_returns_above_ht< 40], cex = 3)

# volume
plot(dataall$MPMResidual~dataall$P60_pctAllOver3m, xlab = '60th percentile * Can Cov')
points(MPMResidual~P60_pctAllOver3m, dataall[abs(dataall$MPMResidual)>100,], col = 'grey', pch = 15, cex = 3)
points(MPMResidual~P60_pctAllOver3m, dataall[abs(dataall$STBIOMSha)>400,], col = 'purple', pch = dataall$pch[abs(dataall$STBIOMSha)>400], cex = 3)

points(MPMResidual~P60_pctAllOver3m, dataall[dataall$STBIOMSha>400 & dataall$Elev_P60< 25, ], col = 'green', pch = dataall$pch[dataall$STBIOMSha>400 & dataall$Elev_P60< 25], cex = 3)
points(MPMResidual~P60_pctAllOver3m, dataall[dataall$STBIOMSha>400 & dataall$Elev_P60< 20, ], col = 'orange', pch = dataall$pch[dataall$STBIOMSha>400 & dataall$Elev_P60< 20], cex = 3)
points(MPMResidual~P60_pctAllOver3m, dataall[dataall$STBIOMSha>400 & dataall$Pct_all_returns_above_ht< 40,], col = 'blue', pch =dataall$pch[dataall$STBIOMSha>400 & dataall$Pct_all_returns_above_ht< 40], cex = 3)

# 60^2
plot(dataall$MPMResidual~dataall$Elev_P602, xlab = '60th percentile^2')
points(MPMResidual~Elev_P602, dataall[abs(dataall$MPMResidual)>100,], col = 'grey', pch = 15, cex = 3)
points(MPMResidual~Elev_P602, dataall[abs(dataall$STBIOMSha)>400,], col = 'purple', pch = dataall$pch[abs(dataall$STBIOMSha)>400], cex = 3)

points(MPMResidual~Elev_P602, dataall[dataall$STBIOMSha>400 & dataall$Elev_P60< 25, ], col = 'green', pch = dataall$pch[dataall$STBIOMSha>400 & dataall$Elev_P60< 25], cex = 3)
points(MPMResidual~Elev_P602, dataall[dataall$STBIOMSha>400 & dataall$Elev_P60< 20, ], col = 'orange', pch = dataall$pch[dataall$STBIOMSha>400 & dataall$Elev_P60< 20], cex = 3)
points(MPMResidual~Elev_P602, dataall[dataall$STBIOMSha>400 & dataall$Pct_all_returns_above_ht< 40,], col = 'blue', pch =dataall$pch[dataall$STBIOMSha>400 & dataall$Pct_all_returns_above_ht< 40], cex = 3)

# 90
plot(dataall$MPMResidual~dataall$Elev_P90, xlab = '90th percentile')
points(MPMResidual~Elev_P90, dataall[abs(dataall$MPMResidual)>100,], col = 'grey', pch = 15, cex = 3)
points(MPMResidual~Elev_P90, dataall[abs(dataall$STBIOMSha)>400,], col = 'purple', pch = dataall$pch[abs(dataall$STBIOMSha)>400], cex = 3)

points(MPMResidual~Elev_P90, dataall[dataall$STBIOMSha>400 & dataall$Elev_P60< 25, ], col = 'green', pch = dataall$pch[dataall$STBIOMSha>400 & dataall$Elev_P60< 25], cex = 3)
points(MPMResidual~Elev_P90, dataall[dataall$STBIOMSha>400 & dataall$Elev_P60< 20, ], col = 'orange', pch = dataall$pch[dataall$STBIOMSha>400 & dataall$Elev_P60< 20], cex = 3)
points(MPMResidual~Elev_P90, dataall[dataall$STBIOMSha>400 & dataall$Pct_all_returns_above_ht< 40,], col = 'blue', pch =dataall$pch[dataall$STBIOMSha>400 & dataall$Pct_all_returns_above_ht< 40], cex = 3)


# cover
plot(dataall$MPMResidual~dataall$Pct_all_returns_above_ht, xlab = 'Can Cov')
points(MPMResidual~Pct_all_returns_above_ht, dataall[abs(dataall$MPMResidual)>100,], col = 'grey', pch = 15, cex = 3)
points(MPMResidual~Pct_all_returns_above_ht, dataall[abs(dataall$STBIOMSha)>400,], col = 'purple', pch = dataall$pch[abs(dataall$STBIOMSha)>400], cex = 3)

points(MPMResidual~Pct_all_returns_above_ht, dataall[dataall$STBIOMSha>400 & dataall$Elev_P60< 25, ], col = 'green', pch = dataall$pch[dataall$STBIOMSha>400 & dataall$Elev_P60< 25], cex = 3)
points(MPMResidual~Pct_all_returns_above_ht, dataall[dataall$STBIOMSha>400 & dataall$Elev_P60< 20, ], col = 'orange', pch = dataall$pch[dataall$STBIOMSha>400 & dataall$Elev_P60< 20], cex = 3)
points(MPMResidual~Pct_all_returns_above_ht, dataall[dataall$STBIOMSha>400 & dataall$Pct_all_returns_above_ht< 40,], col = 'blue', pch =dataall$pch[dataall$STBIOMSha>400 & dataall$Pct_all_returns_above_ht< 40], cex = 3)


# first normalized cov
plot(dataall$MPMResidual~dataall$all_returns_above_ht_div_Total_first_returns_x_100, xlab = 'can cover: all first returns')
points(MPMResidual~all_returns_above_ht_div_Total_first_returns_x_100, dataall[abs(dataall$MPMResidual)>100,], col = 'grey', pch = 15, cex = 3)
points(MPMResidual~all_returns_above_ht_div_Total_first_returns_x_100, dataall[abs(dataall$STBIOMSha)>400,], col = 'red', pch = dataall$pch[abs(dataall$STBIOMSha)>400], cex = 3)
points(MPMResidual~all_returns_above_ht_div_Total_first_returns_x_100, dataall[abs(dataall$STBIOMSha)>400 & abs(dataall$STBIOMSha)>400,], col = 'purple', pch = dataall$pch[abs(dataall$STBIOMSha)>400], cex = 3)

points(MPMResidual~all_returns_above_ht_div_Total_first_returns_x_100, dataall[dataall$STBIOMSha>400 & dataall$Elev_P60< 25, ], col = 'green', pch = dataall$pch[dataall$STBIOMSha>400 & dataall$Elev_P60< 25], cex = 3)
points(MPMResidual~all_returns_above_ht_div_Total_first_returns_x_100, dataall[dataall$STBIOMSha>400 & dataall$Elev_P60< 20, ], col = 'orange', pch = dataall$pch[dataall$STBIOMSha>400 & dataall$Elev_P60< 20], cex = 3)
points(MPMResidual~all_returns_above_ht_div_Total_first_returns_x_100, dataall[dataall$STBIOMSha>400 & dataall$Pct_all_returns_above_ht< 40,], col = 'blue', pch =18, cex = 3)


########################################################
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################

# Density plots
p <- ggplot(data.mod, aes(x=MPMResidual, colour=as.factor(PlotSizeha),fill=as.factor(PlotSizeha))) + geom_density(alpha = .3)

p <- ggplot(dataall, aes(x=STBIOMSha)) + geom_histogram(binwidth=30) +xlim(c(0,500))+
  geom_vline(aes(xintercept=med, group = PlotSizeha),   # Ignore NA values for mean
             color="orange", size=1)+
  geom_vline(aes(xintercept=mean, group = PlotSizeha),   # Ignore NA values for mean
             color="grey", size=1)

p + facet_wrap(~PlotSizeha, nrow = 6) + xlab('Residual (Mg/ha)')
p + facet_wrap(~Size, nrow = 6) + xlab('Field AGB (Mg/ha)')

shapiro.test(data.mod$MPMResidual)




