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
library(ggplot2)
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

data.mod$PlotSizeha <- signif((data.mod$PlotSizeAcres*0.404686), 1)
data.val$PlotSizeha <- signif((data.val$PlotSizeAcres*0.404686), 1)
data.val.ind$PlotSizeha <- signif((data.val.ind$PlotSizeAcres*0.404686), 1)

#######################################################
#######################################################
## merge data and run summary statistics
#######################################################
#######################################################
dataall<-rbind(data.val,data.val.ind, data.mod)

aggregate(dataall[, 'STBIOMSha'], list(dataall$PlotSizeha), mean)
aggregate(dataall[, 'STBIOMSha'], list(dataall$PlotSizeha), sd)


library(dplyr) # With dplyr for example
dataall <- dataall %>% group_by(PlotSizeha) %>%
  mutate(med = median(STBIOMSha))

dataall <- dataall %>% group_by(PlotSizeha) %>%
  mutate(mean = mean(STBIOMSha))

dataall$Size<-paste('plot size =', dataall$PlotSizeha, 'ha')
# Rerun ggplot line with yintercept = med
ggplot(dt, aes(x = as.factor(id), y = y)) +
  geom_boxplot() +
  facet_wrap(~ gr) +
  geom_hline(aes(yintercept = med, group = gr), colour = 'red')

# Density plots
p <- ggplot(dataall, aes(x=STBIOMSha, colour=as.factor(PlotSizeha),fill=as.factor(PlotSizeha))) + geom_density(alpha = .3)
p <- ggplot(dataall, aes(x=STBIOMSha)) + geom_histogram(binwidth=30) +xlim(c(0,500))+
  geom_vline(aes(xintercept=med, group = PlotSizeha),   # Ignore NA values for mean
             color="orange", size=1)+
  geom_vline(aes(xintercept=mean, group = PlotSizeha),   # Ignore NA values for mean
             color="red", size=1)

p + facet_wrap(~PlotSizeha, nrow = 6) + xlab('Field AGB (Mg/ha)')
p + facet_wrap(~Size, nrow = 6) + xlab('Field AGB (Mg/ha)')


aggregate(dataall[, 'MPMlnlnResidual'], list(dataall$PlotSizeha), mean)
aggregate(dataall[, 'MPMlnlnResidual'], list(dataall$PlotSizeha), sd)
#######################################################
#######################################################
dataall$PlotSizeha = factor(dataall$PlotSizeha,c(0.008,0.01, 0.02, 0.03, 0.04,0.08))

dataall$PlotSizeha = factor(dataall$PlotSizeha,c(0.08,0.04, 0.03, 0.02, 0.01,0.008))
boxplot(dataall$MPMlnlnResidual ~ dataall$PlotSizeha, notch = T, ylim = c(-320,320), ylab = "Plot Size (ha)", 
        xlab = "Field AGB - Lidar AGB (Mg/ha)", varwidth = T, horizontal=T)

abline(v = seq(from = -300, to = 300, by = 100), col = 'gray')
abline(v= 0)

boxplot(dataall$MPMlnlnResidual ~ dataall$PlotSizeha, notch = T, add = T, col = 'light gray', varwidth = T, horizontal = T)

percent<-signif(c(13, 322, 1122,329,1412,112)/3310*100,3)


t.test(dataall$MPMEstimates, dataall$STBIOMSha, paired = T)
t.test(dataall$MPMEstimates[dataall$PlotSizeha == 0.008], dataall$STBIOMSha[dataall$PlotSizeha == 0.008], paired = T)
