################################################################################
# Code to add sample weights to the data frame for plot vs lidar comparison

setwd('C:\\Users\\krtenneson\\Desktop\\lidarModel\\VersionControl\\lidarNew')
setwd('\\\\166.2.126.25\\rseat\\Programs\\Reimbursibles\\fy2016\\R3_lidar_equation_transferability\\Analysis\\VersionControl\\lidarNew')

# Load libraries
library(plyr)
library(foreign)

# Read Kim's (processed) data
# df <- read.csv('./Data/OriginalFiles/fromKim/fpc_forest_mix_modeling_set_3sites_CONUS_AlbersXY_dummyVars_guild_p_KT.csv')

###################################################################
## update data by National Forest
###################################################################

################################################################################
# 4 Forests Restoration Initiative (4FRI) 

################################################################################
# 4FRI Phase II lidar, Apache ground plots

# New files came in and are stored here: '.\\Data\\OriginalFiles\\4FRI\\OriginalDataFromMarkNigrelli\\4FRI_PhaseII_Apache\\'
folder <- '.\\Data\\OriginalFiles\\4FRI\\OriginalDataFromMarkNigrelli\\4FRI_PhaseII_Apache\\'

## field metrics
Apache_field <- read.csv(paste0(folder, 'FVSOut_Cloudmetrics_Query.csv'))
head(Apache_field)

## cloud metrics
Apache_cloud <- read.csv(paste0(folder, '4FRI_PhaseII_Apache_cloudmetrics.csv'))
Apache_cloud$PLOT_ID <- substr(Apache_cloud$FileTitle, 10, nchar(as.character(Apache_cloud$FileTitle)))
head(Apache_cloud)


## merge plot info and cloud metrics
Apachev1<- merge(Apache_field, Apache_cloud, by.x = 'StandID', by.y = 'PLOT_ID', 
                 all.y = T)
dim(Apache_field); dim(Apache_cloud); dim(Apachev1)

################################################################################
## plot coords
Apache_coords <- read.csv(paste0(folder, 'LidarPhase3_shp.txt'))
head(Apache_coords)

## merge plot info and cloud metrics
Apachev2<- merge(Apachev1, Apache_coords, by.x = 'StandID', by.y = 'StndPltID', 
                 all.y = T)
dim(Apache_coords); dim(Apachev1); dim(Apachev2)
head(Apachev2)

################################################################################
## dominant species info
folder2 <- '.\\Data\\OriginalFiles\\VegComposition\\'
Apache_spp <- read.table(paste0(folder2, 'unique_dom_types.txt'), sep = '\t', header = T)
colnames(Apache_spp)

## merge species info
Apachev3<- merge(Apachev2, Apache_spp, by.x = 'DOM_TYPE', by.y = 'DOM_TYPE', all.x = T)
dim(Apache_spp); dim(Apachev2); dim(Apachev3)


################################################################################
Apache<-Apachev3

################################################################################
################################################################################
## 4 FRI Phase I column names
################################################################################
################################################################################
dim(Apache)
colnames(Apache)
Site <- rep('fourFri, P2', times = length(Apache[,1]))
Forest <- rep('Apache', times = length(Apache[,1]))
dbhThreshcm <- rep(12.75, times = length(Apache[,1]))
PlotSizeAcres <- rep(0.1, times = length(Apache[,1]))
PlotSizeAcres[Apache$PlotSize == '1/5th Acre'] <- 0.2
RandomUniform <- runif( length(Apache[,1]), 
                        min = 0, max = 1) 

fpc <- rep(1, times = length(Apache[,1]))
ForestType <- rep('update', times = length(Apache[,1]))

Elev_Class <- Apache$Int.P95
CC_Class   <- Apache$Percentage.first.returns.above.3.00
Stratum    <- rep(9999, times = length(Apache[,1]))

# Calculate height strata
fpc[CC_Class < 26 & Elev_Class < 9] <- 11406
fpc[CC_Class < 26 & Elev_Class >= 9 & Elev_Class < 18] <- 54410
fpc[CC_Class < 26 & Elev_Class >= 18] <- 53706

fpc[CC_Class >= 26 & CC_Class < 44 & Elev_Class < 9] <- 5425
fpc[CC_Class >= 26 & CC_Class < 44 & Elev_Class >= 9 & Elev_Class < 18] <- 54688
fpc[CC_Class >= 26 & CC_Class < 44 & Elev_Class >= 18] <- 88551

fpc[CC_Class >= 44 & Elev_Class < 9] <- 1926
fpc[CC_Class >= 44 & Elev_Class >= 9 & Elev_Class < 18] <- 44857
fpc[CC_Class >= 44 & Elev_Class >= 18] <- 105021

Dom_Type_Label
Secondary_Label
GUILD

# Create new data base of missing plots with just the columns needed for the analysis.
ApacheFinal<-cbind(
  Apache[,c('StandID', "X_albers", "Y_albers", "X_Coord", "Y_Coord")],
  
  Site, Forest, dbhThreshcm, PlotSizeAcres,  Apache$PlotSize, RandomUniform, fpc,
  
  Elev_Class, CC_Class, Stratum, 
  
  Apache$DOM_TYPE, Apache[,c('Dom_Type_Label', 'Secondary_Label', 'GUILD')],
  
  ForestType, #get from Kaibab

  Apache[,c("TCUFT", "STBIOMSS",
  "Elev.mean.x",
  "Elev.mode.x", "Elev.stddev.x", "Elev.variance.x", "Elev.CV.x",
  "Elev.IQ.x", "Elev.skewness.x", "Elev.kurtosis.x",
  ##
  "Elev.AAD.x", "Elev.MAD.median.x", "Elev.MAD.mode.x", 
  "Elev.L2.x", "Elev.L3.x", "Elev.L4.x", 
  "Elev.L.CV.x", "Elev.L.skewness.x", "Elev.L.kurtosis.x", 
  'Elev.P01.x', 'Elev.P05.x', 'Elev.P10.x', 'Elev.P20.x', 'Elev.P25.x', 
  'Elev.P30.x', 'Elev.P40.x', 'Elev.P50.x', 'Elev.P60.x', 'Elev.P70.x', 
  'Elev.P75.x', 'Elev.P80.x', 'Elev.P90.x', 'Elev.P95.x', "Elev.P99.x", 

  ####################################################
  'Percentage.first.returns.above.300',
  'Percentage.all.returns.above.300',
  'X.All.returns.above.300.....Total.first.returns....100',
  'First.returns.above.3.00',
  'All.returns.above.3.00',
  'Percentage.first.returns.above.mean.y',
  'Percentage.first.returns.above.mode.y',
  'Percentage.all.returns.above.mean.y',
  'Percentage.all.returns.above.mode.y',
  'X.All.returns.above.mean.....Total.first.returns....100.y',
  'X.All.returns.above.mode.....Total.first.returns....100.y',
  'First.returns.above.mean',
  'First.returns.above.mode',
  'All.returns.above.mean', 
  'All.returns.above.mode', 
  'Total.first.returns', 
  'Total.all.returns')]
)

head(ApacheFinal)

colnamesData <- read.csv('./Data/NKaibab07192017.csv')
colnamesKey <- colnames(colnamesData)

colnames(ApacheFinal) <- colnamesKey
head(ApacheFinal)

################################################################################

write.csv(ApacheFinal, './Data/Apache_09252017.csv', row.names = F)

