################################################################################
# Code to add sample weights to the data frame for plot vs lidar comparison

setwd('C:\\Users\\krtenneson\\Desktop\\lidarModel\\VersionControl\\lidar')
setwd('\\\\166.2.126.25\\rseat\\Programs\\Reimbursibles\\fy2016\\R3_lidar_equation_transferability\\Analysis\\VersionControl\\lidar')

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
# 4FRI Phase II lidar, Phase 2 ground plots, Sitgreaves NF

folder <- '.\\Data\\OriginalFiles\\4FRI\\OriginalDataFromMarkNigrelli\\4FRI_PhaseII_Sitgreaves_7_2017\\'

# New files came in and are stored here: '.\\Data\\OriginalFiles\\4FRI\\OriginalDataFromMarkNigrelli\\4FRI_PhaseII_Sitgreaves_UPDATED_July2017'

## field data from here: 'FVS_Out_4FRI_PhaseII_Sitgreaves_Cloudmetrics_Query.xlsx'
## I had to rename the file, as the original name was too long
### this one has repeat records, but that are blended because of a stand ID merge,
#SitgreavesPhase2 <- read.csv(paste0(folder, '4FRI_SitP2_cloud_FVS.csv'))

## plot size
SitP2_plotSize <- read.csv(paste0(folder, 'LidarPhas2_PlotSize_AddPlotIDNum.csv'))
head(SitP2_plotSize)

## Cloud metrics
SitP2_cloud <- read.csv(paste0(folder, '4FRI_SitP2_cloud.csv'))
head(SitP2_cloud)

## merge plot info and cloud metrics
SitP2v1<- merge(SitP2_plotSize, SitP2_cloud, by.x = 'StndPlotID', by.y = 'StndPlotID', all.y = T)
dim(SitP2_plotSize); dim(SitP2_cloud); dim(SitP2v1)
#SitP2[SitP2v1$StndPlotID == '031102LIDAR20128_0128',]

################################################################################
## load plot coords from here:
SitP2_coords <- read.csv(paste0(folder, '4FRICoords.csv'))
colnames(SitP2_coords)
## merge plot info, cloud metrics, and coordinates
SitP2v2<- merge(SitP2v1, SitP2_coords, by.x = 'StndPlotID', by.y = 'Plot', all.y = T)
dim(SitP2_coords); dim(SitP2v1); dim(SitP2v2)


################################################################################
## load field data from here:
SitP2_field <- read.csv(paste0(folder, '4FRI_SitP2_cloud_FVS.csv'))
colnames(SitP2_field)

## remove plot # 031102LIDAR20128_0128 -- no way to tell which field data to link it to.
SitP2v2 <- SitP2v2[SitP2v2$StndPlotID != '031102LIDAR20128_0128',]

## remove stand # 031102LIDAR20128_0001 -- no way to tell which lidar data to link it to.
SitP2_field <- SitP2_field[SitP2_field$StandID != '031102LIDAR20128_0001',]

## merge plot info, cloud metrics, and coordinates
SitP2v3<- merge(SitP2v2, SitP2_field, by.x = 'PlotID', by.y = 'StandID', all.x = T)
dim(SitP2_field); dim(SitP2v2); dim(SitP2v3)

################################################################################
## dominant species info
folder2 <- '.\\Data\\OriginalFiles\\VegComposition\\'
SitP2_spp <- read.table(paste0(folder2, 'unique_dom_types.txt'), sep = '\t', header = T)
colnames(SitP2_spp)

## merge species info
SitP2v4<- merge(SitP2v3, SitP2_spp, by.x = 'DOM_TYPE', by.y = 'DOM_TYPE', all.x = T)
dim(SitP2_spp); dim(SitP2v3); dim(SitP2v4)


################################################################################
## load fpc info here:

SitP2<-SitP2v4
################################################################################
################################################################################
## 4 FRI Phase I column names
################################################################################
################################################################################
dim(SitP2)
colnames(SitP2)
Site <- rep('fourFri, P2', times = length(SitP2[,1]))
Forest <- rep('Sitgreaves, P2', times = length(SitP2[,1]))
dbhThreshcm <- rep(12.75, times = length(SitP2[,1]))
PlotSizeAcres <- rep(0.1, times = length(SitP2[,1]))
PlotSizeAcres[SitP2$PlotSize.x == '1/5th Acre'] <- 0.2
RandomUniform <- runif( length(SitP2[,1]), 
                        min = 0, max = 1) 

fpc <- rep(1, times = length(SitP2[,1]))
ForestType <- rep('update', times = length(SitP2[,1]))

# Create new data base of missing plots with just the columns needed for the analysis.
SitP2Final<-cbind(
  SitP2[,c('StndPlotID', "X_albers", "Y_albers", "X_Coor", "Y_Coor")],
  
  Site, Forest, dbhThreshcm, PlotSizeAcres,  SitP2$PlotSize.x, RandomUniform, fpc,
  
  SitP2[,c("EL_Ranges", "CC_Ranges", "CC_x_HT")], 
  
  SitP2[ , c('DOM_TYPE', 'Dom_Type_Label', 'Secondary_Label', 'GUILD')],
  
  ForestType, #get from Kaibab

  SitP2[,c("TCUFT", "STBIOMSS",
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

head(SitP2Final)

colnamesData <- read.csv('./Data/NKaibab07192017.csv')
colnamesKey <- colnames(colnamesData)

colnames(SitP2Final) <- colnamesKey
head(SitP2Final)

################################################################################
# Sitgreaves (4FRI)

SitP2Final$Elev_Class[SitP2Final$Elev_Class == '10-Feb'] <- '2-10m'

## unique(SitP2Final$Elev_Class)
## 2-10m   10-15m  16-100m

##unique(SitP2Final$CC_Class)
##  10-23%   10-23% 24-39%  40-100%

# Calculate height strata
SitP2Final$fpc[SitP2Final$Elev_Class == '2-10m'] <- 7831
SitP2Final$fpc[SitP2Final$Elev_Class == '10-15m'] <- 7968
SitP2Final$fpc[SitP2Final$Elev_Class == '16-100m'] <- 8049
SitP2Final$fpc[SitP2Final$Elev_Class == '2-10m'   & SitP2Final$CC_Class == '24-39%'] <- 7998
SitP2Final$fpc[SitP2Final$Elev_Class == '10-15m'  & SitP2Final$CC_Class == '24-39%'] <- 8194
SitP2Final$fpc[SitP2Final$Elev_Class == '16-100m' & SitP2Final$CC_Class == '24-39%'] <- 8278
SitP2Final$fpc[SitP2Final$Elev_Class == '2-10m'   & SitP2Final$CC_Class == '40-100%'] <- 7808
SitP2Final$fpc[SitP2Final$Elev_Class == '10-15m'  & SitP2Final$CC_Class == '40-100%'] <- 8438
SitP2Final$fpc[SitP2Final$Elev_Class == '16-100m' & SitP2Final$CC_Class == '40-100%'] <- 8113

write.csv(SitP2Final, './Data/SitgreavesPhase2_09252017.csv', row.names = F)

