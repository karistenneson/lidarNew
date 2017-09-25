################################################################################
# Code to add sample weights to the data frame for plot vs lidar comparison

setwd('C:\\Users\\krtenneson\\Desktop\\lidarModel\\VersionControl\\lidar')

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

# New files came in and are stored here: '.\\Data\\OriginalFiles\\4FRI\\OriginalDataFromMarkNigrelli\\4FRI_PhaseII_Sitgreaves_UPDATED_July2017'
## field data from here: 'FVS_Out_4FRI_PhaseII_Sitgreaves_Cloudmetrics_Query.xlsx'
## I had to rename the file, as the original name was too long
SitgreavesPhase2 <- read.csv('.\\Data\\OriginalFiles\\4FRI\\OriginalDataFromMarkNigrelli\\4FRI_PhaseII_Sitgreaves_UPDATED_July2017\\4FRI_PhaseII_Sitgreaves_UPDATED_July2017\\FVS_4FRI_PhaseII.csv')

## load fpc info here:

################################################################################
# 4FRI Phase II lidar, Phase 3 ground plots, Apache NF
# New files came in and are stored here: '.\\Data\\OriginalFiles\\4FRI\\OriginalDataFromMarkNigrelli\\4FRI_PhaseII_Apache'
## get field based metrics from here:
#ApachePhase3Field <- read.csv('.\\Data\\4FRI\\OriginalDataFromMarkNigrelli\\PhaseIII_Apache_FromAnita\\coming soon')

#Plot_size<-read.xlsx('.\\Data\\4FRI\\OriginalDataFromMarkNigrelli\\PhaseIII_Apache_FromAnita\\LidarPhase3_CorrectedShapefiles_Merge.dbf')

## load in Phase 2 x and y coordinates
#XYPhase3_4FR<-read.dbf(paste0(root, '4FRI\\OriginalDataFromMarkNigrelli\\PhaseIII_Apache_FromAnita\\Apache2015Lidar_PlotSize.xlsx'))
#dim(XYPhase3_4FR)
#colnames(XYPhase3_4FR)

## load fpc info here:


################################################################################
################################################################################
## 4 FRI Phase I column names
################################################################################
################################################################################

colnames(SitgreavesPhase2)
Site <- rep('fourFri, P2', times = length(SitgreavesPhase2[,1]))
Forest <- rep('Sitgreaves, P2', times = length(SitgreavesPhase2[,1]))
dbhThreshcm <- rep(12.75, times = length(SitgreavesPhase2[,1]))
PlotSizeAcres <- rep(0.1, times = length(SitgreavesPhase2[,1]))
PlotSizeAcres[SitgreavesPhase2$PlotSize == '1/5th Acre'] <- 0.2
fpc  <- 

# Create new data base of missing plots with just the columns needed for the analysis.

SitP2<-cbind(
  data[,c('PLOT_ID',
          "X_albers", "Y_albers", "X_UTM", "Y_UTM")],
  Site, Forest, dbhThreshcm, 
  PlotSizeAcres,  SitgreavesPhase2$PlotSize, 
  "fpc",
  'RandomUniform',
  "Elev_Class", "CC_Class", "Stratum", 
  SitgreavesPhase2$DOM_TYPE, Dom_Type_Label, Secondary_Label,
  "LifeForm", "ForestType",
  "TCUFT",    "STBIOMSS",
  "Elev.mean",
  "Elev.mode", "Elev.stddev", "Elev.variance", "Elev.CV", 
                       "Elev.IQ", "Elev.skewness", "Elev.kurtosis", 
                       ##
                       "Elev.AAD", "Elev.MAD.median", 
                       "Elev.MAD.mode", "Elev.L2", "Elev.L3", "Elev.L4", 
                       "Elev.L.CV", "Elev.L.skewness", "Elev.L.kurtosis", 
                       'Elev.P01', 'Elev.P05', 'Elev.P10', 'Elev.P20', 'Elev.P25', 
                       'Elev.P30', 'Elev.P40', 'Elev.P50', 'Elev.P60', 'Elev.P70', 
                       'Elev.P75', 'Elev.P80', 'Elev.P90', 'Elev.P95', "Elev.P99", 
                       
                       #"first_cover_3" =, 
                       'First.returns.above.300',
                       
                       #"all_cover_3" =,
                       'All.returns.above.300',
                       
                       #"all_1st_cover_3" =,
                       'X.All.returns.above.300.....Total.first.returns....100',
                       
                       #"first_cover_mean" =
                       "First.returns.above.mean",
                       
                       #"first_cover_mode" =,
                       "First.returns.above.mode",
                       
                       #"all_cover_mean" =, "all_cover_mode" =,
                       'All.returns.above.mean', 'All.returns.above.mode', 
                       
                       #"all_1st_cover_mean" =,
                       "X.All.returns.above.mean.....Total.first.returns....100",
                       
                       #"all_1st_cover_mode" =
                       "X.All.returns.above.mode.....Total.first.returns....100")]

