################################################################################
# Code to add sample weights to the data frame for plot vs lidar comparison

setwd('C:\\Users\\krtenneson\\Desktop\\lidarModel\\VersionControl\\lidarNew')
setwd('\\\\166.2.126.25\\rseat\\Programs\\Reimbursibles\\fy2016\\R3_lidar_equation_transferability\\Analysis\\VersionControl\\lidarNew')

# Load libraries
library(plyr)
library(foreign)

# Read in the Kim's data
# df <- read.csv('./Data/OriginalFiles/fromKim/fpc_forest_mix_modeling_set_3sites_CONUS_AlbersXY_dummyVars_guild_p_KT.csv')

###################################################################
# 4 Forests Restoration Initiative (4FRI) 
################################################################################

## Load in full data set from Mark N.
Full4FR<-read.csv('./Data/OriginalFiles/4FRI/OriginalDataFromMarkNigrelli/AllFVSOut_CloudMetrics_Query_wYear.csv')
Full4FR$PLOT_ID<- substr(Full4FR$StandID_Year, 1, 
                         (nchar(as.character(Full4FR$StandID_Year))-5))

## load in Phase 1 x and y coordinates
XYPhase1_4FR<-read.dbf('./Data/OriginalFiles/4FRI\\OriginalDataFromMarkNigrelli\\Phase I Aquisition AllLidarPlots\\AllLidarPlots.dbf')
head(XYPhase1_4FR)

dim(Full4FR)
dim(XYPhase1_4FR)
## We're missing 25 plots when compared to the shapefile. 
## These plots didn't intersect the lidar footprint.

## Merge with 4 FRI
merged4fri <- merge(Full4FR, XYPhase1_4FR[, c('SETTING_ID','X_UTM','Y_UTM','X_albers','Y_albers')], 
                    by.x = 'PLOT_ID', by.y = 'SETTING_ID', all.x = T)

# write.csv(merged4fri, 'archive//test4FRI.csv', row.names = F)

# col 1: 5
all_4FRI <- data.frame(PLOT_ID = merged4fri$PLOT_ID, X_albers = merged4fri$X_albers)
#all_4FRI$PLOT_ID <- nk$FxdPlotID
#all_4FRI$X_albers <- nk$X_Albers 
all_4FRI$Y_albers <- merged4fri$Y_albers 
all_4FRI$X_UTM <- merged4fri$X_UTM 
all_4FRI$Y_UTM <- merged4fri$Y_UTM 
head(all_4FRI)

# col 6:10
all_4FRI$Site <-'fourFri, P1'

all_4FRI$Forest <- 'update'
all_4FRI$Forest[(substr(merged4fri$PLOT_ID,1,4)=="0311")] <- 'Sitgreaves'
all_4FRI$Forest[(substr(merged4fri$PLOT_ID,1,4)=="0312")] <- 'Tonto'
all_4FRI$Forest[(substr(merged4fri$PLOT_ID,1,4)=="0304")] <- 'Coconino'

all_4FRI$dbhThreshcm <- 12.7

all_4FRI$PlotSizeAcres <- 0.1
all_4FRI$PlotSize <- merged4fri$PlotSize
all_4FRI$PlotSizeAcres[all_4FRI$PlotSize=='1/5 Acre'] <- 0.2
all_4FRI$PlotSizeAcres[all_4FRI$PlotSize=='1/15 Acre'] <- 0.0666667
all_4FRI$PlotSizeAcres[all_4FRI$PlotSize=='1/20 Acre'] <- 0.05
all_4FRI$PlotSizeAcres[all_4FRI$PlotSize=='1/30 Acre'] <- 0.0333333
all_4FRI$PlotSizeAcres[all_4FRI$PlotSize=='1/50 Acre'] <- 0.02

# columns 11:15
all_4FRI$RandomUniform <- runif( length(all_4FRI[,1]), 
                                 min = 0, max = 1) 
all_4FRI$fpc <- 0

all_4FRI$Elev_Class <- 999
all_4FRI$CC_Class <- 999
all_4FRI$Stratum <- 999

# columns 16:20
all_4FRI$DOM_TYPE <- merged4fri$DOM_TYPE
all_4FRI$Dom_Type_Label <- 'update'
all_4FRI$Secondary_Label <- 'update'
all_4FRI$LifeForm <- 'update'
all_4FRI$ForestType <- 'update'

#colnames(df[,21:25])
all_4FRI$TCUFT <- merged4fri$TCUFT
all_4FRI$STBIOMS <- merged4fri$STBIOMSS
all_4FRI$Elev_ave <- merged4fri$Elev.mean
all_4FRI$Elev_mode <- merged4fri$Elev.mode
all_4FRI$Elev_stddev <- merged4fri$Elev.stddev

#colnames(df[,26:30])
all_4FRI$Elev_variance <- merged4fri$Elev.variance
all_4FRI$Elev_CV <- merged4fri$Elev.CV
all_4FRI$Elev_IQ <- merged4fri$Elev.IQ
all_4FRI$Elev_skewness <- merged4fri$Elev.skewness
all_4FRI$Elev_kurtosis <- merged4fri$Elev.kurtosis

#colnames(df[,31:35])
all_4FRI$Elev_AAD <- merged4fri$Elev.AAD
all_4FRI$Elev_MAD_median <- merged4fri$Elev.MAD.median
all_4FRI$Elev_MAD_mode <- merged4fri$Elev.MAD.mode
all_4FRI$Elev_L2 <- merged4fri$Elev.L2 
all_4FRI$Elev_L3 <- merged4fri$Elev.L3

#colnames(df[,36:40])
all_4FRI$Elev_L4 <- merged4fri$Elev.L4
all_4FRI$Elev_LCV <- merged4fri$Elev.L.CV
all_4FRI$Elev_Lskewness <- merged4fri$Elev.L.skewness
all_4FRI$Elev_Lkurtosis <- merged4fri$Elev.L.kurtosis
all_4FRI$Elev_P01 <- merged4fri$Elev.P01

#colnames(df[,41:45])
all_4FRI$Elev_P05 <- merged4fri$Elev.P05
all_4FRI$Elev_P10 <- merged4fri$Elev.P10
all_4FRI$Elev_P20 <- merged4fri$Elev.P20
all_4FRI$Elev_P25 <- merged4fri$Elev.P25
all_4FRI$Elev_P30 <- merged4fri$Elev.P30

#colnames(df[,46:50])
all_4FRI$Elev_P40 <- merged4fri$Elev.P40
all_4FRI$Elev_P50 <- merged4fri$Elev.P50
all_4FRI$Elev_P60 <- merged4fri$Elev.P60
all_4FRI$Elev_P70 <- merged4fri$Elev.P70
all_4FRI$Elev_P75 <- merged4fri$Elev.P75

#colnames(df[,51:55])
all_4FRI$Elev_P80 <- merged4fri$Elev.P80
all_4FRI$Elev_P90 <- merged4fri$Elev.P90
all_4FRI$Elev_P95 <- merged4fri$Elev.P95
all_4FRI$Elev_P99 <- merged4fri$Elev.P99


## add in all density columns, in order of FUSION output
all_4FRI$Pct_first_returns_above_ht <- merged4fri$Percentage.first.returns.above.300
all_4FRI$Pct_all_returns_above_ht <- merged4fri$Percentage.all.returns.above.300
all_4FRI$all_returns_above_ht_div_Total_first_returns_x_100 <- 
  merged4fri$X.All.returns.above.300.....Total.first.returns....100

all_4FRI$First_returns_above_ht <- merged4fri$First.returns.above.300
all_4FRI$All_returns_above_ht <- merged4fri$All.returns.above.300

all_4FRI$Pct_first_returns_above_mean <- merged4fri$Percentage.first.returns.above.mean
all_4FRI$Pct_first_returns_above_mode <- merged4fri$Percentage.first.returns.above.mode
all_4FRI$pct_all_returns_above_mean <- merged4fri$Percentage.all.returns.above.mean
all_4FRI$pct_all_returns_above_mode <- merged4fri$Percentage.all.returns.above.mode

all_4FRI$All_returns_above_mean_div_Total_first_returns_x_100 <- 
  merged4fri$X.All.returns.above.mean.....Total.first.returns....100
all_4FRI$All_returns_above_mode_div_Total_first_returns_x_100 <- 
  merged4fri$X.All.returns.above.mode.....Total.first.returns....100

all_4FRI$First_returns_above_mean <- merged4fri$First.returns.above.mean
all_4FRI$First_returns_above_mode <- merged4fri$First.returns.above.mode
all_4FRI$All_returns_above_mean <- merged4fri$All.returns.above.mean
all_4FRI$All_returns_above_mode <- merged4fri$All.returns.above.mode
all_4FRI$Total_first_returns <- merged4fri$Total.first.returns
all_4FRI$Total_all_returns <- merged4fri$Total.all.returns

head(all_4FRI[1:2,])

# Create new data base of missing plots with just the columns needed for the analysis.
################################################################################
# Sitgreaves (4FRI)

sitgreaves <- all_4FRI[substr(all_4FRI$PLOT_ID,1,4)=="0311",]

# Calculate height strata
sitgreaves$Elev_Class <- ifelse(sitgreaves$Elev_P95 >= 2 & sitgreaves$Elev_P95 < 10,1,
                                ifelse(sitgreaves$Elev_P95 >= 10 & sitgreaves$Elev_P95 < 16,2,3))

# Calculate density strata
sitgreaves$CC_Class <- ifelse(sitgreaves$Pct_first_returns_above_ht < 24,1, 
                              ifelse(sitgreaves$Pct_first_returns_above_ht >= 24 & sitgreaves$Pct_first_returns_above_ht < 40,2,3))

# Map combined 4 x 4 strata to area
sitgreaves$Stratum <- (sitgreaves$Elev_Class * 10) + sitgreaves$CC_Class

sitgreaves$fpc <- mapvalues(sitgreaves$Stratum,
                            from = c(11,21,31,12,22,32,13,23,33),
                            to = c(7831,7968,8049,7998,8194,8278,7808,8438,8113))
head(sitgreaves)

write.csv(sitgreaves, './Data/Sitgreaves07162017.csv', row.names = F)

################################################################################
# Coconino (4FRI)

coconino <- all_4FRI[substr(all_4FRI$PLOT_ID,1,4)=="0304",]
head(coconino)
dim(coconino)

# Read in the supplemental data for the Coconino and Tonto
df_sup_C <- read.csv('./Data/OriginalFiles/SampleWeights/Coconino_Stands.csv')
df_sup_C$NumOfPlots <- NULL
df_sup_C$STAND_ID <- as.character(df_sup_C$STAND_ID)
colnames(df_sup_C)[2] <- 'fpc'

# Convert Coconino supplement Stand IDs to Plot ID prefixes
l_C <- nrow(df_sup_C)
# Add a '0' before every Stand ID
df_sup_C$Prefix <- paste0(rep('0',l_C),df_sup_C$STAND_ID)
# Separate the Stand IDs that have length 16 from those that have length 15
df_sup_C_16 <- df_sup_C[nchar(df_sup_C$Prefix)==16,]
df_sup_C_15 <- df_sup_C[nchar(df_sup_C$Prefix)==15,]
# For the Stand IDs of length 15, add a '0' in the 3rd to last position
# e.g. ...817 -> ...8[0]17  = ...8017
l_C_15 <- nrow(df_sup_C_15)
df_sup_C_15$Prefix <- paste0(substr(df_sup_C_15$Prefix,1,13),rep('0',l_C_15),substr(df_sup_C_15$Prefix,14,15))
# Recombine the supplement data frame
df_sup_C <- rbind(df_sup_C_15,df_sup_C_16)

# Extract Prefixes from Plot IDs
coconino$Prefix <- unname(t(data.frame(strsplit(as.character(coconino$PLOT_ID),'_')))[,1])

dim(coconino)
# Join stand areas to plots by using the Prefix
coconino <- merge(x = coconino, y = df_sup_C, by = 'Prefix')
dim(coconino)
head(coconino)
## "Elev_Class"            "CC_Class"              "Stratum" 

# Remove prefix and stand columns
coconino$fpc.x <- coconino$fpc.y
coconino$fpc.y <- coconino$Prefix <- NULL
coconino$Stratum<-coconino$STAND_ID 
coconino$STAND_ID <- NULL
head(coconino)
dim(sitgreaves)

colnames(coconino)[12]<- 'fpc'

write.csv(coconino, './/Data//Coconino07162017.csv', row.names = F)

################################################################################
# Tonto (4FRI)
df_sup_T <- read.csv('./Data/OriginalFiles/SampleWeights/Tonto_Stands.csv')
df_sup_T$NumOfPlots <- NULL
df_sup_T$STAND_ID <- as.character(df_sup_T$STAND_ID)
colnames(df_sup_T)[2] <- 'fpc'

head(df_sup_T)

tonto <- all_4FRI[which(substr(all_4FRI$PLOT_ID,1,4)=="0312"),]
head(tonto)
dim(tonto)

# Convert Tonto supplement Stand IDs to Plot ID prefixes
l_T <- nrow(df_sup_T)
# Add a '0' before every Stand ID
df_sup_T$Prefix <- paste0(rep('0',l_T),df_sup_T$STAND_ID)
# Separate the Stand IDs that have length 16 from those that have length 15
df_sup_T_16 <- df_sup_T[nchar(df_sup_T$Prefix)==16,]
df_sup_T_15 <- df_sup_T[nchar(df_sup_T$Prefix)==15,]
# For the Stand IDs of length 15, add a '0' in the 3rd to last position
# e.g. ...817 -> ...8[0]17  = ...8017
l_T_15 <- nrow(df_sup_T_15)
df_sup_T_15$Prefix <- paste0(substr(df_sup_T_15$Prefix,1,13),rep('0',l_T_15),substr(df_sup_T_15$Prefix,14,15))
# Recombine the supplement data frame
df_sup_T <- rbind(df_sup_T_15,df_sup_T_16)

# Extract Prefixes from Plot IDs
tonto$Prefix <- unname(t(data.frame(strsplit(as.character(tonto$PLOT_ID),'_')))[,1])

# Join stand areas to plots by using the Prefix
tonto <- merge(x = tonto, y = df_sup_T, by = 'Prefix')

# Remove prefix and stand columns
tonto$fpc.x <- tonto$fpc.y
tonto$fpc.y <- NULL
colnames(tonto)[13]<-'fpc'
tonto$Prefix <- NULL
tonto$Stratum<-tonto$STAND_ID 
tonto$STAND_ID <- NULL
head(tonto)
colnames(tonto)

write.csv(tonto, './/Data//Tonto07162017.csv', row.names = F)

