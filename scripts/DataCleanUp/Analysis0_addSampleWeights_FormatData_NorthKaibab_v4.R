################################################################################
# Code to add sample weights to the data frame for plot vs lidar comparison
# Written by Karis Tenneson (krtenneson@fs.fed.us) and Joshua Goldstein (joshuagoldstein@fs.fed.us)
# Last updated: July 2017

# Load libraries
library(plyr)
library(foreign)

# Read Kim's (processed) data
# df <- read.csv('./Data/OriginalFiles/fromKim/fpc_forest_mix_modeling_set_3sites_CONUS_AlbersXY_dummyVars_guild_p_KT.csv')

###################################################################
## North Kaibab (NK)
################################################################################

# Load data for field dom_type and albers coords.
nk<-read.dbf('.\\Data\\OriginalFiles\\Goshawk_Phase_2\\Data\\SampleDesign\\FieldSites_withAttributes.dbf')
head(nk)

# Get lidar and field data from Brent's csv
## from \\166.2.126.25\rseat\Programs\Reimbursibles\fy2016\Goshawk_Phase_2\Data\ModelTablesLaptop
nkLidar <- read.csv('.\\Data\\OriginalFiles\\Kaibab\\fromBrentMitchell\\NKMaster_scrubed_joined_10_31.csv')
head(nkLidar)

nkMerge <- merge(nk, nkLidar, by.x = 'FxdPlotID', by.y = 'Plot_ID', all.x = T)
dim(nkMerge)
#nkMerge[is.na(nkMerge)]<-0
tail(nkMerge)
head(nkMerge)

# col 1: 5
all_NKaibab <- data.frame(PLOT_ID = nkMerge$FxdPlotID, X_albers = nkMerge$X_Albers)
#all_NKaibab$PLOT_ID <- nk$FxdPlotID
#all_NKaibab$X_albers <- nk$X_Albers 
all_NKaibab$Y_albers <- nkMerge$Y_albers 
all_NKaibab$X_UTM <- nkMerge$Easting 
all_NKaibab$Y_UTM <- nkMerge$Northing 
head(all_NKaibab)

# col 6:10
all_NKaibab$Site <-'NorthKaibab'
all_NKaibab$Forest <- 'NorthKaibab'
all_NKaibab$dbhThreshcm <- 20.32 
all_NKaibab$PlotSizeAcres <- 0.1
all_NKaibab$PlotSize <- '1/10 Acre'

head(all_NKaibab)

# columns 11:15
all_NKaibab$RandomUniform <- runif( length(all_NKaibab[,1]), 
                                    min = 0, max = 1) 
all_NKaibab$fpc <- 0

# Calculate height strata
all_NKaibab$Elev_Class <- ifelse(nkMerge$P95.y < 2, 5,
                             ifelse(nkMerge$P95.y >= 2 & nkMerge$P95.y < 17, 1,
                                    ifelse(nkMerge$P95.y >= 17 & nkMerge$P95.y < 22, 2,
                                           ifelse(nkMerge$P95.y >= 22 & nkMerge$P95.y < 27, 3, 4))))

# Calculate density strata
all_NKaibab$CC_Class <- ifelse(nkMerge$all_ab_3.y < 29, 1,
                           ifelse(nkMerge$all_ab_3.y >= 29 & nkMerge$all_ab_3.y < 40, 2,
                                  ifelse(nkMerge$all_ab_3.y >= 40 & nkMerge$all_ab_3.y < 49, 3, 4)))

# Map combined 4 x 4 strata to area
all_NKaibab$Stratum <- (all_NKaibab$Elev_Class * 10) + all_NKaibab$CC_Class
all_NKaibab$fpc <- mapvalues(all_NKaibab$Stratum,
                         from = c(11,21,31,41,12,22,32,42,13,23,33,43,14,24,34,44),
                         to = c(2226,915,607,323,847,1004,796,524,705,1004,983,740,318,532,624,700))

all_NKaibab$Elev_Class[1:9] <- 0
all_NKaibab$CC_Class[1:9] <- 0
all_NKaibab$Stratum[1:9] <- 0
all_NKaibab$fpc[1:9] <- NA

# Check counts and total
freq_nkMerge <- count(all_NKaibab,'Stratum')
N_nk <- sum(freq_nkMerge$freq)

head(all_NKaibab)

# columns 16:20
all_NKaibab$DOM_TYPE <- nkMerge$Dominance_
all_NKaibab$Dom_Type_Label <- "udpate"
all_NKaibab$Secondary_Label <- "udpate"
all_NKaibab$LifeForm <- "udpate"
all_NKaibab$ForestType <- nkMerge$Forest_Typ

#colnames(df[,21:25])
all_NKaibab$TCUFT <- nkMerge$TCUFT.x
all_NKaibab$STBIOMS <- nkMerge$STBIOMSS.x
all_NKaibab$Elev_ave <- nkMerge$H_mean.y
all_NKaibab$Elev_mode <- nkMerge$H_mode.y
all_NKaibab$Elev_stddev <- nkMerge$H_std.y

#colnames(df[,26:30])
all_NKaibab$Elev_variance <- nkMerge$H_var.y
all_NKaibab$Elev_CV <- nkMerge$H_CV.y
all_NKaibab$Elev_IQ <- nkMerge$H_IQ.y
all_NKaibab$Elev_skewness <- nkMerge$H_skew.y
all_NKaibab$Elev_kurtosis <- nkMerge$H_kurt.y

#colnames(df[,31:35])
all_NKaibab$Elev_AAD <- nkMerge$H_AAD.y
all_NKaibab$Elev_MAD_median <- nkMerge$H_MAD_median
all_NKaibab$Elev_MAD_mode <- nkMerge$H_MAD_mode.y
all_NKaibab$Elev_L2 <- nkMerge$L2.y 
all_NKaibab$Elev_L3 <- nkMerge$L3.y

#colnames(df[,36:40])
all_NKaibab$Elev_L4 <- nkMerge$L4.y
all_NKaibab$Elev_LCV <- nkMerge$L_CV.y
all_NKaibab$Elev_Lskewness <- nkMerge$L_skew.y
all_NKaibab$Elev_Lkurtosis <- nkMerge$L_kurt.y
all_NKaibab$Elev_P01 <- nkMerge$P01.y

#colnames(df[,41:45])
all_NKaibab$Elev_P05 <- nkMerge$P01.y
all_NKaibab$Elev_P10 <- nkMerge$P10.y
all_NKaibab$Elev_P20 <- nkMerge$P20.y
all_NKaibab$Elev_P25 <- nkMerge$P25.y
all_NKaibab$Elev_P30 <- nkMerge$P30.y

#colnames(df[,46:50])
all_NKaibab$Elev_P40 <- nkMerge$P40.y
all_NKaibab$Elev_P50 <- nkMerge$P50.y
all_NKaibab$Elev_P60 <- nkMerge$P60.y
all_NKaibab$Elev_P70 <- nkMerge$P70.y
all_NKaibab$Elev_P75 <- nkMerge$P75.y

#colnames(df[,51:55])
all_NKaibab$Elev_P80 <- nkMerge$P80.y
all_NKaibab$Elev_P90 <- nkMerge$P90.y
all_NKaibab$Elev_P95 <- nkMerge$P95.y
all_NKaibab$Elev_P99 <- nkMerge$P99.y

## add in all density columns, in order of FUSION output
all_NKaibab$Pct_first_returns_above_ht <- nkMerge$first_ab_3.y
all_NKaibab$Pct_all_returns_above_ht <- nkMerge$all_ab_3.y
all_NKaibab$all_returns_above_ht_div_Total_first_returns_x_100 <- 
  nkMerge$All_ab_3_div_Tot_first

all_NKaibab$First_returns_above_ht <- NA
all_NKaibab$All_returns_above_ht <- NA

all_NKaibab$Pct_first_returns_above_mean <- nkMerge$first_ab_mean
all_NKaibab$Pct_first_returns_above_mode <- nkMerge$first_ab_mode
all_NKaibab$pct_all_returns_above_mean <- nkMerge$all_ab_mean
all_NKaibab$pct_all_returns_above_mode <- nkMerge$all_ab_mode

all_NKaibab$All_returns_above_mean_div_Total_first_returns_x_100 <- 
  nkMerge$All_ab_mean_div_Tot_first
all_NKaibab$All_returns_above_mode_div_Total_first_returns_x_100 <- 
  nkMerge$All_ab.mode_div_Tot_first

all_NKaibab$First_returns_above_mean <- NA
all_NKaibab$First_returns_above_mode <- NA
all_NKaibab$All_returns_above_mean <- NA
all_NKaibab$All_returns_above_mode <- NA
all_NKaibab$Total_first_returns <- NA
all_NKaibab$Total_all_returns <- NA


all_NKaibab[is.na(all_NKaibab)] <- 0
all_NKaibab$fpc[1:9] <- NA

tail(all_NKaibab)
dim(all_NKaibab)

write.csv(all_NKaibab, 'finalData//NKaibab07192017.csv', row.names = F)

################################################################################
