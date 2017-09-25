################################################################################
# Code to add sample weights to the data frame for plot vs lidar comparison
# Written by Karis Tenneson (krtenneson@fs.fed.us) and Joshua Goldstein (joshuagoldstein@fs.fed.us)
# Last updated: July 11, 2017

# Load libraries
library(plyr)
library(foreign)

# Read in Kim's data to get dominance type.
df <- read.csv('./Data/OriginalFiles/fromKim/fpc_forest_mix_modeling_set_3sites_CONUS_AlbersXY_dummyVars_guild_p_KT.csv')
head(df)
colnames(df)

# Read in the supplemental data for the SwJMZ
## I think this is the same database as that on line 55 below (df_sup_SWJM).
#df_sup_SWJM <- read.csv('SWJM_plots.txt')
# full dat set lives here
SWJM <- read.dbf('.\\Data\\OriginalFiles\\Projected_Albers\\SWJM_plots_CONUS_Albers.dbf')

## connect to spreadsheet to update Dom_Veg.
df_sup_SWJM<-merge(SWJM, 
            df[, c('PLOT_ID',"GUILD", "DOM_TYPE", "Dom_Type_Label", "Secondary_Label")], 
            by.x = 'Comment_2', by.y = 'PLOT_ID', all.x = T)

#write.csv(df_sup_SWJM, 'archive\\testSWJM.csv')
###################################################################
## Southwest Jemez (SWJM)
###################################################################


# col 1: 5
all_SWJM <- data.frame(PLOT_ID = df_sup_SWJM$Comment_2, 
                       X_albers = df_sup_SWJM$X_albers)
all_SWJM$Y_albers <- df_sup_SWJM$Y_albers 
all_SWJM$X_UTM <- df_sup_SWJM$Easting 
all_SWJM$Y_UTM <- df_sup_SWJM$Northing 
head(all_SWJM)

# col 6:10
all_SWJM$Site <-'SWJM'
all_SWJM$Forest <- 'SWJM'
all_SWJM$dbhThreshcm <- 12.7 
all_SWJM$PlotSizeAcres <- 0.1
all_SWJM$PlotSizeAcres[df_sup_SWJM$SampleExpF == 5] <- 0.2
all_SWJM$PlotSize <- '1/10 Acre'
all_SWJM$PlotSize[df_sup_SWJM$SampleExpF == 5] <- '1/5 Acre'

all_SWJM$RandomUniform <- runif( length(all_SWJM[,1]), 
                                 min = 0, max = 1) 

# columns 11:15
# fpc,	percent_first_cover_3,	Elev_Class,	CC_Class,	Stratum
all_SWJM$fpc <- 0

#########################################################
# Calculate height strata

all_SWJM$Elev_Class <- ifelse(df_sup_SWJM$Elev_P99 >= 2 & df_sup_SWJM$Elev_P99 < 14,1,
                          ifelse(df_sup_SWJM$Elev_P99 >= 14 & df_sup_SWJM$Elev_P99 < 18,2,
                                 ifelse(df_sup_SWJM$Elev_P99 >= 18 & df_sup_SWJM$Elev_P99 < 22,3,
                                        ifelse(df_sup_SWJM$Elev_P99 >= 22 & df_sup_SWJM$Elev_P99 < 26,4,5))))

all_SWJM$CC_Class2 <- df_sup_SWJM$Percenta_3

all_SWJM$CC_Class[all_SWJM$Elev_Class == 1] <- ifelse(all_SWJM$CC_Class2[all_SWJM$Elev_Class == 1] < 9,1,
                          ifelse(all_SWJM$CC_Class2[all_SWJM$Elev_Class == 1] >= 9 & all_SWJM$CC_Class2[all_SWJM$Elev_Class == 1] < 35,2,3))

all_SWJM$CC_Class[all_SWJM$Elev_Class == 2] <- ifelse(all_SWJM$CC_Class2[all_SWJM$Elev_Class == 2] < 15,1,
                          ifelse(all_SWJM$CC_Class2[all_SWJM$Elev_Class == 2] >= 15 & all_SWJM$CC_Class2[all_SWJM$Elev_Class == 2] < 41,2,3))

all_SWJM$CC_Class[all_SWJM$Elev_Class == 3] <- ifelse(all_SWJM$CC_Class2[all_SWJM$Elev_Class == 3] < 15,1,
                          ifelse(all_SWJM$CC_Class2[all_SWJM$Elev_Class == 3] >= 15 & all_SWJM$CC_Class2[all_SWJM$Elev_Class == 3] < 47,2,3))

all_SWJM$CC_Class[all_SWJM$Elev_Class == 4] <- ifelse(all_SWJM$CC_Class2[all_SWJM$Elev_Class == 4] < 17,1,
                          ifelse(all_SWJM$CC_Class2[all_SWJM$Elev_Class == 4] >= 17 & all_SWJM$CC_Class2[all_SWJM$Elev_Class == 4] < 53,2,3))

all_SWJM$CC_Class[all_SWJM$Elev_Class == 5] <- ifelse(all_SWJM$CC_Class2[all_SWJM$Elev_Class == 5] < 18,1,
                          ifelse(all_SWJM$CC_Class2[all_SWJM$Elev_Class == 5] >= 18 & all_SWJM$CC_Class2[all_SWJM$Elev_Class == 5] < 60,2,3))


# Map combined 5 x 3 strata to area
all_SWJM$Stratum <- (all_SWJM$Elev_Class * 10) + all_SWJM$CC_Class

all_SWJM$fpc <- mapvalues(all_SWJM$Stratum,
                      from = c(11,12,13,21,22,23,31,32,33,41,42,43,51,52,53),
                      to = c(1889,8385,1990,2594,10602,2132,2208,11887,2242,1852,8531,1836,2109,8516,2250))

# Check counts and total
freq_SWJM <- count(all_SWJM,'Stratum')
N_SWJM <- sum(freq_SWJM$freq)

all_SWJM$CC_Class2 <- NULL
#########################################################

# columns 16:20
all_SWJM$PLOT_ID<- as.character(all_SWJM$PLOT_ID)
all_SWJM$DOM_TYPE<- as.character(df_sup_SWJM$DOM_TYPE)
all_SWJM$DOM_TYPE[is.na(all_SWJM$DOM_TYPE)] <- 'update'
## Email from Tom Mellin (7/11/17) to look up dominant vegetation in missing plots. 
#3060-0620 is PINUS
all_SWJM$DOM_TYPE[all_SWJM$PLOT_ID =='3060-0620']<-'PINUS'
#3192-0600 is PIPO_POTR5
all_SWJM$DOM_TYPE[all_SWJM$PLOT_ID =='3192-0600']<-'PIPO_POTR5'
#3261-0521 is PINUS
all_SWJM$DOM_TYPE[all_SWJM$PLOT_ID =='3261-0521']<-'PINUS'
#3520-1523 is PINUS
all_SWJM$DOM_TYPE[all_SWJM$PLOT_ID =='3520-1523']<-'PINUS'
#3540-0910 is PINUS
all_SWJM$DOM_TYPE[all_SWJM$PLOT_ID =='3540-0910']<-'PINUS'

all_SWJM$Dom_Type_Label <- as.character(df_sup_SWJM$Dom_Type_Label)
all_SWJM$Dom_Type_Label[is.na(all_SWJM$Dom_Type_Label)] <- 'update'
all_SWJM$Secondary_Label <- as.character(df_sup_SWJM$Secondary_Label)
all_SWJM$Secondary_Label[is.na(all_SWJM$Secondary_Label)] <- 'update'
all_SWJM$LifeForm <- as.character(df_sup_SWJM$GUILD)
all_SWJM$LifeForm[is.na(all_SWJM$LifeForm)] <- 'update'
all_SWJM$ForestType <- 'update'

#colnames(df[ ,21:25])
all_SWJM$TCUFT <- df_sup_SWJM$TCUFT
all_SWJM$STBIOMS <- df_sup_SWJM$STBIOMSS
all_SWJM$Elev_ave <- df_sup_SWJM$Elev_mean
all_SWJM$Elev_mode <- df_sup_SWJM$Elev_mode
all_SWJM$Elev_stddev <- df_sup_SWJM$Elev_stdde

#colnames(df[ ,26:30])
all_SWJM$Elev_variance <- df_sup_SWJM$Elev_varia
all_SWJM$Elev_CV <- df_sup_SWJM$Elev_CV
all_SWJM$Elev_IQ <- df_sup_SWJM$Elev_IQ
all_SWJM$Elev_skewness <- df_sup_SWJM$Elev_skewn
all_SWJM$Elev_kurtosis <- df_sup_SWJM$Elev_kurto

#colnames(df[,31:35])
all_SWJM$Elev_AAD <- df_sup_SWJM$Elev_AAD
all_SWJM$Elev_MAD_median <- df_sup_SWJM$Elev_MAD_m
all_SWJM$Elev_MAD_mode <- df_sup_SWJM$Elev_MAD_1
all_SWJM$Elev_L2 <- df_sup_SWJM$Elev_L2
all_SWJM$Elev_L3 <- df_sup_SWJM$Elev_L3

#colnames(df[,36:40])
all_SWJM$Elev_L4 <- df_sup_SWJM$Elev_L4
all_SWJM$Elev_LCV <- df_sup_SWJM$Elev_L_CV
all_SWJM$Elev_Lskewness <- df_sup_SWJM$Elev_L_ske
all_SWJM$Elev_Lkurtosis <- df_sup_SWJM$Elev_L_kur
all_SWJM$Elev_P01 <- df_sup_SWJM$Elev_P01

#colnames(df[,41:45])
all_SWJM$Elev_P05 <- df_sup_SWJM$Elev_P05
all_SWJM$Elev_P10 <- df_sup_SWJM$Elev_P10
all_SWJM$Elev_P20 <- df_sup_SWJM$Elev_P20
all_SWJM$Elev_P25 <- df_sup_SWJM$Elev_P25
all_SWJM$Elev_P30 <- df_sup_SWJM$Elev_P30

#colnames(df[,46:50])
all_SWJM$Elev_P40 <- df_sup_SWJM$Elev_P40
all_SWJM$Elev_P50 <- df_sup_SWJM$Elev_P50
all_SWJM$Elev_P60 <- df_sup_SWJM$Elev_P60
all_SWJM$Elev_P70 <- df_sup_SWJM$Elev_P70
all_SWJM$Elev_P75 <- df_sup_SWJM$Elev_P75

#colnames(df[,51:55])
all_SWJM$Elev_P80 <- df_sup_SWJM$Elev_P80
all_SWJM$Elev_P90 <- df_sup_SWJM$Elev_P90
all_SWJM$Elev_P95 <- df_sup_SWJM$Elev_P95
all_SWJM$Elev_P99 <- df_sup_SWJM$Elev_P99

#################################################
## Karis, start here.
#################################################

## add in all density columns, in order of FUSION output
all_SWJM$Pct_first_returns_above_ht <- df_sup_SWJM$Percentage
all_SWJM$Pct_all_returns_above_ht <- df_sup_SWJM$Percenta_1

all_SWJM$all_returns_above_ht_div_Total_first_returns_x_100 <- 
  df_sup_SWJM$F_All_retu
all_SWJM$First_returns_above_ht <- df_sup_SWJM$First_retu
all_SWJM$All_returns_above_ht <- df_sup_SWJM$All_return

all_SWJM$Pct_first_returns_above_mean <- df_sup_SWJM$Percenta_2
all_SWJM$Pct_first_returns_above_mode <- df_sup_SWJM$Percenta_3
all_SWJM$pct_all_returns_above_mean <- df_sup_SWJM$Percenta_4
all_SWJM$pct_all_returns_above_mode <- df_sup_SWJM$Percenta_5

all_SWJM$All_returns_above_mean_div_Total_first_returns_x_100 <- 
  df_sup_SWJM$F_All_re_1
all_SWJM$All_returns_above_mode_div_Total_first_returns_x_100 <- 
  df_sup_SWJM$F_All_re_2
all_SWJM$First_returns_above_mean <- df_sup_SWJM$First_re_1
all_SWJM$First_returns_above_mode <- df_sup_SWJM$First_re_2
all_SWJM$All_returns_above_mean <- df_sup_SWJM$All_retu_1
all_SWJM$All_returns_above_mode <- df_sup_SWJM$All_retu_2
all_SWJM$Total_first_returns <- df_sup_SWJM$Total_firs
all_SWJM$Total_all_returns <- df_sup_SWJM$Total_all


head(all_SWJM)
write.csv(all_SWJM, 'finalData//SWJM07162017.csv', row.names = F)
