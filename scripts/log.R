# log
log.vsurf <- VSURF(

DATA.mod[ , c('Elev_mode', 'Elev_P01', 'Elev_P10', 'Elev_P30', 'Elev_P60', 'Elev_P90', 'Elev_stddev', ' Elev_kurtosis', 'Elev_MAD_median', 'Elev_MAD_mode', 'Elev_L3', 'Elev_L4', 'Elev_LCV', 'Elev_Lskewness', 'Elev_Lkurtosis', 'Pct_all_returns_above_ht', 'all_returns_above_ht_div_Total_first_returns_x_100', 'pct_all_returns_above_mean', 'All_returns_above_mode_div_Total_first_returns_x_100', 'mode_pctAllOver3m', 'mode_pctAlloverModeDiv1st', 'P01_pctAllOver3m', 'P10_pctAllOver3m', 'P30_pctAllOver3m', 'P60_pctAllOver3m', 'P90_pctAllOver3m', 'elevation', 'aspect', 'slope', 'NDVI_Amp', 'R3ERUlabel)]

, DATA.mod$logSTBIOMSha)

plot(log.vsurf)


# log
BioMass.Mod.log <- bas.lm(logSTBIOMSha ~ Elev_mode + Elev_P01 + Elev_P10 + Elev_P30 + Elev_P60 + Elev_P90 +   
                            Elev_stddev +  Elev_kurtosis + Elev_MAD_median + Elev_MAD_mode + Elev_L3 + Elev_L4 + Elev_LCV + Elev_Lskewness + Elev_Lkurtosis + 
                            Pct_all_returns_above_ht + all_returns_above_ht_div_Total_first_returns_x_100 + pct_all_returns_above_mean + All_returns_above_mode_div_Total_first_returns_x_100 + 
                            mode_pctAllOver3m + mode_pctAlloverModeDiv1st + P01_pctAllOver3m + P10_pctAllOver3m + P30_pctAllOver3m + P60_pctAllOver3m + P90_pctAllOver3m +
                            elevation + aspect + slope + NDVI_Amp + R3ERUlabel, 
                          weights = DATA.mod$SmplWts, 
                          data = DATA.mod, 
                          prior="hyper-g",
                          alpha = 3,
                          modelprior=tr.poisson(10,30),
                          method="MCMC+BAS")
