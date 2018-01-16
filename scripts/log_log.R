# log log
# log log
BioMass.Mod.loglog <- bas.lm(logSTBIOMSha ~   logmode + logP01 + logP10 + logP30 + logP60 + logP90 +
                               Elev_stddev +  Elev_kurtosis + Elev_MAD_median + Elev_MAD_mode + Elev_L3 + Elev_L4 + Elev_LCV + Elev_Lskewness + Elev_Lkurtosis + 
                               log_Pct_all_returns_above_ht + log_pct_all_returns_above_mean + log_all_returns_above_ht_div_Total_first_returns_x_100 + 
                               logmode_pctAllOver3m + logmode_pctAlloverModeDiv1st + logP01_pctAllOver3m + logP10_pctAllOver3m + logP30_pctAllOver3m + logP60_pctAllOver3m + logP90_pctAllOver3m + 
                               elevation + aspect + slope + NDVI_Amp + R3ERUlabel, 
                             weights = DATA.mod$SmplWts,
                             data = DATA.mod, 
                             prior="hyper-g",
                             alpha = 3,
                             modelprior=tr.poisson(10,30),
                             method="MCMC+BAS")
