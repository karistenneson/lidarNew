data.mod <- read.csv('Data//datamodNoZero.csv')
data.mod <- data.mod[(data.mod$Elev_kurtosis < 50), ]

Tonto <- data.frame(data.mod[data.mod$Forest == 'Tonto',])
Tonto$StratumNum <- as.numeric(factor(Tonto$Stratum))

data.mod.svy <- svydesign(id=~StratumNum, fpc = ~fpc04StrataHaPlot, data = data.frame(Tonto))

head(apiclus2[,c(1:13,38:40)])
dim(apiclus2)
