dataall<-rbind(data.val,data.val.ind, data.mod)

aggregate(dataall[, 'MPMResidual'], list(dataall$PlotSizeha), mean)
aggregate(dataall[, 'MPMResidual'], list(dataall$PlotSizeha), sd)

boxplot(dataall[, 'MPMResidual'] ~ dataall$PlotSizeha, notch = T, ylim = c(-320,320), xlab = "Plot Size (ha)", 
        ylab = "Field AGB - Lidar AGB (Mg/ha)", varwidth = T)

abline(h = seq(from = -300, to = 300, by = 100), col = 'gray')
abline(h= 0)

boxplot(dataall[, 'MPMResidual'] ~ dataall$PlotSizeha, notch = T, add = T, col = 'light gray', varwidth = T)

percent<-signif(c(13, 322, 1122,329,1412,112)/3310*100,3)


t.test(dataall$MPMEstimates, dataall$STBIOMSha, paired = T)
t.test(dataall$MPMEstimates[dataall$PlotSizeha == 0.008], dataall$STBIOMSha[dataall$PlotSizeha == 0.008], paired = T)
