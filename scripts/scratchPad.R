### Validate Predictions against Validation set. Can't include Forest, because new Forests! 
### Generate Predictions 
ValBiomass <- predict(object=PredModB, newdata=ValData.mod)
ValTCUFT <- predict(object=PredModT, newdata=ValData.mod)

### Compare to real data

# plots
qqplot(exp(ValBiomass), ValData.mod$STBIOMS)
qqplot(exp(ValTCUFT), ValData.mod$TCUFT)

qplot(exp(ValBiomass), ValData.mod$STBIOMS) +geom_smooth(method=lm, se=T)
qplot(exp(ValTCUFT), ValData.mod$TCUFT) +geom_smooth(method=lm, se=T)


# metrics, Root Mean Square Error, Mean Summed Error, Mean Bias Error
ValBioRMSPE <- rmspe(ValData.mod$STBIOMS, exp(ValBiomass), includeSE=T)
ValBioMBE <- sum((exp(ValBiomass) - ValData.mod$STBIOMS))/length(NewBiomass)

ValCufRMSPE <- rmspe(ValData.mod$TCUFT, exp(ValTCUFT), includeSE=T)
ValCufMBE <- sum((exp(ValTCUFT) - ValData.mod$TCUFT))/length(NewBiomass)

# Robust metrics, drops outliers from data to calculate metrics 
ValBioRMSPE.r <- rtmspe(ValData.mod$STBIOMS, exp(ValBiomass), includeSE=T)

ValCufRMSPE.r <- rtmspe(ValData.mod$TCUFT, exp(ValTCUFT), includeSE=T)

### Predictions from BAS object itself.

VBMABio <- predict(BioMass.Mod, ValData.mod, top='10000')

VBmaBioRMSPE <- rmspe(ValData.mod$STBIOMS, exp(VBMABio$fit), includeSE=T)
VBmaBioMBE <- sum((exp(VBMABio$fit) - ValData.mod$STBIOMS))/length(NewBiomass)


VBMAWood <- predict(TCUFT.Mod, ValData.mod, top='10000')

vBmaWoodRMSPE <- rmspe(ValData.mod$TCUFT, exp(VBMAWood$fit), includeSE=T)
VBmaWoodMBE <- sum((exp(VBMAWood$fit) - ValData.mod$TCUFT))/length(NewBiomass)


# ### Generate robust predictions
# ValBiomass.r <- predict(object=FinModB.r, newdata=ValData.mod)
# ValTCUFT.r <- predict(object=FinModT.r, newdata=ValData.mod)
# 
# ### Compare robust predictions to real data
# 
# # plots
# qqplot(exp(ValBiomass.r), ValData.mod$STBIOMS)
# qqplot(exp(ValTCUFT.r), ValData.mod$TCUFT)
# 
# qplot(exp(ValBiomass.r), ValData.mod$STBIOMS) +geom_smooth(method=lm, se=T)
# qplot(exp(ValTCUFT.r), ValData.mod$TCUFT) +geom_smooth(method=lm, se=T)
# 
# # metrics, Root Mean Square Error, Mean Summed Error, Mean Bias Error
# ValBioRMSPE.rob <- rmspe(ValData.mod$STBIOMS, exp(ValBiomass.r), includeSE=T)
# ValBioMBE.rob <- sum((exp(ValBiomass.r) - ValData.mod$STBIOMS))/length(NewBiomass)
# 
# ValCufRMSPE.rob <- rmspe(ValData.mod$TCUFT, exp(ValTCUFT.r), includeSE=T)
# ValCufMBE.rob <- sum((exp(ValTCUFT.r) - ValData.mod$TCUFT))/length(NewBiomass)