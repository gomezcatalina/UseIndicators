require(zoo)

head(ESS_Q)
ESS_Q_filt <-  ESS_Q_filtered[,c('FPInvertebrates.L','BiomassClupeids')]


Cols <- Cols[! Cols %in% c('InverseFishingPressure.L', 'InverseFishingPressure.L_s', 
                           'InverseFPClupeids.L', 'InverseFPClupeids.L_s', 
                           'InverseFPFinfish.L', 'InverseFPFinfish.L_s', 
                           'InverseFPFlatfish.L', 'Avoidance', 'Behavior2', 'Acoustic_change', 'RL_unit', 'Captive', 'codebeh','alfabeh', 'revcodebeh')]

  data <- Strata_Q_473[,c('YEAR','BiomassClupeids')]
head(data)
plot(data$YEAR, data$BiomassClupeids, main = "Raw")
na.locf(data)
plot(na.approx(data)) 

  plot(data$YEAR, data$BiomassClupeids, main = "Raw")

# Tear lost when temoving NA's
library(plyr)

Strata_Q_440_MissingYears

Strata_Q_473_MissingYears <- Strata_Q_473[,c('YEAR',
                                     'ID',
                                     'BiomassClupeids',
                                     'BiomassFlatfish',
                                     'BiomassForage',
                                     'BiomassSkates',
                                     'BiomassTL2',
                                     'BPelagicToDemersal',
                                     'BTGLargeBenthivore',
                                     'BTGPlanktivore',
                                     'BTGZoopiscivore',
                                     'CCLargeBenthivore',
                                     'CCPlanktivore',
                                     'CCZoopiscivore')]

#?complete.cases
RemoveNAs_Strata_473 <- Strata_Q_MissingYears[complete.cases(Strata_Q_MissingYears),]
RemoveNAs_Strata_473 <- count(RemoveNAs_Strata_473, c('ID'))
RemoveNAs_Strata_473 
cX(RemoveNAs_Strata_473)


str(Strata_Q_MissingYears)
str(RemoveNAs_Strata_473)
