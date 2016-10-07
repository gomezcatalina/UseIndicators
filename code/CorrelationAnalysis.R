library(Hmisc)

##**************************************** Large scale ***********************************************************#
#This uses the dataset extracted in 2014 

SHELF <- read.csv("file:///D:/CatalinaSync/EcosystemIndicators/SPERA_Indicators_2014/3) Hierarchical cluster analysis/rSpearman/SHELF.csv",header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
ESS <- read.csv("file:///D:/CatalinaSync/EcosystemIndicators/SPERA_Indicators_2014/3) Hierarchical cluster analysis/rSpearman/ESS.csv",header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
WSS <- read.csv("file:///D:/CatalinaSync/EcosystemIndicators/SPERA_Indicators_2014/3) Hierarchical cluster analysis/rSpearman/WSS.csv",header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)  
ESS_4VN <- read.csv("file:///D:/CatalinaSync/EcosystemIndicators/SPERA_Indicators_2014/3) Hierarchical cluster analysis/rSpearman/ESS_4VN.csv",header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T) 
ESS_4VS <- read.csv("file:///D:/CatalinaSync/EcosystemIndicators/SPERA_Indicators_2014/3) Hierarchical cluster analysis/rSpearman/ESS_4VS.csv",header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
ESS_4W <- read.csv("file:///D:/CatalinaSync/EcosystemIndicators/SPERA_Indicators_2014/3) Hierarchical cluster analysis/rSpearman/ESS_4W.csv",header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T) 
WSS_4X <- read.csv("file:///D:/CatalinaSync/EcosystemIndicators/SPERA_Indicators_2014/3) Hierarchical cluster analysis/rSpearman/WSS_4X.csv",header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T) 
#WSS_4X == WSS

#plot(SHELF$LargeFishIndicator, SHELF$MeanLengthAbundance)

SHELF_corr <- rcorr(as.matrix(SHELF,type="spearman"))
SHELF_corr2 <- flattenCorrMatrix(SHELF_corr$r, SHELF_corr$P)
SHELF_corr2$ID  <- "SHELF"


library(pvclust)
pdf("/outputs/Cluster analysis SS (nboot=10000).pdf", width=13, height=9)
fit <- pvclust(SS_r_s, method.hclust="complete",
               method.dist="euclidean", nboot = 1) #nboot = 10000
plot(fit, print.num=F,col.pv=c('red','white'),main="Scotian Shelf", float=0.01) # dendogram with p values
pvrect(fit, alpha=.95, border=3, pv="au", lwd=2)
out=pvpick(fit,alpha=.95, pv="au", type="geq", max.only=TRUE)
dput(out,"/Users/catalinagomez/Documents/Indi_Final_Fall2013/3) Hierarchical cluster analysis/Clusters SS.txt")
dev.off()

# write.csv(SHELF_corr2, "outputs/correlations_using_2014_data/SHELF_corr.csv")   
# write.csv(SHELF_corr$r, "outputs/correlations_using_2014_data/SHELFcorrelations_p-value_spearman.csv") 

ESS_corr <- rcorr(as.matrix(ESS,type="spearman"))
ESS_corr2 <- flattenCorrMatrix(ESS_corr$r, ESS_corr$P)
ESS_corr2$ID <- "ESS" 

ESS4VN_corr <- rcorr(as.matrix(ESS_4VN,type="spearman"))
ESS4VN_corr2 <- flattenCorrMatrix(ESS4VN_corr$r, ESS4VN_corr$P)
ESS4VN_corr2$ID <- "ESS4VN"

ESS4VS_corr <- rcorr(as.matrix(ESS_4VS,type="spearman"))
ESS4VS_corr2 <- flattenCorrMatrix(ESS4VS_corr$r, ESS4VS_corr$P)
ESS4VS_corr2$ID <- "ESS4VS" 

ESS4W_corr <- rcorr(as.matrix(ESS_4W,type="spearman"))
ESS4W_corr2 <- flattenCorrMatrix(ESS4W_corr$r, ESS4W_corr$P)
ESS4W_corr2$ID <- "ESS4W"

WSS_corr <- rcorr(as.matrix(WSS,type="spearman"))
WSS_corr2 <- flattenCorrMatrix(WSS_corr$r, WSS_corr$P)
WSS_corr2$ID <- "WSS" 

WSS4X_corr <- rcorr(as.matrix(WSS_4X,type="spearman"))
WSS4X_corr2 <- flattenCorrMatrix(WSS4X_corr$r, WSS4X_corr$P)
WSS4X_corr2$ID <- "WSS4X" 

Correlation_LargeScale <- rbind(SHELF_corr2, ESS_corr2, ESS4VN_corr2, ESS4VS_corr2, ESS4W_corr2, WSS_corr2, WSS4X_corr2)
write.csv(Correlation_LargeScale, "outputs/correlations_using_2014_data/Correlation_LargeScale.csv") 

Correlation_LargeScale_Suite<- Correlation_LargeScale[Correlation_LargeScale$row %in% c('MargalefRichness',
                                                                                        'ShannonDiversity',
                                                                                        'PielouEvenness',
                                                                                        'LargeFishIndicator',
                                                                                        #'MeanLengthAbundance_s',
                                                                                        'CommunityCondition',
                                                                                        'CCMediumBenthivore',
                                                                                        'CCPiscivore',
                                                                                        'CCZoopiscivore',
                                                                                        'CCLargeBenthivore',
                                                                                        'MeanTrophicLevel',
                                                                                        'BTGLargeBenthivore',
                                                                                        'BTGMediumBenthivore',
                                                                                        'BTGZoopiscivore',
                                                                                        'MeanLifespan',
                                                                                        'Intrinsicvulnerabilityindex.L',
                                                                                        'InverseCVBiomass',
                                                                                        'BiomassForage',
                                                                                        'BiomassGroundfish',
                                                                                        'BiomassFlatfish',
                                                                                        'BiomassInvertebrates',
                                                                                        'BiomassSkates',
                                                                                        'FishinginBalance.L',
                                                                                        'FishingPressure.L',
                                                                                        'FPForageFish.L',
                                                                                        'MeanTrophicLevel.L',
                                                                                        'MarineTrophicIndex.L',
                                                                                        'DiversityTargetSpp.L',
                                                                                        'Landings.L',
                                                                                        'LForageFish.L',
                                                                                        'LInvertebrates.L',
                                                                                        'LSkates.L',
                                                                                        'LFlatfish.L',
                                                                                        'LLargePelagic.L'), ]

write.csv(Correlation_LargeScale_Suite, "outputs/correlations_using_2014_data/Correlation_LargeScale_Suite.csv") 

Correlation_LargeScale_Singletons<- Correlation_LargeScale[Correlation_LargeScale$row %in% c(#'MargalefRichness',
                                                                                        #' 'ShannonDiversity',
                                                                                        #' 'PielouEvenness',
                                                                                        #' 'LargeFishIndicator',
                                                                                        #' #'MeanLengthAbundance_s',
                                                                                        #' 'CommunityCondition',
                                                                                         'CCMediumBenthivore',
                                                                                         'CCPiscivore',
                                                                                         'CCZoopiscivore',
                                                                                         'CCLargeBenthivore',
                                                                                         'MeanTrophicLevel',
                                                                                         'BTGLargeBenthivore',
                                                                                         'BTGMediumBenthivore',
                                                                                         'BTGZoopiscivore',
                                                                                         'MeanLifespan',
                                                                                         'Intrinsicvulnerabilityindex.L',
                                                                                         'InverseCVBiomass',
                                                                                        #' 'BiomassForage',
                                                                                        #' 'BiomassGroundfish',
                                                                                         'BiomassFlatfish',
                                                                                        #' 'BiomassInvertebrates',
                                                                                         'BiomassSkates',
                                                                                         'FishinginBalance.L',
                                                                                        #' 'FishingPressure.L',
                                                                                        #' 'FPForageFish.L',
                                                                                         'MeanTrophicLevel.L',
                                                                                         'MarineTrophicIndex.L',
                                                                                        #' 'DiversityTargetSpp.L',
                                                                                        #' 'Landings.L',
                                                                                        #' 'LForageFish.L',
                                                                                        #' 'LInvertebrates.L',
                                                                                        #' 'LSkates.L',
                                                                                        #' 'LFlatfish.L',
                                                                                        'LLargePelagic.L'), ]

Correlation_LargeScale_Singletons<- Correlation_LargeScale_Singletons[Correlation_LargeScale_Singletons$column %in% c(#'MargalefRichness',
                                                                                      #' 'ShannonDiversity',
                                                                                      #' 'PielouEvenness',
                                                                                      #' 'LargeFishIndicator',
                                                                                      #' #'MeanLengthAbundance_s',
                                                                                      #' 'CommunityCondition',
                                                                                      'CCMediumBenthivore',
                                                                                      'CCPiscivore',
                                                                                      'CCZoopiscivore',
                                                                                      'CCLargeBenthivore',
                                                                                      'MeanTrophicLevel',
                                                                                      'BTGLargeBenthivore',
                                                                                      'BTGMediumBenthivore',
                                                                                      'BTGZoopiscivore',
                                                                                      'MeanLifespan',
                                                                                      'Intrinsicvulnerabilityindex.L',
                                                                                      'InverseCVBiomass',
                                                                                      #' 'BiomassForage',
                                                                                      #' 'BiomassGroundfish',
                                                                                      'BiomassFlatfish',
                                                                                      #' 'BiomassInvertebrates',
                                                                                      'BiomassSkates',
                                                                                      'FishinginBalance.L',
                                                                                      #' 'FishingPressure.L',
                                                                                      #' 'FPForageFish.L',
                                                                                      'MeanTrophicLevel.L',
                                                                                      'MarineTrophicIndex.L',
                                                                                      #' 'DiversityTargetSpp.L',
                                                                                      #' 'Landings.L',
                                                                                      #' 'LForageFish.L',
                                                                                      #' 'LInvertebrates.L',
                                                                                      #' 'LSkates.L',
                                                                                      #' 'LFlatfish.L',
                                                                                      'LLargePelagic.L'), ]

write.csv(Correlation_LargeScale_Singletons, "outputs/correlations_using_2014_data/Correlation_LargeScale_Suite_17Singletons.csv") 


##**************************************** Strata scale ***********************************************************#
library(psych) #https://cran.r-project.org/web/packages/psych/psych.pdf
#?corr.test
library(corrgram)
#?corrgram

#This uses the dataset sent by Sylvie on September 9 2016 
#Strata_Q_sylvie <- read.csv("file:///D:/RProjects/UseIndi/data/strata_sylvie/stratsetq2.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
#Strata_Q_sylvie <- read.csv("file:///D:/RProjects/UseIndi/data/strata_sylvie/stratsetq2_NA_0fill.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
#Strata_Q_sylvie <- read.csv("file:///D:/RProjects/UseIndi/data/strata_sylvie//IndiStrata_fillNA.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
Strata_Q_sylvie <- read.csv("file:///D:/RProjects/UseIndi/data/strata_sylvie/Indi_C_s.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)

Strata_Q_440_sylvie <- Strata_Q_sylvie[Strata_Q_sylvie$Strata %in% c('440'), ]
BiodivIndi_440_sylvie <- subset(Strata_Q_440_sylvie, select = c("HillN1Diversity", "ShannonDiversity", "HillN2Dominance", "PielouEvenness"))
head(BiodivIndi_440_sylvie)

cor(BiodivIndi_440_sylvie, method = "spearman") # this recreates results!

#cor(as.matrix(BiodivIndi_440_sylvie,type="spearman")) # I have used this function using as/matrix given that I have a data frame - this gives slighly different values

#rcorr(as.matrix(BiodivIndi_440_sylvie,type="spearman"))

#Correlation results saved by CG
# Div_Indi_440_cor <- rcorr(as.matrix(BiodivIndi_440_sylvie,type="spearman"))
# Div_Indi_440  <- flattenCorrMatrix(Div_Indi_440_cor$r, Div_Indi_440_cor$P)
# Div_Indi_440$ID <- "440" 
# write.csv(Div_Indi_440, "outputs/correlations_using_2014_data/Div_Indi_440.csv") 
Div_Indi_440_cor_r <- cor(BiodivIndi_440_sylvie, method = "spearman")
write.csv(Div_Indi_440_cor_r, "outputs/correlations_using_2014_data/Div_Indi_440_cor_r.csv")



ct_sylvie <- corr.test(BiodivIndi_440_sylvie, method = "spearman") # calculate correlations
ct$r # get correlation coefs
ct$p # get

ct_2015 <- corr.test(BiodivIndi_440_2015, method = "spearman")
ct$r # get correlation coefs
ct$p # get


rcorr(as.matrix(BiodivIndi_440_sylvie,type="spearman"))

#Using other functions / packages
strata440_function_rcorr_sylvie <- rcorr(as.matrix(BiodivIndi_440_sylvie,type="spearman"))
strata440_sylvie <- flattenCorrMatrix(strata440_function_rcorr_sylvie$r, strata440_function_rcorr_sylvie$P)
head(strata440_sylvie)

BiodivIndi_440 <- subset(Strata_Q_440, select = c("HillN1Diversity", "ShannonDiversity", "HillN2Dominance", "PielouEvenness"))
strata440_function_rcorr <- rcorr(as.matrix(BiodivIndi_440,type="spearman"))
strata440 <- flattenCorrMatrix(strata440_function_rcorr$r, strata440_function_rcorr$P)
head(strata440)

strata440_function_rcorr_s <- rcorr(as.matrix(BiodivIndi_440_s,type="spearman"))
strata440_s <- flattenCorrMatrix(strata440_function_rcorr_s$r, strata440_function_rcorr_s$P)
head(strata440_s)


