# READ ME
# August 2016 
# install.packages("devtools")
# devtools::install_github("rstudio/packrat")
# packrat::init()  # -- https://rstudio.github.io/packrat/commands.html # Reads packages needed for plots and analysis

library(ggplot2)
library(reshape2)
library(gridExtra)
library(Hmisc)
library(PerformanceAnalytics)
library(plyr)
library(corrplot)
library(zoo)
library(Hmisc)
theme_set(theme_bw())
source('R/IndiFunctions.R')           # Sources functions that set ggplot settings to plot indicators per each scale (Shelf, NAFO and strata scale)
#system.file(package="ggplot2")       # This checks that you are using private library via packratget


###  ###   **** READ RAW DATASETS   ****  ###  ####
#Reads csv data of indicators extracted using A.Cook's package 

#*********Shelf scale  
          #Note: remove 2014 to 2015 for redundancy analysis? Standardizations will change
SS <- read.csv("data/shelfsetq.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
#SS <- SS[!SS$YEAR %in% c('2014', '2015'), ]

#*********Eastern/Western Scotian Shelf
Shelf_Q <- read.csv("data/esswsssetq.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
#Shelf_Q <- Shelf_Q[!Shelf_Q$YEAR %in% c('2014', '2015'), ]
WSS_Q <- Shelf_Q[Shelf_Q$ID %in% c('WSS'), ]
ESS_Q <- Shelf_Q[Shelf_Q$ID %in% c('ESS'), ]
#IndiQ_SS$ID <- factor(IndiQ_SS$ID, levels=c('SHELF','WSS','ESS'))
#IndiQ_SS <- IndiQ_SS[!IndiQ_SS$YEAR %in% c('2014', '2015'), ]

#*********NAFO scale
IndiQ_NAFO <- read.csv("data/nafosetq.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
#IndiQ_NAFO <- IndiQ_NAFO[!IndiQ_NAFO$YEAR %in% c('2014', '2015'), ]
IndiQ_NAFO_4x <- IndiQ_NAFO[IndiQ_NAFO$ID %in% c('4X'), ]
IndiQ_NAFO_4x$ID <- factor(IndiQ_NAFO_4x$ID)
IndiQ_NAFO <- IndiQ_NAFO[IndiQ_NAFO$ID %in% c('4W','4VS', '4VN'), ]
IndiQ_NAFO_all <- IndiQ_NAFO[IndiQ_NAFO$ID %in% c('4W','4VS', '4VN', '4X'), ]
IndiQ_NAFO <- droplevels(IndiQ_NAFO)
IndiQ_NAFO$ID <- factor(IndiQ_NAFO$ID)
IndiQ_NAFO_4vs <- IndiQ_NAFO[IndiQ_NAFO$ID %in% c('4VS'), ]
IndiQ_NAFO_4vn <- IndiQ_NAFO[IndiQ_NAFO$ID %in% c('4VN'), ]
IndiQ_NAFO_4w <- IndiQ_NAFO[IndiQ_NAFO$ID %in% c('4W'), ]

#*********Strata scale
Strata_Q <- read.csv("data/stratsetq.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
Strata_Q$ID <- factor(Strata_Q$ID)
#Strata_Q <- Strata_Q[!Strata_Q$YEAR %in% c('2014', '2015'), ]
Strata_Q_4vs <- Strata_Q[Strata_Q$ID %in% c('443','444', '445', '446', '447', '448', '449', '450', '451', '452'), ]


###  ###  *****  CREATE NEW CSV FILES FROM RAW DATA FOR ANALYSIS   *****  ###  ####
AllScales_combined <- rbind.fill(SS, WSS_Q, ESS_Q, IndiQ_NAFO_all, Strata_Q)
#write.csv(AllScales_combined, "outputs/AllScales_combined_Oct3_2016(setq).csv", row.names=FALSE)

# Set to NA all candidate indicators that have more than 25% NAs 

#head(Strata_Q$CCLargeBenthivore)
#Strata_Q$CCLargeBenthivore <- NA
#write.csv(Strata_Q, "outputs/stratsetq_filtered.csv", row.names=FALSE)


###  ###  *****  DEFINE GROUPINGS OF INDICATORS  *****  ###  ####
#Using the results of the redundancy analysis (tech report), this section below defines indicators defined as singletons 
# or redundant indicators grouped in clusters (C1 to C12).

LargeScales <- rbind(SS, WSS_Q, ESS_Q ,IndiQ_NAFO) # does not include 4x as 4x == WSS
LargeScales$ID <- factor(LargeScales$ID)

# CandidateIndi <- colnames(LargeScales)
# write.csv(CandidateIndi, "outputs/data.CandidateIndi.csv")

CandidateIndi <- melt(LargeScales ,id=c('YEAR', 'ID'), measure = c(#'Abundance',
                                                           #'Abundance_s',
                                                           'BInvertebrateToDemersal',
                                                           'BInvertebrateToDemersal_s',
                                                           'Biomass',
                                                           'Biomass_s',
                                                           'BiomassClupeids',
                                                           'BiomassClupeids_s',
                                                           'BiomassFinfish',
                                                           'BiomassFinfish_s',
                                                           'BiomassFlatfish',
                                                           'BiomassFlatfish_s',
                                                           'BiomassForage',
                                                           'BiomassForage_s',
                                                           'BiomassGadoids',
                                                           'BiomassGadoids_s',
                                                           'BiomassGroundfish',
                                                           'BiomassGroundfish_s',
                                                           'BiomassInvertebrates',
                                                           'BiomassInvertebrates_s',
                                                           'BiomassSkates',
                                                           'BiomassSkates_s',
                                                           'BiomassTL2',
                                                           'BiomassTL2_s',
                                                           'BiomassTL3',
                                                           'BiomassTL3_s',
                                                           'BiomassTL4',
                                                           'BiomassTL4_s',
                                                           'BPelagicToDemersal',
                                                           'BPelagicToDemersal_s',
                                                           'BTGLargeBenthivore',
                                                           'BTGLargeBenthivore_s',
                                                           'BTGMediumBenthivore',
                                                           'BTGMediumBenthivore_s',
                                                           'BTGPiscivore',
                                                           'BTGPiscivore_s',
                                                           'BTGPlanktivore',
                                                           'BTGPlanktivore_s',
                                                           'BTGZoopiscivore',
                                                           'BTGZoopiscivore_s',
                                                           'CCLargeBenthivore',
                                                           'CCLargeBenthivore_s',
                                                           'CCMediumBenthivore',
                                                           'CCMediumBenthivore_s',
                                                           'CCPiscivore',
                                                           'CCPiscivore_s',
                                                           'CCPlanktivore',
                                                           'CCPlanktivore_s',
                                                           'CCZoopiscivore',
                                                           'CCZoopiscivore_s',
                                                           'CommunityCondition',
                                                           'CommunityCondition_s',
                                                           'DiversityTargetSpp.L',
                                                           'DiversityTargetSpp.L_s',
                                                           'FishinginBalance.L',
                                                           'FishinginBalance.L_s',
                                                           'FishingPressure.L',
                                                           'FishingPressure.L_s',
                                                           'FPClupeids.L',
                                                           'FPClupeids.L_s',
                                                           'FPFinfish.L',
                                                           'FPFinfish.L_s',
                                                           'FPFlatfish.L',
                                                           'FPFlatfish.L_s',
                                                           'FPForageFish.L',
                                                           'FPForageFish.L_s',
                                                           'FPGadoids.L',
                                                           'FPGadoids.L_s',
                                                           'FPGroundfish.L',
                                                           'FPGroundfish.L_s',
                                                           'FPInvertebrates.L',
                                                           #'FPInvertebrates.L_s',
                                                           'FPSkates.L',
                                                           'FPSkates.L_s',
                                                           'Heips',
                                                           'Heips_s',
                                                           'HillN1Diversity',
                                                           'HillN1Diversity_s',
                                                           'HillN2Dominance',
                                                           'HillN2Dominance_s',
                                                           'Intrinsicvulnerabilityindex.L',
                                                           'Intrinsicvulnerabilityindex.L_s',
                                                           'InverseCVBiomass',
                                                           'InverseCVBiomass_s',
                                                           # 'InverseFishingPressure.L',
                                                           # 'InverseFishingPressure.L_s',
                                                           # 'InverseFPClupeids.L',
                                                           # 'InverseFPClupeids.L_s',
                                                           # 'InverseFPFinfish.L',
                                                           # 'InverseFPFinfish.L_s',
                                                           # 'InverseFPFlatfish.L',
                                                           # 'InverseFPFlatfish.L_s',
                                                           # 'InverseFPForageFish.L',
                                                           # 'InverseFPForageFish.L_s',
                                                           # 'InverseFPGadoids.L',
                                                           # 'InverseFPGadoids.L_s',
                                                           # 'InverseFPGroundfish.L',
                                                           # 'InverseFPGroundfish.L_s',
                                                           # 'InverseFPInvertebrates.L',
                                                           # 'InverseFPInvertebrates.L_s',
                                                           # 'InverseFPSkates.L',
                                                           # 'InverseFPSkates.L_s',
                                                           'KemptonQ',
                                                           'KemptonQ_s',
                                                           'Landings.L',
                                                           'Landings.L_s',
                                                           'LargeFishIndicator',
                                                           'LargeFishIndicator_s',
                                                           'LargeSpeciesIndicator',
                                                           'LargeSpeciesIndicator_s',
                                                           'LClupeids.L',
                                                           'LClupeids.L_s',
                                                           'LFinfish.L',
                                                           'LFinfish.L_s',
                                                           'LFlatfish.L',
                                                           'LFlatfish.L_s',
                                                           'LForageFish.L',
                                                           'LForageFish.L_s',
                                                           'LGadoids.L',
                                                           'LGadoids.L_s',
                                                           'LGroundfish.L',
                                                           'LGroundfish.L_s',
                                                           'LInvertebrates.L',
                                                           'LInvertebrates.L_s',
                                                           'LLargePelagic.L',
                                                           'LLargePelagic.L_s',
                                                           'LSkates.L',
                                                           'LSkates.L_s',
                                                           'MargalefGroundfish',
                                                           'MargalefGroundfish_s',
                                                           'MargalefRichness',
                                                           'MargalefRichness_s',
                                                           'MarineTrophicIndex.L',
                                                           'MarineTrophicIndex.L_s',
                                                           'MeanLengthAbundance',
                                                           'MeanLengthAbundance_s',
                                                           'MeanLengthBiomass',
                                                           'MeanLengthBiomass_s',
                                                           'MeanLifespan',
                                                           'MeanLifespan_s',
                                                           'MeanTrophicLevel',
                                                           'MeanTrophicLevel_s',
                                                           'MeanTrophicLevel.L',
                                                           'MeanTrophicLevel.L_s',
                                                           # 'meanTrophicLevelCommunityLengthALLBIOMASS',
                                                           # 'meanTrophicLevelCommunityLengthALLBIOMASS_s',
                                                           'PielouEvenness',
                                                           'PielouEvenness_s',
                                                           'PropPredatoryFish',
                                                           'PropPredatoryFish_s',
                                                           #'resourcePotentialPELAGIC.BIOMASS',
                                                           #'resourcePotentialPELAGIC.BIOMASS_s',
                                                           'ShannonDiversity',
                                                           'ShannonDiversity_s',
                                                           'SpeciesRichness',
                                                           'SpeciesRichness_s',
                                                           'MMLengthBiomass',
                                                           'MMLengthBiomass_s',
                                                           'MMLengthAbundance', #$for some reasons this plot was nto produced
                                                           'MMLengthAbundance_s'))

CandidateIndi_RAW <- melt(LargeScales ,id=c('YEAR', 'ID'), measure = c(#'Abundance',
                                                                   #'Abundance_s',
                                                                   'BInvertebrateToDemersal',
                                                                   #'BInvertebrateToDemersal_s',
                                                                   'Biomass',
                                                                   #'Biomass_s',
                                                                   'BiomassClupeids',
                                                                   #'BiomassClupeids_s',
                                                                   'BiomassFinfish',
                                                                   #'BiomassFinfish_s',
                                                                   'BiomassFlatfish',
                                                                   #'BiomassFlatfish_s',
                                                                   'BiomassForage',
                                                                   #'BiomassForage_s',
                                                                   'BiomassGadoids',
                                                                   #'BiomassGadoids_s',
                                                                   'BiomassGroundfish',
                                                                   #'BiomassGroundfish_s',
                                                                   'BiomassInvertebrates',
                                                                   #'BiomassInvertebrates_s',
                                                                   'BiomassSkates',
                                                                   #'BiomassSkates_s',
                                                                   'BiomassTL2',
                                                                   #'BiomassTL2_s',
                                                                   'BiomassTL3',
                                                                   #'BiomassTL3_s',
                                                                   'BiomassTL4',
                                                                   #'BiomassTL4_s',
                                                                   'BPelagicToDemersal',
                                                                   #'BPelagicToDemersal_s',
                                                                   'BTGLargeBenthivore',
                                                                   #'BTGLargeBenthivore_s',
                                                                   'BTGMediumBenthivore',
                                                                   #'BTGMediumBenthivore_s',
                                                                   'BTGPiscivore',
                                                                   #'BTGPiscivore_s',
                                                                   'BTGPlanktivore',
                                                                   #'BTGPlanktivore_s',
                                                                   'BTGZoopiscivore',
                                                                   #'BTGZoopiscivore_s',
                                                                   'CCLargeBenthivore',
                                                                   #'CCLargeBenthivore_s',
                                                                   'CCMediumBenthivore',
                                                                   #'CCMediumBenthivore_s',
                                                                   'CCPiscivore',
                                                                   #'CCPiscivore_s',
                                                                   'CCPlanktivore',
                                                                   #'CCPlanktivore_s',
                                                                   'CCZoopiscivore',
                                                                   #'CCZoopiscivore_s',
                                                                   'CommunityCondition',
                                                                   #'CommunityCondition_s',
                                                                   'DiversityTargetSpp.L',
                                                                   #'DiversityTargetSpp.L_s',
                                                                   'FishinginBalance.L',
                                                                   #'FishinginBalance.L_s',
                                                                   'FishingPressure.L',
                                                                   #'FishingPressure.L_s',
                                                                   'FPClupeids.L',
                                                                   #'FPClupeids.L_s',
                                                                   'FPFinfish.L',
                                                                   #'FPFinfish.L_s',
                                                                   'FPFlatfish.L',
                                                                   #'FPFlatfish.L_s',
                                                                   'FPForageFish.L',
                                                                   #'FPForageFish.L_s',
                                                                   'FPGadoids.L',
                                                                   #'FPGadoids.L_s',
                                                                   'FPGroundfish.L',
                                                                   #'FPGroundfish.L_s',
                                                                   'FPInvertebrates.L',
                                                                   #'FPInvertebrates.L_s',
                                                                   'FPSkates.L',
                                                                   #'FPSkates.L_s',
                                                                   'Heips',
                                                                   #'Heips_s',
                                                                   'HillN1Diversity',
                                                                   #'HillN1Diversity_s',
                                                                   'HillN2Dominance',
                                                                   #'HillN2Dominance_s',
                                                                   'Intrinsicvulnerabilityindex.L',
                                                                   #'Intrinsicvulnerabilityindex.L_s',
                                                                   'InverseCVBiomass',
                                                                   #'InverseCVBiomass_s',
                                                                   # 'InverseFishingPressure.L',
                                                                   # 'InverseFishingPressure.L_s',
                                                                   # 'InverseFPClupeids.L',
                                                                   # 'InverseFPClupeids.L_s',
                                                                   # 'InverseFPFinfish.L',
                                                                   # 'InverseFPFinfish.L_s',
                                                                   # 'InverseFPFlatfish.L',
                                                                   # 'InverseFPFlatfish.L_s',
                                                                   # 'InverseFPForageFish.L',
                                                                   # 'InverseFPForageFish.L_s',
                                                                   # 'InverseFPGadoids.L',
                                                                   # 'InverseFPGadoids.L_s',
                                                                   # 'InverseFPGroundfish.L',
                                                                   # 'InverseFPGroundfish.L_s',
                                                                   # 'InverseFPInvertebrates.L',
                                                                   # 'InverseFPInvertebrates.L_s',
                                                                   # 'InverseFPSkates.L',
                                                                   # 'InverseFPSkates.L_s',
                                                                   'KemptonQ',
                                                                   #'KemptonQ_s',
                                                                   'Landings.L',
                                                                   #'Landings.L_s',
                                                                   'LargeFishIndicator',
                                                                   #'LargeFishIndicator_s',
                                                                   'LargeSpeciesIndicator',
                                                                   #'LargeSpeciesIndicator_s',
                                                                   'LClupeids.L',
                                                                   #'LClupeids.L_s',
                                                                   'LFinfish.L',
                                                                   #'LFinfish.L_s',
                                                                   'LFlatfish.L',
                                                                   #'LFlatfish.L_s',
                                                                   'LForageFish.L',
                                                                   #'LForageFish.L_s',
                                                                   'LGadoids.L',
                                                                   #'LGadoids.L_s',
                                                                   'LGroundfish.L',
                                                                   #'LGroundfish.L_s',
                                                                   'LInvertebrates.L',
                                                                   #'LInvertebrates.L_s',
                                                                   'LLargePelagic.L',
                                                                   #'LLargePelagic.L_s',
                                                                   'LSkates.L',
                                                                   #'LSkates.L_s',
                                                                   'MargalefGroundfish',
                                                                   #'MargalefGroundfish_s',
                                                                   'MargalefRichness',
                                                                   #'MargalefRichness_s',
                                                                   'MarineTrophicIndex.L',
                                                                   #'MarineTrophicIndex.L_s',
                                                                   'MeanLengthAbundance',
                                                                   #'MeanLengthAbundance_s',
                                                                   'MeanLengthBiomass',
                                                                   #'MeanLengthBiomass_s',
                                                                   'MeanLifespan',
                                                                   #'MeanLifespan_s',
                                                                   'MeanTrophicLevel',
                                                                   'MeanTrophicLevel.L',
                                                                   #'MeanTrophicLevel.L_s',
                                                                   #'MeanTrophicLevel_s',
                                                                   # 'meanTrophicLevelCommunityLengthALLBIOMASS',
                                                                   # 'meanTrophicLevelCommunityLengthALLBIOMASS_s',
                                                                   'MMLengthAbundance',
                                                                   #'MMLengthAbundance_s',
                                                                   'MMLengthBiomass',
                                                                   #'MMLengthBiomass_s',
                                                                   'PielouEvenness',
                                                                   #'PielouEvenness_s',
                                                                   'PropPredatoryFish',
                                                                   #'PropPredatoryFish_s',
                                                                   #'resourcePotentialPELAGIC.BIOMASS',
                                                                   #'resourcePotentialPELAGIC.BIOMASS_s',
                                                                   'ShannonDiversity',
                                                                   #'ShannonDiversity_s',
                                                                   #'SpeciesRichness_s',
                                                                   'SpeciesRichness'))


CandidateIndi_stdz <- melt(LargeScales ,id=c('YEAR', 'ID'), measure = c(#'Abundance',
                                                                      #'Abundance_s',
                                                                      #'BInvertebrateToDemersal',
                                                                      'BInvertebrateToDemersal_s',
                                                                      #'Biomass',
                                                                      'Biomass_s',
                                                                      #'BiomassClupeids',
                                                                      'BiomassClupeids_s',
                                                                      #'BiomassFinfish',
                                                                      'BiomassFinfish_s',
                                                                      #'BiomassFlatfish',
                                                                      'BiomassFlatfish_s',
                                                                      #'BiomassForage',
                                                                      'BiomassForage_s',
                                                                      #'BiomassGadoids',
                                                                      'BiomassGadoids_s',
                                                                      #'BiomassGroundfish',
                                                                      'BiomassGroundfish_s',
                                                                      #'BiomassInvertebrates',
                                                                      'BiomassInvertebrates_s',
                                                                      #'BiomassSkates',
                                                                      'BiomassSkates_s',
                                                                      #'BiomassTL2',
                                                                      'BiomassTL2_s',
                                                                      #'BiomassTL3',
                                                                      'BiomassTL3_s',
                                                                      #'BiomassTL4',
                                                                      'BiomassTL4_s',
                                                                      #'BPelagicToDemersal',
                                                                      'BPelagicToDemersal_s',
                                                                      #'BTGLargeBenthivore',
                                                                      'BTGLargeBenthivore_s',
                                                                      #'BTGMediumBenthivore',
                                                                      'BTGMediumBenthivore_s',
                                                                      #'BTGPiscivore',
                                                                      'BTGPiscivore_s',
                                                                      #'BTGPlanktivore',
                                                                      'BTGPlanktivore_s',
                                                                      #'BTGZoopiscivore',
                                                                      'BTGZoopiscivore_s',
                                                                      #'CCLargeBenthivore',
                                                                      'CCLargeBenthivore_s',
                                                                      #'CCMediumBenthivore',
                                                                      'CCMediumBenthivore_s',
                                                                      #'CCPiscivore',
                                                                      'CCPiscivore_s',
                                                                      #'CCPlanktivore',
                                                                      'CCPlanktivore_s',
                                                                      #'CCZoopiscivore',
                                                                      'CCZoopiscivore_s',
                                                                      #'CommunityCondition',
                                                                      'CommunityCondition_s',
                                                                      #'DiversityTargetSpp.L',
                                                                      'DiversityTargetSpp.L_s',
                                                                      #'FishinginBalance.L',
                                                                      'FishinginBalance.L_s',
                                                                      #'FishingPressure.L',
                                                                      'FishingPressure.L_s',
                                                                      #'FPClupeids.L',
                                                                      'FPClupeids.L_s',
                                                                      #'FPFinfish.L',
                                                                      'FPFinfish.L_s',
                                                                      #'FPFlatfish.L',
                                                                      'FPFlatfish.L_s',
                                                                      #'FPForageFish.L',
                                                                      'FPForageFish.L_s',
                                                                      #'FPGadoids.L',
                                                                      'FPGadoids.L_s',
                                                                      #'FPGroundfish.L',
                                                                      'FPGroundfish.L_s',
                                                                      #'FPInvertebrates.L',
                                                                      'FPInvertebrates.L_s',
                                                                      #'FPSkates.L',
                                                                      'FPSkates.L_s',
                                                                      #'Heips',
                                                                      'Heips_s',
                                                                      #'HillN1Diversity',
                                                                      'HillN1Diversity_s',
                                                                      #'HillN2Dominance',
                                                                      'HillN2Dominance_s',
                                                                      #'Intrinsicvulnerabilityindex.L',
                                                                      'Intrinsicvulnerabilityindex.L_s',
                                                                      #'InverseCVBiomass',
                                                                      'InverseCVBiomass_s',
                                                                      # 'InverseFishingPressure.L',
                                                                      # 'InverseFishingPressure.L_s',
                                                                      # 'InverseFPClupeids.L',
                                                                      # 'InverseFPClupeids.L_s',
                                                                      # 'InverseFPFinfish.L',
                                                                      # 'InverseFPFinfish.L_s',
                                                                      # 'InverseFPFlatfish.L',
                                                                      # 'InverseFPFlatfish.L_s',
                                                                      # 'InverseFPForageFish.L',
                                                                      # 'InverseFPForageFish.L_s',
                                                                      # 'InverseFPGadoids.L',
                                                                      # 'InverseFPGadoids.L_s',
                                                                      # 'InverseFPGroundfish.L',
                                                                      # 'InverseFPGroundfish.L_s',
                                                                      # 'InverseFPInvertebrates.L',
                                                                      # 'InverseFPInvertebrates.L_s',
                                                                      # 'InverseFPSkates.L',
                                                                      # 'InverseFPSkates.L_s',
                                                                     # 'KemptonQ',
                                                                      'KemptonQ_s',
                                                                      #'Landings.L',
                                                                      'Landings.L_s',
                                                                      #'LargeFishIndicator',
                                                                      'LargeFishIndicator_s',
                                                                      #'LargeSpeciesIndicator',
                                                                      'LargeSpeciesIndicator_s',
                                                                      #'LClupeids.L',
                                                                      'LClupeids.L_s',
                                                                      #'LFinfish.L',
                                                                      'LFinfish.L_s',
                                                                      #'LFlatfish.L',
                                                                      'LFlatfish.L_s',
                                                                      #'LForageFish.L',
                                                                      'LForageFish.L_s',
                                                                      #'LGadoids.L',
                                                                      'LGadoids.L_s',
                                                                      #'LGroundfish.L',
                                                                      'LGroundfish.L_s',
                                                                     # 'LInvertebrates.L',
                                                                      'LInvertebrates.L_s',
                                                                      #'LLargePelagic.L',
                                                                      'LLargePelagic.L_s',
                                                                      #'LSkates.L',
                                                                      'LSkates.L_s',
                                                                      #'MargalefGroundfish',
                                                                      'MargalefGroundfish_s',
                                                                      #'MargalefRichness',
                                                                      'MargalefRichness_s',
                                                                      #'MarineTrophicIndex.L',
                                                                      'MarineTrophicIndex.L_s',
                                                                      #'MeanLengthAbundance',
                                                                      'MeanLengthAbundance_s',
                                                                      #'MeanLengthBiomass',
                                                                      'MeanLengthBiomass_s',
                                                                      #'MeanLifespan',
                                                                      'MeanLifespan_s',
                                                                      #'MeanTrophicLevel',
                                                                      #'MeanTrophicLevel.L',
                                                                      'MeanTrophicLevel.L_s',
                                                                      'MeanTrophicLevel_s',
                                                                      # 'meanTrophicLevelCommunityLengthALLBIOMASS',
                                                                      # 'meanTrophicLevelCommunityLengthALLBIOMASS_s',
                                                                      #'MMLengthAbundance',
                                                                      'MMLengthAbundance_s',
                                                                      #'MMLengthBiomass',
                                                                      'MMLengthBiomass_s',
                                                                      #'PielouEvenness',
                                                                      'PielouEvenness_s',
                                                                      #'PropPredatoryFish',
                                                                      'PropPredatoryFish_s',
                                                                      #'resourcePotentialPELAGIC.BIOMASS',
                                                                      #'resourcePotentialPELAGIC.BIOMASS_s',
                                                                      #'ShannonDiversity',
                                                                      'ShannonDiversity_s',
                                                                      #'SpeciesRichness',
                                                                      'SpeciesRichness_s'))

CandidateIndi_strata <- melt(Strata_Q,id=c('YEAR', 'ID'), measure = c(#'Abundance',
                                                                      #'Abundance_s',
                                                                      'BInvertebrateToDemersal',
                                                                      'BInvertebrateToDemersal_s',
                                                                      'Biomass',
                                                                      'Biomass_s',
                                                                      'BiomassClupeids',
                                                                      'BiomassClupeids_s',
                                                                      'BiomassFinfish',
                                                                      'BiomassFinfish_s',
                                                                      'BiomassFlatfish',
                                                                      'BiomassFlatfish_s',
                                                                      'BiomassForage',
                                                                      'BiomassForage_s',
                                                                      'BiomassGadoids',
                                                                      'BiomassGadoids_s',
                                                                      'BiomassGroundfish',
                                                                      'BiomassGroundfish_s',
                                                                      'BiomassInvertebrates',
                                                                      'BiomassInvertebrates_s',
                                                                      'BiomassSkates',
                                                                      'BiomassSkates_s',
                                                                      'BiomassTL2',
                                                                      'BiomassTL2_s',
                                                                      'BiomassTL3',
                                                                      'BiomassTL3_s',
                                                                      'BiomassTL4',
                                                                      'BiomassTL4_s',
                                                                      'BTGMediumBenthivore',
                                                                      'BTGMediumBenthivore_s', 
                                                                      'BTGPiscivore',
                                                                      'BTGPiscivore_s',
                                                                      'BTGLargeBenthivore',
                                                                      'BTGLargeBenthivore_s',
                                                                      'BTGPlanktivore',
                                                                      'BTGPlanktivore_s',
                                                                      'BTGZoopiscivore',
                                                                      'BTGZoopiscivore_s',
                                                                      'BPelagicToDemersal',
                                                                      'BPelagicToDemersal_s',
                                                                      'Heips',
                                                                      'Heips_s',
                                                                      'HillN1Diversity',
                                                                      'HillN1Diversity_s',
                                                                      'HillN2Dominance',
                                                                      'HillN2Dominance_s',
                                                                      'InverseCVBiomass',
                                                                      'InverseCVBiomass_s',
                                                                      'KemptonQ',
                                                                      'KemptonQ_s',
                                                                      'LargeFishIndicator',
                                                                      'LargeFishIndicator_s',
                                                                      'LargeSpeciesIndicator',
                                                                      'LargeSpeciesIndicator_s',
                                                                      'MargalefGroundfish',
                                                                      'MargalefGroundfish_s',
                                                                      'MargalefRichness',
                                                                      'MargalefRichness_s',
                                                                      'MeanLengthAbundance',
                                                                      'MeanLengthAbundance_s',
                                                                      'MeanLengthBiomass',
                                                                      'MeanLengthBiomass_s',
                                                                      'MeanLifespan',
                                                                      'MeanLifespan_s',
                                                                      'MeanTrophicLevel',
                                                                      'MeanTrophicLevel_s',
                                                                      #'meanTrophicLevelCommunityLengthALLBIOMASS',
                                                                      #'meanTrophicLevelCommunityLengthALLBIOMASS_s',
                                                                      'MMLengthAbundance',
                                                                      'MMLengthAbundance_s',
                                                                      'MMLengthBiomass',
                                                                      'MMLengthBiomass_s',
                                                                      'PielouEvenness',
                                                                      'PielouEvenness_s',
                                                                      'PropPredatoryFish',
                                                                      'PropPredatoryFish_s',
                                                                      'ShannonDiversity',
                                                                      'ShannonDiversity_s',
                                                                      'SpeciesRichness',
                                                                      'SpeciesRichness_s', 
                                                                      'CCLargeBenthivore',
                                                                      'CCLargeBenthivore_s',
                                                                      'CCMediumBenthivore',
                                                                      'CCMediumBenthivore_s',
                                                                      'CCPiscivore',
                                                                      'CCPiscivore_s',
                                                                      'CCPlanktivore',
                                                                      'CCPlanktivore_s',
                                                                      'CCZoopiscivore',
                                                                      'CCZoopiscivore_s',
                                                                      'CommunityCondition',
                                                                      'CommunityCondition_s'))

CandidateIndi_strata_RAW <- melt(Strata_Q,id=c('YEAR', 'ID'), measure = c(#'Abundance',
                                                                      #'Abundance_s',
                                                                      'BInvertebrateToDemersal',
                                                                      #'BInvertebrateToDemersal_s',
                                                                      'Biomass',
                                                                      #'Biomass_s',
                                                                      'BiomassClupeids',
                                                                      #'BiomassClupeids_s',
                                                                      'BiomassFinfish',
                                                                      #'BiomassFinfish_s',
                                                                      'BiomassFlatfish',
                                                                      #'BiomassFlatfish_s',
                                                                      'BiomassForage',
                                                                      #'BiomassForage_s',
                                                                      'BiomassGadoids',
                                                                      #'BiomassGadoids_s',
                                                                      'BiomassGroundfish',
                                                                      #'BiomassGroundfish_s',
                                                                      'BiomassInvertebrates',
                                                                      #'BiomassInvertebrates_s',
                                                                      'BiomassSkates',
                                                                      #'BiomassSkates_s',
                                                                      'BiomassTL2',
                                                                      #'BiomassTL2_s',
                                                                      'BiomassTL3',
                                                                      #'BiomassTL3_s',
                                                                      'BiomassTL4',
                                                                      #'BiomassTL4_s',
                                                                      'BTGMediumBenthivore',
                                                                      #'BTGMediumBenthivore_s', 
                                                                      'BTGPiscivore',
                                                                      #'BTGPiscivore_s',
                                                                      'BTGLargeBenthivore',
                                                                      #'BTGLargeBenthivore_s',
                                                                      'BTGPlanktivore',
                                                                      #'BTGPlanktivore_s',
                                                                      'BTGZoopiscivore',
                                                                      #'BTGZoopiscivore_s',
                                                                      'BPelagicToDemersal',
                                                                      #'BPelagicToDemersal_s',
                                                                      'Heips',
                                                                      #'Heips_s',
                                                                      'HillN1Diversity',
                                                                      #'HillN1Diversity_s',
                                                                      'HillN2Dominance',
                                                                      #'HillN2Dominance_s',
                                                                      'InverseCVBiomass',
                                                                      #'InverseCVBiomass_s',
                                                                      'KemptonQ',
                                                                      #'KemptonQ_s',
                                                                      'LargeFishIndicator',
                                                                      #'LargeFishIndicator_s',
                                                                      'LargeSpeciesIndicator',
                                                                      #'LargeSpeciesIndicator_s',
                                                                      'MargalefGroundfish',
                                                                      #'MargalefGroundfish_s',
                                                                      'MargalefRichness',
                                                                      #'MargalefRichness_s',
                                                                      'MeanLengthAbundance',
                                                                      #'MeanLengthAbundance_s',
                                                                      'MeanLengthBiomass',
                                                                      #'MeanLengthBiomass_s',
                                                                      'MeanLifespan',
                                                                      #'MeanLifespan_s',
                                                                      'MeanTrophicLevel',
                                                                      #'MeanTrophicLevel_s',
                                                                      #'meanTrophicLevelCommunityLengthALLBIOMASS',
                                                                      #'meanTrophicLevelCommunityLengthALLBIOMASS_s',
                                                                      'MMLengthAbundance',
                                                                      #'MMLengthAbundance_s',
                                                                      'MMLengthBiomass',
                                                                      #'MMLengthBiomass_s',
                                                                      'PielouEvenness',
                                                                      #'PielouEvenness_s',
                                                                      'PropPredatoryFish',
                                                                      #'PropPredatoryFish_s',
                                                                      'ShannonDiversity',
                                                                      #'ShannonDiversity_s',
                                                                      'SpeciesRichness',
                                                                      #'SpeciesRichness_s', 
                                                                      'CCLargeBenthivore',
                                                                      #'CCLargeBenthivore_s',
                                                                      'CCMediumBenthivore',
                                                                      #'CCMediumBenthivore_s',
                                                                      'CCPiscivore',
                                                                      #'CCPiscivore_s',
                                                                      'CCPlanktivore',
                                                                      #'CCPlanktivore_s',
                                                                      'CCZoopiscivore',
                                                                      #'CCZoopiscivore_s',
                                                                      #'CommunityCondition_s',
                                                                      'CommunityCondition'))



CandidateIndi_strata_stdz <- melt(Strata_Q,id=c('YEAR', 'ID'), measure = c(#'Abundance',
                                                                          #'Abundance_s',
                                                                          #'BInvertebrateToDemersal',
                                                                          'BInvertebrateToDemersal_s',
                                                                          #'Biomass',
                                                                          'Biomass_s',
                                                                          #'BiomassClupeids',
                                                                          'BiomassClupeids_s',
                                                                          #'BiomassFinfish',
                                                                          'BiomassFinfish_s',
                                                                          #'BiomassFlatfish',
                                                                          'BiomassFlatfish_s',
                                                                          #'BiomassForage',
                                                                          'BiomassForage_s',
                                                                          #'BiomassGadoids',
                                                                          'BiomassGadoids_s',
                                                                          #'BiomassGroundfish',
                                                                          'BiomassGroundfish_s',
                                                                          #'BiomassInvertebrates',
                                                                          'BiomassInvertebrates_s',
                                                                          #'BiomassSkates',
                                                                          'BiomassSkates_s',
                                                                          #'BiomassTL2',
                                                                          'BiomassTL2_s',
                                                                          #'BiomassTL3',
                                                                          'BiomassTL3_s',
                                                                          #'BiomassTL4',
                                                                          'BiomassTL4_s',
                                                                          #'BTGMediumBenthivore',
                                                                          'BTGMediumBenthivore_s', 
                                                                          #'BTGPiscivore',
                                                                          'BTGPiscivore_s',
                                                                          #'BTGLargeBenthivore',
                                                                          'BTGLargeBenthivore_s',
                                                                          #'BTGPlanktivore',
                                                                          'BTGPlanktivore_s',
                                                                          #'BTGZoopiscivore',
                                                                          'BTGZoopiscivore_s',
                                                                          #'BPelagicToDemersal',
                                                                          'BPelagicToDemersal_s',
                                                                          #'Heips',
                                                                          'Heips_s',
                                                                          #'HillN1Diversity',
                                                                          'HillN1Diversity_s',
                                                                          #'HillN2Dominance',
                                                                          'HillN2Dominance_s',
                                                                          #'InverseCVBiomass',
                                                                          'InverseCVBiomass_s',
                                                                          #'KemptonQ',
                                                                          'KemptonQ_s',
                                                                          #'LargeFishIndicator',
                                                                          'LargeFishIndicator_s',
                                                                          #'LargeSpeciesIndicator',
                                                                          'LargeSpeciesIndicator_s',
                                                                          #'MargalefGroundfish',
                                                                          'MargalefGroundfish_s',
                                                                          #'MargalefRichness',
                                                                          'MargalefRichness_s',
                                                                          #'MeanLengthAbundance',
                                                                          'MeanLengthAbundance_s',
                                                                          #'MeanLengthBiomass',
                                                                          'MeanLengthBiomass_s',
                                                                          #'MeanLifespan',
                                                                          'MeanLifespan_s',
                                                                          #'MeanTrophicLevel',
                                                                          'MeanTrophicLevel_s',
                                                                          #'meanTrophicLevelCommunityLengthALLBIOMASS',
                                                                          #'meanTrophicLevelCommunityLengthALLBIOMASS_s',
                                                                          #'MMLengthAbundance',
                                                                          'MMLengthAbundance_s',
                                                                          #'MMLengthBiomass',
                                                                          'MMLengthBiomass_s',
                                                                          #'PielouEvenness',
                                                                          'PielouEvenness_s',
                                                                          #'PropPredatoryFish',
                                                                          'PropPredatoryFish_s',
                                                                          #'ShannonDiversity',
                                                                          'ShannonDiversity_s',
                                                                          #'SpeciesRichness',
                                                                          'SpeciesRichness_s', 
                                                                          #'CCLargeBenthivore',
                                                                          'CCLargeBenthivore_s',
                                                                          #'CCMediumBenthivore',
                                                                          'CCMediumBenthivore_s',
                                                                          #'CCPiscivore',
                                                                          'CCPiscivore_s',
                                                                          #'CCPlanktivore',
                                                                          'CCPlanktivore_s',
                                                                          #'CCZoopiscivore',
                                                                          'CCZoopiscivore_s',
                                                                          'CommunityCondition_s'
                                                                          #'CommunityCondition'
                                                                          ))



          ### Clusters and singletons identified in the redundancy analysis at large scales ###

# # C1
meltC1 <- melt(IndiQ_SS,id=c('YEAR', 'ID'), measure = c('Biomass_s',
                                                        'BiomassClupeids_s',
                                                        'BiomassFinfish_s',
                                                        'BiomassForage_s',
                                                        'BiomassTL3_s',
                                                        'BPelagicToDemersal_s',
                                                        'BTGPlanktivore_s'))

meltC1_NAFO <- melt(IndiQ_NAFO,id=c('YEAR', 'ID'), measure = c('Biomass_s',
                                                               'BiomassClupeids_s',
                                                               'BiomassFinfish_s',
                                                               'BiomassForage_s',
                                                               'BiomassTL3_s',
                                                               'BPelagicToDemersal_s',
                                                               'BTGPlanktivore_s'))

meltC1_4VS <- melt(IndiQ_NAFO_4vs,id=c('YEAR', 'ID'), measure = c('Biomass_s',
                                                                  'BiomassClupeids_s',
                                                                  'BiomassFinfish_s',
                                                                  'BiomassForage_s',
                                                                  'BiomassTL3_s',
                                                                  'BPelagicToDemersal_s',
                                                                  'BTGPlanktivore_s'))

meltC1_4VS_2 <- melt(IndiQ_NAFO_4vs,id=c('YEAR', 'ID'), measure = c('Biomass_s',
                                                                  'BiomassClupeids_s'))

#  C2
meltC2 <- melt(IndiQ_SS,id=c('YEAR', 'ID'), measure = c("MargalefRichness_s",
                                                        "SpeciesRichness_s"))

meltC2_NAFO <- melt(IndiQ_NAFO,id=c('YEAR', 'ID'), measure = c("MargalefRichness_s",
                                                               "SpeciesRichness_s"))


# #  C3
meltC3 <- melt(IndiQ_SS,id=c('YEAR', 'ID'), measure = c("HillN1Diversity_s",
                                                        "ShannonDiversity_s",
                                                        "HillN2Dominance_s"))

meltC3_NAFO <- melt(IndiQ_NAFO,id=c('YEAR', 'ID'), measure = c("HillN1Diversity_s",
                                                               "ShannonDiversity_s",
                                                               "HillN2Dominance_s"))

# #  C4
meltC4 <- melt(IndiQ_SS,id=c('YEAR', 'ID'), measure = c("LargeFishIndicator_s",
                                                        "LargeSpeciesIndicator_s",
                                                        "PropPredatoryFish_s",
                                                        "MeanLengthBiomass_s",
                                                        "MMLengthBiomass_s"))

meltC4_NAFO <- melt(IndiQ_NAFO,id=c('YEAR', 'ID'), measure = c("LargeFishIndicator_s",
                                                               "LargeSpeciesIndicator_s",
                                                               "PropPredatoryFish_s",
                                                               "MeanLengthBiomass_s",
                                                               "MMLengthBiomass_s"))

# #  C5
meltC5 <- melt(IndiQ_SS,id=c('YEAR', 'ID'), measure = c("BiomassGadoids_s",
                                                        "BiomassGroundfish_s",
                                                        "BiomassTL4_s",
                                                        "BTGPiscivore_s"))

meltC5_NAFO <- melt(IndiQ_NAFO,id=c('YEAR', 'ID'), measure = c("BiomassGadoids_s",
                                                               "BiomassGroundfish_s",
                                                               "BiomassTL4_s",
                                                               "BTGPiscivore_s"))

# #  C6
meltC6 <- melt(IndiQ_SS,id=c('YEAR', 'ID'), measure = c("CommunityCondition_s",
                                                        "CCPlanktivore_s"))
meltC6_NAFO <- melt(IndiQ_NAFO,id=c('YEAR', 'ID'), measure = c("CommunityCondition_s",
                                                               "CCPlanktivore_s"))


# #  C7
meltC7 <- melt(IndiQ_SS,id=c('YEAR', 'ID'), measure = c('Heips_s', 'PielouEvenness_s'))
meltC7_NAFO <- melt(IndiQ_NAFO,id=c('YEAR', 'ID'), measure = c('Heips_s', 'PielouEvenness_s'))



# #  C8
meltC8 <- melt(IndiQ_SS,id=c('YEAR', 'ID'), measure = c("LClupeids.L_s",
                                                        "LForageFish.L_s"))
meltC8_NAFO <- melt(IndiQ_NAFO,id=c('YEAR', 'ID'), measure = c("LClupeids.L_s",
                                                               "LForageFish.L_s"))

# #  C9
meltC9 <- melt(IndiQ_SS,id=c('YEAR', 'ID'), measure = c("Landings.L_s",
                                                        "LFinfish.L_s",
                                                        "LGroundfish.L_s",
                                                        "LGadoids.L_s",
                                                        "FPGroundfish.L_s",
                                                        "FPGadoids.L_s"))
meltC9_NAFO <- melt(IndiQ_NAFO,id=c('YEAR', 'ID'), measure = c("Landings.L_s",
                                                               "LFinfish.L_s",
                                                               "LGroundfish.L_s",
                                                               "LGadoids.L_s",
                                                               "FPGroundfish.L_s",
                                                               "FPGadoids.L_s"))


# #  C10
meltC10 <- melt(IndiQ_SS,id=c('YEAR', 'ID'), measure = c("DiversityTargetSpp.L_s",
                                                         "LInvertebrates.L_s"))

meltC10_NAFO <- melt(IndiQ_NAFO,id=c('YEAR', 'ID'), measure = c("DiversityTargetSpp.L_s",
                                                                "LInvertebrates.L_s"))


# #  C11
meltC11 <- melt(IndiQ_SS,id=c('YEAR', 'ID'), measure = c("FishingPressure.L_s",
                                                         "FPFinfish.L_s"))

meltC11_NAFO <- melt(IndiQ_NAFO,id=c('YEAR', 'ID'), measure = c("FishingPressure.L_s",
                                                                "FPFinfish.L_s"))



# #  C12
meltC12 <- melt(IndiQ_SS,id=c('YEAR', 'ID'), measure = c("FPClupeids.L_s",
                                                         "FPForageFish.L_s"))
meltC12_NAFO <- melt(IndiQ_NAFO,id=c('YEAR', 'ID'), measure = c("FPClupeids.L_s",
                                                                "FPForageFish.L_s"))



# #  C13
meltC13 <- melt(IndiQ_SS,id=c('YEAR', 'ID'), measure = c("FPSkates.L_s",
                                                         "LSkates.L_s"))
meltC13_NAFO <- melt(IndiQ_NAFO,id=c('YEAR', 'ID'), measure = c("FPSkates.L_s",
                                                                "LSkates.L_s"))

# #  C14
meltC14 <- melt(IndiQ_SS,id=c('YEAR', 'ID'), measure = c("FPFlatfish.L_s",
                                                         "LFlatfish.L_s"))
meltC14_NAFO <- melt(IndiQ_NAFO,id=c('YEAR', 'ID'), measure = c("FPFlatfish.L_s",
                                                                "LFlatfish.L_s"))

# # Singletons
meltS1 <- melt(IndiQ_SS,id=c('YEAR', 'ID'), measure = c("MargalefGroundfish_s",
                                                        "KemptonQ_s"))
meltS1_NAFO <- melt(IndiQ_NAFO,id=c('YEAR', 'ID'), measure = c("MargalefGroundfish_s",
                                                               "KemptonQ_s"))

meltS2 <- melt(IndiQ_SS,id=c('YEAR', 'ID'), measure = c("BTGLargeBenthivore_s",
                                                        "BTGMediumBenthivore_s",
                                                        "BTGZoopiscivore_s"))
meltS2_NAFO <- melt(IndiQ_NAFO,id=c('YEAR', 'ID'), measure = c("BTGLargeBenthivore_s",
                                                               "BTGMediumBenthivore_s",
                                                               "BTGZoopiscivore_s"))

meltS3 <- melt(IndiQ_SS,id=c('YEAR', 'ID'), measure = c("CCMediumBenthivore_s",
                                                        "CCPiscivore_s",
                                                        "CCZoopiscivore_s",
                                                        "CCLargeBenthivore_s"))
meltS3_NAFO <- melt(IndiQ_NAFO,id=c('YEAR', 'ID'), measure = c("CCMediumBenthivore_s",
                                                               "CCPiscivore_s",
                                                               "CCZoopiscivore_s",
                                                               "CCLargeBenthivore_s"))

meltS4 <- melt(IndiQ_SS,id=c('YEAR', 'ID'), measure = c("BInvertebrateToDemersal_s",
                                                        "MeanLengthAbundance_s",
                                                        "MeanTrophicLevel_s"))
meltS4_NAFO <- melt(IndiQ_NAFO,id=c('YEAR', 'ID'), measure = c("BInvertebrateToDemersal_s",
                                                               "MeanLengthAbundance_s",
                                                               "MeanTrophicLevel_s"))

meltS5 <- melt(IndiQ_SS,id=c('YEAR', 'ID'), measure = c("MeanLifespan_s",
                                                        "MMLengthAbundance_s",
                                                        "Intrinsicvulnerabilityindex.L_s",
                                                        "BiomassTL2_s",
                                                        "InverseCVBiomass_s"))
meltS5_NAFO <- melt(IndiQ_NAFO,id=c('YEAR', 'ID'), measure = c("MeanLifespan_s",
                                                               "MMLengthAbundance_s",
                                                               "Intrinsicvulnerabilityindex.L_s",
                                                               "BiomassTL2_s",
                                                               "InverseCVBiomass_s"))

meltS6 <- melt(IndiQ_SS,id=c('YEAR', 'ID'), measure = c("BiomassFlatfish_s",
                                                        "BiomassInvertebrates_s",
                                                        "BiomassSkates_s",
                                                        "FishinginBalance.L_s"))
meltS6_NAFO <- melt(IndiQ_NAFO,id=c('YEAR', 'ID'), measure = c("BiomassFlatfish_s",
                                                               "BiomassInvertebrates_s",
                                                               "BiomassSkates_s",
                                                               "FishinginBalance.L_s"))

meltS7 <- melt(IndiQ_SS,id=c('YEAR', 'ID'), measure = c("MeanTrophicLevel.L_s",
                                                        "MarineTrophicIndex.L_s",
                                                        "LLargePelagic.L_s",
                                                        "FPInvertebrates.L_s"))
meltS7_NAFO <- melt(IndiQ_NAFO,id=c('YEAR', 'ID'), measure = c("FPInvertebrates.L_s",
                                                               "MeanTrophicLevel.L_s",
                                                               "MarineTrophicIndex.L_s",
                                                               "LLargePelagic.L_s",
                                                               "FPInvertebrates.L_s"))


        ### Clusters and singletons identified in the redundancy analysis at large scales ###

#Cluster A
# 92%
# MargalefRichness
# SpeciesRichness

meltCa <- melt(Strata_Q,id=c('YEAR', 'ID'), measure = c("MargalefRichness_s",
                                                        "SpeciesRichness_s"))


#Cluster B
# 74%
# HillN1Diversity
# ShannonDiversity
# HillN2Dominance
# PielouEvenness


meltCb <- melt(Strata_Q,id=c('YEAR', 'ID'), measure = c("HillN1Diversity_s",
                                                        "ShannonDiversity_s",
                                                        "HillN2Dominance_s",
                                                        "PielouEvenness_s"))



#Cluster B + HEIPS
# 74%
# HillN1Diversity
# ShannonDiversity
# HillN2Dominance
# PielouEvenness


meltCb_Heips <- melt(Strata_Q,id=c('YEAR', 'ID'), measure = c("HillN1Diversity_s",
                                                              "ShannonDiversity_s",
                                                              "HillN2Dominance_s",
                                                              "PielouEvenness_s",
                                                              "Heips_s"))


Cb_Heips_Cor.L <- subset(Strata_Q, select = c("HillN1Diversity_s",  #Subset used for the correlation analysis: Dominance & diversity indi strata.pdf
                                   "ShannonDiversity_s",
                                   "HillN2Dominance_s",
                                   "PielouEvenness_s",
                                   "Heips_s"))



#Cluster C
# 65%
# BiomassClupeids*
# BiomassForage*
# BPelagicToDemersal*
# BTGPlanktivore*
# Biomass*
# BiomassFinfish*
# BiomassTL3*

meltCc <- melt(Strata_Q,id=c('YEAR', 'ID'), measure = c('Biomass_s',
                                                        'BiomassClupeids_s',
                                                        'BiomassFinfish_s',
                                                        'BiomassForage_s',
                                                        'BiomassTL3_s',
                                                        'BPelagicToDemersal_s',
                                                        'BTGPlanktivore_s'))


meltCc_4vs <- melt(Strata_Q_4vs,id=c('YEAR', 'ID'), measure = c('Biomass_s',
                                                                     'BiomassClupeids_s',
                                                                     'BiomassFinfish_s',
                                                                     'BiomassForage_s',
                                                                     'BiomassTL3_s',
                                                                     'BPelagicToDemersal_s',
                                                                     'BTGPlanktivore_s'))
#Cluster D
# 52%
# BiomassFlatfish
# BTGMediumBenthivore

meltCd <- melt(Strata_Q,id=c('YEAR', 'ID'), measure = c("BiomassFlatfish_s",
                                                        "BTGMediumBenthivore_s"))




#Cluster E
# 50%
# BiomassSkates
# BTGLargeBenthivore

meltCe <- melt(Strata_Q,id=c('YEAR', 'ID'), measure = c("BiomassSkates_s",
                                                        "BTGLargeBenthivore_s"))



#Cluster F
# 44%
# BiomassGadoids
# BiomassTL4
# BTGPiscivore
# BiomassGroundfish

meltCf <- melt(Strata_Q,id=c('YEAR', 'ID'), measure = c("BiomassGadoids_s",
                                                        "BiomassGroundfish_s",
                                                        "BiomassTL4_s",
                                                        "BTGPiscivore_s"))


# #Cluster G
# 38%
# CommunityCondition
# CCMediumBenthivore
# CCPiscivore

meltCg <- melt(Strata_Q,id=c('YEAR', 'ID'), measure = c("CommunityCondition_s",
                                                        "CCMediumBenthivore_s",
                                                        "CCPiscivore_s"))



# #Cluster H
# 37%
# MMLengthAbundance
# MMLengthBiomass
# PropPredatoryFish
# LargeFishIndicator
# MeanLengthBiomass
# LargeSpeciesIndicator
# MeanLengthAbundance

meltCh <- melt(Strata_Q,id=c('YEAR', 'ID'), measure = c("MMLengthAbundance_s",
                                                        "MeanLengthAbundance_s",
                                                        "LargeFishIndicator_s",
                                                        "LargeSpeciesIndicator_s",
                                                        "PropPredatoryFish_s",
                                                        "MeanLengthBiomass_s",
                                                        "MMLengthBiomass_s"))


# #Cluster I
# 21%
# MeanLifespan
# BTGZoopiscivore

meltCi <- melt(Strata_Q,id=c('YEAR', 'ID'), measure = c("MeanLifespan_s",
                                                        "BTGZoopiscivore_s"))


# #"Cluster" J
# 19%
# KemptonQ
# MargalefGroundfish

meltCj <- melt(Strata_Q,id=c('YEAR', 'ID'), measure = c("MargalefGroundfish_s",
                                                        "KemptonQ_s"))


# #Singletons
# InverseCVBiomass
# MeanTrophicLevel
# CCZoopiscivore
# BiomassTL2

meltS1_strata <- melt(Strata_Q,id=c('YEAR', 'ID'), measure = c("InverseCVBiomass_s",
                                                               "MeanTrophicLevel_s",
                                                               "CCZoopiscivore_s",
                                                               "BiomassTL2_s"))



melt_missingIndi <- melt(Strata_Q,id=c('YEAR', 'ID'), measure = c(#"BiomassInvertebrates_s",
  #"BInvertebrateToDemersal_s",
  "CCPlanktivore_s"))#,
#"Heips_s"))


          ### Final suite of indicators selected (for the shelf and NAFO scales) ###
Suite <- melt(LargeScales ,id=c('YEAR', 'ID'), measure = c('MargalefRichness_s',
                                                           'ShannonDiversity_s',
                                                           'PielouEvenness_s',
                                                           'LargeFishIndicator_s',
                                                           #'MeanLengthAbundance_s',
                                                           'CommunityCondition_s',
                                                           'CCMediumBenthivore_s',
                                                           'CCPiscivore_s',
                                                           'CCZoopiscivore_s',
                                                           'CCLargeBenthivore_s',
                                                           'MeanTrophicLevel_s',
                                                           'BTGLargeBenthivore_s',
                                                           'BTGMediumBenthivore_s',
                                                           'BTGZoopiscivore_s',
                                                           'MeanLifespan_s',
                                                           'Intrinsicvulnerabilityindex.L_s',
                                                           'InverseCVBiomass_s',
                                                           'BiomassForage_s',
                                                           'BiomassGroundfish_s',
                                                           'BiomassFlatfish_s',
                                                           'BiomassInvertebrates_s',
                                                           'BiomassSkates_s',
                                                           'FishinginBalance.L_s',
                                                           'FishingPressure.L_s',
                                                           'FPForageFish.L_s',
                                                           'MeanTrophicLevel.L_s',
                                                           'MarineTrophicIndex.L_s',
                                                           'DiversityTargetSpp.L_s',
                                                           'Landings.L_s',
                                                           'LForageFish.L_s',
                                                           'LInvertebrates.L_s',
                                                           'LSkates.L_s',
                                                           'LFlatfish.L_s',
                                                           'LLargePelagic.L_s'))


Suite_ESSvsWSS <- melt(Shelf_Q ,id=c('YEAR', 'ID'), measure = c('MargalefRichness_s',
                                                                'ShannonDiversity_s',
                                                                'PielouEvenness_s',
                                                                'LargeFishIndicator_s',
                                                                #'MeanLengthAbundance_s',
                                                                'CommunityCondition_s',
                                                                'CCMediumBenthivore_s',
                                                                'CCPiscivore_s',
                                                                'CCZoopiscivore_s',
                                                                'CCLargeBenthivore_s',
                                                                'MeanTrophicLevel_s',
                                                                'BTGLargeBenthivore_s',
                                                                'BTGMediumBenthivore_s',
                                                                'BTGZoopiscivore_s',
                                                                'MeanLifespan_s',
                                                                'Intrinsicvulnerabilityindex.L_s',
                                                                'InverseCVBiomass_s',
                                                                'BiomassForage_s',
                                                                'BiomassGroundfish_s',
                                                                'BiomassFlatfish_s',
                                                                'BiomassInvertebrates_s',
                                                                'BiomassSkates_s',
                                                                'FishinginBalance.L_s',
                                                                'FishingPressure.L_s',
                                                                'FPForageFish.L_s',
                                                                'MeanTrophicLevel.L_s',
                                                                'MarineTrophicIndex.L_s',
                                                                'DiversityTargetSpp.L_s',
                                                                'Landings.L_s',
                                                                'LForageFish.L_s',
                                                                'LInvertebrates.L_s',
                                                                'LSkates.L_s',
                                                                'LFlatfish.L_s',
                                                                'LLargePelagic.L_s'))

Diversity_Indi <- melt(Shelf_Q ,id=c('YEAR', 'ID'), measure = c('MargalefRichness_s',
                                                                'ShannonDiversity_s',
                                                                'PielouEvenness_s'))

Biomass1_Indi <- melt(Shelf_Q ,id=c('YEAR', 'ID'), measure = c('BTGLargeBenthivore_s',
                                                              'BTGMediumBenthivore_s',
                                                              'BTGZoopiscivore_s',
                                                              'BiomassForage_s',
                                                              'BiomassGroundfish_s'))

Biomass2_Indi <- melt(Shelf_Q ,id=c('YEAR', 'ID'), measure = c('BiomassFlatfish_s',
                                                              'BiomassInvertebrates_s',
                                                              'BiomassSkates_s',
                                                              'InverseCVBiomass_s'))

Size_Indi <- melt(Shelf_Q ,id=c('YEAR', 'ID'), measure = c('LargeFishIndicator_s',
                                                           #'MeanLengthAbundance_s',
                                                           'MeanTrophicLevel_s',
                                                           'MeanLifespan_s'))


CC_Indi <- melt(Shelf_Q ,id=c('YEAR', 'ID'), measure = c('CommunityCondition_s',
                                                         'CCMediumBenthivore_s',
                                                         'CCPiscivore_s',
                                                         'CCZoopiscivore_s',
                                                         'CCLargeBenthivore_s'))

Pressure1_Indi<- melt(Shelf_Q ,id=c('YEAR', 'ID'), measure = c('Intrinsicvulnerabilityindex.L_s',
                                                                'FishinginBalance.L_s',
                                                                'FishingPressure.L_s',
                                                                'FPForageFish.L_s',
                                                                'MeanTrophicLevel.L_s',
                                                                'Landings.L_s',
                                                                'LForageFish.L_s'))

Pressure2_Indi <- melt(Shelf_Q ,id=c('YEAR', 'ID'), measure = c('MarineTrophicIndex.L_s',
                                                                'DiversityTargetSpp.L_s',
                                                                'LInvertebrates.L_s',
                                                                'LSkates.L_s',
                                                                'LFlatfish.L_s',
                                                                'LLargePelagic.L_s'))







ESS <- Shelf_Q[Shelf_Q$ID %in% c('ESS'), ]
ESS <- subset(ESS, select = -c(YEAR, ID) )
ESS_Cor<- subset(ESS, select = c('MargalefRichness_s',
                                 'ShannonDiversity_s',
                                 'PielouEvenness_s',
                                 'LargeFishIndicator_s',
                                 #'MeanLengthAbundance_s',
                                 'CommunityCondition_s',
                                 'CCMediumBenthivore_s',
                                 'CCPiscivore_s',
                                 'CCZoopiscivore_s',
                                 'CCLargeBenthivore_s',
                                 'MeanTrophicLevel_s',
                                 'InverseCVBiomass_s',
                                 'BiomassForage_s',
                                 'BiomassGroundfish_s',
                                 'BiomassFlatfish_s',
                                 'BiomassInvertebrates_s',
                                 'BiomassSkates_s',
                                 'BTGLargeBenthivore_s',
                                 'BTGMediumBenthivore_s',
                                 'BTGZoopiscivore_s',
                                 'MeanLifespan_s'))


ESS_Cor.L<- subset(ESS, select = c('Intrinsicvulnerabilityindex.L_s',
                                   'FishinginBalance.L_s',
                                   'FishingPressure.L_s',
                                   'FPForageFish.L_s',
                                   'MeanTrophicLevel.L_s',
                                   'MarineTrophicIndex.L_s',
                                   'DiversityTargetSpp.L_s',
                                   'Landings.L_s',
                                   'LForageFish.L_s',
                                   'LInvertebrates.L_s',
                                   'LSkates.L_s',
                                   'LFlatfish.L_s',
                                   'LLargePelagic.L_s'))

WSS <- Shelf_Q[Shelf_Q$ID %in% c('WSS'), ]
WSS <- subset(WSS, select = -c(YEAR, ID) )
WSS_Cor<- subset(WSS, select = c('MargalefRichness_s',
                                 'ShannonDiversity_s',
                                 'PielouEvenness_s',
                                 'LargeFishIndicator_s',
                                 'MeanLengthAbundance_s',
                                 'CommunityCondition_s',
                                 'CCMediumBenthivore_s',
                                 'CCPiscivore_s',
                                 'CCZoopiscivore_s',
                                 'CCLargeBenthivore_s',
                                 'MeanTrophicLevel_s',
                                 'InverseCVBiomass_s',
                                 'BiomassForage_s',
                                 'BiomassGroundfish_s',
                                 'BiomassFlatfish_s',
                                 'BiomassInvertebrates_s',
                                 'BiomassSkates_s',
                                 'BTGLargeBenthivore_s',
                                 'BTGMediumBenthivore_s',
                                 'BTGZoopiscivore_s',
                                 'MeanLifespan_s'))

WSS_Cor.L<- subset(WSS, select = c('Intrinsicvulnerabilityindex.L_s',
                                   'FishinginBalance.L_s',
                                   'FishingPressure.L_s',
                                   'FPForageFish.L_s',
                                   'MeanTrophicLevel.L_s',
                                   'MarineTrophicIndex.L_s',
                                   'DiversityTargetSpp.L_s',
                                   'Landings.L_s',
                                   'LForageFish.L_s',
                                   'LInvertebrates.L_s',
                                   'LSkates.L_s',
                                   'LFlatfish.L_s',
                                   'LLargePelagic.L_s'))

### Filer for the strata scale



Suite_strata <- melt(Strata_Q,id=c('YEAR', 'ID'), measure = c('MargalefRichness_s',
                                                              'ShannonDiversity_s',
                                                              'PielouEvenness_s',
                                                              'LargeFishIndicator_s',
                                                              'MeanLengthAbundance_s',
                                                              'CommunityCondition_s',
                                                              'CCMediumBenthivore_s',
                                                              'CCPiscivore_s',
                                                              'CCZoopiscivore_s',
                                                              'CCLargeBenthivore_s',
                                                              'MeanTrophicLevel_s',
                                                              'BTGLargeBenthivore_s',
                                                              'BTGMediumBenthivore_s',
                                                              'BTGZoopiscivore_s',
                                                              'MeanLifespan_s',
                                                              'InverseCVBiomass_s',
                                                              'BiomassForage_s',
                                                              'BiomassGroundfish_s',
                                                              'BiomassFlatfish_s',
                                                              'BiomassInvertebrates_s',
                                                              'BiomassSkates_s'))


##
Strata_Q_440 <- Strata_Q[Strata_Q$ID %in% c('440'), ]
Strata_Q_441 <- Strata_Q[Strata_Q$ID %in% c('441'), ]
Strata_Q_442 <- Strata_Q[Strata_Q$ID %in% c('442'), ]
Strata_Q_443 <- Strata_Q[Strata_Q$ID %in% c('443'), ]
Strata_Q_444 <- Strata_Q[Strata_Q$ID %in% c('444'), ]
Strata_Q_445 <- Strata_Q[Strata_Q$ID %in% c('445'), ]
Strata_Q_446 <- Strata_Q[Strata_Q$ID %in% c('446'), ]
Strata_Q_447 <- Strata_Q[Strata_Q$ID %in% c('447'), ]
Strata_Q_448 <- Strata_Q[Strata_Q$ID %in% c('448'), ]
Strata_Q_449 <- Strata_Q[Strata_Q$ID %in% c('449'), ]
Strata_Q_450 <- Strata_Q[Strata_Q$ID %in% c('450'), ]
Strata_Q_451 <- Strata_Q[Strata_Q$ID %in% c('451'), ]
Strata_Q_452 <- Strata_Q[Strata_Q$ID %in% c('452'), ]
Strata_Q_453 <- Strata_Q[Strata_Q$ID %in% c('453'), ]
Strata_Q_454 <- Strata_Q[Strata_Q$ID %in% c('454'), ]
Strata_Q_455 <- Strata_Q[Strata_Q$ID %in% c('455'), ]
Strata_Q_456 <- Strata_Q[Strata_Q$ID %in% c('456'), ]
Strata_Q_457 <- Strata_Q[Strata_Q$ID %in% c('457'), ]
Strata_Q_458 <- Strata_Q[Strata_Q$ID %in% c('458'), ]
Strata_Q_459 <- Strata_Q[Strata_Q$ID %in% c('459'), ]
Strata_Q_460 <- Strata_Q[Strata_Q$ID %in% c('460'), ]
Strata_Q_461 <- Strata_Q[Strata_Q$ID %in% c('461'), ]
Strata_Q_462 <- Strata_Q[Strata_Q$ID %in% c('462'), ]
Strata_Q_463 <- Strata_Q[Strata_Q$ID %in% c('463'), ]
Strata_Q_464 <- Strata_Q[Strata_Q$ID %in% c('464'), ]
Strata_Q_465 <- Strata_Q[Strata_Q$ID %in% c('465'), ]
Strata_Q_466 <- Strata_Q[Strata_Q$ID %in% c('466'), ]
Strata_Q_470 <- Strata_Q[Strata_Q$ID %in% c('470'), ]
Strata_Q_471 <- Strata_Q[Strata_Q$ID %in% c('471'), ]
Strata_Q_472 <- Strata_Q[Strata_Q$ID %in% c('472'), ]
Strata_Q_473 <- Strata_Q[Strata_Q$ID %in% c('473'), ]
Strata_Q_474 <- Strata_Q[Strata_Q$ID %in% c('474'), ]
Strata_Q_475 <- Strata_Q[Strata_Q$ID %in% c('475'), ]
Strata_Q_476 <- Strata_Q[Strata_Q$ID %in% c('476'), ]
Strata_Q_477 <- Strata_Q[Strata_Q$ID %in% c('477'), ]
Strata_Q_478 <- Strata_Q[Strata_Q$ID %in% c('478'), ]
Strata_Q_480 <- Strata_Q[Strata_Q$ID %in% c('480'), ]
Strata_Q_481 <- Strata_Q[Strata_Q$ID %in% c('481'), ]
Strata_Q_482 <- Strata_Q[Strata_Q$ID %in% c('482'), ]
Strata_Q_483 <- Strata_Q[Strata_Q$ID %in% c('483'), ]
Strata_Q_484 <- Strata_Q[Strata_Q$ID %in% c('484'), ]
Strata_Q_485 <- Strata_Q[Strata_Q$ID %in% c('485'), ]
Strata_Q_490 <- Strata_Q[Strata_Q$ID %in% c('490'), ]
Strata_Q_491 <- Strata_Q[Strata_Q$ID %in% c('491'), ] 
Strata_Q_492 <- Strata_Q[Strata_Q$ID %in% c('492'), ]
Strata_Q_493 <- Strata_Q[Strata_Q$ID %in% c('493'), ]
Strata_Q_494 <- Strata_Q[Strata_Q$ID %in% c('494'), ]
Strata_Q_495 <- Strata_Q[Strata_Q$ID %in% c('495'), ]

#List of all indicators - this lists are used with PlotIndi functions

AllIndi <-  list(meltC1, meltC1_NAFO, meltC2, meltC2_NAFO, meltC3, meltC3_NAFO, meltC4,meltC4_NAFO, meltC5, meltC5_NAFO, meltC6, meltC6_NAFO, meltC7, meltC7_NAFO, meltC8, meltC8_NAFO, meltC9, meltC9_NAFO, meltC10, meltC10_NAFO, meltC11, meltC11_NAFO, meltC12,  meltC12_NAFO, meltC13,  meltC13_NAFO, meltC14,  meltC14_NAFO,
                 meltS1, meltS1_NAFO, meltS2, meltS2_NAFO, meltS3,  meltS3_NAFO, meltS4, meltS4_NAFO, meltS5, meltS5_NAFO, meltS6, meltS6_NAFO, meltS7, meltS7_NAFO)

AllIndi_strata <-  list(meltCa, meltCb, meltCb_Heips, meltCc, meltCd, meltCe, meltCf, meltCg, meltCh, meltCi, meltCj,
                        meltS1_strata, melt_missingIndi)



