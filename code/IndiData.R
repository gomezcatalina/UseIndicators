#  August 2016 
#  install.packages("devtools")
#  devtools::install_github("rstudio/packrat")
packrat::init()                     # -- https://rstudio.github.io/packrat/commands.html # Reads packages needed for plots and analysis
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
library(zoo)
library(imputeTS)
#system.file(package="ggplot2")      # This checks that you are using private library via packratget
require(devtools) 
require(bio.base)                    # install_github('Beothuk/bio.base') #compiled functions from me, jae and brad and mike mcmahon
require(bio.utilities)               # https://github.com/Beothuk/bio.utilities    #install_github('Beothuk/bio.utilities') #compiled functions from ACook, Jae, Brad and Mike Mcmahon
library(missForest)                  # randomForest approximation to fill in missing values....full dataset using relationships bwn variables to fill in NAs
source('code/IndiFunctions.R')       # Sources functions that set ggplot settings to plot indicators per each scale (Shelf, NAFO and strata scale)
source('code/keepdropcolumn.R')

# Part I.     ####  #   #  *****  Reads, filters and interpolates csv data of indicators extracted using A.Cook's package  *****  #  ####
# There are several NAs in the time-series data. The hierarchical cluster analysis 
# ignores entire rows (i.e. years) in place where there is a NA (it omits all the other data for the year of the missing value, therefore excluding the whole year from the analysis).
# In this script, we are ommitting indicators with > 25% of NA's in the time-series (BInvertebrateToDemersal; FPInvertebrates.L; InverseFPInvertebrates.L; BiomassInvertebrates) 
# and we interpolate missing data for indicators with missing data , but <=25%. 
# The invertebrate based indicators with missing data are also excluded 

#*********SHELF
SS <- read.csv("data/shelfsetq.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
SS$ID <- factor(SS$ID)
#SS <- SS[!SS$YEAR %in% "2015", ]
drop_s <- Select_PatternMatching("_s", SS)
drop <- c('InverseFishingPressure.L', 
          'InverseFPClupeids.L',  
          'InverseFPFinfish.L', 
          'InverseFPFlatfish.L', 
          'InverseFPForageFish.L',  
          'InverseFPGadoids.L', 
          'InverseFPGroundfish.L',
          'InverseFPInvertebrates.L',
          'InverseFPSkates.L', 
          'Abundance', 
          'MeanTrophicLevelStanza', 
          'BInvertebrateToDemersal', 
          'BiomassInvertebrates', 
          'FPInvertebrates.L')

dropIndi <- cbind(drop, drop_s)
SS <- KeepDrop(data=SS,cols=dropIndi, newdata=dt, drop=1)
SS_filtered <- KeepDrop(data=SS,cols=dropIndi, newdata=dt, drop=1)
SS_i <- missForest(SS_filtered)$ximp    
#write.csv(SS_i, "output/data/shelfsetq_filtered&interpolated.csv", row.names=FALSE)
colnames(SS_i) <- paste(colnames(SS_i), "i", sep = "_")
SS_plot <- cbind(SS_i, SS)

pdf('output/figures/shelfsetq_NAs.pdf', width=5,height=4)
PlotNAs(SS_plot)
dev.off()


#*********ESSWSS
Shelf_Q <- read.csv("data/esswsssetq.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
Shelf_Q$ID <- factor(Shelf_Q$ID)
##Shelf_Q <- Shelf_Q[!Shelf_Q$YEAR %in% "2015", ]
Shelf_Q_filtered <- KeepDrop(data=Shelf_Q,cols=dropIndi, newdata=dt, drop=1)
Shelf_i <- missForest(Shelf_Q_filtered)$ximp  
#write.csv(Shelf_i, "output/data/esswsssetq_filtered&interpolated.csv", row.names=FALSE)
colnames(Shelf_i) <- paste(colnames(Shelf_i), "i", sep = "_")
Shelf_plot <- cbind(Shelf_Q_filtered, Shelf_i)
WSS_Q_plot <- Shelf_plot[Shelf_plot$ID %in% c('WSS'), ]
ESS_Q_plot <- Shelf_plot[Shelf_plot$ID %in% c('ESS'), ]

ShelfIndi <- list(WSS_Q_plot, ESS_Q_plot) 

#pdf('output/figures/esswsssetq_NAs.pdf', width=5,height=4)
Plots_NAs_Shelf <- lapply(ShelfIndi, PlotNAs)
dev.off()


#********NAFO 
IndiQ_NAFO <- read.csv("data/nafosetq.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
#IndiQ_NAFO <- IndiQ_NAFO[!IndiQ_NAFO$YEAR %in% "2015", ]
IndiQ_NAFO$ID <- factor(IndiQ_NAFO$ID)
IndiQ_NAFO_filtered <- KeepDrop(data=IndiQ_NAFO,cols=dropIndi, newdata=dt, drop=1)
NAFO_i <- missForest(IndiQ_NAFO_filtered)$ximp 

NAFO_4W <- NAFO_i[NAFO_i$ID %in% c('4W'), ]
NAFO_4X <- NAFO_i[NAFO_i$ID %in% c('4X'), ]
NAFO_4VS <- NAFO_i[NAFO_i$ID %in% c('4VS'), ]
NAFO_4VS <- KeepDrop(data=NAFO_4VS,cols="CCPlanktivore BiomassClupeids", newdata=dt, drop=1)
NAFO_4VN <- NAFO_i[NAFO_i$ID %in% c('4VN'), ]
NAFO_4VN <- KeepDrop(data=NAFO_4VN,cols="BiomassTL2 CCPlanktivore", newdata=dt, drop=1)
  
  # write.csv(NAFO_4W, "output/data/nafo4Wsetq_filtered&interpolated.csv", row.names=FALSE)
  # write.csv(NAFO_4X, "output/data/nafo4Xsetq_filtered&interpolated.csv", row.names=FALSE)
  # write.csv(NAFO_4VS, "output/data/nafo4VSsetq_filtered&interpolated.csv", row.names=FALSE)
  # write.csv(NAFO_4VN, "output/data/nafo4VNsetq_filtered&interpolated.csv", row.names=FALSE)

colnames(NAFO_i) <- paste(colnames(NAFO_i), "i", sep = "_")

NAFO_plot <- cbind(IndiQ_NAFO_filtered, NAFO_i)

NAFO_4W_plot <- NAFO_plot[NAFO_plot$ID %in% c('4W'), ]
NAFO_4X_plot <- NAFO_plot[NAFO_plot$ID %in% c('4X'), ]
NAFO_4VS_plot <- NAFO_plot[NAFO_plot$ID %in% c('4VS'), ]
NAFO_4VN_plot <- NAFO_plot[NAFO_plot$ID %in% c('4VN'), ]

NafoIndi <- list(NAFO_4W_plot, NAFO_4X_plot, NAFO_4VN_plot, NAFO_4VS_plot) 

#pdf('output/figures/nafosetq_NAs.pdf', width=5, height=4)
Plots_NAs_NAFO <- lapply(NafoIndi, PlotNAs_NAFO)
dev.off()


#*********Strata scale
Strata_Q <- read.csv("data/stratsetq.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
#Strata_Q <- Strata_Q[!Strata_Q$YEAR %in% "2015", ]
Strata_Q$ID <- factor(Strata_Q$ID)
Strata_Q_filtered <- KeepDrop(data=Strata_Q,cols=dropIndi, newdata=dt, drop=1)
Strata_Q_filtered <- KeepDrop(data=Strata_Q_filtered,cols="BiomassTL2", newdata=dt, drop=1)
Strata_Q_filt_int <- missForest(Strata_Q_filtered)$ximp 

#Strata_Q_4vs <- Strata_Q[Strata_Q$ID %in% c('443','444', '445', '446', '447', '448', '449', '450', '451', '452'), ]

Strata_Q_440 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('440'), ]
Strata_Q_440 <- KeepDrop(data=Strata_Q_440,cols="BiomassClupeids BiomassForage BPelagicToDemersal BTGLargeBenthivore BTGPlanktivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
write.csv(Strata_Q_440, "output/data/stratasetq440_filt&int.csv", row.names=FALSE)

Strata_Q_441 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('441'), ]
Strata_Q_441 <- KeepDrop(data=Strata_Q_441,cols="BiomassClupeids BiomassForage BPelagicToDemersal BTGPlanktivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
write.csv(Strata_Q_441, "output/data/stratasetq441_filt&int.csv", row.names=FALSE)

Strata_Q_442 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('442'), ]
Strata_Q_442 <- KeepDrop(data=Strata_Q_442,cols="BiomassClupeids BTGLargeBenthivore BTGZoopiscivore CCLargeBenthivore CCPlanktivore CCZoopiscivore", newdata=dt, drop=1)
write.csv(Strata_Q_442, "output/data/stratasetq442_filt&int.csv", row.names=FALSE)

Strata_Q_443 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('443'), ]
Strata_Q_443 <- KeepDrop(data=Strata_Q_443,cols="BiomassClupeids BiomassForage BPelagicToDemersal BTGPlanktivore BTGZoopiscivore CCLargeBenthivore CCPlanktivore CCZoopiscivore", newdata=dt, drop=1)
write.csv(Strata_Q_443, "output/data/stratasetq443_filt&int.csv", row.names=FALSE)

Strata_Q_444 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('444'), ]
Strata_Q_444 <- KeepDrop(data=Strata_Q_444,cols="BiomassClupeids BiomassForage BPelagicToDemersal BTGPlanktivore CCPlanktivore", newdata=dt, drop=1)
write.csv(Strata_Q_444, "output/data/stratasetq444_filt&int.csv", row.names=FALSE)

Strata_Q_445 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('445'), ]
Strata_Q_445 <- KeepDrop(data=Strata_Q_445,cols="BiomassClupeids BTGLargeBenthivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
write.csv(Strata_Q_445, "output/data/stratasetq445_filt&int.csv", row.names=FALSE)

Strata_Q_446 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('446'), ]
Strata_Q_446 <- KeepDrop(data=Strata_Q_446,cols="BiomassClupeids BiomassForage BPelagicToDemersal BTGLargeBenthivore BTGPlanktivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
write.csv(Strata_Q_446, "output/data/stratasetq446_filt&int.csv", row.names=FALSE)

Strata_Q_447 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('447'), ]
Strata_Q_447 <- KeepDrop(data=Strata_Q_447,cols="BiomassClupeids BiomassForage BPelagicToDemersal BTGLargeBenthivore BTGPlanktivore BTGZoopiscivore CCLargeBenthivore CCPlanktivore CCZoopiscivore", newdata=dt, drop=1)
write.csv(Strata_Q_447, "output/data/stratasetq447_filt&int.csv", row.names=FALSE)

Strata_Q_448 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('448'), ]
Strata_Q_448 <- KeepDrop(data=Strata_Q_448,cols="BiomassClupeids BTGLargeBenthivore BTGZoopiscivore CCLargeBenthivore CCPlanktivore CCZoopiscivore", newdata=dt, drop=1)
write.csv(Strata_Q_448, "output/data/stratasetq448_filt&int.csv", row.names=FALSE)

Strata_Q_449 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('449'), ]
Strata_Q_449 <- KeepDrop(data=Strata_Q_449,cols="BiomassClupeids BiomassForage BPelagicToDemersal BTGLargeBenthivore BTGPlanktivore BTGZoopiscivore CCLargeBenthivore CCPlanktivore CCZoopiscivore", newdata=dt, drop=1)
write.csv(Strata_Q_449, "output/data/stratasetq449_filt&int.csv", row.names=FALSE)

Strata_Q_450 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('450'), ]
Strata_Q_450 <- KeepDrop(data=Strata_Q_450,cols="BiomassClupeids BiomassForage BPelagicToDemersal BTGLargeBenthivore BTGPlanktivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
write.csv(Strata_Q_450, "output/data/stratasetq450_filt&int.csv", row.names=FALSE)

Strata_Q_451 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('451'), ]
Strata_Q_451 <- KeepDrop(data=Strata_Q_451,cols="BiomassClupeids BTGLargeBenthivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
write.csv(Strata_Q_451, "output/data/stratasetq451_filt&int.csv", row.names=FALSE)

Strata_Q_452 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('452'), ]
Strata_Q_452 <- KeepDrop(data=Strata_Q_452,cols="BiomassClupeids BiomassForage BPelagicToDemersal BTGLargeBenthivore BTGPlanktivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
write.csv(Strata_Q_452, "output/data/stratasetq452_filt&int.csv", row.names=FALSE)

Strata_Q_453 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('453'), ]
Strata_Q_453 <- KeepDrop(data=Strata_Q_453,cols="BiomassClupeids BiomassForage BiomassSkates BPelagicToDemersal BTGLargeBenthivore BTGPlanktivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
write.csv(Strata_Q_453, "output/data/stratasetq453_filt&int.csv", row.names=FALSE)

Strata_Q_454 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('454'), ]
Strata_Q_454 <- KeepDrop(data=Strata_Q_454,cols="BiomassClupeids BiomassForage BPelagicToDemersal BTGLargeBenthivore BTGPlanktivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
write.csv(Strata_Q_454, "output/data/stratasetq454_filt&int.csv", row.names=FALSE)

Strata_Q_455 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('455'), ]
Strata_Q_455 <- KeepDrop(data=Strata_Q_455,cols="BiomassClupeids BTGLargeBenthivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
write.csv(Strata_Q_455, "output/data/stratasetq455_filt&int.csv", row.names=FALSE)

Strata_Q_456 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('456'), ]
Strata_Q_456 <- KeepDrop(data=Strata_Q_456,cols="BTGLargeBenthivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
write.csv(Strata_Q_456, "output/data/stratasetq456_filt&int.csv", row.names=FALSE)

Strata_Q_457 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('457'), ]
Strata_Q_457 <- KeepDrop(data=Strata_Q_457,cols="BiomassClupeids BTGLargeBenthivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
write.csv(Strata_Q_457, "output/data/stratasetq457_filt&int.csv", row.names=FALSE)

Strata_Q_458 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('458'), ]
Strata_Q_458 <- KeepDrop(data=Strata_Q_458,cols="BiomassClupeids BTGLargeBenthivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
write.csv(Strata_Q_458, "output/data/stratasetq458_filt&int.csv", row.names=FALSE)

Strata_Q_459 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('459'), ]
Strata_Q_459 <- KeepDrop(data=Strata_Q_459,cols="CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
write.csv(Strata_Q_459, "output/data/stratasetq459_filt&int.csv", row.names=FALSE)

Strata_Q_460 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('460'), ]
Strata_Q_460 <- KeepDrop(data=Strata_Q_460,cols="BiomassSkates BTGLargeBenthivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
write.csv(Strata_Q_460, "output/data/stratasetq460_filt&int.csv", row.names=FALSE)

Strata_Q_461 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('461'), ]
Strata_Q_461 <- KeepDrop(data=Strata_Q_461,cols="BiomassClupeids BiomassSkates BTGLargeBenthivore CCLargeBenthivore", newdata=dt, drop=1)
write.csv(Strata_Q_461, "output/data/stratasetq461_filt&int.csv", row.names=FALSE)

Strata_Q_462 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('462'), ]
Strata_Q_462 <- KeepDrop(data=Strata_Q_462,cols="BiomassSkates BTGLargeBenthivore CCLargeBenthivore", newdata=dt, drop=1)
write.csv(Strata_Q_462, "output/data/stratasetq462_filt&int.csv", row.names=FALSE)

Strata_Q_463 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('463'), ]
Strata_Q_463 <- KeepDrop(data=Strata_Q_463,cols="BiomassClupeids BiomassForage BiomassSkates BPelagicToDemersal BTGLargeBenthivore BTGPlanktivore BTGZoopiscivore CCLargeBenthivore CCPlanktivore CCZoopiscivore", newdata=dt, drop=1)
write.csv(Strata_Q_463, "output/data/stratasetq463_filt&int.csv", row.names=FALSE)

Strata_Q_464 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('464'), ]
Strata_Q_464 <- KeepDrop(data=Strata_Q_464,cols="BiomassClupeids BTGLargeBenthivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
write.csv(Strata_Q_464, "output/data/stratasetq464_filt&int.csv", row.names=FALSE)

Strata_Q_465 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('465'), ]
Strata_Q_465 <- KeepDrop(data=Strata_Q_465,cols="BiomassClupeids BTGLargeBenthivore CCLargeBenthivore", newdata=dt, drop=1)
write.csv(Strata_Q_465, "output/data/stratasetq465_filt&int.csv", row.names=FALSE)

Strata_Q_466 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('466'), ]
Strata_Q_466 <- KeepDrop(data=Strata_Q_466,cols="BiomassClupeids BiomassForage BiomassSkates BPelagicToDemersal BTGLargeBenthivore BTGPlanktivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
write.csv(Strata_Q_466, "output/data/stratasetq466_filt&int.csv", row.names=FALSE)

Strata_Q_470 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('470'), ]
Strata_Q_470 <- KeepDrop(data=Strata_Q_470,cols="BTGLargeBenthivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
write.csv(Strata_Q_470, "output/data/stratasetq470_filt&int.csv", row.names=FALSE)

Strata_Q_471 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('471'), ]
Strata_Q_471 <- KeepDrop(data=Strata_Q_471,cols="BiomassClupeids BiomassSkates BTGLargeBenthivore CCLargeBenthivore", newdata=dt, drop=1)
write.csv(Strata_Q_471, "output/data/stratasetq471_filt&int.csv", row.names=FALSE)

Strata_Q_472 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('472'), ]
Strata_Q_472 <- KeepDrop(data=Strata_Q_472,cols="BiomassClupeids BiomassSkates BTGLargeBenthivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
write.csv(Strata_Q_472, "output/data/stratasetq472_filt&int.csv", row.names=FALSE)

Strata_Q_473 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('473'), ]
Strata_Q_473 <- KeepDrop(data=Strata_Q_473,cols="BiomassClupeids BiomassFlatfish BiomassForage BiomassSkates BPelagicToDemersal BTGLargeBenthivore BTGPlanktivore BTGZoopiscivore CCLargeBenthivore CCPlanktivore CCZoopiscivore", newdata=dt, drop=1)
write.csv(Strata_Q_473, "output/data/stratasetq473_filt&int.csv", row.names=FALSE)

Strata_Q_474 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('474'), ]
Strata_Q_474 <- KeepDrop(data=Strata_Q_474,cols="BiomassClupeids BiomassForage BiomassSkates BPelagicToDemersal BTGLargeBenthivore BTGPlanktivore BTGZoopiscivore CCLargeBenthivore CCPlanktivore CCZoopiscivore", newdata=dt, drop=1)
write.csv(Strata_Q_474, "output/data/stratasetq474_filt&int.csv", row.names=FALSE)

Strata_Q_475 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('475'), ]
Strata_Q_475 <- KeepDrop(data=Strata_Q_475,cols="BiomassClupeids BiomassForage BiomassSkates BPelagicToDemersal BTGPlanktivore BTGZoopiscivore CCLargeBenthivore CCPlanktivore CCZoopiscivore", newdata=dt, drop=1)
write.csv(Strata_Q_475, "output/data/stratasetq475_filt&int.csv", row.names=FALSE)

Strata_Q_476 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('476'), ]
Strata_Q_476 <- KeepDrop(data=Strata_Q_476,cols="BiomassClupeids BiomassForage BPelagicToDemersal BTGLargeBenthivore BTGPlanktivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
write.csv(Strata_Q_476, "output/data/stratasetq476_filt&int.csv", row.names=FALSE)

Strata_Q_477 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('477'), ]
Strata_Q_477 <- KeepDrop(data=Strata_Q_477,cols="BiomassClupeids BiomassForage BPelagicToDemersal BTGLargeBenthivore BTGPlanktivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
write.csv(Strata_Q_477, "output/data/stratasetq477_filt&int.csv", row.names=FALSE)

Strata_Q_478 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('478'), ]
Strata_Q_478 <- KeepDrop(data=Strata_Q_478,cols="BiomassClupeids BiomassSkates BTGLargeBenthivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
write.csv(Strata_Q_478, "output/data/stratasetq478_filt&int.csv", row.names=FALSE)

Strata_Q_480 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('480'), ]
Strata_Q_480 <- KeepDrop(data=Strata_Q_480,cols="BiomassClupeids BiomassForage BPelagicToDemersal BTGPlanktivore BTGZoopiscivore CCPlanktivore CCZoopiscivore", newdata=dt, drop=1)
write.csv(Strata_Q_480, "output/data/stratasetq480_filt&int.csv", row.names=FALSE)

Strata_Q_481 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('481'), ]
Strata_Q_481 <- KeepDrop(data=Strata_Q_481,cols="BiomassClupeids CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
write.csv(Strata_Q_481, "output/data/stratasetq481_filt&int.csv", row.names=FALSE)

Strata_Q_482 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('482'), ]
Strata_Q_482 <- KeepDrop(data=Strata_Q_482,cols="BiomassClupeids BiomassFlatfish BTGLargeBenthivore CCLargeBenthivore", newdata=dt, drop=1)
write.csv(Strata_Q_482, "output/data/stratasetq482_filt&int.csv", row.names=FALSE)

Strata_Q_483 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('483'), ]
Strata_Q_483 <- KeepDrop(data=Strata_Q_483,cols="BiomassClupeids BTGLargeBenthivore CCLargeBenthivore", newdata=dt, drop=1)
write.csv(Strata_Q_483, "output/data/stratasetq483_filt&int.csv", row.names=FALSE)

Strata_Q_484 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('484'), ]
Strata_Q_484 <- KeepDrop(data=Strata_Q_484,cols="BTGLargeBenthivore CCLargeBenthivore", newdata=dt, drop=1)
write.csv(Strata_Q_484, "output/data/stratasetq484_filt&int.csv", row.names=FALSE)

Strata_Q_485 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('485'), ]
Strata_Q_485 <- KeepDrop(data=Strata_Q_485,cols="BTGLargeBenthivore CCLargeBenthivore", newdata=dt, drop=1)
write.csv(Strata_Q_485, "output/data/stratasetq485_filt&int.csv", row.names=FALSE)

Strata_Q_490 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('490'), ]
Strata_Q_490 <- KeepDrop(data=Strata_Q_490,cols="CCLargeBenthivore", newdata=dt, drop=1)
write.csv(Strata_Q_490, "output/data/stratasetq490_filt&int.csv", row.names=FALSE)

Strata_Q_491 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('491'), ]
Strata_Q_491 <- KeepDrop(data=Strata_Q_491,cols="BTGLargeBenthivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
write.csv(Strata_Q_491, "output/data/stratasetq491_filt&int.csv", row.names=FALSE)

Strata_Q_492 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('492'), ]
Strata_Q_492 <- KeepDrop(data=Strata_Q_492,cols="BTGLargeBenthivore CCLargeBenthivore", newdata=dt, drop=1)
write.csv(Strata_Q_492, "output/data/stratasetq492_filt&int.csv", row.names=FALSE)

Strata_Q_493 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('493'), ]
Strata_Q_493 <- KeepDrop(data=Strata_Q_493,cols="BTGLargeBenthivore	CCLargeBenthivore", newdata=dt, drop=1)
write.csv(Strata_Q_493, "output/data/stratasetq493_filt&int.csv", row.names=FALSE)

Strata_Q_494 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('494'), ]
Strata_Q_494 <- KeepDrop(data=Strata_Q_494,cols="BTGLargeBenthivore CCLargeBenthivore", newdata=dt, drop=1)
write.csv(Strata_Q_494, "output/data/stratasetq494_filt&int.csv", row.names=FALSE)

Strata_Q_495 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('495'), ]
Strata_Q_495 <- KeepDrop(data=Strata_Q_495,cols="BTGLargeBenthivore CCLargeBenthivore CCPlanktivore CCZoopiscivore", newdata=dt, drop=1)
write.csv(Strata_Q_495, "output/data/stratasetq495_filt&int.csv", row.names=FALSE)

colnames(Strata_Q_filt_int) <- paste(colnames(Strata_Q_filt_int), "i", sep = "_")
strata_plot <- cbind(Strata_Q_filt_int, Strata_Q_filtered) # --> this is to plot before and after imputations

Q_440 <- strata_plot[strata_plot$ID %in% c('440'), ]
Q_441 <- strata_plot[strata_plot$ID %in% c('441'), ]
Q_442 <- strata_plot[strata_plot$ID %in% c('442'), ]
Q_443 <- strata_plot[strata_plot$ID %in% c('443'), ]
Q_444 <- strata_plot[strata_plot$ID %in% c('444'), ]
Q_445 <- strata_plot[strata_plot$ID %in% c('445'), ]
Q_446 <- strata_plot[strata_plot$ID %in% c('446'), ]
Q_447 <- strata_plot[strata_plot$ID %in% c('447'), ]
Q_448 <- strata_plot[strata_plot$ID %in% c('448'), ]
Q_449 <- strata_plot[strata_plot$ID %in% c('449'), ]
Q_450 <- strata_plot[strata_plot$ID %in% c('450'), ]
Q_451 <- strata_plot[strata_plot$ID %in% c('451'), ]
Q_452 <- strata_plot[strata_plot$ID %in% c('452'), ]
Q_453 <- strata_plot[strata_plot$ID %in% c('453'), ]
Q_454 <- strata_plot[strata_plot$ID %in% c('454'), ]
Q_455 <- strata_plot[strata_plot$ID %in% c('455'), ]
Q_456 <- strata_plot[strata_plot$ID %in% c('456'), ]
Q_457 <- strata_plot[strata_plot$ID %in% c('457'), ]
Q_458 <- strata_plot[strata_plot$ID %in% c('458'), ]
Q_459 <- strata_plot[strata_plot$ID %in% c('459'), ]
Q_460 <- strata_plot[strata_plot$ID %in% c('460'), ]
Q_461 <- strata_plot[strata_plot$ID %in% c('461'), ]
Q_462 <- strata_plot[strata_plot$ID %in% c('462'), ]
Q_463 <- strata_plot[strata_plot$ID %in% c('463'), ]
Q_464 <- strata_plot[strata_plot$ID %in% c('464'), ]
Q_465 <- strata_plot[strata_plot$ID %in% c('465'), ]
Q_466 <- strata_plot[strata_plot$ID %in% c('466'), ]
Q_470 <- strata_plot[strata_plot$ID %in% c('470'), ]
Q_471 <- strata_plot[strata_plot$ID %in% c('471'), ]
Q_472 <- strata_plot[strata_plot$ID %in% c('472'), ]
Q_473 <- strata_plot[strata_plot$ID %in% c('473'), ]
Q_474 <- strata_plot[strata_plot$ID %in% c('474'), ]
Q_475 <- strata_plot[strata_plot$ID %in% c('475'), ]
Q_476 <- strata_plot[strata_plot$ID %in% c('476'), ]
Q_477 <- strata_plot[strata_plot$ID %in% c('477'), ]
Q_478 <- strata_plot[strata_plot$ID %in% c('478'), ]
Q_480 <- strata_plot[strata_plot$ID %in% c('480'), ]
Q_481 <- strata_plot[strata_plot$ID %in% c('481'), ]
Q_482 <- strata_plot[strata_plot$ID %in% c('482'), ]
Q_483 <- strata_plot[strata_plot$ID %in% c('483'), ]
Q_484 <- strata_plot[strata_plot$ID %in% c('484'), ]
Q_485 <- strata_plot[strata_plot$ID %in% c('485'), ]
Q_490 <- strata_plot[strata_plot$ID %in% c('490'), ]
Q_491 <- strata_plot[strata_plot$ID %in% c('491'), ] 
Q_492 <- strata_plot[strata_plot$ID %in% c('492'), ]
Q_493 <- strata_plot[strata_plot$ID %in% c('493'), ]
Q_494 <- strata_plot[strata_plot$ID %in% c('494'), ]
Q_495 <- strata_plot[strata_plot$ID %in% c('495'), ]

StrataIndi <- list(Q_440, Q_441, Q_442, Q_443, Q_444, Q_445, Q_446, Q_447,
                   Q_448, Q_449, Q_450, Q_451, Q_452, Q_453, Q_454, Q_455,
                   Q_456, Q_457, Q_458, Q_459, Q_460, Q_461, Q_462, Q_463,
                   Q_464, Q_465, Q_466, Q_470, Q_471, Q_472, Q_473, Q_474,
                   Q_475, Q_476, Q_477, Q_478, Q_480, Q_481, Q_482, Q_483, Q_484,
                   Q_485, Q_490, Q_491, Q_492, Q_493, Q_494, Q_495)

#pdf('output/figures/stratasetq_NAs.pdf', width=5,height=4)
Plots_NAs_strata <- lapply(StrataIndi, PlotNAs_strata)
Plots_NAs_strata
dev.off()


# Part II.           ####  #   #  *****  DEFINE GROUPINGS OF INDICATORS  *****  #  ####

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

IndiTotalMelt  <- melt(SS_filtered_interpolated ,id=c('YEAR', 'ID'), measure = c('Biomass',
                                                                                'BiomassClupeids',
                                                                                'BiomassFinfish',
                                                                                'BiomassFlatfish',
                                                                                'BiomassForage',
                                                                                'BiomassGadoids',
                                                                                'BiomassGroundfish',
                                                                                'BiomassSkates',
                                                                                'BiomassTL2',
                                                                                'BiomassTL3',
                                                                                'BiomassTL4',
                                                                                'BPelagicToDemersal',
                                                                                'BTGLargeBenthivore',
                                                                                'BTGMediumBenthivore',
                                                                                'BTGPiscivore',
                                                                                'BTGPlanktivore',
                                                                                'BTGZoopiscivore',
                                                                                'CCLargeBenthivore',
                                                                                'CCMediumBenthivore',
                                                                                'CCPiscivore',
                                                                                'CCPlanktivore',
                                                                                'CCZoopiscivore',
                                                                                'CommunityCondition',
                                                                                'DiversityTargetSpp.L',
                                                                                'FishinginBalance.L',
                                                                                'FishingPressure.L',
                                                                                'FPClupeids.L',
                                                                                'FPFinfish.L',
                                                                                'FPFlatfish.L',
                                                                                'FPForageFish.L',
                                                                                'FPGadoids.L',
                                                                                'FPGroundfish.L',
                                                                                'FPSkates.L',
                                                                                'Heips',
                                                                                'HillN1Diversity',
                                                                                'HillN2Dominance',
                                                                                'Intrinsicvulnerabilityindex.L',
                                                                                'InverseCVBiomass',
                                                                                'KemptonQ',
                                                                                'Landings.L',
                                                                                'LargeFishIndicator',
                                                                                'LargeSpeciesIndicator',
                                                                                'LClupeids.L',
                                                                                'LFinfish.L',
                                                                                'LFlatfish.L',
                                                                                'LForageFish.L',
                                                                                'LGadoids.L',
                                                                                'LGroundfish.L',
                                                                                'LInvertebrates.L',
                                                                                'LLargePelagic.L',
                                                                                'LSkates.L',
                                                                                'MargalefGroundfish',
                                                                                'MargalefRichness',
                                                                                'MarineTrophicIndex.L',
                                                                                'MeanLengthAbundance',
                                                                                'MeanLengthBiomass',
                                                                                'MeanLifespan',
                                                                                'MeanTrophicLevel',
                                                                                'MeanTrophicLevel.L',
                                                                                'MMLengthAbundance',
                                                                                'MMLengthBiomass',
                                                                                'PielouEvenness',
                                                                                'PropPredatoryFish',
                                                                                'ShannonDiversity',
                                                                                'SpeciesRichness'))
