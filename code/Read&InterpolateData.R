#  August 2016 
#install.packages("devtools")
setwd("C:/RProjects/UseIndicators")
#  devtools::install_github("rstudio/packrat")
#packrat::init()                     # -- https://rstudio.github.io/packrat/commands.html # Reads packages needed for plots and analysis
library(ggplot2)
theme_set(theme_bw())
library(reshape2)
system.file(package="ggplot2")      # This checks that you are NOT using private library via packrat IF you want to do ggplots

library(gridExtra)
library(Hmisc)
library(PerformanceAnalytics)
library(plyr)
library(dplyr)
library(corrplot)
library(zoo)
library(Hmisc)
library(zoo)
library(imputeTS)

require(devtools) 
# install_github('Beothuk/bio.base', force = TRUE) 
# require(bio.base)                    # compiled functions from me, jae and brad and mike mcmahon
# require(bio.utilities)               # https://github.com/Beothuk/bio.utilities    #install_github('Beothuk/bio.utilities') #compiled functions from ACook, Jae, Brad and Mike Mcmahon
# install_github('Beothuk/bio.utilities', force = TRUE) 
library(missForest)                  # randomForest approximation to fill in missing values....full dataset using relationships bwn variables to fill in NAs
source('code/IndiFunctions.R')       # Sources functions that set ggplot settings to plot indicators per each scale (Shelf, NAFO and strata scale)
source('code/keepdropcolumn.R')
path <- file.path('C:/RProjects/ExtractIndicators')
require(ODBC)
source(paste(path,'/R/amc helpers.R',sep=""))
source('C:/RProjects/ExtractIndicators/R/stdize.R')
source('C:/RProjects/ExtractIndicators/R/stdizeFrame.R')

# --------SECTION I. Reads, filters and interpolates csv data of indicators extracted using A.Cook's package  *****  #  ####
# There are several NAs in the time-series data. The hierarchical cluster analysis 
# ignores entire rows (i.e. years) in place where there is a NA (it omits all the other data for the year of the missing value, therefore excluding the whole year from the analysis).
# In this script, we are ommitting indicators with > 25% of NA's in the time-series (BInvertebrateToDemersal; FPInvertebrates.L; InverseFPInvertebrates.L; BiomassInvertebrates) 
# and we interpolate missing data for indicators with missing data , but <=25%. 
# The invertebrate based indicators with missing data are also excluded 

# ********************************** SHELF  -----------------------------------------
SS <- read.csv("data/shelfsetq.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
SS$ID <- factor(SS$ID)
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
          'BiomassPelagic',
          'Abundance', 
          'MeanTrophicLevelStanza', 
          'BInvertebrateToDemersal', 
          'BiomassInvertebrates', 
          'FPInvertebrates.L')
dropIndi <- cbind(drop, drop_s)
SS <- KeepDrop(data=SS,cols=dropIndi, newdata=dt, drop=1)
SS_filtered <- KeepDrop(data=SS,cols=dropIndi, newdata=dt, drop=1)
#SS_i <- missForest(SS_filtered)$ximp    
#write.csv(SS_i, "output/data/largescales/shelfsetq_filtered&interpolated.csv", row.names=FALSE)
colnames(SS_i) <- paste(colnames(SS_i), "i", sep = "_")
SS_plot <- cbind(SS_i, SS)
head(SS_plot)
#pdf('output/figures/shelfsetq_NAs.pdf', width=5,height=4)
PlotNAs(SS_plot)
dev.off()

#*********************************** esswss    -----------------------------------------
Shelf_Q <- read.csv("data/esswsssetq.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
Shelf_Q$ID <- factor(Shelf_Q$ID)
Shelf_Q_filtered <- KeepDrop(data=Shelf_Q,cols=dropIndi, newdata=dt, drop=1)
Shelf_i <- missForest(Shelf_Q_filtered)$ximp  # this line perfomrs the imputations
#write.csv(Shelf_i, "output/data/largescales/esswsssetq_filtered&interpolated.csv", row.names=FALSE)

colnames(Shelf_i) <- paste(colnames(Shelf_i), "i", sep = "_")
Shelf_plot <- cbind(Shelf_Q_filtered, Shelf_i)
WSS_Q_plot <- Shelf_plot[Shelf_plot$ID %in% c('WSS'), ]
ESS_Q_plot <- Shelf_plot[Shelf_plot$ID %in% c('ESS'), ]
ShelfIndi <- list(WSS_Q_plot, ESS_Q_plot) 

#pdf('output/figures/esswsssetq_NAs.pdf', width=5,height=4)
Plots_NAs_Shelf <- lapply(ShelfIndi, PlotNAs)
dev.off()


#***********************************NAFO  -----------------------------------------
IndiQ_NAFO <- read.csv("data/nafosetq.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
IndiQ_NAFO$ID <- factor(IndiQ_NAFO$ID)
IndiQ_NAFO_filtered <- KeepDrop(data=IndiQ_NAFO,cols=dropIndi, newdata=dt, drop=1)
#NAFO_i <- missForest(IndiQ_NAFO_filtered)$ximp 

NAFO_4W <- NAFO_i[NAFO_i$ID %in% c('4W'), ]
NAFO_4X <- NAFO_i[NAFO_i$ID %in% c('4X'), ]
NAFO_4VS <- NAFO_i[NAFO_i$ID %in% c('4VS'), ]
NAFO_4VS <- KeepDrop(data=NAFO_4VS,cols="CCPlanktivore BiomassClupeids", newdata=dt, drop=1)
NAFO_4VN <- NAFO_i[NAFO_i$ID %in% c('4VN'), ]
NAFO_4VN <- KeepDrop(data=NAFO_4VN,cols="BiomassTL2 CCPlanktivore", newdata=dt, drop=1)
 # write.csv(NAFO_4W, "output/data/largescales/nafo4Wsetq_filtered&interpolated.csv", row.names=FALSE)
 # write.csv(NAFO_4X, "output/data/largescales/nafo4Xsetq_filtered&interpolated.csv", row.names=FALSE)
 # write.csv(NAFO_4VS, "output/data/largescales/nafo4VSsetq_filtered&interpolated.csv", row.names=FALSE)
 # write.csv(NAFO_4VN, "output/data/largescales/nafo4VNsetq_filtered&interpolated.csv", row.names=FALSE)

#Plot new interpolated data sets:
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


#*********************************** Strata scale  -----------------------------------------
Strata_Q <- read.csv("data/stratsetq.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
Strata_Q$ID <- factor(Strata_Q$ID)
Strata_Q_filtered <- KeepDrop(data=Strata_Q,cols=dropIndi, newdata=dt, drop=1)
Strata_Q_filtered <- KeepDrop(data=Strata_Q_filtered,cols="BiomassTL2", newdata=dt, drop=1)
#Strata_Q_filt_int <- missForest(Strata_Q_filtered)$ximp 
#Strata_Q_4vs <- Strata_Q[Strata_Q$ID %in% c('443','444', '445', '446', '447', '448', '449', '450', '451', '452'), ]

Strata_Q_440 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('440'), ]
Strata_Q_440 <- KeepDrop(data=Strata_Q_440,cols="BiomassClupeids BiomassForage BPelagicToDemersal BTGLargeBenthivore BTGPlanktivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
#write.csv(Strata_Q_440, "output/data/stratasetq440_filt&int.csv", row.names=FALSE)

Strata_Q_441 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('441'), ]
Strata_Q_441 <- KeepDrop(data=Strata_Q_441,cols="BiomassClupeids BiomassForage BPelagicToDemersal BTGPlanktivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
#write.csv(Strata_Q_441, "output/data/stratasetq441_filt&int.csv", row.names=FALSE)

Strata_Q_442 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('442'), ]
Strata_Q_442 <- KeepDrop(data=Strata_Q_442,cols="BiomassClupeids BTGLargeBenthivore BTGZoopiscivore CCLargeBenthivore CCPlanktivore CCZoopiscivore", newdata=dt, drop=1)
#write.csv(Strata_Q_442, "output/data/stratasetq442_filt&int.csv", row.names=FALSE)

Strata_Q_441 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('441'), ]
Strata_Q_441 <- KeepDrop(data=Strata_Q_441,cols="BiomassClupeids BiomassForage BPelagicToDemersal BTGPlanktivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
#write.csv(Strata_Q_441, "output/data/stratasetq441_filt&int.csv", row.names=FALSE)

Strata_Q_442 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('442'), ]
Strata_Q_442 <- KeepDrop(data=Strata_Q_442,cols="BiomassClupeids BTGLargeBenthivore BTGZoopiscivore CCLargeBenthivore CCPlanktivore CCZoopiscivore", newdata=dt, drop=1)
#write.csv(Strata_Q_442, "output/data/stratasetq442_filt&int.csv", row.names=FALSE)

Strata_Q_443 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('443'), ]
Strata_Q_443 <- KeepDrop(data=Strata_Q_443,cols="BiomassClupeids BiomassForage BPelagicToDemersal BTGPlanktivore BTGZoopiscivore CCLargeBenthivore CCPlanktivore CCZoopiscivore", newdata=dt, drop=1)
#write.csv(Strata_Q_443, "output/data/stratasetq443_filt&int.csv", row.names=FALSE)

Strata_Q_444 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('444'), ]
Strata_Q_444 <- KeepDrop(data=Strata_Q_444,cols="BiomassClupeids BiomassForage BPelagicToDemersal BTGPlanktivore CCPlanktivore", newdata=dt, drop=1)
#write.csv(Strata_Q_444, "output/data/stratasetq444_filt&int.csv", row.names=FALSE)

Strata_Q_445 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('445'), ]
Strata_Q_445 <- KeepDrop(data=Strata_Q_445,cols="BiomassClupeids BTGLargeBenthivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
#write.csv(Strata_Q_445, "output/data/stratasetq445_filt&int.csv", row.names=FALSE)

Strata_Q_446 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('446'), ]
Strata_Q_446 <- KeepDrop(data=Strata_Q_446,cols="BiomassClupeids BiomassForage BPelagicToDemersal BTGLargeBenthivore BTGPlanktivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
#write.csv(Strata_Q_446, "output/data/stratasetq446_filt&int.csv", row.names=FALSE)

Strata_Q_447 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('447'), ]
Strata_Q_447 <- KeepDrop(data=Strata_Q_447,cols="BiomassClupeids BiomassForage BPelagicToDemersal BTGLargeBenthivore BTGPlanktivore BTGZoopiscivore CCLargeBenthivore CCPlanktivore CCZoopiscivore", newdata=dt, drop=1)
#write.csv(Strata_Q_447, "output/data/stratasetq447_filt&int.csv", row.names=FALSE)

Strata_Q_448 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('448'), ]
Strata_Q_448 <- KeepDrop(data=Strata_Q_448,cols="BiomassClupeids BTGLargeBenthivore BTGZoopiscivore CCLargeBenthivore CCPlanktivore CCZoopiscivore", newdata=dt, drop=1)
#write.csv(Strata_Q_448, "output/data/stratasetq448_filt&int.csv", row.names=FALSE)

Strata_Q_449 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('449'), ]
Strata_Q_449 <- KeepDrop(data=Strata_Q_449,cols="BiomassClupeids BiomassForage BPelagicToDemersal BTGLargeBenthivore BTGPlanktivore BTGZoopiscivore CCLargeBenthivore CCPlanktivore CCZoopiscivore", newdata=dt, drop=1)
#write.csv(Strata_Q_449, "output/data/stratasetq449_filt&int.csv", row.names=FALSE)

Strata_Q_450 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('450'), ]
Strata_Q_450 <- KeepDrop(data=Strata_Q_450,cols="BiomassClupeids BiomassForage BPelagicToDemersal BTGLargeBenthivore BTGPlanktivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
#write.csv(Strata_Q_450, "output/data/stratasetq450_filt&int.csv", row.names=FALSE)

Strata_Q_451 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('451'), ]
Strata_Q_451 <- KeepDrop(data=Strata_Q_451,cols="BiomassClupeids BTGLargeBenthivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
#write.csv(Strata_Q_451, "output/data/stratasetq451_filt&int.csv", row.names=FALSE)

Strata_Q_452 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('452'), ]
Strata_Q_452 <- KeepDrop(data=Strata_Q_452,cols="BiomassClupeids BiomassForage BPelagicToDemersal BTGLargeBenthivore BTGPlanktivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
#write.csv(Strata_Q_452, "output/data/stratasetq452_filt&int.csv", row.names=FALSE)

Strata_Q_453 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('453'), ]
Strata_Q_453 <- KeepDrop(data=Strata_Q_453,cols="BiomassClupeids BiomassForage BiomassSkates BPelagicToDemersal BTGLargeBenthivore BTGPlanktivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
#write.csv(Strata_Q_453, "output/data/stratasetq453_filt&int.csv", row.names=FALSE)

Strata_Q_454 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('454'), ]
Strata_Q_454 <- KeepDrop(data=Strata_Q_454,cols="BiomassClupeids BiomassForage BPelagicToDemersal BTGLargeBenthivore BTGPlanktivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
#write.csv(Strata_Q_454, "output/data/stratasetq454_filt&int.csv", row.names=FALSE)

Strata_Q_455 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('455'), ]
Strata_Q_455 <- KeepDrop(data=Strata_Q_455,cols="BiomassClupeids BTGLargeBenthivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
#write.csv(Strata_Q_455, "output/data/stratasetq455_filt&int.csv", row.names=FALSE)

Strata_Q_456 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('456'), ]
Strata_Q_456 <- KeepDrop(data=Strata_Q_456,cols="BTGLargeBenthivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
#write.csv(Strata_Q_456, "output/data/stratasetq456_filt&int.csv", row.names=FALSE)

Strata_Q_457 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('457'), ]
Strata_Q_457 <- KeepDrop(data=Strata_Q_457,cols="BiomassClupeids BTGLargeBenthivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
#write.csv(Strata_Q_457, "output/data/stratasetq457_filt&int.csv", row.names=FALSE)

Strata_Q_458 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('458'), ]
Strata_Q_458 <- KeepDrop(data=Strata_Q_458,cols="BiomassClupeids BTGLargeBenthivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
#write.csv(Strata_Q_458, "output/data/stratasetq458_filt&int.csv", row.names=FALSE)

Strata_Q_459 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('459'), ]
Strata_Q_459 <- KeepDrop(data=Strata_Q_459,cols="CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
#write.csv(Strata_Q_459, "output/data/stratasetq459_filt&int.csv", row.names=FALSE)

Strata_Q_460 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('460'), ]
Strata_Q_460 <- KeepDrop(data=Strata_Q_460,cols="BiomassSkates BTGLargeBenthivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
#write.csv(Strata_Q_460, "output/data/stratasetq460_filt&int.csv", row.names=FALSE)

Strata_Q_461 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('461'), ]
Strata_Q_461 <- KeepDrop(data=Strata_Q_461,cols="BiomassClupeids BiomassSkates BTGLargeBenthivore CCLargeBenthivore", newdata=dt, drop=1)
#write.csv(Strata_Q_461, "output/data/stratasetq461_filt&int.csv", row.names=FALSE)

Strata_Q_462 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('462'), ]
Strata_Q_462 <- KeepDrop(data=Strata_Q_462,cols="BiomassSkates BTGLargeBenthivore CCLargeBenthivore", newdata=dt, drop=1)
#write.csv(Strata_Q_462, "output/data/stratasetq462_filt&int.csv", row.names=FALSE)

Strata_Q_463 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('463'), ]
Strata_Q_463 <- KeepDrop(data=Strata_Q_463,cols="BiomassClupeids BiomassForage BiomassSkates BPelagicToDemersal BTGLargeBenthivore BTGPlanktivore BTGZoopiscivore CCLargeBenthivore CCPlanktivore CCZoopiscivore", newdata=dt, drop=1)
#write.csv(Strata_Q_463, "output/data/stratasetq463_filt&int.csv", row.names=FALSE)

Strata_Q_464 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('464'), ]
Strata_Q_464 <- KeepDrop(data=Strata_Q_464,cols="BiomassClupeids BTGLargeBenthivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
#write.csv(Strata_Q_464, "output/data/stratasetq464_filt&int.csv", row.names=FALSE)

Strata_Q_465 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('465'), ]
Strata_Q_465 <- KeepDrop(data=Strata_Q_465,cols="BiomassClupeids BTGLargeBenthivore CCLargeBenthivore", newdata=dt, drop=1)
#write.csv(Strata_Q_465, "output/data/stratasetq465_filt&int.csv", row.names=FALSE)

Strata_Q_466 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('466'), ]
Strata_Q_466 <- KeepDrop(data=Strata_Q_466,cols="BiomassClupeids BiomassForage BiomassSkates BPelagicToDemersal BTGLargeBenthivore BTGPlanktivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
#write.csv(Strata_Q_466, "output/data/stratasetq466_filt&int.csv", row.names=FALSE)

Strata_Q_470 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('470'), ]
Strata_Q_470 <- KeepDrop(data=Strata_Q_470,cols="BTGLargeBenthivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
#write.csv(Strata_Q_470, "output/data/stratasetq470_filt&int.csv", row.names=FALSE)

Strata_Q_471 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('471'), ]
Strata_Q_471 <- KeepDrop(data=Strata_Q_471,cols="BiomassClupeids BiomassSkates BTGLargeBenthivore CCLargeBenthivore", newdata=dt, drop=1)
#write.csv(Strata_Q_471, "output/data/stratasetq471_filt&int.csv", row.names=FALSE)

Strata_Q_472 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('472'), ]
Strata_Q_472 <- KeepDrop(data=Strata_Q_472,cols="BiomassClupeids BiomassSkates BTGLargeBenthivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
#write.csv(Strata_Q_472, "output/data/stratasetq472_filt&int.csv", row.names=FALSE)

Strata_Q_473 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('473'), ]
Strata_Q_473 <- KeepDrop(data=Strata_Q_473,cols="BiomassClupeids BiomassFlatfish BiomassForage BiomassSkates BPelagicToDemersal BTGLargeBenthivore BTGPlanktivore BTGZoopiscivore CCLargeBenthivore CCPlanktivore CCZoopiscivore", newdata=dt, drop=1)
#write.csv(Strata_Q_473, "output/data/stratasetq473_filt&int.csv", row.names=FALSE)

Strata_Q_474 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('474'), ]
Strata_Q_474 <- KeepDrop(data=Strata_Q_474,cols="BiomassClupeids BiomassForage BiomassSkates BPelagicToDemersal BTGLargeBenthivore BTGPlanktivore BTGZoopiscivore CCLargeBenthivore CCPlanktivore CCZoopiscivore", newdata=dt, drop=1)
#write.csv(Strata_Q_474, "output/data/stratasetq474_filt&int.csv", row.names=FALSE)

Strata_Q_475 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('475'), ]
Strata_Q_475 <- KeepDrop(data=Strata_Q_475,cols="BiomassClupeids BiomassForage BiomassSkates BPelagicToDemersal BTGPlanktivore BTGZoopiscivore CCLargeBenthivore CCPlanktivore CCZoopiscivore", newdata=dt, drop=1)
#write.csv(Strata_Q_475, "output/data/stratasetq475_filt&int.csv", row.names=FALSE)

Strata_Q_476 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('476'), ]
Strata_Q_476 <- KeepDrop(data=Strata_Q_476,cols="BiomassClupeids BiomassForage BPelagicToDemersal BTGLargeBenthivore BTGPlanktivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
#write.csv(Strata_Q_476, "output/data/stratasetq476_filt&int.csv", row.names=FALSE)

Strata_Q_477 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('477'), ]
Strata_Q_477 <- KeepDrop(data=Strata_Q_477,cols="BiomassClupeids BiomassForage BPelagicToDemersal BTGLargeBenthivore BTGPlanktivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
#write.csv(Strata_Q_477, "output/data/stratasetq477_filt&int.csv", row.names=FALSE)

Strata_Q_478 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('478'), ]
Strata_Q_478 <- KeepDrop(data=Strata_Q_478,cols="BiomassClupeids BiomassSkates BTGLargeBenthivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
#write.csv(Strata_Q_478, "output/data/stratasetq478_filt&int.csv", row.names=FALSE)

Strata_Q_480 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('480'), ]
Strata_Q_480 <- KeepDrop(data=Strata_Q_480,cols="BiomassClupeids BiomassForage BPelagicToDemersal BTGPlanktivore BTGZoopiscivore CCPlanktivore CCZoopiscivore", newdata=dt, drop=1)
#write.csv(Strata_Q_480, "output/data/stratasetq480_filt&int.csv", row.names=FALSE)

Strata_Q_481 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('481'), ]
Strata_Q_481 <- KeepDrop(data=Strata_Q_481,cols="BiomassClupeids CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
#write.csv(Strata_Q_481, "output/data/stratasetq481_filt&int.csv", row.names=FALSE)

Strata_Q_482 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('482'), ]
Strata_Q_482 <- KeepDrop(data=Strata_Q_482,cols="BiomassClupeids BiomassFlatfish BTGLargeBenthivore CCLargeBenthivore", newdata=dt, drop=1)
#write.csv(Strata_Q_482, "output/data/stratasetq482_filt&int.csv", row.names=FALSE)

Strata_Q_483 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('483'), ]
Strata_Q_483 <- KeepDrop(data=Strata_Q_483,cols="BiomassClupeids BTGLargeBenthivore CCLargeBenthivore", newdata=dt, drop=1)
#write.csv(Strata_Q_483, "output/data/stratasetq483_filt&int.csv", row.names=FALSE)

Strata_Q_484 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('484'), ]
Strata_Q_484 <- KeepDrop(data=Strata_Q_484,cols="BTGLargeBenthivore CCLargeBenthivore", newdata=dt, drop=1)
#write.csv(Strata_Q_484, "output/data/stratasetq484_filt&int.csv", row.names=FALSE)

Strata_Q_485 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('485'), ]
Strata_Q_485 <- KeepDrop(data=Strata_Q_485,cols="BTGLargeBenthivore CCLargeBenthivore", newdata=dt, drop=1)
#write.csv(Strata_Q_485, "output/data/stratasetq485_filt&int.csv", row.names=FALSE)

Strata_Q_490 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('490'), ]
Strata_Q_490 <- KeepDrop(data=Strata_Q_490,cols="CCLargeBenthivore", newdata=dt, drop=1)
#write.csv(Strata_Q_490, "output/data/stratasetq490_filt&int.csv", row.names=FALSE)

Strata_Q_491 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('491'), ]
Strata_Q_491 <- KeepDrop(data=Strata_Q_491,cols="BTGLargeBenthivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
#write.csv(Strata_Q_491, "output/data/stratasetq491_filt&int.csv", row.names=FALSE)

Strata_Q_492 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('492'), ]
Strata_Q_492 <- KeepDrop(data=Strata_Q_492,cols="BTGLargeBenthivore CCLargeBenthivore", newdata=dt, drop=1)
#write.csv(Strata_Q_492, "output/data/stratasetq492_filt&int.csv", row.names=FALSE)

Strata_Q_493 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('493'), ]
Strata_Q_493 <- KeepDrop(data=Strata_Q_493,cols="BTGLargeBenthivore	CCLargeBenthivore", newdata=dt, drop=1)
#write.csv(Strata_Q_493, "output/data/stratasetq493_filt&int.csv", row.names=FALSE)

Strata_Q_494 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('494'), ]
Strata_Q_494 <- KeepDrop(data=Strata_Q_494,cols="BTGLargeBenthivore CCLargeBenthivore", newdata=dt, drop=1)
#write.csv(Strata_Q_494, "output/data/stratasetq494_filt&int.csv", row.names=FALSE)

Strata_Q_495 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('495'), ]
Strata_Q_495 <- KeepDrop(data=Strata_Q_495,cols="BTGLargeBenthivore CCLargeBenthivore CCPlanktivore CCZoopiscivore", newdata=dt, drop=1)
#write.csv(Strata_Q_495, "output/data/stratasetq495_filt&int.csv", row.names=FALSE)

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
#pdf('output/figures/stratasetq_NAs.pdf', width=4,height=4)
Plots_NAs_strata <- lapply(StrataIndi, PlotNAs_strata)
# Plots_NAs_strata
dev.off()


# --------SECTION II Standardize indicators *****  #  ####
shelf_raw <- read.csv("output/data/largescales/shelfsetq_filtered&interpolated.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
shelf_s = stdizeFrame(shelf_raw)
shelf_s <- shelf_s[ , order(names(shelf_s))]
#write.csv(shelf_s, "output/data/shelfsetq_filtered&interpolated_s.csv", row.names=FALSE)
esswss_raw <- read.csv("output/data/largescales/esswsssetq_filtered&interpolated.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
esswss_s = stdizeFrame(esswss_raw)
esswss_s <- esswss_s[ , order(names(esswss_s))]
wss_s <- esswss_s[esswss_s$ID %in% c('WSS'), ]
ess_s <- esswss_s[esswss_s$ID %in% c('ESS'), ]
# write.csv(ess_s, "output/data/largescales/esssetq_filtered&interpolated_s.csv", row.names=FALSE)
# write.csv(wss_s, "output/data/largescales/wsssetq_filtered&interpolated_s.csv", row.names=FALSE)

nafo4vs_raw <- read.csv("output/data/largescales/nafo4VSsetq_filtered&interpolated.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
nafo4vs_s = stdizeFrame(nafo4vs_raw)
nafo4vs_s <- nafo4vs_s[ , order(names(nafo4vs_s))]
#write.csv(nafo4vs_s, "output/data/nafo4VSsetq_filtered&interpolated_s.csv", row.names=FALSE)
nafo4vn_raw <- read.csv("output/data/largescales/nafo4VNsetq_filtered&interpolated.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
nafo4vn_s = stdizeFrame(nafo4vn_raw)
nafo4vn_s <- nafo4vn_s[ , order(names(nafo4vn_s))]
#write.csv(nafo4vn_s, "output/data/nafo4VNsetq_filtered&interpolated_s.csv", row.names=FALSE)
nafo4w_raw <- read.csv("output/data/largescales/nafo4Wsetq_filtered&interpolated.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
nafo4w_s = stdizeFrame(nafo4w_raw)
nafo4w_s <- nafo4w_s[ , order(names(nafo4w_s))]
#write.csv(nafo4w_s, "output/data/nafo4Wsetq_filtered&interpolated_s.csv", row.names=FALSE)
nafo4x_raw <- read.csv("output/data/largescales/nafo4Xsetq_filtered&interpolated.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
nafo4x_s = stdizeFrame(nafo4x_raw)
nafo4x_s<- nafo4x_s[ , order(names(nafo4x_s))]
#write.csv(nafo4x_s, "output/data/nafo4xsetq_filtered&interpolated_s.csv", row.names=FALSE)

filenames <- list.files("C:/RProjects/UseIndicators/output/data/strata", pattern="*.csv", full.names=TRUE)
indiStrata <- lapply(filenames, read.csv)
strata_s  <- lapply(indiStrata , stdizeFrame)


# --------SECTION III Plot indicators *****  #  ####
shelf_melt <- melt(shelf_s, id=c('YEAR', 'ID'))
esswss_melt <- melt(esswss_s, id=c('YEAR', 'ID'))
nafo4vn_melt <- melt(nafo4vn_s, id=c('YEAR', 'ID'))
nafo4vs_melt <- melt(nafo4vs_s, id=c('YEAR', 'ID'))
nafo4w_melt <- melt(nafo4w_s, id=c('YEAR', 'ID'))
nafo4vs_melt <- melt(nafo4vs_s, id=c('YEAR', 'ID'))
nafo4x_melt <- melt(nafo4x_s, id=c('YEAR', 'ID'))
nafo_melt <- rbind(nafo4vn_melt, nafo4vs_melt, nafo4w_melt, nafo4vs_melt, nafo4x_melt)
strata_melt <- melt(strata_s, id=c('YEAR', 'ID'))
strata_melt <- strata_melt[order(strata_melt$variable),]
shelfesswss_melt <- rbind(shelf_melt,esswss_melt)


#pdf("output/figures/shelfset_filtered&interpolated.pdf", width=15,height=10)
dlply(shelf_melt, .(variable), function(dat) {
  ggplot(data = dat, aes(x = YEAR, y = value, linetype = variable, group=variable)) +
    scale_color_brewer(palette="Paired") +
    stat_smooth(se=F) + geom_point() +
    facet_wrap(variable ~ ID, scales = "free") +
    theme(text = element_text(size=15)) +
    theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) +
    theme(legend.title=element_blank()) +
    theme(legend.position='bottom') +
    #ggtitle("") +
    theme(legend.position="none") +
    theme(strip.text.x = element_text(size=13),
          strip.background = element_rect(fill="white"))
})
dev.off()

#pdf("output/figures/esswsssetq_filtered&interpolated.pdf", width=15,height=10)
dlply(esswss_melt, .(variable), function(dat) {
  ggplot(data = dat, aes(x = YEAR, y = value, linetype = variable, group=variable)) +
    scale_color_brewer(palette="Paired") +
    stat_smooth(se=F) + geom_point() +
    facet_wrap(variable ~ ID, scales = "free") +
    theme(text = element_text(size=15)) +
    theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) +
    theme(legend.title=element_blank()) +
    theme(legend.position='bottom') +
    #ggtitle("") +
    theme(legend.position="none") +
    theme(strip.text.x = element_text(size=13),
          strip.background = element_rect(fill="white"))
})
dev.off()

#pdf("output/figures/nafosetq_filtered&interpolated.pdf", width=15,height=10)
dlply(nafo_melt, .(variable), function(dat) {
  ggplot(data = dat, aes(x = YEAR, y = value, linetype = variable, group=variable)) +
    scale_color_brewer(palette="Paired") +
    stat_smooth(se=F) + geom_point() +
    facet_wrap(variable ~ ID, scales = "free") +
    theme(text = element_text(size=15)) +
    theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) +
    theme(legend.title=element_blank()) +
    theme(legend.position='bottom') +
    #ggtitle("") +
    theme(legend.position="none") +
    theme(strip.text.x = element_text(size=13),
          strip.background = element_rect(fill="white"))
})
dev.off()


#pdf("output/figures/clusters&singletons/stratasetq_filtered&interpolated.pdf", width=23,height=13)
dlply(strata_melt, .(variable), function(dat) {
  ggplot(data = dat, aes(x = YEAR, y = value, linetype = variable, group=variable)) +
    scale_color_brewer(palette="Paired") +
    stat_smooth(se=F) + geom_point() +
    facet_wrap(variable ~ ID, scales = "free") +
    theme(text = element_text(size=15)) +
    theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) +
    theme(legend.title=element_blank()) +
    theme(legend.position='bottom') +
    #ggtitle("") +
    theme(legend.position="none") +
    theme(strip.text.x = element_text(size=13),
          strip.background = element_rect(fill="white"))
})
dev.off()

# --------SECTION IV Define clusters of indicators and singletons *****  #  ####
# Using the results of the redundancy analysis (tech report), this section below defines indicators defined as singletons 
# or redundant indicators grouped in clusters (C1 to C12).

## Clusters Identified in the redundancy analysis at large scales ###
# # C1
C1 <- shelfesswss_melt[shelfesswss_melt$variable %in% c('Biomass_s',
                                                        'BiomassClupeids_s',
                                                        'BiomassFinfish_s',
                                                        'BiomassForage_s',
                                                        'BiomassTL3_s',
                                                        'BPelagicToDemersal_s',
                                                        'BTGPlanktivore_s'), ]
C1_nafo <- nafo_melt[nafo_melt$variable %in% c('Biomass_s',
                                               'BiomassClupeids_s',
                                               'BiomassFinfish_s',
                                               'BiomassForage_s',
                                               'BiomassTL3_s',
                                               'BPelagicToDemersal_s',
                                               'BTGPlanktivore_s'), ]

#  C2
C2 <- shelfesswss_melt[shelfesswss_melt$variable %in% c("MargalefRichness_s",
                                                        "SpeciesRichness_s"), ]
C2_nafo <- nafo_melt[nafo_melt$variable %in% c("MargalefRichness_s",
                                              "SpeciesRichness_s"), ]
#   C3
C3 <- shelfesswss_melt[shelfesswss_melt$variable %in% c("HillN1Diversity_s",
                                                        "ShannonDiversity_s",
                                                        "HillN2Dominance_s"), ]
C3_nafo <- nafo_melt[nafo_melt$variable %in% c("HillN1Diversity_s",
                                               "ShannonDiversity_s",
                                               "HillN2Dominance_s"), ]
# #  C4
C4 <- shelfesswss_melt[shelfesswss_melt$variable %in% c("LargeFishIndicator_s",
                                                        "LargeSpeciesIndicator_s",
                                                        "PropPredatoryFish_s",
                                                        "MeanLengthBiomass_s",
                                                        "MMLengthBiomass_s"), ]
C4_nafo <- nafo_melt[nafo_melt$variable %in% c("LargeFishIndicator_s",
                                               "LargeSpeciesIndicator_s",
                                               "PropPredatoryFish_s",
                                               "MeanLengthBiomass_s",
                                               "MMLengthBiomass_s"), ]
# #  C5
C5 <- shelfesswss_melt[shelfesswss_melt$variable %in% c("BiomassGadoids_s",
                                                        "BiomassGroundfish_s",
                                                        "BiomassTL4_s",
                                                        "BTGPiscivore_s"), ]
C5_nafo <- nafo_melt[nafo_melt$variable %in% c("BiomassGadoids_s",
                                               "BiomassGroundfish_s",
                                               "BiomassTL4_s",
                                               "BTGPiscivore_s"), ]
# #  C6
C6 <- shelfesswss_melt[shelfesswss_melt$variable %in% c("CommunityCondition_s",
                                                        "CCPlanktivore_s"), ]
C6_nafo <- nafo_melt[nafo_melt$variable %in% c("CommunityCondition_s",
                                               "CCPlanktivore_s"), ]
# #  C7
C7 <- shelfesswss_melt[shelfesswss_melt$variable %in% c('Heips_s', 
                                                        'PielouEvenness_s'), ]
C7_nafo <- nafo_melt[nafo_melt$variable %in% c('Heips_s', 
                                              'PielouEvenness_s'), ]
# #  C8
C8 <- shelfesswss_melt[shelfesswss_melt$variable %in% c("LClupeids.L_s",
                                                        "LForageFish.L_s"), ]
C8_nafo <- nafo_melt[nafo_melt$variable %in% c("LClupeids.L_s",
                                               "LForageFish.L_s"), ]
# #  C9
C9 <- shelfesswss_melt[shelfesswss_melt$variable %in% c("Landings.L_s",
                                                        "LFinfish.L_s",
                                                        "LGroundfish.L_s",
                                                        "LGadoids.L_s",
                                                        "FPGroundfish.L_s",
                                                        "FPGadoids.L_s"), ]
C9_nafo <- nafo_melt[nafo_melt$variable %in% c("Landings.L_s",
                                               "LFinfish.L_s",
                                               "LGroundfish.L_s",
                                               "LGadoids.L_s",
                                               "FPGroundfish.L_s",
                                               "FPGadoids.L_s"), ]
# #  C10
C10 <- shelfesswss_melt[shelfesswss_melt$variable %in% c("DiversityTargetSpp.L_s",
                                                        "LInvertebrates.L_s"), ]
C10_nafo <- nafo_melt[nafo_melt$variable %in% c("DiversityTargetSpp.L_s",
                                               "LInvertebrates.L_s"), ]

# #  C11
C11 <- shelfesswss_melt[shelfesswss_melt$variable %in% c("FishingPressure.L_s",
                                                         "FPFinfish.L_s"), ]
C11_nafo <- nafo_melt[nafo_melt$variable %in% c("FishingPressure.L_s",
                                                "FPFinfish.L_s"), ]

# #  C12
C12 <- shelfesswss_melt[shelfesswss_melt$variable %in% c("FPClupeids.L_s",
                                                         "FPForageFish.L_s"), ]
C12_nafo <- nafo_melt[nafo_melt$variable %in% c("FPClupeids.L_s",
                                                "FPForageFish.L_s"), ]
# #  C13
C13 <- shelfesswss_melt[shelfesswss_melt$variable %in% c("FPSkates.L_s",
                                                         "LSkates.L_s"), ]
C13_nafo <- nafo_melt[nafo_melt$variable %in% c("FPSkates.L_s",
                                                "LSkates.L_s"), ]
# #  C14
C14 <- shelfesswss_melt[shelfesswss_melt$variable %in% c("FPFlatfish.L_s",
                                                         "LFlatfish.L_s"), ]
C14_nafo <- nafo_melt[nafo_melt$variable %in% c("FPFlatfish.L_s",
                                                "LFlatfish.L_s"), ]

# # Singletons
S1 <- shelfesswss_melt[shelfesswss_melt$variable %in% c("MargalefGroundfish_s",
                                                        "KemptonQ_s"), ]
S1_nafo <- nafo_melt[nafo_melt$variable %in% c("MargalefGroundfish_s",
                                               "KemptonQ_s"), ]

S2 <- shelfesswss_melt[shelfesswss_melt$variable %in% c("BTGLargeBenthivore_s",
                                                        "BTGMediumBenthivore_s",
                                                        "BTGZoopiscivore_s"), ]
S2_nafo <- nafo_melt[nafo_melt$variable %in% c("BTGLargeBenthivore_s",
                                               "BTGMediumBenthivore_s",
                                               "BTGZoopiscivore_s"), ]

S3 <- shelfesswss_melt[shelfesswss_melt$variable %in% c("CCMediumBenthivore_s",
                                                        "CCPiscivore_s",
                                                        "CCZoopiscivore_s",
                                                        "CCLargeBenthivore_s"), ]
S3_nafo <- nafo_melt[nafo_melt$variable %in% c("CCMediumBenthivore_s",
                                               "CCPiscivore_s",
                                               "CCZoopiscivore_s",
                                               "CCLargeBenthivore_s"), ]

S4 <- shelfesswss_melt[shelfesswss_melt$variable %in% c("BInvertebrateToDemersal_s",
                                                        "MeanLengthAbundance_s",
                                                        "MeanTrophicLevel_s"), ]
S4_nafo <- nafo_melt[nafo_melt$variable %in% c("BInvertebrateToDemersal_s",
                                               "MeanLengthAbundance_s",
                                               "MeanTrophicLevel_s"), ]

S5 <- shelfesswss_melt[shelfesswss_melt$variable %in% c("MeanLifespan_s",
                                                        "MMLengthAbundance_s",
                                                        "Intrinsicvulnerabilityindex.L_s",
                                                        "BiomassTL2_s",
                                                        "InverseCVBiomass_s"), ]
S5_nafo <- nafo_melt[nafo_melt$variable %in% c("MeanLifespan_s",
                                               "MMLengthAbundance_s",
                                               "Intrinsicvulnerabilityindex.L_s",
                                               "BiomassTL2_s",
                                               "InverseCVBiomass_s"), ]

S6 <- shelfesswss_melt[shelfesswss_melt$variable %in% c("BiomassFlatfish_s",
                                                        "BiomassInvertebrates_s",
                                                        "BiomassSkates_s",
                                                        "FishinginBalance.L_s"), ]

S6_nafo <- nafo_melt[nafo_melt$variable %in% c("BiomassFlatfish_s",
                                               "BiomassInvertebrates_s",
                                               "BiomassSkates_s",
                                               "FishinginBalance.L_s"), ]

S7 <- shelfesswss_melt[shelfesswss_melt$variable %in% c("MeanTrophicLevel.L_s",
                                                        "MarineTrophicIndex.L_s",
                                                        "LLargePelagic.L_s",
                                                        "FPInvertebrates.L_s"), ]

S7_nafo <- nafo_melt[nafo_melt$variable %in% c("MeanTrophicLevel.L_s",
                                               "MarineTrophicIndex.L_s",
                                               "LLargePelagic.L_s",
                                               "FPInvertebrates.L_s"), ]

#Clusters and singletons identified in the redundancy analysis at the strata scale

#Cluster A
# 92%
# MargalefRichness
# SpeciesRichness
Ca <- strata_melt[strata_melt$variable %in% c("MargalefRichness_s",
                                              "SpeciesRichness_s"), ]
#Cluster B
# 74%
# HillN1Diversity
# ShannonDiversity
# HillN2Dominance
# PielouEvenness
Cb <- strata_melt[strata_melt$variable %in% c("HillN1Diversity_s",
                                              "ShannonDiversity_s",
                                              "HillN2Dominance_s",
                                              "PielouEvenness_s",
                                              "Heips_s"), ]
#Cluster C
# 65%
# BiomassClupeids*
# BiomassForage*
# BPelagicToDemersal*
# BTGPlanktivore*
# Biomass*
# BiomassFinfish*
# BiomassTL3*
Cc <- strata_melt[strata_melt$variable %in% c('Biomass_s',
                                              'BiomassClupeids_s',
                                              'BiomassFinfish_s',
                                              'BiomassForage_s',
                                              'BiomassTL3_s',
                                              'BPelagicToDemersal_s',
                                              'BTGPlanktivore_s'), ]
#Cluster D
# 52%
# BiomassFlatfish
# BTGMediumBenthivore
Cd <- strata_melt[strata_melt$variable %in% c("BiomassFlatfish_s",
                                              "BTGMediumBenthivore_s"), ]
#Cluster E
# 50%
# BiomassSkates
# BTGLargeBenthivore
Ce <- strata_melt[strata_melt$variable %in% c("BiomassSkates_s",
                                              "BTGLargeBenthivore_s"), ]
# Cluster F
# 44%
# BiomassGadoids
# BiomassTL4
# BTGPiscivore
# BiomassGroundfish
Cf <- strata_melt[strata_melt$variable %in% c("BiomassGadoids_s",
                                              "BiomassGroundfish_s",
                                              "BiomassTL4_s",
                                              "BTGPiscivore_s"), ]
# Cluster G
# 38%
# CommunityCondition
# CCMediumBenthivore
# CCPiscivore
Cg <- strata_melt[strata_melt$variable %in% c("CommunityCondition_s",
                                              "CCMediumBenthivore_s",
                                              "CCPiscivore_s"), ]
# Cluster H
# 37%
# MMLengthAbundance
# MMLengthBiomass
# PropPredatoryFish
# LargeFishIndicator
# MeanLengthBiomass
# LargeSpeciesIndicator
# MeanLengthAbundance
Ch <- strata_melt[strata_melt$variable %in% c("MMLengthAbundance_s",
                                              "MeanLengthAbundance_s",
                                              "LargeFishIndicator_s",
                                              "LargeSpeciesIndicator_s",
                                              "PropPredatoryFish_s",
                                              "MeanLengthBiomass_s",
                                              "MMLengthBiomass_s"), ]
# Cluster I
# 21%
# MeanLifespan
# BTGZoopiscivore
Ci <- strata_melt[strata_melt$variable %in% c("MeanLifespan_s",
                                              "BTGZoopiscivore_s"), ]

# #"Cluster" J
# 19%
# KemptonQ
# MargalefGroundfish
Cj <- strata_melt[strata_melt$variable %in% c("MargalefGroundfish_s",
                                              "KemptonQ_s"), ]
# #Singletons
# InverseCVBiomass
# MeanTrophicLevel
# CCZoopiscivore
# BiomassTL2
S_strata <- strata_melt[strata_melt$variable %in% c("InverseCVBiomass_s",
                                                    "MeanTrophicLevel_s",
                                                    "CCZoopiscivore_s",
                                                    "BiomassTL2_s"), ]



# --------SECTION V Plot clsuters & singletons *****  #  ####
AllIndi <-  list(C1, C1_nafo, C2, C2_nafo, C3, C3_nafo, C4, C4_nafo, 
                 C5, C5_nafo, C6, C6_nafo, C7, C7_nafo, C8, C8_nafo, 
                 C9, C9_nafo, C10, C10_nafo, C11, C11_nafo, C12,  C12_nafo, 
                 C13,  C13_nafo, C14,  C14_nafo, 
                 S1, S1_nafo, S2, S2_nafo, S3,  S3_nafo, S4, S4_nafo, 
                 S5, S5_nafo, S6, S6_nafo, S7, S7_nafo)

AllIndi_strata <-  list(Ca, Cb, Cc, Cd, Ce, Cf, Cg, Ch, Ci, Cj, S_strata)

source('code/IndiFunctions.R')     

pdf("output/figures/clusters&singletons/clusters&singletons_largeScales.pdf", width=20,height=8)
Plots_all_Indi <- lapply(AllIndi, PlotIndi)
Plots_all_Indi
dev.off()

pdf("output/figures/clusters&singletons/clusters&singletons_largeScales_withPoints.pdf", width=20,height=8)
lapply(AllIndi, PlotIndi_with_datapoints)
dev.off()

pdf("output/figures/clusters&singletons/clusters&singletons_strata.pdf", width=25,height=15)
lapply(AllIndi_strata, PlotIndi_strata)
dev.off()

pdf("output/figures/clusters&singletons/clusters&singletons_strataScales_withPoints.pdf", width=25,height=15)
lapply(AllIndi_strata, PlotIndi_strata_withdatapoints)
dev.off()




