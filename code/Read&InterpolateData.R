#  August 2016 
#Note: To plot indicators (without doing interpolations etc, just read first 20 lines here and go straight to Section II (standardize indicators) )
#install.packages("devtools")
setwd("C:/RProjects/UseIndicators")
#  devtools::install_github("rstudio/packrat")
#packrat::init()                     # -- https://rstudio.github.io/packrat/commands.html # Reads packages needed for plots and analysis
#disable(project = NULL, restart = TRUE)
library(ggplot2)
library(ggthemes)
theme_set(theme_bw())
library(reshape2)
system.file(package="ggplot2")      # This checks that you are NOT using private library via packrat IF you want to do ggplots
source('C:/RProjects/ExtractIndicators/R/stdize.R')
source('C:/RProjects/ExtractIndicators/R/stdizeFrame.R')
source('code/IndiFunctions.R')       # Sources functions that set ggplot settings to plot indicators per each scale (Shelf, NAFO and strata scale)
source('code/keepdropcolumn.R')
library(gridExtra)

# library(Hmisc)
# library(PerformanceAnalytics)
# library(plyr)
# library(dplyr)
# library(corrplot)
# library(zoo)
# library(Hmisc)
# library(zoo)
# library(imputeTS)

require(devtools) 
# install_github('Beothuk/bio.base', force = TRUE) 
# require(bio.base)                    # compiled functions from me, jae and brad and mike mcmahon
# require(bio.utilities)               # https://github.com/Beothuk/bio.utilities    #install_github('Beothuk/bio.utilities') #compiled functions from ACook, Jae, Brad and Mike Mcmahon
# install_github('Beothuk/bio.utilities', force = TRUE) 
library(missForest)                  # randomForest approximation to fill in missing values....full dataset using relationships bwn variables to fill in NAs
path <- file.path('C:/RProjects/ExtractIndicators')
require(ODBC)
source(paste(path,'/R/amc helpers.R',sep=""))


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
# write.csv(esswss_s, "output/data/largescales/esswsssetq_filtered&interpolated_s.csv", row.names=FALSE)

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
ess_melt <- melt(ess_s, id=c('YEAR', 'ID'))
wss_melt <- melt(wss_s, id=c('YEAR', 'ID'))
esswss_melt <- melt(esswss_s, id=c('YEAR', 'ID'))
nafo4vn_melt <- melt(nafo4vn_s, id=c('YEAR', 'ID'))
nafo4vs_melt <- melt(nafo4vs_s, id=c('YEAR', 'ID'))
nafo4w_melt <- melt(nafo4w_s, id=c('YEAR', 'ID'))
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
C1 <- shelfesswss_melt[shelfesswss_melt$variable %in% c('LClupeids.L_s',
                                                        'LForageFish.L_s',
                                                        'LGroundfish.L_s',
                                                        'LGadoids.L_s',
                                                        'LFinfish.L_s',
                                                        'Landings.L_s', 
                                                        'FPGroundfish.L_s',
                                                        'FPGadoids.L_s'), ]

C1_nafo <- nafo_melt[nafo_melt$variable %in% c('LClupeids.L_s',
                                               'LForageFish.L_s',
                                               'LGroundfish.L_s',
                                               'LGadoids.L_s',
                                               'LFinfish.L_s',
                                               'Landings.L_s', 
                                               'FPGroundfish.L_s',
                                               'FPGadoids.L_s'), ]

#   C2
C2 <- shelfesswss_melt[shelfesswss_melt$variable %in% c("FishingPressure.L_s",
                                                        "FPFinfish.L_s"), ]

C2_nafo <- nafo_melt[nafo_melt$variable %in% c("FishingPressure.L_s",
                                               "FPFinfish.L_s"), ]

# #  C3
C3 <- shelfesswss_melt[shelfesswss_melt$variable %in% c("FishinginBalance.L_s",
                                                        "MeanTrophicLevel.L_s"), ]

C3_nafo <- nafo_melt[nafo_melt$variable %in% c("FishinginBalance.L_s",
                                               "MeanTrophicLevel.L_s"), ]

# #  C4
C4 <- shelfesswss_melt[shelfesswss_melt$variable %in% c("CCPlanktivore_s",
                                                        "CommunityCondition_s"), ]

C4_nafo <- nafo_melt[nafo_melt$variable %in% c("CCPlanktivore_s",
                                               "CommunityCondition_s"), ]

# #  C5
C5 <- shelfesswss_melt[shelfesswss_melt$variable %in% c('LargeFishIndicator_s', 
                                                        'MeanLengthBiomass_s',
                                                        'MMLengthBiomass_s',
                                                        'MMLengthAbundance_s',
                                                        'PropPredatoryFish_s',
                                                        'LargeSpeciesIndicator_s'), ]

C5_nafo <- nafo_melt[nafo_melt$variable %in% c('LargeFishIndicator_s', 
                                               'MeanLengthBiomass_s',
                                               'MMLengthBiomass_s',
                                               'MMLengthAbundance_s',
                                               'PropPredatoryFish_s',
                                               'LargeSpeciesIndicator_s'), ]

# #  C6
C6 <- shelfesswss_melt[shelfesswss_melt$variable %in% c('BiomassGroundfish_s',
                                                         'BiomassGadoids_s',
                                                         'BiomassTL4_s',
                                                        'BTGPiscivore_s'), ]

C6_nafo <- nafo_melt[nafo_melt$variable %in% c('BiomassGroundfish_s',
                                                'BiomassGadoids_s',
                                                'BiomassTL4_s',
                                               'BTGPiscivore_s'), ]


# #  C7
C7 <- shelfesswss_melt[shelfesswss_melt$variable %in% c("FPSkates.L_s",
                                                        "LSkates.L_s"), ]

C7_nafo <- nafo_melt[nafo_melt$variable %in% c("FPSkates.L_s",
                                               "LSkates.L_s"), ]

# #  C8
C8 <- shelfesswss_melt[shelfesswss_melt$variable %in% c("PielouEvenness_s",
                                                        "Heips_s",
                                                        "HillN1Diversity_s",
                                                        "HillN2Dominance_s",
                                                        "ShannonDiversity_s"), ]

C8_nafo <- nafo_melt[nafo_melt$variable %in% c("PielouEvenness_s",
                                               "Heips_s",
                                               "HillN1Diversity_s",
                                               "HillN2Dominance_s",
                                               "ShannonDiversity_s"), ]
# #  C9
C9 <- shelfesswss_melt[shelfesswss_melt$variable %in% c("Biomass_s",
                                                         "BiomassClupeids_s",
                                                         "BiomassFinfish_s",
                                                         "BiomassForage_s",
                                                         "BiomassTL3_s",
                                                         "BPelagicToDemersal_s",
                                                         "BTGPlanktivore_s"), ]

C9_nafo <- nafo_melt[nafo_melt$variable %in% c("Biomass_s",
                                                "BiomassClupeids_s",
                                                "BiomassFinfish_s",
                                                "BiomassForage_s",
                                                "BiomassTL3_s",
                                                "BPelagicToDemersal_s",
                                                "BTGPlanktivore_s"), ]

C9_OnlyBiomass <- shelfesswss_melt[shelfesswss_melt$variable %in% c("Biomass_s"), ]

C9_nafoOnlyBiomass <- nafo_melt[nafo_melt$variable %in% c("Biomass_s"), ]

# #  C10
C10 <- shelfesswss_melt[shelfesswss_melt$variable %in% c("FPFlatfish.L_s",
                                                         "LFlatfish.L_s"), ]

C10_nafo <- nafo_melt[nafo_melt$variable %in% c("FPFlatfish.L_s",
                                                "LFlatfish.L_s"), ]

# #  C11
C11 <- shelfesswss_melt[shelfesswss_melt$variable %in% c("MargalefRichness_s",
                                                         "SpeciesRichness_s",
                                                         "MargalefGroundfish_s",
                                                         "KemptonQ_s"), ]

C11_nafo <- nafo_melt[nafo_melt$variable %in% c("MargalefRichness_s",
                                                "SpeciesRichness_s",
                                                "MargalefGroundfish_s",
                                                "KemptonQ_s"), ]


# #  C12
C12 <- shelfesswss_melt[shelfesswss_melt$variable %in% c("DiversityTargetSpp.L_s",
                                                         "LInvertebrates.L_s"), ]

C12_nafo <- nafo_melt[nafo_melt$variable %in% c("DiversityTargetSpp.L_s",
                                                "LInvertebrates.L_s"), ]


# #  C12
C12 <- shelfesswss_melt[shelfesswss_melt$variable %in% c("DiversityTargetSpp.L_s",
                                                         "LInvertebrates.L_s"), ]

C12_nafo <- nafo_melt[nafo_melt$variable %in% c("DiversityTargetSpp.L_s",
                                                "LInvertebrates.L_s"), ]


# #  C13
C13 <- shelfesswss_melt[shelfesswss_melt$variable %in% c("BiomassFlatfish_s",
                                                         "BTGMediumBenthivore_s"), ]

C13_nafo <- nafo_melt[nafo_melt$variable %in% c("BiomassFlatfish_s",
                                                "BTGMediumBenthivore_s"), ]

# #  C14
C14 <- shelfesswss_melt[shelfesswss_melt$variable %in% c("BiomassSkates_s",
                                                         "BTGLargeBenthivore_s"), ]

C14_nafo <- nafo_melt[nafo_melt$variable %in% c("BiomassSkates_s",
                                                "BTGLargeBenthivore_s"), ]

# #  C15
C15 <- shelfesswss_melt[shelfesswss_melt$variable %in% c("FPClupeids.L_s",
                                                         "FPForageFish.L_s"), ]

C15_nafo <- nafo_melt[nafo_melt$variable %in% c("FPClupeids.L_s",
                                                "FPForageFish.L_s"), ]


# # Singletons

S1 <- shelfesswss_melt[shelfesswss_melt$variable %in% c("BiomassTL2_s",
                                                        "BTGZoopiscivore_s"), ]

S1_nafo <- nafo_melt[nafo_melt$variable %in% c("BiomassTL2_s",
                                                "BTGZoopiscivore_s"), ]

S2 <- shelfesswss_melt[shelfesswss_melt$variable %in% c("CCPiscivore_s",
                                                        "CCZoopiscivore_s"), ]

S2_nafo <- nafo_melt[nafo_melt$variable %in% c("CCPiscivore_s",
                                               "CCZoopiscivore_s"), ]

S3 <- shelfesswss_melt[shelfesswss_melt$variable %in% c("CCMediumBenthivore_s",
                                                        "CCLargeBenthivore_s"), ]

S3_nafo <- nafo_melt[nafo_melt$variable %in% c("CCMediumBenthivore_s",
                                               "CCLargeBenthivore_s"), ]


S4 <- shelfesswss_melt[shelfesswss_melt$variable %in% c("InverseCVBiomass_s",
                                                        "Intrinsicvulnerabilityindex.L_s",
                                                        "LLargePelagic.L_s"), ]

S4_nafo <- nafo_melt[nafo_melt$variable %in% c("InverseCVBiomass_s",
                                               "Intrinsicvulnerabilityindex.L_s",
                                               "LLargePelagic.L_s"), ]


S5 <- shelfesswss_melt[shelfesswss_melt$variable %in% c("MarineTrophicIndex.L_s",
                                                        "MeanLifespan_s",
                                                        "MeanTrophicLevel_s",
                                                        "MeanLengthAbundance_s"), ]

S5_nafo <- nafo_melt[nafo_melt$variable %in% c("MarineTrophicIndex.L_s",
                                               "MeanLifespan_s",
                                               "MeanTrophicLevel_s",
                                               "MeanLengthAbundance_s"), ]



#Clusters and singletons identified in the redundancy analysis at the strata scale

Ca <- strata_melt[strata_melt$variable %in% c("MargalefRichness_s",
                                              "SpeciesRichness_s",
                                              "KemptonQ_s",
                                              "MargalefGroundfish_s"), ]

Cb <- strata_melt[strata_melt$variable %in% c('BiomassGroundfish_s',
                                              'BiomassGadoids_s',
                                              'BiomassTL4_s',
                                              'BTGPiscivore_s'), ]

Cc <- strata_melt[strata_melt$variable %in% c("BiomassFlatfish_s",
                                              "BTGMediumBenthivore_s"), ]

Cd <- strata_melt[strata_melt$variable %in% c("PielouEvenness_s",
                                              "Heips_s",
                                              "HillN1Diversity_s",
                                              "HillN2Dominance_s",
                                              "ShannonDiversity_s"), ]

Ce <- strata_melt[strata_melt$variable %in% c('LargeFishIndicator_s', 
                                              'MeanLengthBiomass_s',
                                              'MMLengthBiomass_s',
                                              'MMLengthAbundance_s',
                                              'PropPredatoryFish_s',
                                              'LargeSpeciesIndicator_s'), ]

Cf <- strata_melt[strata_melt$variable %in% c("Biomass_s",
                                              "BiomassClupeids_s",
                                              "BiomassFinfish_s",
                                              "BiomassForage_s",
                                              "BiomassTL3_s",
                                              "BPelagicToDemersal_s",
                                              "BTGPlanktivore_s"), ]

Cg <- strata_melt[strata_melt$variable %in% c("MeanLifespan_s",
                                              "BTGZoopiscivore_s"), ]

Ch <- strata_melt[strata_melt$variable %in% c("CommunityCondition_s",
                                              "CCZoopiscivore_s"), ]
 
S_strata <- strata_melt[strata_melt$variable %in% c("InverseCVBiomass_s",
                                                    "MeanTrophicLevel_s", 
                                                    "MeanLengthAbundance_s"), ]

S_strata2 <- strata_melt[strata_melt$variable %in% c("CCPiscivore_s",
                                                    "CCPlanktivore_s",
                                                    "CCMediumBenthivore_s"), ]

S_strata3 <- strata_melt[strata_melt$variable %in% c("BTGLargeBenthivore_s",
                                                     "BiomassSkates_s"), ]

# --------SECTION V Plot clusters & singletons *****  #  ####

AllIndi <-  list(C1, C1_nafo, C2, C2_nafo, C3, C3_nafo, C4, C4_nafo, 
                 C5, C5_nafo, C6, C6_nafo, C7, C7_nafo, C8, C8_nafo, 
                 C9, C9_nafo, C10, C10_nafo, C11, C11_nafo, C12, C12_nafo,
                 C13,  C13_nafo, C14,  C14_nafo, C15,  C15_nafo)

AllIndi_S <-  list(S1, S1_nafo, S2, S2_nafo, S3,  S3_nafo, S4, S4_nafo, 
                 S5, S5_nafo)

AllIndi_strata <-  list(Ca, Cb, Cc, Cd, Ce, Cf, Cg, Ch)

AllIndi_S_strata <- list(S_strata, S_strata2, S_strata3)

# #  C15
C1_2_3_10 <- shelfesswss_melt[shelfesswss_melt$variable %in% c('LClupeids.L_s',
                                                        'LForageFish.L_s',
                                                        'LGroundfish.L_s',
                                                        'LGadoids.L_s',
                                                        'LFinfish.L_s',
                                                        'Landings.L_s', 
                                                        'FPGroundfish.L_s',
                                                        'FPGadoids.L_s',
                                                        'FPFlatfish.L_s',
                                                        'LFlatfish.L_s',
                                                        'FishinginBalance.L_s',
                                                        'MeanTrophicLevel.L_s',
                                                        'FishingPressure.L_s',
                                                        'FPFinfish.L_s'), ]

C1_2_3_10_nafo <- nafo_melt[nafo_melt$variable %in% c('LClupeids.L_s',
                                                  'LForageFish.L_s',
                                                  'LGroundfish.L_s',
                                                  'LGadoids.L_s',
                                                  'LFinfish.L_s',
                                                  'Landings.L_s', 
                                                  'FPGroundfish.L_s',
                                                  'FPGadoids.L_s',
                                                  'FPFlatfish.L_s',
                                                  'LFlatfish.L_s',
                                                  'FishinginBalance.L_s',
                                                  'MeanTrophicLevel.L_s',
                                                  'FishingPressure.L_s',
                                                  'FPFinfish.L_s'), ]

 Other  <-  list(C1C10, C1C10_nafo)
 lapply(Other, PlotIndi_with_line)
 png("output/figures/clusters&singletons/C1&C2&C3&C10.png", width=1100,height=800, res=72)
 grid.arrange(PlotIndi_with_line(C1_2_3_10), PlotIndi_with_line(C1_2_3_10_nafo))
 dev.off()
 
# lapply(Other, PlotIndi)
# lapply(Other, PlotIndi_with_datapoints)

 # **** Save strata scale clusters ###
pdf("output/figures/clusters&singletons/clusters_largeScales.pdf", width=22,height=8)
Plots_all_Indi <- lapply(AllIndi, PlotIndi_with_line)
Plots_all_Indi
dev.off()

# **** Save same plot as above but as individual png files 

#pdf("output/figures/clusters&singletons/singletons_largeScales.pdf", width=22,height=8)
Plots_all_Indi_S <- lapply(AllIndi_S, PlotIndi_with_line)
Plots_all_Indi_S
dev.off()

# **** Save same plot as above but as individual png files 
png("output/figures/clusters&singletons/cluster_1.png", width=1100,height=800, res=72)
grid.arrange(PlotIndi_with_line(C1), PlotIndi_with_line(C1_nafo))
dev.off()
png("output/figures/clusters&singletons/cluster_2.png", width=1100,height=800, res=72)
grid.arrange(PlotIndi_with_line(C2), PlotIndi_with_line(C2_nafo))
dev.off()
png("output/figures/clusters&singletons/cluster_3.png", width=1100,height=800, res=72)
grid.arrange(PlotIndi_with_line(C3), PlotIndi_with_line(C3_nafo))
dev.off()
png("output/figures/clusters&singletons/cluster_4.png", width=1100,height=800, res=72)
grid.arrange(PlotIndi_with_line(C4), PlotIndi_with_line(C4_nafo))
dev.off()
png("output/figures/clusters&singletons/cluster_5.png", width=1100,height=800, res=72)
grid.arrange(PlotIndi_with_line(C5), PlotIndi_with_line(C5_nafo))
dev.off()
png("output/figures/clusters&singletons/cluster_6.png", width=1100,height=800, res=72)
grid.arrange(PlotIndi_with_line(C6), PlotIndi_with_line(C6_nafo))
dev.off()
png("output/figures/clusters&singletons/cluster_7.png", width=1100,height=800, res=72)
grid.arrange(PlotIndi_with_line(C7), PlotIndi_with_line(C7_nafo))
dev.off()
png("output/figures/clusters&singletons/cluster_8.png", width=1100,height=800, res=72)
grid.arrange(PlotIndi_with_line(C8), PlotIndi_with_line(C8_nafo))
dev.off()
png("output/figures/clusters&singletons/cluster_9.png", width=1100,height=800, res=72)
grid.arrange(PlotIndi_with_line(C9), PlotIndi_with_line(C9_nafo))
dev.off()
png("output/figures/clusters&singletons/cluster_10.png", width=1100,height=800, res=72)
grid.arrange(PlotIndi_with_line(C10), PlotIndi_with_line(C10_nafo))
dev.off()
png("output/figures/clusters&singletons/cluster_11.png", width=1100,height=800, res=72)
grid.arrange(PlotIndi_with_line(C11), PlotIndi_with_line(C11_nafo))
dev.off()
png("output/figures/clusters&singletons/cluster_12.png", width=1100,height=800, res=72)
grid.arrange(PlotIndi_with_line(C12), PlotIndi_with_line(C12_nafo))
dev.off()
png("output/figures/clusters&singletons/cluster_13.png", width=1100,height=800, res=72)
grid.arrange(PlotIndi_with_line(C13), PlotIndi_with_line(C13_nafo))
dev.off()
png("output/figures/clusters&singletons/cluster_14.png", width=1100,height=800, res=72)
grid.arrange(PlotIndi_with_line(C14), PlotIndi_with_line(C14_nafo))
dev.off()
png("output/figures/clusters&singletons/cluster_15.png", width=1100,height=800, res=72)
grid.arrange(PlotIndi_with_line(C15), PlotIndi_with_line(C15_nafo))
dev.off()

png("output/figures/clusters&singletons/C9_OnlyBiomass.png", width=1100,height=800, res=72)
grid.arrange(PlotIndi_with_line(C9_OnlyBiomass), PlotIndi_with_line(C9_nafoOnlyBiomass))
dev.off()


# **** Save strata scale clusters 
pdf("output/figures/clusters&singletons/clusters_strata.pdf", width=25,height=20)
lapply(AllIndi_strata, PlotIndi_strata_withline)
dev.off()

# **** Save same plot as above but as individual png files 
png("output/figures/clusters&singletons/cluster_a.png", width=1800,height=1200, res=72)
PlotIndi_strata_withline(Ca)
dev.off()
png("output/figures/clusters&singletons/cluster_b.png", width=1800,height=1200, res=72)
PlotIndi_strata_withline(Cb)
dev.off()
png("output/figures/clusters&singletons/cluster_c.png", width=1800,height=1200, res=72)
PlotIndi_strata_withline(Cc)
dev.off()
png("output/figures/clusters&singletons/cluster_d.png", width=1800,height=1200, res=72)
PlotIndi_strata_withline(Cd)
dev.off()
png("output/figures/clusters&singletons/cluster_e.png", width=1800,height=1200, res=72)
PlotIndi_strata_withline(Ce)
dev.off()
png("output/figures/clusters&singletons/cluster_f.png", width=1800,height=1200, res=72)
PlotIndi_strata_withline(Cf)
dev.off()
png("output/figures/clusters&singletons/cluster_g.png", width=1800,height=1200, res=72)
PlotIndi_strata_withline(Cg)
dev.off()
png("output/figures/clusters&singletons/cluster_h.png", width=1800,height=1200, res=72)
PlotIndi_strata_withline(Ch)
dev.off()

#pdf("output/figures/clusters&singletons/singletons_strata.pdf", width=25,height=20)
lapply(AllIndi_S_strata, PlotIndi_strata_withline)
dev.off()


# --------SECTION VI Plot final suite of indi per attribute *****  #  ####

###################### Report card plots ### # ---------------

##############################ESS*************************************************#
MargalefRichness_ess <- ess_melt[ess_melt$variable %in% c("MargalefRichness"), ]
ShannonDiversity_ess <- ess_melt[ess_melt$variable %in% c("ShannonDiversity"), ]
png("output/figures/ReportCard/Biodiv_ess.png", width=850,height=420, res=72)
grid.arrange(PlotIndi_with_line_noPanels(MargalefRichness_ess), PlotIndi_with_line_noPanels(ShannonDiversity_ess))
dev.off()

Biomass_ess <- ess_melt[ess_melt$variable %in% c("Biomass"), ]
BiomassGroundfish_ess <- ess_melt[ess_melt$variable %in% c("BiomassGroundfish"), ]
BiomassFlatfish_ess <- ess_melt[ess_melt$variable %in% c("BiomassFlatfish"), ]
BiomassSkates_ess <- ess_melt[ess_melt$variable %in% c("BiomassSkates"), ]
BiomassInvertebrates_ess <- ess_melt2[ess_melt2$variable %in% c("BiomassInvertebrates"), ]

png("output/figures/ReportCard/ResourcePotential_1_ess.png", width=800,height=630, res=72)
grid.arrange(PlotIndi_with_line_noPanels(BiomassInvertebrates_ess), 
             PlotIndi_with_line_noPanels(BiomassFlatfish_ess),
             PlotIndi_with_line_noPanels(BiomassSkates_ess))
dev.off()
png("output/figures/ReportCard/ResourcePotential_2_ess.png", width=800,height=420, res=72)
grid.arrange(PlotIndi_with_line_noPanels(Biomass_ess), 
             PlotIndi_with_line_noPanels(BiomassGroundfish_ess))
dev.off()


MeanLifespan_ess <- ess_melt[ess_melt$variable %in% c("MeanLifespan"), ]
Intrinsicvulnerabilityindex_ess <- ess_melt[ess_melt$variable %in% c("Intrinsicvulnerabilityindex.L"), ]
InverseCVBiomass_ess <- ess_melt[ess_melt$variable %in% c("InverseCVBiomass"), ]
BiomassTL2_ess <- ess_melt[ess_melt$variable %in% c("BiomassTL2"), ]

png("output/figures/ReportCard/St&RstoPert_ess.png", width=800,height=840, res=72)
grid.arrange(PlotIndi_with_line_noPanels(MeanLifespan_ess), 
             PlotIndi_with_line_noPanels(Intrinsicvulnerabilityindex_ess),
             PlotIndi_with_line_noPanels(InverseCVBiomass_ess),
             PlotIndi_with_line_noPanels(BiomassTL2_ess), ncol=1)
dev.off()



LargeFishIndicator_ess <- ess_melt[ess_melt$variable %in% c("LargeFishIndicator"), ]
MeanLengthAbundance_ess <- ess_melt[ess_melt$variable %in% c("MeanLengthAbundance"), ]
MeanTrophicLevel_ess <- ess_melt[ess_melt$variable %in% c("MeanTrophicLevel"), ]
CommunityCondition_ess <- ess_melt[ess_melt$variable %in% c("CommunityCondition"), ]
CCMediumBenthivore_ess <- ess_melt[ess_melt$variable %in% c("CCMediumBenthivore"), ]
CCPiscivore_ess <- ess_melt[ess_melt$variable %in% c("CCPiscivore"), ]
CCZoopiscivore_ess <- ess_melt[ess_melt$variable %in% c("CCZoopiscivore"), ]
CCLargeBenthivore_ess <- ess_melt[ess_melt$variable %in% c("CCLargeBenthivore"), ]
BInvertebrateToDemersal_ess <- ess_melt2[ess_melt2$variable %in% c("BInvertebrateToDemersal"), ]
BTGZoopiscivore_ess <- ess_melt[ess_melt$variable %in% c("BTGZoopiscivore"), ]

png("output/figures/ReportCard/St&Function_1_ess.png", width=800,height=630, res=72)
grid.arrange(PlotIndi_with_line_noPanels(LargeFishIndicator_ess), 
             PlotIndi_with_line_noPanels(MeanLengthAbundance_ess),
             PlotIndi_with_line_noPanels(MeanTrophicLevel_ess), ncol=1)
dev.off()

png("output/figures/ReportCard/St&Function_2_ess.png", width=800,height=1060, res=72)
grid.arrange(PlotIndi_with_line_noPanels(CommunityCondition_ess), 
             PlotIndi_with_line_noPanels(CCPiscivore_ess),
             PlotIndi_with_line_noPanels(CCZoopiscivore_ess),
             PlotIndi_with_line_noPanels(CCMediumBenthivore_ess), 
             PlotIndi_with_line_noPanels(CCLargeBenthivore_ess), ncol=1)
dev.off()

png("output/figures/ReportCard/St&Function_3_ess.png", width=800,height=420, res=72)
grid.arrange(PlotIndi_with_line_noPanels(BInvertebrateToDemersal_ess), 
             PlotIndi_with_line_noPanels(BTGZoopiscivore_ess))
dev.off()



FishingPressure.L_ess <- ess_melt[ess_melt$variable %in% c("FishingPressure.L"), ]
FPClupeids.L_ess <- ess_melt[ess_melt$variable %in% c("FPClupeids.L"), ]
FPInvertebrates.L_ess <- ess_melt2[ess_melt2$variable %in% c("FPInvertebrates.L"), ]
MeanTrophicLevel.L_ess <- ess_melt[ess_melt$variable %in% c("MeanTrophicLevel.L"), ]
MarineTrophicIndex.L_ess <- ess_melt[ess_melt$variable %in% c("MarineTrophicIndex.L"), ]
DiversityTargetSpp.L_ess <- ess_melt[ess_melt$variable %in% c("DiversityTargetSpp.L"), ]
Landings.L_ess <- ess_melt[ess_melt$variable %in% c("Landings.L"), ]
LSkates.L_ess <- ess_melt[ess_melt$variable %in% c("LSkates.L"), ]
LFlatfish.L_ess <- ess_melt[ess_melt$variable %in% c("LFlatfish.L"), ]
LLargePelagic.L_ess <- ess_melt[ess_melt$variable %in% c("LLargePelagic.L"), ]

png("output/figures/ReportCard/FishingPressure_1_ess.png", width=800,height=630, res=72)
grid.arrange(PlotIndi_with_line_noPanels(FishingPressure.L_ess), 
             PlotIndi_with_line_noPanels(FPClupeids.L_ess),
             PlotIndi_with_line_noPanels(FPInvertebrates.L_ess), ncol=1)
dev.off()

png("output/figures/ReportCard/FishingPressure_2_ess.png", width=800,height=840, res=72)
grid.arrange(PlotIndi_with_line_noPanels(Landings.L_ess), 
             PlotIndi_with_line_noPanels(LSkates.L_ess),
             PlotIndi_with_line_noPanels(LFlatfish.L_ess),
             PlotIndi_with_line_noPanels(LLargePelagic.L_ess), ncol=1)
dev.off()

png("output/figures/ReportCard/FishingPressure_3_ess.png", width=800,height=630, res=72)
grid.arrange(PlotIndi_with_line_noPanels(MeanTrophicLevel.L_ess), 
             PlotIndi_with_line_noPanels(MarineTrophicIndex.L_ess),
             PlotIndi_with_line_noPanels(DiversityTargetSpp.L_ess), ncol=1)
dev.off()

##############################WSS*************************************************#

MargalefRichness_wss <- wss_melt[wss_melt$variable %in% c("MargalefRichness"), ]
ShannonDiversity_wss <- wss_melt[wss_melt$variable %in% c("ShannonDiversity"), ]
png("output/figures/ReportCard/Biodiv_wss.png", width=850,height=420, res=72)
grid.arrange(PlotIndi_with_line_noPanels(MargalefRichness_wss), PlotIndi_with_line_noPanels(ShannonDiversity_wss))
dev.off()

Biomass_wss <- wss_melt[wss_melt$variable %in% c("Biomass"), ]
BiomassGroundfish_wss <- wss_melt[wss_melt$variable %in% c("BiomassGroundfish"), ]
BiomassFlatfish_wss <- wss_melt[wss_melt$variable %in% c("BiomassFlatfish"), ]
BiomassSkates_wss <- wss_melt[wss_melt$variable %in% c("BiomassSkates"), ]
BiomassInvertebrates_wss <- wss_melt2[wss_melt2$variable %in% c("BiomassInvertebrates"), ]

png("output/figures/ReportCard/ResourcePotential_1_wss.png", width=800,height=630, res=72)
grid.arrange(PlotIndi_with_line_noPanels(BiomassInvertebrates_wss), 
             PlotIndi_with_line_noPanels(BiomassFlatfish_wss),
             PlotIndi_with_line_noPanels(BiomassSkates_wss))
dev.off()
png("output/figures/ReportCard/ResourcePotential_2_wss.png", width=800,height=420, res=72)
grid.arrange(PlotIndi_with_line_noPanels(Biomass_wss), 
             PlotIndi_with_line_noPanels(BiomassGroundfish_wss))
dev.off()


MeanLifespan_wss <- wss_melt[wss_melt$variable %in% c("MeanLifespan"), ]
Intrinsicvulnerabilityindex_wss <- wss_melt[wss_melt$variable %in% c("Intrinsicvulnerabilityindex.L"), ]
InverseCVBiomass_wss <- wss_melt[wss_melt$variable %in% c("InverseCVBiomass"), ]
BiomassTL2_wss <- wss_melt[wss_melt$variable %in% c("BiomassTL2"), ]

png("output/figures/ReportCard/St&RstoPert_wss.png", width=800,height=840, res=72)
grid.arrange(PlotIndi_with_line_noPanels(MeanLifespan_wss), 
             PlotIndi_with_line_noPanels(Intrinsicvulnerabilityindex_wss),
             PlotIndi_with_line_noPanels(InverseCVBiomass_wss),
             PlotIndi_with_line_noPanels(BiomassTL2_wss), ncol=1)
dev.off()



LargeFishIndicator_wss <- wss_melt[wss_melt$variable %in% c("LargeFishIndicator"), ]
MeanLengthAbundance_wss <- wss_melt[wss_melt$variable %in% c("MeanLengthAbundance"), ]
MeanTrophicLevel_wss <- wss_melt[wss_melt$variable %in% c("MeanTrophicLevel"), ]
CommunityCondition_wss <- wss_melt[wss_melt$variable %in% c("CommunityCondition"), ]
CCMediumBenthivore_wss <- wss_melt[wss_melt$variable %in% c("CCMediumBenthivore"), ]
CCPiscivore_wss <- wss_melt[wss_melt$variable %in% c("CCPiscivore"), ]
CCZoopiscivore_wss <- wss_melt[wss_melt$variable %in% c("CCZoopiscivore"), ]
CCLargeBenthivore_wss <- wss_melt[wss_melt$variable %in% c("CCLargeBenthivore"), ]
BInvertebrateToDemersal_wss <- wss_melt2[wss_melt2$variable %in% c("BInvertebrateToDemersal"), ]
BTGZoopiscivore_wss <- wss_melt[wss_melt$variable %in% c("BTGZoopiscivore"), ]

png("output/figures/ReportCard/St&Function_1_wss.png", width=800,height=630, res=72)
grid.arrange(PlotIndi_with_line_noPanels(LargeFishIndicator_wss), 
             PlotIndi_with_line_noPanels(MeanLengthAbundance_wss),
             PlotIndi_with_line_noPanels(MeanTrophicLevel_wss), ncol=1)
dev.off()

png("output/figures/ReportCard/St&Function_2_wss.png", width=800,height=1060, res=72)
grid.arrange(PlotIndi_with_line_noPanels(CommunityCondition_wss), 
             PlotIndi_with_line_noPanels(CCPiscivore_wss),
             PlotIndi_with_line_noPanels(CCZoopiscivore_wss),
             PlotIndi_with_line_noPanels(CCMediumBenthivore_wss), 
             PlotIndi_with_line_noPanels(CCLargeBenthivore_wss), ncol=1)
dev.off()

png("output/figures/ReportCard/St&Function_3_wss.png", width=800,height=420, res=72)
grid.arrange(PlotIndi_with_line_noPanels(BInvertebrateToDemersal_wss), 
             PlotIndi_with_line_noPanels(BTGZoopiscivore_wss))
dev.off()



FishingPressure.L_wss <- wss_melt[wss_melt$variable %in% c("FishingPressure.L"), ]
FPClupeids.L_wss <- wss_melt[wss_melt$variable %in% c("FPClupeids.L"), ]
FPInvertebrates.L_wss <- wss_melt2[wss_melt2$variable %in% c("FPInvertebrates.L"), ]
MeanTrophicLevel.L_wss <- wss_melt[wss_melt$variable %in% c("MeanTrophicLevel.L"), ]
MarineTrophicIndex.L_wss <- wss_melt[wss_melt$variable %in% c("MarineTrophicIndex.L"), ]
DiversityTargetSpp.L_wss <- wss_melt[wss_melt$variable %in% c("DiversityTargetSpp.L"), ]
Landings.L_wss <- wss_melt[wss_melt$variable %in% c("Landings.L"), ]
LSkates.L_wss <- wss_melt[wss_melt$variable %in% c("LSkates.L"), ]
LFlatfish.L_wss <- wss_melt[wss_melt$variable %in% c("LFlatfish.L"), ]
LLargePelagic.L_wss <- wss_melt[wss_melt$variable %in% c("LLargePelagic.L"), ]

png("output/figures/ReportCard/FishingPressure_1_wss.png", width=800,height=630, res=72)
grid.arrange(PlotIndi_with_line_noPanels(FishingPressure.L_wss), 
             PlotIndi_with_line_noPanels(FPClupeids.L_wss),
             PlotIndi_with_line_noPanels(FPInvertebrates.L_wss), ncol=1)
dev.off()

png("output/figures/ReportCard/FishingPressure_2_wss.png", width=800,height=840, res=72)
grid.arrange(PlotIndi_with_line_noPanels(Landings.L_wss), 
             PlotIndi_with_line_noPanels(LSkates.L_wss),
             PlotIndi_with_line_noPanels(LFlatfish.L_wss),
             PlotIndi_with_line_noPanels(LLargePelagic.L_wss), ncol=1)
dev.off()

png("output/figures/ReportCard/FishingPressure_3_wss.png", width=800,height=630, res=72)
grid.arrange(PlotIndi_with_line_noPanels(MeanTrophicLevel.L_wss), 
             PlotIndi_with_line_noPanels(MarineTrophicIndex.L_wss),
             PlotIndi_with_line_noPanels(DiversityTargetSpp.L_wss), ncol=1)
dev.off()

##############################END*************************************************#


Biodiv_shelf <- shelf_melt[shelf_melt$variable %in% c("MargalefRichness_s",
                                                "ShannonDiversity_s"), ]

Biodiv_ess <- ess_melt[ess_melt$variable %in% c("MargalefRichness_s",
                                               "ShannonDiversity_s"), ]

Biodiv_wss <- wss_melt[wss_melt$variable %in% c("MargalefRichness_s",
                                                  "ShannonDiversity_s"), ]

Biodiv_nafo4vn <- nafo4vn_melt[nafo4vn_melt$variable %in% c("MargalefRichness_s",
                                                  "ShannonDiversity_s"), ]

Biodiv_nafo4vs <- nafo4vs_melt[nafo4vs_melt$variable %in% c("MargalefRichness_s",
                                                  "ShannonDiversity_s"), ]

Biodiv_nafo4w <- nafo4w_melt[nafo4w_melt$variable %in% c("MargalefRichness_s",
                                                  "ShannonDiversity_s"), ]

Biodiv_nafo4x <- nafo4x_melt[nafo4x_melt$variable %in% c("MargalefRichness_s",
                                                  "ShannonDiversity_s"), ]

Biodiv  <-  list(Biodiv_shelf,Biodiv_ess,Biodiv_wss,Biodiv_nafo4vn,Biodiv_nafo4vs,Biodiv_nafo4w, Biodiv_nafo4x)

pdf("output/figures/Biodiversity.pdf", width=10,height=8)
lapply(Biodiv, PlotIndi_with_line)
dev.off()





StrFc1_shelf <- shelf_melt[shelf_melt$variable %in% c("LargeFishIndicator_s",
                                                      "MeanLengthAbundance_s",
                                                      "MeanLengthBiomass_s"), ]

StrFc1_ess <- ess_melt[ess_melt$variable %in% c("LargeFishIndicator_s",
                                               "MeanLengthAbundance_s",
                                               "MeanLengthBiomass_s"), ]

StrFc1_wss <- wss_melt[wss_melt$variable %in% c("LargeFishIndicator_s",
                                               "MeanLengthAbundance_s",
                                               "MeanLengthBiomass_s"), ]

StrFc1_nafo4vn <- nafo4vn_melt[nafo4vn_melt$variable %in% c("LargeFishIndicator_s",
                                                           "MeanLengthAbundance_s",
                                                           "MeanLengthBiomass_s"), ]

StrFc1_nafo4vs <- nafo4vs_melt[nafo4vs_melt$variable %in% c("LargeFishIndicator_s",
                                                           "MeanLengthAbundance_s",
                                                           "MeanLengthBiomass_s"), ]

StrFc1_nafo4w <- nafo4w_melt[nafo4w_melt$variable %in% c("LargeFishIndicator_s",
                                                        "MeanLengthAbundance_s",
                                                        "MeanLengthBiomass_s"), ]

StrFc1_nafo4x <- nafo4x_melt[nafo4x_melt$variable %in% c("LargeFishIndicator_s",
                                                      "MeanLengthAbundance_s",
                                                      "MeanLengthBiomass_s"), ]

StrFc2_shelf <- shelf_melt[shelf_melt$variable %in% c("CommunityCondition_s",
                                                     "CCPiscivore_s",
                                                     "CCZoopiscivore_s"), ]

StrFc2_ess <- ess_melt[ess_melt$variable %in% c("CommunityCondition_s",
                                                "CCPiscivore_s",
                                                "CCZoopiscivore_s"), ]

StrFc2_wss <- wss_melt[wss_melt$variable %in% c("CommunityCondition_s",
                                                "CCPiscivore_s",
                                                "CCZoopiscivore_s"), ]

StrFc2_nafo4vn <- nafo4vn_melt[nafo4vn_melt$variable %in% c("CommunityCondition_s",
                                                            "CCPiscivore_s",
                                                            "CCZoopiscivore_s"), ]

StrFc2_nafo4vs <- nafo4vs_melt[nafo4vs_melt$variable %in% c("CommunityCondition_s",
                                                            "CCPiscivore_s",
                                                            "CCZoopiscivore_s"), ]

StrFc2_nafo4w <- nafo4w_melt[nafo4w_melt$variable %in% c("CommunityCondition_s",
                                                         "CCPiscivore_s",
                                                         "CCZoopiscivore_s"), ]

StrFc2_nafo4x <- nafo4x_melt[nafo4x_melt$variable %in% c("CommunityCondition_s",
                                                         "CCPiscivore_s",
                                                         "CCZoopiscivore_s"), ]

StrFc2 <- list(StrFc2_shelf,StrFc2_ess,StrFc2_wss,StrFc2_nafo4vn,StrFc2_nafo4vs,StrFc2_nafo4w, StrFc2_nafo4x)


StrFc3_shelf <- shelf_melt[shelf_melt$variable %in% c("CCMediumBenthivore_s",
                                                      "CCLargeBenthivore_s"), ]

StrFc3_ess <- ess_melt[ess_melt$variable %in% c("CCMediumBenthivore_s",
                                                "CCLargeBenthivore_s"), ]

StrFc3_wss <- wss_melt[wss_melt$variable %in% c("CCMediumBenthivore_s",
                                                "CCLargeBenthivore_s"), ]

StrFc3_nafo4vn <- nafo4vn_melt[nafo4vn_melt$variable %in% c("CCMediumBenthivore_s",
                                                            "CCLargeBenthivore_s"), ]

StrFc3_nafo4vs <- nafo4vs_melt[nafo4vs_melt$variable %in% c("CCMediumBenthivore_s",
                                                            "CCLargeBenthivore_s"), ]

StrFc3_nafo4w <- nafo4w_melt[nafo4w_melt$variable %in% c("CCMediumBenthivore_s",
                                                         "CCLargeBenthivore_s"), ]

StrFc3_nafo4x <- nafo4x_melt[nafo4x_melt$variable %in% c("CCMediumBenthivore_s",
                                                         "CCLargeBenthivore_s"), ]

#

StrFc4_shelf <- shelf_melt[shelf_melt$variable %in% c("MeanTrophicLevel_s",
                                                      "BInvertebrateToDemersal_s",
                                                      "BPelagicToDemersal_s"), ]

StrFc4_ess <- ess_melt[ess_melt$variable %in% c("MeanTrophicLevel_s",
                                                "BInvertebrateToDemersal_s",
                                                "BPelagicToDemersal_s"), ]

StrFc4_wss <- wss_melt[wss_melt$variable %in% c("MeanTrophicLevel_s",
                                                "BInvertebrateToDemersal_s",
                                                "BPelagicToDemersal_s"), ]

StrFc4_nafo4vn <- nafo4vn_melt[nafo4vn_melt$variable %in% c("MeanTrophicLevel_s",
                                                            "BInvertebrateToDemersal_s",
                                                            "BPelagicToDemersal_s"), ]

StrFc4_nafo4vs <- nafo4vs_melt[nafo4vs_melt$variable %in% c("MeanTrophicLevel_s",
                                                            "BInvertebrateToDemersal_s",
                                                            "BPelagicToDemersal_s"), ]

StrFc4_nafo4w <- nafo4w_melt[nafo4w_melt$variable %in% c("MeanTrophicLevel_s",
                                                         "BInvertebrateToDemersal_s",
                                                         "BPelagicToDemersal_s"), ]

StrFc4_nafo4x <- nafo4x_melt[nafo4x_melt$variable %in% c("MeanTrophicLevel_s",
                                                         "BInvertebrateToDemersal_s",
                                                         "BPelagicToDemersal_s"), ]

#
StrFc5_shelf <- shelf_melt[shelf_melt$variable %in% c("BTGPlanktivore_s",
                                                      "BTGPiscivore_s",
                                                      "BTGZoopiscivore_s"), ]

StrFc5_ess <- ess_melt[ess_melt$variable %in% c("BTGPlanktivore_s",
                                                "BTGPiscivore_s",
                                                "BTGZoopiscivore_s"), ]

StrFc5_wss <- wss_melt[wss_melt$variable %in% c("BTGPlanktivore_s",
                                                "BTGPiscivore_s",
                                                "BTGZoopiscivore_s"), ]

StrFc5_nafo4vn <- nafo4vn_melt[nafo4vn_melt$variable %in% c("BTGPlanktivore_s",
                                                            "BTGPiscivore_s",
                                                            "BTGZoopiscivore_s"), ]

StrFc5_nafo4vs <- nafo4vs_melt[nafo4vs_melt$variable %in% c("BTGPlanktivore_s",
                                                            "BTGPiscivore_s",
                                                            "BTGZoopiscivore_s"), ]

StrFc5_nafo4w <- nafo4w_melt[nafo4w_melt$variable %in% c("BTGPlanktivore_s",
                                                         "BTGPiscivore_s",
                                                         "BTGZoopiscivore_s"), ]

StrFc5_nafo4x <- nafo4x_melt[nafo4x_melt$variable %in% c("BTGPlanktivore_s",
                                                         "BTGPiscivore_s",
                                                         "BTGZoopiscivore_s"), ]

#
StrFc6_shelf <- shelf_melt[shelf_melt$variable %in% c("BTGLargeBenthivore_s",
                                                      "BTGMediumBenthivore_s"), ]

StrFc6_ess <- ess_melt[ess_melt$variable %in% c("BTGLargeBenthivore_s",
                                                "BTGMediumBenthivore_s"), ]

StrFc6_wss <- wss_melt[wss_melt$variable %in% c("BTGLargeBenthivore_s",
                                                "BTGMediumBenthivore_s"), ]

StrFc6_nafo4vn <- nafo4vn_melt[nafo4vn_melt$variable %in% c("BTGLargeBenthivore_s",
                                                            "BTGMediumBenthivore_s"), ]

StrFc6_nafo4vs <- nafo4vs_melt[nafo4vs_melt$variable %in% c("BTGLargeBenthivore_s",
                                                            "BTGMediumBenthivore_s"), ]

StrFc6_nafo4w <- nafo4w_melt[nafo4w_melt$variable %in% c("BTGLargeBenthivore_s",
                                                         "BTGMediumBenthivore_s"), ]

StrFc6_nafo4x <- nafo4x_melt[nafo4x_melt$variable %in% c("BTGLargeBenthivore_s",
                                                         "BTGMediumBenthivore_s"), ]


StrFc <- list(StrFc1_shelf,StrFc1_ess,StrFc1_wss,StrFc1_nafo4vn,StrFc1_nafo4vs,StrFc1_nafo4w, StrFc1_nafo4x,
               StrFc2_shelf,StrFc2_ess,StrFc2_wss,StrFc2_nafo4vn,StrFc2_nafo4vs,StrFc2_nafo4w, StrFc2_nafo4x,
               StrFc3_shelf,StrFc3_ess,StrFc3_wss,StrFc3_nafo4vn,StrFc3_nafo4vs,StrFc3_nafo4w, StrFc3_nafo4x,
               StrFc4_shelf,StrFc4_ess,StrFc4_wss,StrFc4_nafo4vn,StrFc4_nafo4vs,StrFc4_nafo4w, StrFc4_nafo4x,
               StrFc5_shelf,StrFc5_ess,StrFc5_wss,StrFc5_nafo4vn,StrFc5_nafo4vs,StrFc5_nafo4w, StrFc5_nafo4x,
               StrFc6_shelf,StrFc6_ess,StrFc6_wss,StrFc6_nafo4vn,StrFc6_nafo4vs,StrFc6_nafo4w, StrFc6_nafo4x)

pdf("output/figures/Structure&Functioning.pdf", width=10,height=8)
lapply(StrFc, PlotIndi_with_line)
dev.off()


StR1_shelf <- shelf_melt[shelf_melt$variable %in% c("MeanLifespan_s",
                                                      "Intrinsicvulnerabilityindex.L_s"), ]

StR1_ess <- ess_melt[ess_melt$variable %in% c("MeanLifespan_s",
                                              "Intrinsicvulnerabilityindex.L_s"), ]

StR1_wss <- wss_melt[wss_melt$variable %in% c("MeanLifespan_s",
                                              "Intrinsicvulnerabilityindex.L_s"), ]

StR1_nafo4vn <- nafo4vn_melt[nafo4vn_melt$variable %in% c("MeanLifespan_s",
                                                          "Intrinsicvulnerabilityindex.L_s"), ]

StR1_nafo4vs <- nafo4vs_melt[nafo4vs_melt$variable %in% c("MeanLifespan_s",
                                                          "Intrinsicvulnerabilityindex.L_s"), ]

StR1_nafo4w <- nafo4w_melt[nafo4w_melt$variable %in% c("MeanLifespan_s",
                                                       "Intrinsicvulnerabilityindex.L_s"), ]

StR1_nafo4x <- nafo4x_melt[nafo4x_melt$variable %in% c("MeanLifespan_s",
                                                       "Intrinsicvulnerabilityindex.L_s"), ]

#
StR2_shelf <- shelf_melt[shelf_melt$variable %in% c("MMLengthAbundance_s",
                                                    "MMLengthBiomass_s"), ]

StR2_ess <- ess_melt[ess_melt$variable %in% c("MMLengthAbundance_s",
                                              "MMLengthBiomass_s"), ]

StR2_wss <- wss_melt[wss_melt$variable %in% c("MMLengthAbundance_s",
                                              "MMLengthBiomass_s"), ]

StR2_nafo4vn <- nafo4vn_melt[nafo4vn_melt$variable %in% c("MMLengthAbundance_s",
                                                          "MMLengthBiomass_s"), ]

StR2_nafo4vs <- nafo4vs_melt[nafo4vs_melt$variable %in% c("MMLengthAbundance_s",
                                                          "MMLengthBiomass_s"), ]

StR2_nafo4w <- nafo4w_melt[nafo4w_melt$variable %in% c("MMLengthAbundance_s",
                                                       "MMLengthBiomass_s"), ]

StR2_nafo4x <- nafo4x_melt[nafo4x_melt$variable %in% c("MMLengthAbundance_s",
                                                       "MMLengthBiomass_s"), ]

#
StR3_shelf <- shelf_melt[shelf_melt$variable %in% c("InverseCVBiomass_s"), ]

StR3_ess <- ess_melt[ess_melt$variable %in% c("InverseCVBiomass_s"), ]

StR3_wss <- wss_melt[wss_melt$variable %in% c("InverseCVBiomass_s"), ]

StR3_nafo4vn <- nafo4vn_melt[nafo4vn_melt$variable %in% c("InverseCVBiomass_s"), ]

StR3_nafo4vs <- nafo4vs_melt[nafo4vs_melt$variable %in% c("InverseCVBiomass_s"), ]

StR3_nafo4w <- nafo4w_melt[nafo4w_melt$variable %in% c("InverseCVBiomass_s"), ]

StR3_nafo4x <- nafo4x_melt[nafo4w_melt$variable %in% c("InverseCVBiomass_s"), ]
#

StR <- list(StR1_shelf,StR1_ess,StR1_wss,StR1_nafo4vn,StR1_nafo4vs,StR1_nafo4w, StR1_nafo4x,
              StR2_shelf,StR2_ess,StR2_wss,StR2_nafo4vn,StR2_nafo4vs,StR2_nafo4w, StR2_nafo4x,
              StR3_shelf,StR3_ess,StR3_wss,StR3_nafo4vn,StR3_nafo4vs,StR3_nafo4w, StR3_nafo4x)

pdf("output/figures/Stab&Resistance.pdf", width=10,height=8)
lapply(StR, PlotIndi_with_line)
dev.off()


RP1_shelf <- shelf_melt[shelf_melt$variable %in% c("Biomass_s",
                                                   "BiomassGroundfish_s",
                                                   "BiomassFlatfish_s",
                                                   "BiomassInvertebrates_s"), ]

RP1_ess <- ess_melt[ess_melt$variable %in% c("Biomass_s",
                                             "BiomassGroundfish_s",
                                             "BiomassFlatfish_s",
                                             "BiomassInvertebrates_s"), ]

RP1_wss <- wss_melt[wss_melt$variable %in% c("Biomass_s",
                                             "BiomassGroundfish_s",
                                             "BiomassFlatfish_s",
                                             "BiomassInvertebrates_s"), ]

RP1_nafo4vn <- nafo4vn_melt[nafo4vn_melt$variable %in% c("Biomass_s",
                                                         "BiomassGroundfish_s",
                                                         "BiomassFlatfish_s",
                                                         "BiomassInvertebrates_s"), ]

RP1_nafo4vs <- nafo4vs_melt[nafo4vs_melt$variable %in% c("Biomass_s",
                                                         "BiomassGroundfish_s",
                                                         "BiomassFlatfish_s",
                                                         "BiomassInvertebrates_s"), ]

RP1_nafo4w <- nafo4w_melt[nafo4w_melt$variable %in% c("Biomass_s",
                                                      "BiomassGroundfish_s",
                                                      "BiomassFlatfish_s",
                                                      "BiomassInvertebrates_s"), ]

RP1_nafo4x <- nafo4x_melt[nafo4x_melt$variable %in% c("Biomass_s",
                                                      "BiomassGroundfish_s",
                                                      "BiomassFlatfish_s",
                                                      "BiomassInvertebrates_s"), ]

#
RP2_shelf <- shelf_melt[shelf_melt$variable %in% c("BiomassSkates_s",
                                                   "MeanTrophicLevel.L_s"), ]

RP2_ess <- ess_melt[ess_melt$variable %in% c("BiomassSkates_s",
                                             "MeanTrophicLevel.L_s"), ]

RP2_wss <- wss_melt[wss_melt$variable %in% c("BiomassSkates_s",
                                             "MeanTrophicLevel.L_s"), ]

RP2_nafo4vn <- nafo4vn_melt[nafo4vn_melt$variable %in% c("BiomassSkates_s",
                                                         "MeanTrophicLevel.L_s"), ]

RP2_nafo4vs <- nafo4vs_melt[nafo4vs_melt$variable %in% c("BiomassSkates_s",
                                                         "MeanTrophicLevel.L_s"), ]

RP2_nafo4w <- nafo4w_melt[nafo4w_melt$variable %in% c("BiomassSkates_s",
                                                      "MeanTrophicLevel.L_s"), ]

RP2_nafo4x <- nafo4x_melt[nafo4x_melt$variable %in% c("BiomassSkates_s",
                                                      "MeanTrophicLevel.L_s"), ]

RP <- list(RP1_shelf,RP1_ess,RP1_wss,RP1_nafo4vn,RP1_nafo4vs,RP1_nafo4w, RP1_nafo4x,
            RP2_shelf,RP2_ess,RP2_wss,RP2_nafo4vn,RP2_nafo4vs,RP2_nafo4w, RP2_nafo4x)

pdf("output/figures/ResourcePotential.pdf", width=10,height=8)
lapply(RP, PlotIndi_with_line)
dev.off()

#

FP1_shelf <- shelf_melt[shelf_melt$variable %in% c("FishingPressure.L_s",
                                                   "FPClupeids.L_s"), ]

FP1_ess <- ess_melt[ess_melt$variable %in% c("FishingPressure.L_s",
                                             "FPClupeids.L_s"), ]

FP1_wss <- wss_melt[wss_melt$variable %in% c("FishingPressure.L_s",
                                             "FPClupeids.L_s"), ]

FP1_nafo4vn <- nafo4vn_melt[nafo4vn_melt$variable %in% c("FishingPressure.L_s",
                                                         "FPClupeids.L_s"), ]

FP1_nafo4vs <- nafo4vs_melt[nafo4vs_melt$variable %in% c("FishingPressure.L_s",
                                                         "FPClupeids.L_s"), ]

FP1_nafo4w <- nafo4w_melt[nafo4w_melt$variable %in% c("FishingPressure.L_s",
                                                      "FPClupeids.L_s"), ]

FP1_nafo4x <- nafo4x_melt[nafo4x_melt$variable %in% c("FishingPressure.L_s",
                                                      "FPClupeids.L_s"), ]
#

FP2_shelf <- shelf_melt[shelf_melt$variable %in% c("MeanTrophicLevel.L_s",
                                                   "MarineTrophicIndex.L_s",
                                                   "DiversityTargetSpp.L_s"), ]

FP2_ess <- ess_melt[ess_melt$variable %in% c("MeanTrophicLevel.L_s",
                                             "MarineTrophicIndex.L_s",
                                             "DiversityTargetSpp.L_s"), ]

FP2_wss <- wss_melt[wss_melt$variable %in% c("MeanTrophicLevel.L_s",
                                             "MarineTrophicIndex.L_s",
                                             "DiversityTargetSpp.L_s"), ]

FP2_nafo4vn <- nafo4vn_melt[nafo4vn_melt$variable %in% c("MeanTrophicLevel.L_s",
                                                         "MarineTrophicIndex.L_s",
                                                         "DiversityTargetSpp.L_s"), ]

FP2_nafo4vs <- nafo4vs_melt[nafo4vs_melt$variable %in% c("MeanTrophicLevel.L_s",
                                                         "MarineTrophicIndex.L_s",
                                                         "DiversityTargetSpp.L_s"), ]

FP2_nafo4w <- nafo4w_melt[nafo4w_melt$variable %in% c("MeanTrophicLevel.L_s",
                                                      "MarineTrophicIndex.L_s",
                                                      "DiversityTargetSpp.L_s"), ]

FP2_nafo4x <- nafo4x_melt[nafo4x_melt$variable %in% c("MeanTrophicLevel.L_s",
                                                      "MarineTrophicIndex.L_s",
                                                      "DiversityTargetSpp.L_s"), ]
#


FP3_shelf <- shelf_melt[shelf_melt$variable %in% c("Landings.L_s",
                                                   "LSkates.L_s",
                                                   "LFlatfish.L_s"), ]

FP3_ess <- ess_melt[ess_melt$variable %in% c("Landings.L_s",
                                             "LSkates.L_s",
                                             "LFlatfish.L_s"), ]

FP3_wss <- wss_melt[wss_melt$variable %in% c("Landings.L_s",
                                             "LSkates.L_s",
                                             "LFlatfish.L_s"), ]

FP3_nafo4vn <- nafo4vn_melt[nafo4vn_melt$variable %in% c("Landings.L_s",
                                                         "LSkates.L_s",
                                                         "LFlatfish.L_s"), ]

FP3_nafo4vs <- nafo4vs_melt[nafo4vs_melt$variable %in% c("Landings.L_s",
                                                         "LSkates.L_s",
                                                         "LFlatfish.L_s"), ]

FP3_nafo4w <- nafo4w_melt[nafo4w_melt$variable %in% c("Landings.L_s",
                                                      "LSkates.L_s",
                                                      "LFlatfish.L_s"), ]

FP3_nafo4x <- nafo4x_melt[nafo4x_melt$variable %in% c("Landings.L_s",
                                                      "LSkates.L_s",
                                                      "LFlatfish.L_s"), ]
#

FP4_shelf <- shelf_melt[shelf_melt$variable %in% c("LLargePelagic.L_s",
                                                   "DiversityTargetSpp.L_s"), ]

FP4_ess <- ess_melt[ess_melt$variable %in% c("LLargePelagic.L_s",
                                             "DiversityTargetSpp.L_s"), ]

FP4_wss <- wss_melt[wss_melt$variable %in% c("LLargePelagic.L_s",
                                             "DiversityTargetSpp.L_s"), ]

FP4_nafo4vn <- nafo4vn_melt[nafo4vn_melt$variable %in% c("LLargePelagic.L_s",
                                                         "DiversityTargetSpp.L_s"), ]

FP4_nafo4vs <- nafo4vs_melt[nafo4vs_melt$variable %in% c("LLargePelagic.L_s",
                                                         "DiversityTargetSpp.L_s"), ]

FP4_nafo4w <- nafo4w_melt[nafo4w_melt$variable %in% c("LLargePelagic.L_s",
                                                      "DiversityTargetSpp.L_s"), ]

FP4_nafo4x <- nafo4x_melt[nafo4x_melt$variable %in% c("LLargePelagic.L_s",
                                                      "DiversityTargetSpp.L_s"), ]


FP <- list(FP1_shelf,FP1_ess,FP1_wss,FP1_nafo4vn,FP1_nafo4vs,FP1_nafo4w, FP1_nafo4x,
           FP2_shelf,FP2_ess,FP2_wss,FP2_nafo4vn,FP2_nafo4vs,FP2_nafo4w, FP2_nafo4x,
           FP3_shelf,FP3_ess,FP3_wss,FP3_nafo4vn,FP3_nafo4vs,FP3_nafo4w, FP3_nafo4x,
           FP4_shelf,FP4_ess,FP4_wss,FP4_nafo4vn,FP4_nafo4vs,FP4_nafo4w, FP4_nafo4x)

pdf("output/figures/FishingPressure.pdf", width=10,height=8)
lapply(FP, PlotIndi_with_line)
dev.off()

#######################
# -------- Read & Plotting those indicators that were filtered out due to  > 25% NAs -------------------------------
SS <- read.csv("data/shelfsetq.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
Shelf_Q <- read.csv("data/esswsssetq.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
Shelf_Q_without_interpolationORfilters <- stdizeFrame(SS)
esswss_without_interpolationORfilters <- stdizeFrame(Shelf_Q)
Shelf_Q_without_interpolationORfilters$BiomassInvertebrates_s
Shelf_Q_without_interpolationORfilters$BInvertebrateToDemersal_s
ess_without_interpolationORfilters <- esswss_without_interpolationORfilters[esswss_without_interpolationORfilters$ID %in% c('ESS'), ]
wss_without_interpolationORfilters <- esswss_without_interpolationORfilters[esswss_without_interpolationORfilters$ID %in% c('WSS'), ]
nafo_without_interpolationORfilters <- stdizeFrame(IndiQ_NAFO)
nafo4vn_without_interpolationORfilters <- nafo_without_interpolationORfilters[nafo_without_interpolationORfilters$ID %in% c('4VN'), ]
nafo4vs_without_interpolationORfilters <- nafo_without_interpolationORfilters[nafo_without_interpolationORfilters$ID %in% c('4VS'), ]
nafo4w_without_interpolationORfilters <- nafo_without_interpolationORfilters[nafo_without_interpolationORfilters$ID %in% c('4W'), ]
nafo4x_without_interpolationORfilters <- nafo_without_interpolationORfilters[nafo_without_interpolationORfilters$ID %in% c('4X'), ]


shelf_melt2 <- melt(Shelf_Q_without_interpolationORfilters, id=c('YEAR', 'ID'))
ess_melt2 <- melt(ess_without_interpolationORfilters, id=c('YEAR', 'ID'))
wss_melt2 <- melt(wss_without_interpolationORfilters, id=c('YEAR', 'ID'))
nafo4vn_melt2 <- melt(nafo4vn_without_interpolationORfilters, id=c('YEAR', 'ID'))
nafo4vs_melt2 <- melt(nafo4vs_without_interpolationORfilters, id=c('YEAR', 'ID'))
nafo4w_melt2 <- melt(nafo4w_without_interpolationORfilters, id=c('YEAR', 'ID'))
nafo4x_melt2 <- melt(nafo4x_without_interpolationORfilters, id=c('YEAR', 'ID'))


Inv_shelf <- shelf_melt2[shelf_melt2$variable %in% c("BInvertebrateToDemersal_s"), ]

Inv_ess <- ess_melt2[ess_melt2$variable %in% c("BInvertebrateToDemersal_s"), ]

Inv_wss <- wss_melt2[wss_melt2$variable %in% c("BInvertebrateToDemersal_s"), ]

Inv_nafo4vn <- nafo4vn_melt2[nafo4vn_melt2$variable %in% c("BInvertebrateToDemersal_s"), ]

Inv_nafo4vs <- nafo4vs_melt2[nafo4vs_melt2$variable %in% c("BInvertebrateToDemersal_s"), ]

Inv_nafo4w <- nafo4w_melt2[nafo4w_melt2$variable %in% c("BInvertebrateToDemersal_s"), ]

Inv_nafo4x <- nafo4x_melt2[nafo4x_melt2$variable %in% c("BInvertebrateToDemersal_s"), ]

#

Inv2_shelf <- shelf_melt2[shelf_melt2$variable %in% c("FPInvertebrates.L_s"), ]

Inv2_ess <- ess_melt2[ess_melt2$variable %in% c("FPInvertebrates.L_s"), ]

Inv2_wss <- wss_melt2[wss_melt2$variable %in% c("FPInvertebrates.L_s"), ]

Inv2_nafo4vn <- nafo4vn_melt2[nafo4vn_melt2$variable %in% c("FPInvertebrates.L_s"), ]

Inv2_nafo4vs <- nafo4vs_melt2[nafo4vs_melt2$variable %in% c("FPInvertebrates.L_s"), ]

Inv2_nafo4w <- nafo4w_melt2[nafo4w_melt2$variable %in% c("FPInvertebrates.L_s"), ]

Inv2_nafo4x <- nafo4x_melt2[nafo4x_melt2$variable %in% c("FPInvertebrates.L_s"), ]

#

Inv3_shelf <- shelf_melt2[shelf_melt2$variable %in% c("BiomassInvertebrates_s"), ]

Inv3_ess <- ess_melt2[ess_melt2$variable %in% c("BiomassInvertebrates_s"), ]

Inv3_wss <- wss_melt2[wss_melt2$variable %in% c("BiomassInvertebrates_s"), ]

Inv3_nafo4vn <- nafo4vn_melt2[nafo4vn_melt2$variable %in% c("BiomassInvertebrates_s"), ]

Inv3_nafo4vs <- nafo4vs_melt2[nafo4vs_melt2$variable %in% c("BiomassInvertebrates_s"), ]

Inv3_nafo4w <- nafo4w_melt2[nafo4w_melt2$variable %in% c("BiomassInvertebrates_s"), ]

Inv3_nafo4x <- nafo4x_melt2[nafo4x_melt2$variable %in% c("BiomassInvertebrates_s"), ]

Inv <- list(Inv_shelf, Inv_ess, Inv_wss,Inv_nafo4vn,Inv_nafo4vs,Inv_nafo4w, Inv_nafo4x,
            Inv2_shelf, Inv2_ess, Inv2_wss,Inv2_nafo4vn,Inv2_nafo4vs,Inv2_nafo4w, Inv2_nafo4x,
            Inv3_shelf, Inv3_ess, Inv3_wss,Inv3_nafo4vn,Inv3_nafo4vs,Inv3_nafo4w, Inv3_nafo4x)

pdf("output/figures/Invertebrates.pdf", width=10,height=8)
lapply(Inv, PlotIndi_with_line)
dev.off()




