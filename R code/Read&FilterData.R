# Read and filter indicators for redundancy analysis (Fall 2016) 

require(zoo)
source('R/keepdropcolumn.R')
library(imputeTS) # https://cran.r-project.org/web/packages/imputeTS/imputeTS.pdf
# There are several NAs in the time-series adat; the hierarchical cluster analysis ignores entire rows (i.e. years) 
# whethere is a NA (it omits all the other data for the year of the missing value, therefore excluding the whole year 
# from the analysis).
# In this script, we are ommitting indicators with > 25% of NA's in the time-series
# and we interpolate missing data for indicators with missing data , but <=25%. 
# The invertebrate based indicators with missing data are also excluded (BInvertebrateToDemersal; FPInvertebrates.L; InverseFPInvertebrates.L; BiomassInvertebrates) 

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


#*********SHELF
SS <- read.csv("data/shelfsetq.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
SS$ID <- factor(SS$ID)
#SS <- SS[!SS$YEAR %in% "2015", ]
SS_filtered <- KeepDrop(data=SS,cols=dropIndi, newdata=dt, drop=1)
SS_filtered_interpolated <- na.locf(SS_filtered)    # Stands for last observation carried forward and does just what it says the last observation before NA or a string of NA is used to replace the NA 
#write.csv(SS_filtered_interpolated, "data/shelfsetq_filtered&interpolated.csv", row.names=FALSE)


#*********ESS
Shelf_Q <- read.csv("data/esswsssetq.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
Shelf_Q$ID <- factor(Shelf_Q$ID)
#Shelf_Q <- Shelf_Q[!Shelf_Q$YEAR %in% "2015", ]
ESS_Q <- Shelf_Q[Shelf_Q$ID %in% c('ESS'), ]
ESS_Q_filtered <- KeepDrop(data=ESS_Q,cols=dropIndi, newdata=dt, drop=1)
#write.csv(ESS_Q_filtered_interpolated, "data/esssetq_filtered&interpolated.csv", row.names=FALSE)


#********WSS
WSS_Q <- Shelf_Q[Shelf_Q$ID %in% c('WSS'), ]
WSS_Q_filtered <- KeepDrop(data=WSS_Q,cols=dropIndi, newdata=dt, drop=1)
WSS_Q_filtered <- KeepDrop(data=WSS_Q_filtered ,cols="CCLargeBenthivore BiomassTL2", newdata=dt, drop=1)
head(WSS_Q_filtered)
#write.csv(WSS_Q_filtered_interpolated, "data/wsssetq_filtered&interpolated.csv", row.names=FALSE)


#********NAFO - 4W
IndiQ_NAFO <- read.csv("data/nafosetq.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
#IndiQ_NAFO <- IndiQ_NAFO[!IndiQ_NAFO$YEAR %in% "2015", ]
IndiQ_NAFO$ID <- factor(IndiQ_NAFO$ID)
IndiQ_NAFO_4w <- IndiQ_NAFO[IndiQ_NAFO$ID %in% c('4W'), ]
IndiQ_NAFO_4w_filtered <- KeepDrop(data=IndiQ_NAFO_4w,cols=dropIndi, newdata=dt, drop=1)
#write.csv(IndiQ_NAFO_4w_filtered_interpolated, "data/nafo4w_setq_filtered&interpolated.csv", row.names=FALSE)

#********NAFO - 4vs
IndiQ_NAFO_4vs <- IndiQ_NAFO[IndiQ_NAFO$ID %in% c('4VS'), ]
IndiQ_NAFO_4vs_filtered <- KeepDrop(data=IndiQ_NAFO_4vs,cols=dropIndi, newdata=dt, drop=1)
IndiQ_NAFO_4vs_filtered <- KeepDrop(data=IndiQ_NAFO_4vs_filtered,cols="CCPlanktivore BiomassClupeids", newdata=dt, drop=1)
IndiQ_NAFO_4vs_filtered_interpolated <- na.locf(IndiQ_NAFO_4vs_filtered)    
#write.csv(IndiQ_NAFO_4vs_filtered_interpolated, "data/nafo4vs_setq_filtered&interpolated.csv", row.names=FALSE)

#********NAFO - 4vn
IndiQ_NAFO_4vn <- IndiQ_NAFO[IndiQ_NAFO$ID %in% c('4VN'), ]
IndiQ_NAFO_4vn_filtered <- KeepDrop(data=IndiQ_NAFO_4vn,cols=dropIndi, newdata=dt, drop=1)
IndiQ_NAFO_4vn_filtered <- KeepDrop(data=IndiQ_NAFO_4vn_filtered,cols="BiomassTL2 CCPlanktivore", newdata=dt, drop=1)
IndiQ_NAFO_4vn_filtered_interpolated <- na.locf(IndiQ_NAFO_4vn_filtered) 
#write.csv(IndiQ_NAFO_4vn_filtered_interpolated, "data/nafo4vn_setq_filtered&interpolated.csv", row.names=FALSE)
