source('code/keepdropcolumn.R')
require(missForest)

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
SS <- SS[!SS$YEAR %in% "2015", ]
SS_filtered <- KeepDrop(data=SS,cols=dropIndi, newdata=dt, drop=1)
SS_filtered_interpolated <- missForest(SS_filtered)$ximp    
  write.csv(SS_filtered_interpolated, "output/data/shelfsetq_filtered&interpolated.csv", row.names=FALSE)

#*********ESSWSS
Shelf_Q <- read.csv("data/esswsssetq.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
Shelf_Q$ID <- factor(Shelf_Q$ID)
Shelf_Q <- Shelf_Q[!Shelf_Q$YEAR %in% "2015", ]
Shelf_Q_filtered <- KeepDrop(data=Shelf_Q,cols=dropIndi, newdata=dt, drop=1)
Shelf_filtered_interpolated <- missForest(Shelf_Q_filtered)$ximp  
  write.csv(Shelf_filtered_interpolated, "output/data/esswsssetq_filtered&interpolated.csv", row.names=FALSE)


#********NAFO 
IndiQ_NAFO <- read.csv("data/nafosetq.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
IndiQ_NAFO <- IndiQ_NAFO[!IndiQ_NAFO$YEAR %in% "2015", ]
IndiQ_NAFO$ID <- factor(IndiQ_NAFO$ID)
IndiQ_NAFO_filtered <- KeepDrop(data=IndiQ_NAFO,cols=dropIndi, newdata=dt, drop=1)
IndiQ_NAFO_filtered_interpolated <- missForest(IndiQ_NAFO_filtered)$ximp 

#********NAFO - 4vs
IndiQ_NAFO_4W <- IndiQ_NAFO_filtered_interpolated[IndiQ_NAFO_filtered_interpolated$ID %in% c('4W'), ]
IndiQ_NAFO_4X <- IndiQ_NAFO_filtered_interpolated[IndiQ_NAFO_filtered_interpolated$ID %in% c('4X'), ]
IndiQ_NAFO_4VS <- IndiQ_NAFO_filtered_interpolated[IndiQ_NAFO_filtered_interpolated$ID %in% c('4VS'), ]
IndiQ_NAFO_4VS <- KeepDrop(data=IndiQ_NAFO_4vs,cols="CCPlanktivore BiomassClupeids", newdata=dt, drop=1)
IndiQ_NAFO_4VN <- IndiQ_NAFO_filtered_interpolated[IndiQ_NAFO_filtered_interpolated$ID %in% c('4VN'), ]
IndiQ_NAFO_4VN <- KeepDrop(data=IndiQ_NAFO_4vn_filtered,cols="BiomassTL2 CCPlanktivore", newdata=dt, drop=1)

# write.csv(IndiQ_NAFO_4W, "output/data/nafo4Wsetq_filtered&interpolated.csv", row.names=FALSE)
# write.csv(IndiQ_NAFO_4X, "output/data/nafo4Xsetq_filtered&interpolated.csv", row.names=FALSE)
# write.csv(IndiQ_NAFO_4VS, "output/data/nafo4VSsetq_filtered&interpolated.csv", row.names=FALSE)
# write.csv(IndiQ_NAFO_4VN, "output/data/nafo4VNsetq_filtered&interpolated.csv", row.names=FALSE)

#********strata Scale
Strata_Q <- read.csv("data/stratsetq.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
Strata_Q <- Strata_Q[!Strata_Q$YEAR %in% "2015", ]
Strata_Q$ID <- factor(Strata_Q$ID)
Strata_Q_filtered <- KeepDrop(data=Strata_Q,cols=dropIndi, newdata=dt, drop=1)
Strata_Q_filtered <- KeepDrop(data=Strata_Q_filtered,cols="BiomassTL2", newdata=dt, drop=1)
Strata_Q_filt_int <- missForest(Strata_Q_filtered)$ximp 

Strata_Q_440 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('440'), ]
Strata_Q_440 <- KeepDrop(data=Strata_Q_440,cols="BiomassClupeids BiomassForage BPelagicToDemersal BTGLargeBenthivore BTGPlanktivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
#write.csv(Strata_Q_440, "output/data/stratasetq440_filt&int.csv", row.names=FALSE)

Strata_Q_441 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('441'), ]
Strata_Q_441 <- KeepDrop(data=Strata_Q_441,cols="BiomassClupeids BiomassForage BPelagicToDemersal BTGPlanktivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
#write.csv(Strata_Q_441, "output/data/stratasetq441_filt&int.csv", row.names=FALSE)

Strata_Q_442 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('442'), ]
Strata_Q_442 <- KeepDrop(data=Strata_Q_442,cols="BiomassClupeids BTGLargeBenthivore BTGZoopiscivore CCLargeBenthivore CCPlanktivore CCZoopiscivore", newdata=dt, drop=1)
#write.csv(Strata_Q_442, "output/data/stratasetq442_filt&int.csv", row.names=FALSE)

Strata_Q_443 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('443'), ]
Strata_Q_443 <- KeepDrop(data=Strata_Q_443,cols="BiomassClupeids BiomassForage BPelagicToDemersal BTGPlanktivore BTGZoopiscivore CCLargeBenthivore CCPlanktivore CCZoopiscivore", newdata=dt, drop=1)
write.csv(Strata_Q_443, "output/data/stratasetq443_filt&int.csv", row.names=FALSE)

Strata_Q_444 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('444'), ]
Strata_Q_444 <- KeepDrop(data=Strata_Q_444,cols="BiomassClupeids BiomassForage BPelagicToDemersal BTGPlanktivore CCPlanktivore", newdata=dt, drop=1)
#write.csv(Strata_Q_444, "output/data/stratasetq444_filt&int.csv", row.names=FALSE)

Strata_Q_445 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('445'), ]
Strata_Q_445 <- KeepDrop(data=Strata_Q_445,cols="BiomassClupeids BTGLargeBenthivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
#write.csv(Strata_Q_445, "output/data/stratasetq445_filt&int.csv", row.names=FALSE)

Strata_Q_446 <- Strata_Q_filt_int[Strata_Q_filt_int$ID %in% c('446'), ]
Strata_Q_446 <- KeepDrop(data=Strata_Q_446,cols="BiomassClupeids BiomassForage BPelagicToDemersal BTGLargeBenthivore BTGPlanktivore CCLargeBenthivore CCPlanktivore", newdata=dt, drop=1)
write.csv(Strata_Q_446, "output/data/stratasetq446_filt&int.csv", row.names=FALSE)

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





