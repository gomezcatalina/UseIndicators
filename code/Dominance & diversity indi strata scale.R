
CorrIndi_strata <- function(x) {chart.Correlation(x, histogram=FALSE, pch=19, 
                                                  method = c("pearson"), 
                                                  main ="Spearman correlations for all data points combined (p-values: 0***, 0.001**, 0.01*)")
}

CorrIndi_Individualstrata <- function(x, y) {chart.Correlation(x, histogram=FALSE, pch=19, 
                                                               method = c("pearson"), 
                                                               main = y)}

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

#Analysis in 2014 was used with indicators without standardizing - s or no t gives the exac same result!! but need to decide what to do here - probably remove _s to be consistent 
BiodivIndi_440 <- subset(Strata_Q_440, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_441 <- subset(Strata_Q_441, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_442 <- subset(Strata_Q_442, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_443 <- subset(Strata_Q_443, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_444 <- subset(Strata_Q_444, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_445 <- subset(Strata_Q_445, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_446 <- subset(Strata_Q_446, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_447 <- subset(Strata_Q_447, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_448 <- subset(Strata_Q_448, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_449 <- subset(Strata_Q_449, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_450 <- subset(Strata_Q_450, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_451 <- subset(Strata_Q_451, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_452 <- subset(Strata_Q_452, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_453 <- subset(Strata_Q_453, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_454 <- subset(Strata_Q_454, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_455 <- subset(Strata_Q_455, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_456 <- subset(Strata_Q_456, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_457 <- subset(Strata_Q_457, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_458 <- subset(Strata_Q_458, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_459 <- subset(Strata_Q_459, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_460 <- subset(Strata_Q_460, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_461 <- subset(Strata_Q_461, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_462 <- subset(Strata_Q_462, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_463 <- subset(Strata_Q_463, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_464 <- subset(Strata_Q_464, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_465 <- subset(Strata_Q_465, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_466 <- subset(Strata_Q_466, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_470 <- subset(Strata_Q_470, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_471 <- subset(Strata_Q_471, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_472 <- subset(Strata_Q_472, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_473 <- subset(Strata_Q_473, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_474 <- subset(Strata_Q_474, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_475 <- subset(Strata_Q_475, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_476 <- subset(Strata_Q_476, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_477 <- subset(Strata_Q_477, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_478 <- subset(Strata_Q_478, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_480 <- subset(Strata_Q_480, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_481 <- subset(Strata_Q_481, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_482 <- subset(Strata_Q_482, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_483 <- subset(Strata_Q_483, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_484 <- subset(Strata_Q_484, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_485 <- subset(Strata_Q_485, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_490 <- subset(Strata_Q_490, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_491 <- subset(Strata_Q_491, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_492 <- subset(Strata_Q_492, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_493 <- subset(Strata_Q_493, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_494 <- subset(Strata_Q_494, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))
BiodivIndi_495 <- subset(Strata_Q_495, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "Heips_s"))


BiodivIndi_Individual_strata <-  list(BiodivIndi_440, BiodivIndi_441, BiodivIndi_442,
                                      BiodivIndi_443, BiodivIndi_444, BiodivIndi_445,
                                      BiodivIndi_446, BiodivIndi_447, BiodivIndi_448,
                                      BiodivIndi_449, BiodivIndi_450, BiodivIndi_451,
                                      BiodivIndi_452, BiodivIndi_453, BiodivIndi_454,
                                      BiodivIndi_455, BiodivIndi_456, BiodivIndi_457,
                                      BiodivIndi_458, BiodivIndi_459, BiodivIndi_460,
                                      BiodivIndi_461, BiodivIndi_462, BiodivIndi_463,
                                      BiodivIndi_464, BiodivIndi_465, BiodivIndi_466,
                                      #BiodivIndi_467, BiodivIndi_468, BiodivIndi_469,
                                      BiodivIndi_470, BiodivIndi_471, BiodivIndi_472,
                                      BiodivIndi_473, BiodivIndi_474, BiodivIndi_475,
                                      BiodivIndi_476, BiodivIndi_477, BiodivIndi_478,
                                      BiodivIndi_480, BiodivIndi_481, BiodivIndi_482,
                                      BiodivIndi_483, BiodivIndi_484, BiodivIndi_485,
                                      BiodivIndi_490, BiodivIndi_491, BiodivIndi_492,
                                      BiodivIndi_493, BiodivIndi_494, BiodivIndi_495)


names(BiodivIndi_Individual_strata) <-  list("BiodivIndi_440", "BiodivIndi_441", "BiodivIndi_442",
                                             "BiodivIndi_443", "BiodivIndi_444", "BiodivIndi_445",
                                             "BiodivIndi_446", "BiodivIndi_447", "BiodivIndi_448",
                                             "BiodivIndi_449", "BiodivIndi_450", "BiodivIndi_451",
                                             "BiodivIndi_452", "BiodivIndi_453", "BiodivIndi_454",
                                             "BiodivIndi_455", "BiodivIndi_456", "BiodivIndi_457",
                                             "BiodivIndi_458", "BiodivIndi_459", "BiodivIndi_460",
                                             "BiodivIndi_461", "BiodivIndi_462", "BiodivIndi_463",
                                             "BiodivIndi_464", "BiodivIndi_465", "BiodivIndi_466",
                                             #BiodivIndi_467, BiodivIndi_468, BiodivIndi_469,
                                             "BiodivIndi_470", "BiodivIndi_471", "BiodivIndi_472",
                                             "BiodivIndi_473", "BiodivIndi_474", "BiodivIndi_475",
                                             "BiodivIndi_476", "BiodivIndi_477", "BiodivIndi_478",
                                             "BiodivIndi_480", "BiodivIndi_481", "BiodivIndi_482",
                                             "BiodivIndi_483", "BiodivIndi_484", "BiodivIndi_485",
                                             "BiodivIndi_490", "BiodivIndi_491", "BiodivIndi_492",
                                             "BiodivIndi_493", "BiodivIndi_494", "BiodivIndi_495")

pdf("outputs/figs/Diversity&DominanceIndi_strata_scale.pdf", width=16,height=14)
PlotIndi_strata(meltCb_Heips)
PlotIndi_strata_withdatapoints(meltCb_Heips)
CorrIndi_strata(Cb_Heips_Cor.L)
#Corr_all_Indi <- lapply(BiodivIndi_Individual_strata, CorrIndi_Individualstrata)
CorrIndi_Individualstrata(BiodivIndi_440, names(BiodivIndi_Individual_strata[1]))
CorrIndi_Individualstrata(BiodivIndi_441, names(BiodivIndi_Individual_strata[2]))
CorrIndi_Individualstrata(BiodivIndi_442, names(BiodivIndi_Individual_strata[3]))
CorrIndi_Individualstrata(BiodivIndi_443, names(BiodivIndi_Individual_strata[4]))
CorrIndi_Individualstrata(BiodivIndi_444, names(BiodivIndi_Individual_strata[5]))
CorrIndi_Individualstrata(BiodivIndi_445, names(BiodivIndi_Individual_strata[6]))
CorrIndi_Individualstrata(BiodivIndi_446, names(BiodivIndi_Individual_strata[7]))
CorrIndi_Individualstrata(BiodivIndi_447, names(BiodivIndi_Individual_strata[8]))
CorrIndi_Individualstrata(BiodivIndi_448, names(BiodivIndi_Individual_strata[9]))
CorrIndi_Individualstrata(BiodivIndi_449, names(BiodivIndi_Individual_strata[10]))
CorrIndi_Individualstrata(BiodivIndi_450, names(BiodivIndi_Individual_strata[11]))
CorrIndi_Individualstrata(BiodivIndi_451, names(BiodivIndi_Individual_strata[12]))
CorrIndi_Individualstrata(BiodivIndi_452, names(BiodivIndi_Individual_strata[13]))
CorrIndi_Individualstrata(BiodivIndi_453, names(BiodivIndi_Individual_strata[14]))
CorrIndi_Individualstrata(BiodivIndi_454, names(BiodivIndi_Individual_strata[15]))
CorrIndi_Individualstrata(BiodivIndi_455, names(BiodivIndi_Individual_strata[16]))
CorrIndi_Individualstrata(BiodivIndi_456, names(BiodivIndi_Individual_strata[17]))
CorrIndi_Individualstrata(BiodivIndi_457, names(BiodivIndi_Individual_strata[18]))
CorrIndi_Individualstrata(BiodivIndi_458, names(BiodivIndi_Individual_strata[19]))
CorrIndi_Individualstrata(BiodivIndi_459, names(BiodivIndi_Individual_strata[20]))
CorrIndi_Individualstrata(BiodivIndi_460, names(BiodivIndi_Individual_strata[21]))
CorrIndi_Individualstrata(BiodivIndi_461, names(BiodivIndi_Individual_strata[22]))
CorrIndi_Individualstrata(BiodivIndi_462, names(BiodivIndi_Individual_strata[23]))
CorrIndi_Individualstrata(BiodivIndi_463, names(BiodivIndi_Individual_strata[24]))
CorrIndi_Individualstrata(BiodivIndi_464, names(BiodivIndi_Individual_strata[25]))
CorrIndi_Individualstrata(BiodivIndi_465, names(BiodivIndi_Individual_strata[26]))
CorrIndi_Individualstrata(BiodivIndi_466, names(BiodivIndi_Individual_strata[27]))
CorrIndi_Individualstrata(BiodivIndi_470, names(BiodivIndi_Individual_strata[28]))
CorrIndi_Individualstrata(BiodivIndi_471, names(BiodivIndi_Individual_strata[29]))
CorrIndi_Individualstrata(BiodivIndi_472, names(BiodivIndi_Individual_strata[30]))
CorrIndi_Individualstrata(BiodivIndi_473, names(BiodivIndi_Individual_strata[31]))
CorrIndi_Individualstrata(BiodivIndi_474, names(BiodivIndi_Individual_strata[32]))
CorrIndi_Individualstrata(BiodivIndi_475, names(BiodivIndi_Individual_strata[33]))
CorrIndi_Individualstrata(BiodivIndi_476, names(BiodivIndi_Individual_strata[34]))
CorrIndi_Individualstrata(BiodivIndi_477, names(BiodivIndi_Individual_strata[35]))
CorrIndi_Individualstrata(BiodivIndi_478, names(BiodivIndi_Individual_strata[36]))
CorrIndi_Individualstrata(BiodivIndi_480, names(BiodivIndi_Individual_strata[37]))
CorrIndi_Individualstrata(BiodivIndi_481, names(BiodivIndi_Individual_strata[38]))
CorrIndi_Individualstrata(BiodivIndi_482, names(BiodivIndi_Individual_strata[39]))
CorrIndi_Individualstrata(BiodivIndi_483, names(BiodivIndi_Individual_strata[40]))
CorrIndi_Individualstrata(BiodivIndi_484, names(BiodivIndi_Individual_strata[41]))
CorrIndi_Individualstrata(BiodivIndi_485, names(BiodivIndi_Individual_strata[42]))
CorrIndi_Individualstrata(BiodivIndi_490, names(BiodivIndi_Individual_strata[43]))
CorrIndi_Individualstrata(BiodivIndi_491, names(BiodivIndi_Individual_strata[44]))
CorrIndi_Individualstrata(BiodivIndi_492, names(BiodivIndi_Individual_strata[45]))
CorrIndi_Individualstrata(BiodivIndi_493, names(BiodivIndi_Individual_strata[46]))
CorrIndi_Individualstrata(BiodivIndi_494, names(BiodivIndi_Individual_strata[47]))
CorrIndi_Individualstrata(BiodivIndi_495, names(BiodivIndi_Individual_strata[48]))
dev.off()

########### Correlations ONLY for strata 440
head(BiodivIndi_440)
#Spearman correlation analysis using Hmisc
strata440_function_rcorr <- rcorr(as.matrix(BiodivIndi_440,type="spearman"))
strata440 <- flattenCorrMatrix(strata440_function_rcorr$r, strata440_function_rcorr$P)
write.csv(strata440, "outputs/data/strata440.csv")   


#Spearman correlation analysis: comparing results above using other packages/functions 
strata440_function_cor <- cor(as.matrix(BiodivIndi_440,type="spearman")) # same as if using function above rcorr

chart.Correlation(BiodivIndi_440, histogram=FALSE, pch=19, 
                  method = c("spearman"), 
                  main = "BiodivIndi_440_spearman_does not match results of rcorr and cor")

chart.Correlation(BiodivIndi_440, histogram=FALSE, pch=19, 
                  method = c("pearson"), 
                  main = "BiodivIndi_440_pearson_pearson matches spearman results?")



#Spearman correlation analysis: using a different data set: stratsetq (used by ACook and Sylvie previously)
data2 <- read.csv("file:///C:/Users/gomezc/Desktop/stratsetq_TRIAL.csv",header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
data2 <- data2[data2$ID %in% c('440'), ]
data2 <- subset(data2, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s"))

trial440 <- rcorr(as.matrix(data2,type="spearman"))
trial440 <- flattenCorrMatrix(trial440$r, trial440$P)
write.csv(trial440, "file:///C:/Users/gomezc/Desktop/440_trial.csv")   



# Other plots that did not end up using but kept in case we want to see general trends for these indicators

# ggplot(meltCb_Heips, aes(YEAR, value, colour=variable, group=variable)) +
#   scale_color_brewer(palette="Paired") +
#   stat_smooth(se=F) + geom_point() +
#   #facet_wrap( ~ ID) +
#   theme(text = element_text(size=15)) +
#   theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) +
#   theme(legend.title=element_blank()) +
#   theme(legend.position='bottom') +
#   ggtitle("All data points at the strata scale - smooth: loess") +
#   theme(strip.text.x = element_text(size=13),
#         strip.background = element_rect(fill="white"))
# 
# ggplot(meltCb_Heips, aes(YEAR, value, colour=variable, group=variable)) +
#   scale_color_brewer(palette="Paired") +
#   stat_smooth(se=F) + #geom_point() +
#   #facet_wrap( ~ ID) +
#   theme(text = element_text(size=15)) +
#   theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) +
#   theme(legend.title=element_blank()) +
#   theme(legend.position='bottom') +
#   ggtitle("Same as previous plot but removing individual data points") +
#   theme(strip.text.x = element_text(size=13),
#         strip.background = element_rect(fill="white"))

