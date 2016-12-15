library(Hmisc)
source('code/IndiFunctions.R') 
source('C:/RProjects/ExtractIndicators/R/stdize.R')
source('C:/RProjects/ExtractIndicators/R/stdizeFrame.R')
##
#*********************************** Read large scale datasets    -----------------------------------------
filenames <- list.files("C:/RProjects/UseIndicators/output/data/largescales", pattern="_s.csv", full.names=TRUE)
indiLargeScales <- lapply(filenames, read.csv)
#shelf_raw <- read.csv("output/data/largescales/shelfsetq_filtered&interpolated_s.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
indiLargeScales_s <- list()
for(i in 1:length(indiLargeScales)) {
  j =indiLargeScales[[i]]
  nn = c('ID',names(j)[grep('_s',names(j))])
  indiLargeScales_s[[i]] <- j[,c(nn)]
  }

#*********************************** Read strata datasets    -----------------------------------------
filenames <- list.files("C:/RProjects/UseIndicators/output/data/strata", pattern="*.csv", full.names=TRUE)
indiStrata <- lapply(filenames, read.csv)
indiStrata  <- lapply(indiStrata , stdizeFrame)
indiStrataScales_s <- list()
for(i in 1:length(indiStrata)) {
  j =indiStrata[[i]]
  nn = c('ID',names(j)[grep('_s',names(j))])
  indiStrataScales_s[[i]] <- j[,c(nn)]
}

# str(indiLargeScales_s)
# x = indiLargeScales_s  
# ii = unique(x$ID)
# ii

#*********************************** Define functions for spearman correlation -----------------------------------------
SpearCorr <- function(x) {
  ii = unique(x$ID)
  for(i in 1:length(ii)) {
    xi = subset(x,ID==ii[i],select=names(x[, !names(x) %in% c("ID")]) )
    mypath <- file.path("output","analysis","SpearCorr",paste("SpearCorr_", ii[i], ".csv", sep = ""))
    #correlacion <- rcorr(as.matrix(xi,type="spearman"))
    correlacion <- rcorr(as.matrix((xi[, !names(xi) %in% c("ID")]),type="spearman"))
    correlacion_flat <- flattenCorrMatrix(correlacion$r, correlacion$P)
    correlacion_flat$ID  <- unique(xi$ID)
    write.csv(correlacion_flat,file=mypath)
      }
}

SpearCorr_notFlatten <- function(x) {
  ii = unique(x$ID)
  for(i in 1:length(ii)) {
    xi = subset(x,ID==ii[i],select=names(x[, !names(x) %in% c("ID")]) )
    mypath <- file.path("output","analysis","SpearCorr", paste("SpearCorr_r_", ii[i], ".csv", sep = ""))
    mypath2 <- file.path("output","analysis","SpearCorr", paste("SpearCorr_p_", ii[i], ".csv", sep = ""))
    correlacion <- rcorr(as.matrix((xi[, !names(xi) %in% c("ID")]),type="spearman"))
    correlacion_r <- correlacion$r
    correlacion_p  <- correlacion$P
    write.csv(correlacion_r,file=mypath)
    write.csv(correlacion_p,file=mypath2)
  }
}


#*********************************** Do correlations at all spatial scales -----------------------------------------
corr_all <- lapply(indiLargeScales_s, SpearCorr)
corr_all <- lapply(indiLargeScales_s, SpearCorr_notFlatten)
corr_strata <- lapply(indiStrataScales_s, SpearCorr)
corr_strata <- lapply(indiStrataScales_s, SpearCorr_notFlatten)

#*********************************** Organize outputs and save as one csv for all scales -----------------------------------------
SpearCorr_SHELF <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_SHELF.csv")
SpearCorr_SHELF$ID <- "SHELF"
SpearCorr_ESS <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_ESS.csv")
SpearCorr_ESS$ID <- "ESS"
SpearCorr_WSS <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_WSS.csv")
SpearCorr_WSS$ID <- "WSS"
SpearCorr_4VN <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_4VN.csv")
SpearCorr_4VN$ID <- "4VN"
SpearCorr_4VS <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_4VS.csv")
SpearCorr_4VS$ID <- "4VS"
SpearCorr_4W <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_4W.csv")
SpearCorr_4W$ID <- "4W"
SpearCorr_4X <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_4X.csv")
SpearCorr_4X$ID <- "4X"
SpearCorr_440 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_440.csv")
SpearCorr_440$ID <- "S440"
SpearCorr_441 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_441.csv")
SpearCorr_441$ID <- "S441"
SpearCorr_442 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_442.csv")
SpearCorr_442$ID <- "S442"
SpearCorr_443 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_443.csv")
SpearCorr_443$ID <- "S443"
SpearCorr_444 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_444.csv")
SpearCorr_444$ID <- "S444"
SpearCorr_445 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_445.csv")
SpearCorr_445$ID <- "S445"
SpearCorr_446 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_446.csv")
SpearCorr_446$ID <- "S446"
SpearCorr_447 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_447.csv")
SpearCorr_447$ID <- "S447"
SpearCorr_448 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_448.csv")
SpearCorr_448$ID <- "S448"
SpearCorr_449 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_449.csv")
SpearCorr_449$ID <- "S449"
SpearCorr_450 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_450.csv")
SpearCorr_450$ID <- "S450"
SpearCorr_451 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_451.csv")
SpearCorr_451$ID <- "S451"
SpearCorr_452 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_452.csv")
SpearCorr_452$ID <- "S452"
SpearCorr_453 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_453.csv")
SpearCorr_453$ID <- "S453"
SpearCorr_454 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_454.csv")
SpearCorr_454$ID <- "S454"
SpearCorr_455 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_455.csv")
SpearCorr_455$ID <- "S455"
SpearCorr_456 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_456.csv")
SpearCorr_456$ID <- "S456"
SpearCorr_457 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_457.csv")
SpearCorr_457$ID <- "S457"
SpearCorr_458 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_458.csv")
SpearCorr_458$ID <- "S458"
SpearCorr_459 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_459.csv")
SpearCorr_459$ID <- "S459"
SpearCorr_460 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_460.csv")
SpearCorr_460$ID <- "S460"
SpearCorr_461 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_461.csv")
SpearCorr_461$ID <- "S461"
SpearCorr_462 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_462.csv")
SpearCorr_462$ID <- "S462"
SpearCorr_463 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_463.csv")
SpearCorr_463$ID <- "S463"
SpearCorr_464 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_464.csv")
SpearCorr_464$ID <- "S464"
SpearCorr_465 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_465.csv")
SpearCorr_465$ID <- "S465"
SpearCorr_466 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_466.csv")
SpearCorr_466$ID <- "S466"
SpearCorr_470 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_470.csv")
SpearCorr_470$ID <- "S470"
SpearCorr_471 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_471.csv")
SpearCorr_471$ID <- "S471"
SpearCorr_472 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_472.csv")
SpearCorr_472$ID <- "S472"
SpearCorr_473 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_473.csv")
SpearCorr_473$ID <- "S473"
SpearCorr_474 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_474.csv")
SpearCorr_474$ID <- "S474"
SpearCorr_475 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_475.csv")
SpearCorr_475$ID <- "S475"
SpearCorr_476 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_476.csv")
SpearCorr_476$ID <- "S476"
SpearCorr_477 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_477.csv")
SpearCorr_477$ID <- "S477"
SpearCorr_478 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_478.csv")
SpearCorr_478$ID <- "S478"
SpearCorr_480 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_480.csv")
SpearCorr_480$ID <- "S480"
SpearCorr_481 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_481.csv")
SpearCorr_481$ID <- "S481"
SpearCorr_482 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_482.csv")
SpearCorr_482$ID <- "S482"
SpearCorr_483 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_483.csv")
SpearCorr_483$ID <- "S483"
SpearCorr_484 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_484.csv")
SpearCorr_484$ID <- "S484"
SpearCorr_485 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_485.csv")
SpearCorr_485$ID <- "S485"
SpearCorr_490 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_490.csv")
SpearCorr_490$ID <- "S490"
SpearCorr_491 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_491.csv")
SpearCorr_491$ID <- "S491"
SpearCorr_492 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_492.csv")
SpearCorr_492$ID <- "S492"
SpearCorr_493 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_493.csv")
SpearCorr_493$ID <- "S493"
SpearCorr_494 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_494.csv")
SpearCorr_494$ID <- "S494"
SpearCorr_495 <- read.csv("C:/RProjects/UseIndicators/output/analysis/SpearCorr/SpearCorr_495.csv")
SpearCorr_495$ID <- "S495"

SpearCorrResults <- rbind(SpearCorr_440, SpearCorr_441, SpearCorr_442, SpearCorr_443, SpearCorr_444,
                          SpearCorr_445, SpearCorr_446, SpearCorr_447, SpearCorr_448, SpearCorr_449,
                          SpearCorr_450, SpearCorr_451, SpearCorr_452, SpearCorr_453, SpearCorr_454,
                          SpearCorr_455, SpearCorr_456, SpearCorr_457, SpearCorr_458, SpearCorr_459,
                          SpearCorr_460, SpearCorr_461, SpearCorr_462, SpearCorr_463, SpearCorr_464,
                          SpearCorr_465, SpearCorr_466, SpearCorr_470, SpearCorr_471, SpearCorr_472,
                          SpearCorr_473, SpearCorr_474, SpearCorr_475, SpearCorr_476, SpearCorr_477,
                          SpearCorr_478, SpearCorr_480, SpearCorr_481, SpearCorr_482, SpearCorr_483,
                          SpearCorr_484, SpearCorr_485, SpearCorr_490, SpearCorr_491, SpearCorr_492,
                          SpearCorr_493, SpearCorr_494, SpearCorr_495, SpearCorr_4VN, SpearCorr_4VS,
                          SpearCorr_4W, SpearCorr_4X, SpearCorr_ESS, SpearCorr_SHELF, SpearCorr_WSS)
                          
head(SpearCorrResults)
SpearCorrResults_SUMMARY <- SpearCorrResults[,c('ID', 'row', 'column', 'cor', 'p')] 
write.csv(SpearCorrResults_SUMMARY, "output/analysis/SpearCorr/SpearCorrResults_summary.csv", row.names=FALSE)


