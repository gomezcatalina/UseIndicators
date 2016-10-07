#write.csv(BiodivIndi_440, "outputs/data/data.csv") 
#data <- read.csv("outputs/data/data.csv",header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
#data <- read.csv("file:///D:/RProjects/UseIndi/data/strata_sylvie/stratsetq2.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
#data <- read.csv("file:///D:/RProjects/UseIndi/data/strata_sylvie/Indi_C_s.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
#data <- read.csv("file:///D:/RProjects/UseIndi/data/strata_sylvie/Indi_C_s.csv")
# data <- data[data$Strata %in% c('440'), ]
# table(data$Year, data$Strata)

#*Used by CG for regundancy analysis in 2014
#data <- read.csv("file:///D:/CatalinaSync/EcosystemIndicators/SPERA_Indicators_2014/3) Hierarchical cluster analysis/rSpearman/ESS.csv",header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)

#*Used by CG for redundancy analysis in 2014
 # data <- read.csv("file:///D:/CatalinaSync/EcosystemIndicators/SPERA_Indicators_2014/Data/Indicators with Heips Index/esswsssetq_AB_Heips.csv",header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
 # data <- Shelf_Q[Shelf_Q$ID %in% c('ESS'), ]
 # data <- subset(data, select = c("HillN1Diversity", "ShannonDiversity", "HillN2Dominance", "PielouEvenness"))

#*data latest extraction which matches redundancy analysis  
data <- read.csv("data/esswsssetq.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
data <- Shelf_Q[Shelf_Q$ID %in% c('ESS'), ]
data <- subset(data, select = c("HillN1Diversity", "ShannonDiversity", "HillN2Dominance", "PielouEvenness"))

data <- subset(data, select = c("HillN1Diversity", "ShannonDiversity", "HillN2Dominance", "PielouEvenness"))
head(data)


library(Hmisc)

rcorr(as.matrix(data3,type="spearman")) #Used by CG in large scale correlation analysis

cor(as.matrix(data,type="spearman")) 

cor(data, method = "spearman") # used to re-create strata results - not that it producea different results as line above


library(PerformanceAnalytics)

chart.Correlation(data, histogram=FALSE, pch=19, 
                  method = "spearman", 
                  main = "does not match results of rcorr and cor")

chart.Correlation(data, histogram=FALSE, pch=19, 
                  method = "pearson", 
                  main = "pearson matches spearman results?")

?corr.test
library(psych)
ct <- corr.test(data, method = "spearman") # calculate correlation using another package
ct$r # get correlation coefs
ct$p # get





