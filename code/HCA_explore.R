library(pvclust)
rm(list=ls())
data <- read.csv("data/nafosetq.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
data$ID <- factor(data$ID)
data <- data[data$ID %in% c('4VS'), ]
#data <- subset(data, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s"))
#data <- subset(data, select = c("HillN1Diversity_s", "ShannonDiversity_s", "HillN2Dominance_s", "PielouEvenness_s", "BiomassInvertebrates_s"))
data <- subset(data, select = c("HillN1Diversity_s", "ShannonDiversity_s", "SpeciesRichness_s"))
data <- subset(data, select = c("HillN1Diversity_s", "ShannonDiversity_s", "SpeciesRichness_s", "BiomassTL2"))
data$BiomassTL2 <- NA
head(data)
pdf("outputs/RedundancyAnalysis/Cluster analysis (SS).pdf", width=13, height=9)
fit <- pvclust(data, method.hclust="complete",
               method.dist="euclidean", nboot = 1) #nboot = 10000
plot(fit, print.num=F,col.pv=c('red','white'),main="NAFO 4VS", float=0.01) # dendogram with p values
pvrect(fit, alpha=.95, border=3, pv="au", lwd=2)
out=pvpick(fit,alpha=.95, pv="au", type="geq", max.only=TRUE)
dput(out,"outputs/RedundancyAnalysis/Clusters SS.txt")
dev.off()