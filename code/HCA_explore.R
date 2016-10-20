library(pvclust)
rm(list=ls())
data <- read.csv("output/data/nafo4VNsetq_filtered&interpolated.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
data$ID <- factor(data$ID)
myvars <- names(data) %in% c("YEAR", "ID") 
data_s <- data[!myvars]
data_s = apply(data_s,2,function(x){(x-mean(x, na.rm=T))/sd(x, na.rm=T)}) 
head(data_s)

# pdf("output/figures/redundancy/HCA NAFO 4VN).pdf", width=13, height=9)
# fit <- pvclust(data_s, method.hclust="complete",
#                method.dist="euclidean", nboot = 1) #nboot = 10000
# plot(fit, print.num=F,col.pv=c('red','white'),main="NAFO 4VS", float=0.01) # dendogram with p values
# pvrect(fit, alpha=.95, border=3, pv="au", lwd=2)
# out=pvpick(fit,alpha=.95, pv="au", type="geq", max.only=TRUE)
# dput(out,"output/figures/redundancy/NAFO 4VN.txt")
# dev.off()

HCA <- function(x, y) {
  mypath <- file.path("output","figures","redundancy",paste("HCA_", unique(y$ID), ".pdf", sep = ""))
  mypath2 <- file.path("output","figures","redundancy",paste("HCA_", unique(y$ID), ".txt", sep = ""))
  pdf(mypath, width=13, height=9)
  
  fit <- pvclust(x, method.hclust="complete",
                 method.dist="euclidean", nboot = 1) #nboot = 10000
  plot(fit, print.num=F,col.pv=c('red','white'), main= unique(y$ID), float=0.01) # dendogram with p values
  pvrect(fit, alpha=.95, border=3, pv="au", lwd=2)
  out=pvpick(fit,alpha=.95, pv="au", type="geq", max.only=TRUE)
  dput(out,mypath2)
  dev.off()
}

HCA(data_s, data)

