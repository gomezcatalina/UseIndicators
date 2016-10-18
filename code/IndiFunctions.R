  #Plots time-series of indicators depending on the scale (ID: shelf, NAFO or strata
  #Data set is defined in IndiData.R

PlotNAs <- function(x) {
plot(x[,c('BiomassTL2_i')], pch=16, cex=0.5, main = unique(x$ID))
points(x$BiomassTL2, pch=1,cex=0.8, col='red', main = unique(x$ID))
plot(x[,c('CCLargeBenthivore_i')], pch=16, cex=0.5, main = unique(x$ID))
points(x$CCLargeBenthivore, pch=1,cex=0.8, col='red', main = unique(x$ID))
plot(x[,c('CCPlanktivore_i')], pch=16, cex=0.5, main = unique(x$ID))
points(x$CCPlanktivore, pch=1,cex=0.8, col='red', main = unique(x$ID))
plot(x[,c('InverseCVBiomass_i')], pch=16, cex=0.5, main = unique(x$ID))
points(x$InverseCVBiomass, pch=1,cex=0.8, col='red', main = unique(x$ID))
}

PlotNAs_NAFO <- function(x) {
  plot(x[,c('BiomassTL2_i')], pch=16, cex=0.5, main = unique(x$ID))
  points(x$BiomassTL2, pch=16,cex=0.5, col='red')
  plot(x[,c('CCLargeBenthivore_i')], pch=16, cex=0.5, main = unique(x$ID))
  points(x$CCLargeBenthivore, pch=1,cex=0.5, col='red')
  plot(x[,c('CCPlanktivore_i')], pch=16, cex=0.5, main = unique(x$ID))
  points(x$CCPlanktivore, pch=1,cex=0.5, col='red')
  plot(x[,c('InverseCVBiomass_i')], pch=16, cex=0.5, main = unique(x$ID))
  points(x$InverseCVBiomass, pch=1,cex=0.5, col='red')
  plot(x[,c('FPClupeids.L_i')], pch=16, cex=0.5, main = unique(x$ID))
  points(x$FPClupeids.L, pch=1,cex=0.5, col='red')
  plot(x[,c('FPForageFish.L_i')], pch=16, cex=0.5, main = unique(x$ID))
  points(x$FPForageFish.L, pch=1,cex=0.5, col='red')
  plot(x[,c('BPelagicToDemersal_i')], pch=16, cex=0.5, main = unique(x$ID))
  points(x$BPelagicToDemersal, pch=1,cex=0.5, col='red')
  plot(x[,c('BiomassClupeids_i')], pch=16, cex=0.5, main = unique(x$ID))
  points(x$BiomassClupeids, pch=1,cex=0.5, col='red')
  plot(x[,c('BiomassForage_i')], pch=16, cex=0.5, main = unique(x$ID))
  points(x$BiomassForage, pch=1,cex=0.5, col='red')
  plot(x[,c('BiomassPelagic_i')], pch=16, cex=0.5, main = unique(x$ID))
  points(x$BiomassPelagic, pch=1,cex=0.5, col='red')
}


PlotNAs_strata <- function(x) {
  plot(x[,c('CCLargeBenthivore_i')], pch=16, cex=0.5, main = unique(x$ID))
  points(x$CCLargeBenthivore, pch=1,cex=0.5, col='red')
  plot(x[,c('CCPlanktivore_i')], pch=16, cex=0.5, main = unique(x$ID))
  points(x$CCPlanktivore, pch=1,cex=0.5, col='red')
  plot(x[,c('InverseCVBiomass_i')], pch=16, cex=0.5, main = unique(x$ID))
  points(x$InverseCVBiomass, pch=1,cex=0.5, col='red')
  plot(x[,c('BPelagicToDemersal_i')], pch=16, cex=0.5, main = unique(x$ID))
  points(x$BPelagicToDemersal, pch=1,cex=0.5, col='red')
  plot(x[,c('BiomassClupeids_i')], pch=16, cex=0.5, main = unique(x$ID))
  points(x$BiomassClupeids, pch=1,cex=0.5, col='red')
  plot(x[,c('BiomassForage_i')], pch=16, cex=0.5, main = unique(x$ID))
  points(x$BiomassForage, pch=1,cex=0.5, col='red')
  plot(x[,c('BiomassPelagic_i')], pch=16, cex=0.5, main = unique(x$ID))
  points(x$BiomassPelagic, pch=1,cex=0.5, col='red')
}



# Plot clusters of indicators # e.g. x = meltC1
PlotIndi <- function(x) {
  ggplot(x, aes(YEAR, value, colour=variable,linetype = variable, group=variable)) +
    scale_color_brewer(palette="Paired") +
    stat_smooth(se=F) +  #geom_point() +
    facet_grid(. ~ ID) +
    theme(text = element_text(size=15)) +
    theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) +
    theme(legend.title=element_blank()) +
    theme(legend.position='bottom')
    #ggtitle(z)
    #theme(legend.position="none")
}

PlotIndi_with_datapoints <- function(x) {
  
  ggplot(x, aes(YEAR, value, colour=variable,linetype = variable, group=variable)) +
    scale_color_brewer(palette="Paired") +
    stat_smooth(se=F) + geom_point() +
    facet_grid(. ~ ID) +
    theme(text = element_text(size=15)) +
    theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) +
    theme(legend.title=element_blank()) +
    theme(legend.position='bottom')
  #ggtitle(z)
  #theme(legend.position="none")
}

                                  # e.g. x = meltCa

PlotIndi_strata <- function(x) {
  ggplot(x, aes(YEAR, value, colour=variable,group=variable)) + #linetype = variable, 
    scale_color_brewer(palette="Paired") +
    stat_smooth(se=F) + #geom_point() +
    facet_wrap( ~ ID) +
    theme(text = element_text(size=15)) +
    theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) +
    theme(legend.title=element_blank()) +
    theme(legend.position='bottom') +
    #ggtitle("Ci (21%)") +
    theme(strip.text.x = element_text(size=13),
          strip.background = element_rect(fill="white"))
}


PlotIndi_strata_withdatapoints <- function(x) {
  ggplot(x, aes(YEAR, value, colour=variable,group=variable)) + #linetype = variable, 
    scale_color_brewer(palette="Paired") +
    stat_smooth(se=F) + geom_point() +
    facet_wrap( ~ ID) +
    theme(text = element_text(size=15)) +
    theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) +
    theme(legend.title=element_blank()) +
    theme(legend.position='bottom') +
    #ggtitle("Ci (21%)") +
    theme(strip.text.x = element_text(size=13),
          strip.background = element_rect(fill="white"))
}



flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

stdize <- function(x) {                                                                                   
  y <- (x-mean(x,na.rm=T))/sd(x,na.rm=T)                                                                                
  return(y)                                                                                             
}                 
