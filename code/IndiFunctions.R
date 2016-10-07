
  #Plots time-series of indicators depending on the scale (ID: shelf, NAFO or strata
  #Data set is defined in IndiData.R

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
