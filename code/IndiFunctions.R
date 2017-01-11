  #Plots time-series of indicators depending on the scale (ID: shelf, NAFO or strata
  #Data set is defined in IndiData.R

PlotIndicators <- function(x, y) {
  plot(x[,c('YEAR', y)], pch=16, cex=0.5, main = unique(x$ID))
}


PlotNAs <- function(x) {
  plot(x[,c('YEAR', 'BiomassTL2_i')], pch=16, cex=0.5, main = unique(x$ID))
  points(x[,c('YEAR', 'BiomassTL2')], pch=1,cex=0.8, col='red', main = unique(x$ID))
  plot(x[,c('YEAR', 'CCLargeBenthivore_i')], pch=16, cex=0.5, main = unique(x$ID))
  points(x[,c('YEAR', 'CCLargeBenthivore')], pch=1,cex=0.8, col='red', main = unique(x$ID))
  plot(x[,c('YEAR','CCPlanktivore_i')], pch=16, cex=0.5, main = unique(x$ID))
  points(x[,c('YEAR', 'CCPlanktivore')], pch=1,cex=0.8, col='red', main = unique(x$ID))
  plot(x[,c('YEAR','InverseCVBiomass_i')], pch=16, cex=0.5, main = unique(x$ID))
  points(x[,c('YEAR', 'InverseCVBiomass')], pch=1,cex=0.8, col='red', main = unique(x$ID))
}


# plot(SS_plot[,c('YEAR', 'BiomassTL2')], pch=1, cex=0.5)
# points(SS_plot[,c('YEAR', 'BiomassTL2_i')], pch=1,cex=0.8, col='red')
PlotNAs <- function(x) {
plot(x[,c('YEAR', 'BiomassTL2_i')], pch=16, cex=0.5, main = unique(x$ID))
points(x[,c('YEAR', 'BiomassTL2')], pch=1,cex=0.8, col='red', main = unique(x$ID))
plot(x[,c('YEAR', 'CCLargeBenthivore_i')], pch=16, cex=0.5, main = unique(x$ID))
points(x[,c('YEAR', 'CCLargeBenthivore')], pch=1,cex=0.8, col='red', main = unique(x$ID))
plot(x[,c('YEAR','CCPlanktivore_i')], pch=16, cex=0.5, main = unique(x$ID))
points(x[,c('YEAR', 'CCPlanktivore')], pch=1,cex=0.8, col='red', main = unique(x$ID))
plot(x[,c('YEAR','InverseCVBiomass_i')], pch=16, cex=0.5, main = unique(x$ID))
points(x[,c('YEAR', 'InverseCVBiomass')], pch=1,cex=0.8, col='red', main = unique(x$ID))
}

# PlotNAs <- function(x) {
#   plot(x[,c('BiomassTL2_i')], pch=16, cex=0.5, main = unique(x$ID))
#   points(x$BiomassTL2, pch=1,cex=0.8, col='red', main = unique(x$ID))
#   plot(x[,c('CCLargeBenthivore_i')], pch=16, cex=0.5, main = unique(x$ID))
#   points(x$CCLargeBenthivore, pch=1,cex=0.8, col='red', main = unique(x$ID))
#   plot(x[,c('CCPlanktivore_i')], pch=16, cex=0.5, main = unique(x$ID))
#   points(x$CCPlanktivore, pch=1,cex=0.8, col='red', main = unique(x$ID))
#   plot(x[,c('InverseCVBiomass_i')], pch=16, cex=0.5, main = unique(x$ID))
#   points(x$InverseCVBiomass, pch=1,cex=0.8, col='red', main = unique(x$ID))
# }


PlotNAs_NAFO <- function(x) {
  plot(x[,c('YEAR','BiomassTL2_i')], pch=16, cex=0.5, main = unique(x$ID))
  points(x[,c('YEAR', 'BiomassTL2')], pch=1,cex=0.5, col='red')
  plot(x[,c('YEAR','CCLargeBenthivore_i')], pch=16, cex=0.5, main = unique(x$ID))
  points(x[,c('YEAR', 'CCLargeBenthivore')], pch=1,cex=0.5, col='red')
  plot(x[,c('YEAR','CCPlanktivore_i')], pch=16, cex=0.5, main = unique(x$ID))
  points(x[,c('YEAR', 'CCPlanktivore')], pch=1,cex=0.5, col='red')
  plot(x[,c('YEAR','InverseCVBiomass_i')], pch=16, cex=0.5, main = unique(x$ID))
  points(x[,c('YEAR', 'InverseCVBiomass')], pch=1,cex=0.5, col='red')
  plot(x[,c('YEAR','FPClupeids.L_i')], pch=16, cex=0.5, main = unique(x$ID))
  points(x[,c('YEAR', 'FPClupeids.L')], pch=1,cex=0.5, col='red')
  plot(x[,c('YEAR','FPForageFish.L_i')], pch=16, cex=0.5, main = unique(x$ID))
  points(x[,c('YEAR', 'FPForageFish.L')], pch=1,cex=0.5, col='red')
  plot(x[,c('YEAR','BPelagicToDemersal_i')], pch=16, cex=0.5, main = unique(x$ID))
  points(x[,c('YEAR', 'BPelagicToDemersal')], pch=1,cex=0.5, col='red')
  plot(x[,c('YEAR','BiomassClupeids_i')], pch=16, cex=0.5, main = unique(x$ID))
  points(x[,c('YEAR', 'BiomassClupeids')], pch=1,cex=0.5, col='red')
  plot(x[,c('YEAR','BiomassForage_i')], pch=16, cex=0.5, main = unique(x$ID))
  points(x[,c('YEAR', 'BiomassForage')], pch=1,cex=0.5, col='red')
}


PlotNAs_strata <- function(x) {
  plot(x[,c('YEAR','CCLargeBenthivore_i')], pch=16, cex=0.5, main = unique(x$ID))
  points(x[,c('YEAR', 'CCLargeBenthivore')], pch=1,cex=0.5, col='red')
  plot(x[,c('YEAR','CCPlanktivore_i')], pch=16, cex=0.5, main = unique(x$ID))
  points(x[,c('YEAR', 'CCPlanktivore')], pch=1,cex=0.5, col='red')
  plot(x[,c('YEAR','InverseCVBiomass_i')], pch=16, cex=0.5, main = unique(x$ID))
  points(x[,c('YEAR', 'InverseCVBiomass')], pch=1,cex=0.5, col='red')
  plot(x[,c('YEAR','BPelagicToDemersal_i')], pch=16, cex=0.5, main = unique(x$ID))
  points(x[,c('YEAR', 'BPelagicToDemersal')], pch=1,cex=0.5, col='red')
  plot(x[,c('YEAR','BiomassClupeids_i')], pch=16, cex=0.5, main = unique(x$ID))
  points(x[,c('YEAR', 'BiomassClupeids')], pch=1,cex=0.5, col='red')
  plot(x[,c('YEAR','BiomassForage_i')], pch=16, cex=0.5, main = unique(x$ID))
  points(x[,c('YEAR', 'BiomassForage')], pch=1,cex=0.5, col='red')
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
    theme(legend.position='bottom') #+
   # ggtitle(unique(x$ID))
    #theme(legend.position="none")
}

PlotIndi_with_datapoints <- function(x) {
  
  ggplot(x, aes(YEAR, value, colour=variable,linetype = variable, group=variable)) +
    scale_color_brewer(palette='Set1') +
    stat_smooth(se=F) + geom_point() +
    facet_grid(. ~ ID) +
    theme(text = element_text(size=15)) +
    theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) +
    theme(legend.title=element_blank()) +
    theme(legend.position='bottom')
  #ggtitle(z)
  #theme(legend.position="none")
}


PlotIndi_with_line <- function(x) {
  
  ggplot(x, aes(YEAR, value, colour=variable,
                group=variable)) +
    scale_colour_manual(values=cbbPalette) +
    geom_point(size = 0.5) + geom_line(size=1.3) +
    facet_grid(. ~ ID) +
    theme(text = element_text(size=26)) +
    theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) +
    theme(legend.title=element_blank()) +
    theme(legend.position='bottom') +
    theme(strip.text.x = element_text(size=13),
          strip.background = element_rect(fill="white"))

}

PlotIndi_with_line_noPanels <- function(x) {
  
  ggplot(x, aes(YEAR, value, colour=variable,
                group=variable)) +
    scale_colour_manual(values=cbbPalette) +
    geom_point(size = 0.5) + geom_line(size=1.3) +
    #facet_grid(. ~ ID) +
    theme(text = element_text(size=26)) +
    theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) +
    theme(legend.title=element_blank()) +
    theme(legend.position='bottom') +
    theme(strip.text.x = element_text(size=13),
          strip.background = element_rect(fill="white")) 
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

PlotIndi_strata_withline <- function(x) {
  ggplot(x, aes(YEAR, value, colour=variable, group=variable)) + #linetype = variable, 
    #scale_color_brewer(palette="Paired") +
    scale_colour_manual(values=cbbPalette) +
    geom_point(size = 0.5) + geom_line(size=1.3) +
    facet_wrap( ~ ID) +
    theme(text = element_text(size=22)) +
    theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) +
    theme(legend.title=element_blank()) +
    theme(legend.position='bottom') +
    #ggtitle("Ci (21%)") +
    theme(strip.text.x = element_text(size=22),
          strip.background = element_rect(fill="white")) 
    #ggsave("myplot.png", path = 'C:/RProjects/UseIndicators/output/figures/clusters&singletons/strata')
   }
#PlotIndi_strata_withline(Ca)






PlotDecoupledIndi <- function(x) {
  
  ggplot(x, aes(YEAR, BIOMASS, colour=Species,
                group=Species)) +
    scale_colour_manual(values=cbbPalette) +
    geom_point(size=0.5) + geom_line(size=1.3) +
    scale_colour_manual(values=cbbPalette) +
    facet_grid(. ~ ID) +
    theme(text = element_text(size=26)) +
    theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) +
    theme(legend.title=element_blank()) +
    theme(legend.position='bottom')
 }


cbbPalette <- c("#3A74BF", "#D41200", "#080100", "#F0E442", "#1f7000", "#00d5b1", 
                "#CC79A7", "#697172", "#7dfc07", "#f7b927", "#f727ec", "#cfc1ff",
                "#e04e00", "#4c3002", "#b2aea7", "#66032c", "#285d66", "#6b1393", "#27e0f7", "#ffffff")

PlotDecoupledIndi_with_area <- function(x,y) {
ggplot() + 
  geom_area(data=y, aes(x=YEAR, y=value), color='lightgrey',alpha=0.2) +
  geom_line(data=x, aes(x=YEAR, y=BIOMASS, colour=Species, group=Species), size=1.1) +
  geom_point(data=x, aes(x=YEAR, y=BIOMASS, colour=Species, group=Species), size=1.5) +  
  scale_colour_manual(values=cbbPalette) +
  facet_grid(. ~ ID) +
  theme(text = element_text(size=26)) +
  theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) +
  theme(legend.title=element_blank()) +
  theme(legend.justification = "top") +
  theme(legend.key.height=unit(1.5,"line")) +
  ggtitle(unique(y$variable))
    
}

PlotDecoupledIndi_with_area2 <- function(x,y) {
  ggplot() + 
    geom_area(data=y, aes(x=YEAR, y=Biomass_minusForage), color='lightgrey',alpha=0.2) +
    geom_line(data=x, aes(x=YEAR, y=BIOMASS, colour=Species, group=Species), size=1.1) +
    geom_point(data=x, aes(x=YEAR, y=BIOMASS, colour=Species, group=Species), size=1.5) + 
    scale_colour_manual(values=cbbPalette) +
    facet_grid(. ~ ID) +
    theme(text = element_text(size=26)) +
    theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) +
    theme(legend.title=element_blank()) +
    theme(legend.justification = "top") +
    theme(legend.key.height=unit(1.5,"line")) +
    ggtitle("Biomass_minusForage")
  
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



#The following is from https://gist.github.com/dfalster/5589956
##' Modifies 'data' by adding new values supplied in newDataFileName
##'
##' newDataFileName is expected to have columns 
##' c(lookupVariable,lookupValue,newVariable,newValue,source)
##' 
##' Within the column 'newVariable', replace values that
##' match 'lookupValue' within column 'lookupVariable' with the value
##' newValue'.  If 'lookupVariable' is NA, then replace *all* elements
##' of 'newVariable' with the value 'newValue'.
##'
##' Note that lookupVariable can be the same as newVariable.
##'
##' @param newDataFileName name of lookup table
##' @param data existing data.frame
##' @param allowedVars vector of permissible variable names for newVariable
##' @return modified data.frame
addNewData <- function(newDataFileName, data, allowedVars){
  
  import <- readNewData(newDataFileName, allowedVars)
  
  if( !is.null(import)){    
    for(i in seq_len(nrow(import))){  #Make replacements
      col.to <- import$newVariable[i] 
      col.from <- import$lookupVariable[i]
      if(is.na(col.from)){ # apply to whole column
        data[col.to] <- import$newValue[i]
      } else { # apply to subset
        rows <- data[[col.from]] == import$lookupValue[i]
        data[rows,col.to] <- import$newValue[i]
      }
    }   
  }      
  data
}

##' Utility function to read/process newDataFileName for addNewData
##' 
##' @param newDataFileName name of lookup table
##' @param allowedVars vector of permissible variable names for newVariable
##' @return data.frame with columns c(lookupVariable,lookupValue,newVariable,newValue,source)
readNewData <- function(newDataFileName, allowedVars){
  
  if( file.exists(newDataFileName)){
    import <- read.csv(newDataFileName, header=TRUE, stringsAsFactors=FALSE,
                       strip.white=TRUE)
    if( nrow(import)> 0 ){
      
      #Check columns names for import are right
      expectedColumns<- c("lookupVariable","lookupValue","newVariable","newValue")
      nameIsOK <-  expectedColumns %in% names(import)
      if(any(!nameIsOK))
        stop("Incorrect name in lookup table for ",
             newDataFileName, "--> ", paste(expectedColumns[!nameIsOK],
                                            collapse=", "))
      
      #Check values of newVariable are in list of allowed variables
      import$lookupVariable[import$lookupVariable == ""] <- NA
      nameIsOK <- import$newVariable %in% allowedVars
      if(any(!nameIsOK))
        stop("Incorrect name(s) in newVariable column of ",
             newDataFileName, "--> ", paste(import$newVariable[!nameIsOK],
                                            collapse=", "))
    } else {
      import <- NULL
    }
  } else {
    import <- NULL
  }
  import
}


