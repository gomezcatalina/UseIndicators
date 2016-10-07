
#Plot individual indicators (x = indicator -->colnames(LargeScales), y = data set (LargeScales, Shelf_Q, IndiQ_NAFO, or Strata_Q))
IndividualIndi <- ggplot(IndiQ_NAFO_4vs, aes(YEAR, BiomassClupeids)) + 
  stat_smooth(se=F, colour = "red") + geom_point(size = 2) + facet_wrap( ~ ID, scales = "free") +
  theme(text = element_text(size=15)) +  theme(strip.text.x = element_text(size=18), strip.background = element_rect(fill="white")) +  scale_fill_grey() 

IndividualIndi_stdize <- ggplot(IndiQ_NAFO_4vs, aes(YEAR, BiomassClupeids_s)) + 
  stat_smooth(se=F, colour = "red") + geom_point(size = 2) + facet_wrap( ~ ID, scales = "free") +
  theme(text = element_text(size=15)) +  theme(strip.text.x = element_text(size=18), strip.background = element_rect(fill="white")) +  scale_fill_grey() 

#pdf("outputs/figs/BiomassClupeids.pdf", width=16,height=14)
IndividualIndi
IndividualIndi_stdize
dev.off()

#Plot data series of indicators for all spatial scales 
Plots_all_Indi <- lapply(AllIndi, PlotIndi)
Plots_all_Indi_strata <- lapply(AllIndi_strata, PlotIndi_strata)

  # Save data series plots of all indicators: singletons and clusters
#pdf("outputs/figs/Clusters&Singletons_all scales.pdf", width=15,height=10)
Plots_all_Indi
Plots_all_Indi_strata
dev.off()

#  Heatmaps to show missing data at the large scale
# pdf("outputs/figs/MissingData_largescales.pdf", width=10,height=10)
Suite_heatmap_largescale <- ggplot(CandidateIndi_stdz, aes(YEAR, variable)) +
   geom_tile(aes(fill = value), colour = "white") +
   scale_fill_gradient(low = "white", high = "blue") +
   facet_grid(. ~ ID) +
   theme(text = element_text(size=12)) +
   theme(legend.title=element_blank()) +
   theme(legend.position='bottom') +
   theme(legend.position="none")  +
   theme(strip.text.x = element_text(size=13),strip.background = element_rect(fill="white")) +
   theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank())
 dlply(CandidateIndi_stdz, .(ID), function(x) Suite_heatmap_largescale %+% x + ggtitle(x$ID))
 dev.off()

# Heatmaps to show missing data at the strata scale
#pdf("outputs/figs/MissingData_strataScale.pdf", width=15,height=12)
Suite_heatmap_strata <- ggplot(CandidateIndi_strata_stdz, aes(YEAR, variable)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  #facet_grid(. ~ ID) +
  theme(text = element_text(size=16)) +
  theme(legend.title=element_blank()) +
  theme(legend.position='bottom') +
  theme(legend.position="none")  +
  theme(strip.text.x = element_text(size=15),strip.background = element_rect(fill="white")) +
  theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) +
  ggtitle("All strata combined (missing data: grey squares)")
Suite_heatmap_strata
dlply(CandidateIndi_strata_stdz, .(ID), function(x) Suite_heatmap_strata %+% x + ggtitle(x$ID))
dev.off()


  #Plot/save all candidate indicators + extra indi calculated (e.g. abundance)

#pdf("outputs/figs/CandidateIndi.pdf", width=15,height=10)
dlply(CandidateIndi , .(variable), function(dat) {
  ggplot(data = dat, aes(x = YEAR, y = value, linetype = variable, group=variable)) +
    scale_color_brewer(palette="Paired") +
    stat_smooth(se=F) + geom_point() +
    facet_wrap(variable ~ ID, scales = "free") +
    theme(text = element_text(size=15)) +
    theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) +
    theme(legend.title=element_blank()) +
    theme(legend.position='bottom') +
    #ggtitle("") +
    theme(legend.position="none") +
    theme(strip.text.x = element_text(size=13),
          strip.background = element_rect(fill="white"))
})
dev.off()

# ggplot(LargeScales, aes(YEAR, FPInvertebrates.L_s)) + 
#   #stat_smooth(se=F, colour = "red") + 
#   geom_point(size = 2) + 
#   facet_wrap( ~ ID, scales = "free") +
#   theme(text = element_text(size=15)) +  
#   theme(strip.text.x = element_text(size=18), 
#   strip.background = element_rect(fill="white")) +  
#   scale_fill_grey() 
# 
# write.csv(LargeScales, "outputs/data/LargeScales.csv")

#pdf("outputs/figs/CandidateIndi_strata.pdf", width=20,height=13)
dlply(CandidateIndi_strata , .(variable), function(dat) {
  ggplot(data = dat, aes(x = YEAR, y = value, linetype = variable, group=variable)) +
    scale_color_brewer(palette="Paired") +
    stat_smooth(se=F) + 
    geom_point() +
    facet_wrap(variable ~ ID) +
    theme(text = element_text(size=15)) +
    theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) +
    theme(legend.title=element_blank()) +
    theme(legend.position='bottom') +
    #ggtitle("") +
    theme(legend.position="none") +
    theme(strip.text.x = element_text(size=13),
          strip.background = element_rect(fill="white"))
})
dev.off()









