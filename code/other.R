#Plot Q corrected Indicators for indicators selected that were similar but in different attributes 

meltOther <- melt(IndiQ_SS,id=c('YEAR', 'ID'), measure = c("MeanLifespan_s",
                                                        "MMLengthAbundance_s",
                                                        "MMLengthBiomass_s", 
                                                        "LargeFishIndicator_s"))
meltOther_NAFO <- melt(IndiQ_NAFO,id=c('YEAR', 'ID'), measure = c("MeanLifespan_s",
                                                          "MMLengthAbundance_s",
                                                          "MMLengthBiomass_s", 
                                                          "LargeFishIndicator_s"))

S_other <- ggplot(meltOther, aes(YEAR, value, colour=variable,linetype = variable, group=variable)) + 
  scale_color_brewer(palette="Paired") +
  stat_smooth(se=F) + #geom_point() + 
  facet_grid(. ~ ID) +
  theme(text = element_text(size=15)) +
  theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) + 
  theme(legend.title=element_blank()) +
  theme(legend.position='bottom') +
  ggtitle("Singletons  A: stability & resistance to perturbations") +
  theme(legend.position="none")
S_other 

S_otherNAFO <- ggplot(meltOther_NAFO, aes(YEAR, value, colour=variable,linetype = variable, group=variable)) + 
  scale_color_brewer(palette="Paired") +
  stat_smooth(se=F) + #geom_point() + 
  facet_grid(. ~ ID) +
  theme(text = element_text(size=15)) +
  theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) + 
  theme(legend.title=element_blank()) +
  theme(legend.position='bottom') #+
#   ggtitle("Singletons    A: stability & resistance to perturbations") 
S_otherNAFO
getwd()
pdf("Plots/Sub-a_EResistance.pdf", width=9,height=8)
grid.arrange(S_other, S_otherNAFO, nrow=2)
dev.off()