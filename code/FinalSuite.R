
ggplot(Suite, aes(YEAR, variable)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  facet_grid(. ~ ID) +
  theme(text = element_text(size=12)) +
  theme(legend.title=element_blank()) +
  theme(legend.position='bottom') +
  theme(legend.position="none")  +
  theme(strip.text.x = element_text(size=13),strip.background = element_rect(fill="white")) +
  theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank())

Suite_heatmap_ESSvsWSS <- ggplot(Suite_ESSvsWSS, aes(YEAR, variable)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  facet_grid(. ~ ID) +
  theme(text = element_text(size=12)) +
  theme(legend.title=element_blank()) +
  theme(legend.position='bottom') +
  theme(legend.position="none")  +
  theme(strip.text.x = element_text(size=13),strip.background = element_rect(fill="white")) +
  theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank())


Plot_IndiSuite <- dlply(Suite_ESSvsWSS , .(variable), function(dat) {
  ggplot(data = dat, aes(x = YEAR, y = value, linetype = variable, group=variable)) +
    stat_smooth(se=F) + geom_point() +
    facet_grid(variable ~ ID) +
    theme(text = element_text(size=15)) +
    theme(axis.title.x=element_blank()) + #theme(axis.title.y=element_blank()) +
    theme(legend.title=element_blank()) +
    theme(legend.position='bottom') +
    #ggtitle("") +
    theme(legend.position="none") +
    theme(strip.text.x = element_text(size=13),
          strip.background = element_rect(fill="white"))
})


Diversity_IndiPlot <- ggplot(Diversity_Indi, aes(YEAR, value, colour=variable,linetype = variable, group=variable)) + scale_color_brewer(palette="Paired") + stat_smooth(se=F) + facet_grid(. ~ ID) + theme(text = element_text(size=15)) + theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) + theme(legend.title=element_blank()) + theme(legend.position='bottom') #ggtitle("C1") + theme(legend.position="none")
Biomass1_IndiPlot <- ggplot(Biomass1_Indi, aes(YEAR, value, colour=variable,linetype = variable, group=variable)) + scale_color_brewer(palette="Paired") + stat_smooth(se=F) + facet_grid(. ~ ID) + theme(text = element_text(size=15)) + theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) + theme(legend.title=element_blank()) + theme(legend.position='bottom') #ggtitle("C1") + theme(legend.position="none")
Biomass2_IndiPlot <- ggplot(Biomass2_Indi, aes(YEAR, value, colour=variable,linetype = variable, group=variable)) + scale_color_brewer(palette="Paired") + stat_smooth(se=F) + facet_grid(. ~ ID) + theme(text = element_text(size=15)) + theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) + theme(legend.title=element_blank()) + theme(legend.position='bottom') #ggtitle("C1") + theme(legend.position="none")
Size_IndiPlot <- ggplot(Size_Indi, aes(YEAR, value, colour=variable,linetype = variable, group=variable)) + scale_color_brewer(palette="Paired") + stat_smooth(se=F) + facet_grid(. ~ ID) + theme(text = element_text(size=15)) + theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) + theme(legend.title=element_blank()) + theme(legend.position='bottom') #ggtitle("C1") + theme(legend.position="none")
CC_IndiPlot <- ggplot(CC_Indi, aes(YEAR, value, colour=variable,linetype = variable, group=variable)) + scale_color_brewer(palette="Paired") + stat_smooth(se=F) + facet_grid(. ~ ID) + theme(text = element_text(size=15)) + theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) + theme(legend.title=element_blank()) + theme(legend.position='bottom') #ggtitle("C1") + theme(legend.position="none")
Pressure1_Indi <- ggplot(Pressure1_Indi, aes(YEAR, value, colour=variable,linetype = variable, group=variable)) + scale_color_brewer(palette="Paired") + stat_smooth(se=F) + facet_grid(. ~ ID) + theme(text = element_text(size=15)) + theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) + theme(legend.title=element_blank()) + theme(legend.position='bottom') #ggtitle("C1") + theme(legend.position="none")
Pressure2_Indi <- ggplot(Pressure2_Indi, aes(YEAR, value, colour=variable,linetype = variable, group=variable)) + scale_color_brewer(palette="Paired") + stat_smooth(se=F) + facet_grid(. ~ ID) + theme(text = element_text(size=15)) + theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) + theme(legend.title=element_blank()) + theme(legend.position='bottom') #ggtitle("C1") + theme(legend.position="none")


pdf("outputs/figs/FinalIndiSuite.pdf", width=19,height=16)
grid.arrange(Diversity_IndiPlot, Size_IndiPlot, Biomass1_IndiPlot, Biomass2_IndiPlot,CC_IndiPlot, Pressure1_Indi,
             Pressure2_Indi, nrow=4)
dev.off()


pdf("outputs/figs/FinalIndiSuite_additionalPlots.pdf", width=14,height=9)
chart.Correlation(ESS_Cor, histogram=FALSE, pch=19, method = c("pearson"), main ="Spearman correlations for the suite of indicators in the ESS (p-values: 0***, 0.001**, 0.01*)")
chart.Correlation(ESS_Cor.L, histogram=FALSE, pch=19, method = c("pearson"), main ="Spearman correlations for the suite of indicators (derived from landings) in the ESS")
chart.Correlation(WSS_Cor, histogram=FALSE, pch=19, method = c("pearson"), main ="Spearman correlations for the suite of indicators in the WSS")
chart.Correlation(WSS_Cor.L, histogram=FALSE, pch=19, method = c("pearson"), main ="Spearman correlations for the suite of indicators (derived from landings) in the WSS")
Plot_IndiSuite
Suite_heatmap_ESSvsWSS
# Diversity_IndiPlot
# Size_IndiPlot
# Biomass1_IndiPlot
# Biomass2_IndiPlot
# CC_IndiPlot
# Pressure1_Indi
# Pressure2_Indi
dlply(Suite, .(ID), function(x) Suite_heatmap %+% x)
dev.off()


strata440_function_rcorr <- rcorr(as.matrix(BiodivIndi_440,type="spearman"))
strata440 <- flattenCorrMatrix(strata440_function_rcorr$r, strata440_function_rcorr$P)
write.csv(strata440, "outputs/data/strata440.csv")   







