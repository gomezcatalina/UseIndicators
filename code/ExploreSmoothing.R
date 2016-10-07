
pdf("outputs/figs/BClupeids_MovingAverage.pdf", width=14,height=16)
par(mfrow=c(2,1))
MovAvg_10 <- rollapply(ESS$BiomassClupeids, 10, mean, na.rm=TRUE)
MovAvg_5 <- rollapply(ESS$BiomassClupeids, 5, mean, na.rm=TRUE)
MovAvg_3 <- rollapply(ESS$BiomassClupeids, 3, mean, na.rm=TRUE)
MovAvg_2 <- rollapply(ESS$BiomassClupeids, 2, mean, na.rm=TRUE)
plot(ESS$YEAR, ESS$BiomassClupeids, pch = 18)
par(new=T)
plot.ts(MovAvg_2, col= "green", axes = F, ann = FALSE, lwd = 2)
par(new=T)
plot.ts(MovAvg_5, col= "blue", axes = F, ann = FALSE, lwd = 2)
par(new=T)
plot.ts(MovAvg_3, col= "black", axes = F, ann = FALSE, lwd = 2)
par(new=T)
plot.ts(MovAvg_10, col= "red", axes = F, ann = FALSE, lwd = 2)
legend("topleft", legend=c("10yr", "5yr", "3yr", "2yr"), 
       text.col=c('green','blue','black', 'red'), ncol=4) 
title("Moving average - ESS")

MovAvg_10 <- rollapply(IndiQ_NAFO_4vs$BiomassClupeids, 10, mean, na.rm=TRUE)
MovAvg_5 <- rollapply(IndiQ_NAFO_4vs$BiomassClupeids, 5, mean, na.rm=TRUE)
MovAvg_3 <- rollapply(IndiQ_NAFO_4vs$BiomassClupeids, 3, mean, na.rm=TRUE)
MovAvg_2 <- rollapply(IndiQ_NAFO_4vs$BiomassClupeids, 2, mean, na.rm=TRUE)
plot(IndiQ_NAFO_4vs$YEAR, IndiQ_NAFO_4vs$BiomassClupeids, pch = 18)
par(new=T)
plot.ts(MovAvg_2, col= "green", axes = F, ann = FALSE, lwd = 2)
par(new=T)
plot.ts(MovAvg_5, col= "blue", axes = F, ann = FALSE, lwd = 2)
par(new=T)
plot.ts(MovAvg_3, col= "black", axes = F, ann = FALSE, lwd = 2)
par(new=T)
plot.ts(MovAvg_10, col= "red", axes = F, ann = FALSE, lwd = 2)
legend("topleft", legend=c("10yr", "5yr", "3yr", "2yr"), 
       text.col=c('green','blue','black', 'red'), ncol=4) 
title("Moving average 4Vs")
dev.off()

  

pdf("outputs/figs/BClupeids_SmoothMethods.pdf", width=10,height=8)
p <- ggplot(ESS, aes(YEAR, BiomassClupeids)) 
p <- p + geom_point(size = 1)
p + stat_smooth (method = "lm", span = 0.75, formula = y ~ x, size = 1, se = FALSE, colour = "red") +   
  stat_smooth(method = "loess", formula = y ~ x, size = 1, se = FALSE, colour = "black") + 
  stat_smooth(method = "gam", formula = y ~ s(x, k = 5), size = 1, se = TRUE, colour = "blue") +
  ggtitle("ESS
            red: linear regression line
            black: loess smoothed fit curve 
            blue: gam, k = 5 with 95% confidence region")

p <- ggplot(IndiQ_NAFO_4vs, aes(YEAR, BiomassClupeids)) 
p <- p + geom_point(size = 1)
p + stat_smooth (method = "lm", span = 0.75, formula = y ~ x, size = 1, se = FALSE, colour = "red") +   
  stat_smooth(method = "loess", formula = y ~ x, size = 1, se = FALSE, colour = "black") + 
  stat_smooth(method = "gam", formula = y ~ s(x, k = 5), size = 1, se = TRUE, colour = "blue") +
  ggtitle("4Vs
          red: linear regression line
          black: loess smoothed fit curve 
          blue: gam, k = 5 with 95% confidence region")
dev.off()

pdf("outputs/figs/BClupeids_4VS_GAM.pdf", width=10,height=8)
p <- ggplot(IndiQ_NAFO_4vs, aes(YEAR, BiomassClupeids)) 
p <- p + geom_point(size = 1)
p + stat_smooth(method = "gam", formula = y ~ s(x, k = 5), size = 1, se = TRUE, colour = "red") +
    stat_smooth(method = "gam", formula = y ~ s(x, k = 1), size = 1, se = FALSE, colour = "black") +
    stat_smooth(method = "gam", formula = y ~ s(x, k = 20), size = 1, se = FALSE, colour = "blue") +
    stat_smooth(method = "gam", formula = y ~ s(x, k = 20), size = 1, se = FALSE, colour = "green") +
  ggtitle("red: gam k = 5
          black: gam k = 2
          blue: gam, k = 10
          green: gam, k = 20")
dev.off()
