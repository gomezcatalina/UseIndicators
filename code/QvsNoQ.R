#Plot individual indicators at the shelf scale using Q vs noQ

Shelf_Q$Catch <- "Q"
Shelf_Q$ID <- factor(Shelf_Q$ID)
Shelf_Q$Catch <- factor(Shelf_Q$Catch)
#colnames(Shelf_Q) <- paste(colnames(Shelf_Q), "Q", sep = "_")
#colnames(Shelf_Q)[colnames(Shelf_Q)=="YEAR_Q"] <- "YEAR"
#colnames(Shelf_Q)[colnames(Shelf_Q)=="ID_Q"] <- "ID"
Shelf_noQ <- read.csv("data/esswssnonq.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
Shelf_noQ$Catch <- "noQ"
Shelf_noQ$ID <- factor(Shelf_noQ$ID)
Shelf_noQ$Catch <- factor(Shelf_noQ$Catch)
Shelf <- rbind(Shelf_Q, Shelf_noQ)
           
#Plot Heips q vs noQ
ggplot(Shelf, aes(YEAR, Heips_s)) + 
    aes(linetype=Catch, colour=Catch) + scale_color_manual(values=c('black', 'darkgrey')) +
    stat_smooth(se=F) + geom_point() + facet_grid(. ~ ID) +
    theme(text = element_text(size=15)) 

