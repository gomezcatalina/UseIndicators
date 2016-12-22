# Read and Plot decoupled Indicators
source('C:/RProjects/UseIndicators/code/IndiFunctions.R')    
library(plyr)
library(reshape)
library(dplyr)
library(FSAdata)
library(plotrix)
library(ggplot2)
theme_set(theme_bw())

# -------- Step 1: Use the START.R function to extract biomass of individual species *****  #  ####

#Indicators were decoupled using code in section 6 of ExtractIndicators/inst/START.R - December 19 2016
#Results were saved in RProjects\UseIndicators\data\SpeciesBiomass/12-19/
 

# -------- Step 2: Read all files extracted in step 1, combine them and re-name them based on species names *****  #  ####
#Read Biomass per individual species

#q corrected esswss and nafo areas
# setwd("C://RProjects/UseIndicators/data/SpeciesBiomass/12-19/Qadj/stratifiedLevel")
# filenames <- list.files(path = "C://RProjects/UseIndicators/data/SpeciesBiomass/12-19/Qadj/stratifiedLevel", pattern = NULL, all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE)
# read_csv_filename <- function(filename){
#   ret <- read.csv(filename)
#   ret$Species <- filename #EDIT
#   ret
# }
# import.list <- ldply(filenames, read_csv_filename)
# import.list$X <- NULL
# str(import.list)
# allowedVars <- c("Spp")
# BiomassPerSpecies <- addNewData("C:/RProjects/ExtractIndicators/extra info/variableNameRecode2.csv", import.list, allowedVars)
# #write.csv(BiomassPerSpecies, "C:/RProjects/UseIndicators/data/SpeciesBiomass/BiomassPerSpp_q.csv", row.names = F)

#No q corrected esswss and nafo areas
# rm(list=ls(all=TRUE))
# setwd("C:/RProjects/UseIndicators")
# source('code/IndiFunctions.R')
# setwd("C://RProjects/UseIndicators/data/SpeciesBiomass/12-19/nonQadj/Output")
# filenames <- list.files(path = "C://RProjects/UseIndicators/data/SpeciesBiomass/12-19/nonQadj/Output", pattern = NULL, all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE)
# read_csv_filename <- function(filename){
#   ret <- read.csv(filename)
#   ret$Species <- filename #EDIT
#   ret
# }
# import.list <- ldply(filenames, read_csv_filename)
# import.list$X <- NULL
# str(import.list)
# allowedVars <- c("Spp")
# BiomassPerSpecies <- addNewData("C:/RProjects/ExtractIndicators/extra info/variableNameRecode2.csv", import.list, allowedVars)
# write.csv(BiomassPerSpecies, "C:/RProjects/UseIndicators/data/SpeciesBiomass/BiomassPerSpp_No_q.csv", row.names = F)



# -------- Step 3: Read biomass of all species from step 1 and 2*****  #  ####
                    # ******* WARNING******#
                    #******** USE BiomassPerSpp_q OR BiomassPerSpp_No_q****#
                    #******** that will affect the rest of the script as is the main file used! ****#

# q corrected **********************************************************************************************************  
#BiomassPerSpecies <- read.csv("C:/RProjects/UseIndicators/data/SpeciesBiomass/BiomassPerSpp_q.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
#  **********************************************************************************************************************

rm(list = ls())
#no q corerected **********************************************************************************************************  
BiomassPerSpecies <- read.csv("C:/RProjects/UseIndicators/data/SpeciesBiomass/BiomassPerSpp_No_q.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
#  **********************************************************************************************************************


# -------- Step 4: Read the indicator Biomass of the community *****  #  ####

#----- Read original indicators: Biomass of the community Q CORRECTED
Indi_q <- read.csv("C:/RProjects/UseIndicators/data/esswsssetq.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
esswss_melt_q <- melt(Indi_q, id=c('YEAR', 'ID'))
Biomass_esswss_q <- esswss_melt_q[esswss_melt_q$variable %in% c("Biomass"), ] 
Biomass_ess_q <- Biomass_esswss_q[Biomass_esswss_q$ID %in% c('ESS'), ]
Biomass_wss_q <- Biomass_esswss_q[Biomass_esswss_q$ID %in% c('WSS'), ]

#----- Read original indicators: Biomass of the community NO Q CORRECTED
Indi_noq <- read.csv("C:/RProjects/UseIndicators/data/esswssnonq.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
esswss_melt_noq <- melt(Indi_noq, id=c('YEAR', 'ID'))
Biomass_esswss_noq <- esswss_melt_noq[esswss_melt_noq$variable %in% c("Biomass"), ]
Biomass_ess_noq <- Biomass_esswss_noq[Biomass_esswss_noq$ID %in% c('ESS'), ]
Biomass_wss_noq <- Biomass_esswss_noq[Biomass_esswss_noq$ID %in% c('WSS'), ]


# -------- Step 4: Create and/or read new indicator: Biomass of the community without forage fish

#Part a) Sum of all forage species I extracted on December 19 2016 : "Herring(Atlantic)",  "Shad American",  "Alewife",  "Rainbow Smelt",  "Capelin",  "Argentine (Atl)",  "Northern Sand Lance", "Mackerel(Atlantic)".
# Biomass_Forage <- BiomassPerSpecies[BiomassPerSpecies$Spp %in% c("Herring(Atlantic)",
#                                                                  "Shad American",
#                                                                  "Alewife",
#                                                                  "Rainbow Smelt",
#                                                                  "Capelin",
#                                                                  "Argentine (Atl)",
#                                                                  "Northern Sand Lance",
#                                                                  "Mackerel(Atlantic)"), ]
# 
# Biomass_ess_Forage <- Biomass_Forage  %>% filter(ID %in% "ESS") %>% group_by(YEAR) %>%
#                       summarize(Biomass_Forage=sum(BIOMASS)) %>%  mutate(Biomass_Forage)
# 
# Biomass_ess_Forage$ID <- "ESS"

# write.csv(Biomass_ess_Forage,
#          file = "C:/RProjects/UseIndicators/data/SpeciesBiomass/BiomassCommunity_without forage fish/Biomass_ess_q_Forage.csv", row.names=FALSE)
#  write.csv(Biomass_ess_Forage,
#           file = "C:/RProjects/UseIndicators/data/SpeciesBiomass/BiomassCommunity_without forage fish/Biomass_ess_NOq_Forage.csv", row.names=FALSE)
# 
# Biomass_wss_Forage <- Biomass_Forage  %>% filter(ID %in% "WSS") %>% group_by(YEAR) %>%
#                       summarize(Biomass_Forage=sum(BIOMASS)) %>%  mutate(Biomass_Forage)
# Biomass_wss_Forage$ID <- "WSS"
#  
# # write.csv(Biomass_wss_Forage,
# #          file = "C:/RProjects/UseIndicators/data/SpeciesBiomass/BiomassCommunity_without forage fish/Biomass_wss_q_Forage.csv", row.names=FALSE)
# write.csv(Biomass_wss_Forage,
#          file = "C:/RProjects/UseIndicators/data/SpeciesBiomass/BiomassCommunity_without forage fish/Biomass_wss_NOq_Forage.csv", row.names=FALSE)

# Q
Biomass_ess_Forage_Q <- read.csv("C:/RProjects/UseIndicators/data/SpeciesBiomass/BiomassCommunity_without forage fish/Biomass_ess_q_Forage.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
Biomass_ess_Forage_Q <- melt(Biomass_ess_Forage_Q, id=c('YEAR', 'ID'))
Biomass_wss_Forage_Q <- read.csv("C:/RProjects/UseIndicators/data/SpeciesBiomass/BiomassCommunity_without forage fish/Biomass_wss_q_Forage.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
Biomass_wss_Forage_Q <- melt(Biomass_wss_Forage_Q, id=c('YEAR', 'ID'))

Biomass_NOForage_q_ess <- Biomass_ess_q$value - Biomass_ess_Forage_Q$value
Biomass_NOForage_q_wss <- Biomass_wss_q$value - Biomass_wss_Forage_Q$value

Biomass_minusForage_q_ess <- Biomass_ess_Forage_Q  %>% mutate(Biomass_NOForage_q_ess)
Biomass_minusForage_q_wss <- Biomass_wss_Forage_Q  %>% mutate(Biomass_NOForage_q_wss)

# NoQ
Biomass_ess_Forage_noQ <- read.csv("C:/RProjects/UseIndicators/data/SpeciesBiomass/BiomassCommunity_without forage fish/Biomass_ess_NOq_Forage.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
Biomass_ess_Forage_noQ <- melt(Biomass_ess_Forage_noQ, id=c('YEAR', 'ID'))
Biomass_wss_Forage_noQ <- read.csv("C:/RProjects/UseIndicators/data/SpeciesBiomass/BiomassCommunity_without forage fish/Biomass_wss_NOq_Forage.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
Biomass_wss_Forage_noQ <- melt(Biomass_wss_Forage_noQ, id=c('YEAR', 'ID'))

Biomass_NOForage_Noq_ess <- Biomass_ess_noq$value - Biomass_ess_Forage_noQ$value
Biomass_NOForage_Noq_wss <- Biomass_wss_noq$value - Biomass_wss_Forage_noQ$value

Biomass_minusForage_Noq_ess <- Biomass_ess_Forage_noQ  %>% mutate(Biomass_NOForage_Noq_ess)
Biomass_minusForage_Noq_wss <- Biomass_wss_Forage_noQ  %>% mutate(Biomass_NOForage_Noq_wss)


# -------- Step 5: Find top 10 species in biomass of the community  *****  #  ####

#******** USE BiomassPerSpp_q OR BiomassPerSpp_No_q****#
#******** that will affect the rest of the script as is the main file used! ****#
# q corrected **********************************************************************************************************  
#BiomassPerSpecies <- read.csv("C:/RProjects/UseIndicators/data/SpeciesBiomass/BiomassPerSpp_q.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
#  **********************************************************************************************************************
rm(list = ls())
#no q corerected **********************************************************************************************************  
#BiomassPerSpecies <- read.csv("C:/RProjects/UseIndicators/data/SpeciesBiomass/BiomassPerSpp_No_q.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)

#  **********************************************************************************************************************
BiomassPerSpecies$Spp <- factor(BiomassPerSpecies$Spp)
BiomassPerSpecies$Species <- NULL
names(BiomassPerSpecies)[names(BiomassPerSpecies) == 'Spp'] <- 'Species'

Biomass_TOP10 <-BiomassPerSpecies %>%
  filter(ID %in% "ESS") %>%
  group_by(Species) %>%
  summarize(mn=mean(BIOMASS)) %>%
  arrange(desc(mn)) %>%
  top_n(n = 10)  
Biomass_TOP10

#write.csv(Biomass_TOP10, file = "C:/RProjects/UseIndicators/data/SpeciesBiomass/BiomassWSS_q_TOP10.csv",row.names=FALSE)
Top10 <- as.character(Biomass_q_TOP10$Species) 
Top10 <- dQuote(Top10) 
toString(Top10)  # copy paste this putput to the melt functions below

# *---* ESS 
BTop10_ess <- BiomassPerSpecies[BiomassPerSpecies$ID %in% c('ESS'), ]
# ---- ESS q corrected (used in redundancy analysis)
#BTop10_ess <- BTop10_ess[BTop10_ess$Species %in% c('Herring(Atlantic)', 'Northern Sand Lance', 'Redfish Unseparated', 'American Plaice', 'Cod(Atlantic)', 'Mackerel(Atlantic)', 'Haddock', 'Silver Hake', 'Yellowtail Flounder', 'Capelin'), ]
#BTop5_ess <- BTop10_ess[BTop10_ess$Species %in% c('Herring(Atlantic)', 'Northern Sand Lance', 'Redfish Unseparated', 'American Plaice', 'Cod(Atlantic)'), ]

# ------ ESS noq (make sure you read different csv file!)
# BTop10_ess <- BTop10_ess[BTop10_ess$Species %in% c("Redfish Unseparated", "Cod(Atlantic)", "Haddock", "American Plaice", "Herring(Atlantic)", "Silver Hake", "Pollock", "Yellowtail Flounder", "Thorny Skate", "Short-Fin Squid"), ]
# BTop5_ess <- BTop10_ess[BTop10_ess$Species %in% c("Redfish Unseparated", "Cod(Atlantic)", "Haddock", "American Plaice", "Herring(Atlantic)"), ]

# *---* WSS 
BTop10_wss <- BiomassPerSpecies[BiomassPerSpecies$ID %in% c('WSS'), ]
#----- WSS q
# BTop10_wss <- BTop10_wss[BTop10_wss$Species %in% c('Herring(Atlantic)', 'Redfish Unseparated', 'Argentine (Atl)', 'Spiny Dogfish', 'Haddock', 'Silver Hake', 'Pollock', 'Alewife', 'Shad American', 'White Hake'), ]
# BTop5_wss <- BTop10_wss[BTop10_wss$Species %in% c('Herring(Atlantic)', 'Redfish Unseparated', 'Argentine (Atl)', 'Spiny Dogfish', 'Haddock'), ]
#------ WSS noq (make sure you read different csv file!)
# BTop10_wss <- BTop10_wss[BTop10_wss$Species %in% c("Herring(Atlantic)", "Spiny Dogfish", "Redfish Unseparated", "Haddock", "Pollock", "White Hake", "Cod(Atlantic)", "Spider/(Queen,Snow)Unid", "Silver Hake", "Short-Fin Squid"), ]
# BTop5_wss <- BTop10_wss[BTop10_wss$Species %in% c("Herring(Atlantic)", "Spiny Dogfish", "Redfish Unseparated", "Haddock", "Pollock"), ]


#----- Plot 

#  pdf("C:/RProjects/UseIndicators/output/figures/decoupled/TopSpp_BiomassCommunity.pdf", width=16,height=10)
# PlotDecoupledIndi_with_area(BTop10_ess, Biomass_ess)
# PlotDecoupledIndi_with_area(BTop5_ess, Biomass_ess)
# PlotDecoupledIndi_with_area(BTop10_wss, Biomass_wss)
# PlotDecoupledIndi_with_area(BTop5_wss, Biomass_wss)
# dev.off()

#Becareful here! hacve to read a different part of the code to produce nonq which is commented
# pdf("C:/RProjects/UseIndicators/output/figures/decoupled/TopSpp_BiomassCommunity_noQ.pdf", width=16,height=10)
# PlotDecoupledIndi_with_area(BTop10_ess, Biomass_ess_noq)
# PlotDecoupledIndi_with_area(BTop5_ess, Biomass_ess_noq)
# PlotDecoupledIndi_with_area(BTop10_wss, Biomass_wss_noq)
# PlotDecoupledIndi_with_area(BTop5_wss, Biomass_wss_noq)
# dev.off()


# ***************  Find top 10 spp based on biomass WITHOUT forage fish!
Biomass_NoForage <- BiomassPerSpecies[!BiomassPerSpecies$Species %in% c("Herring(Atlantic)",
                                                                 "Shad American",
                                                                 "Alewife",
                                                                 "Rainbow Smelt",
                                                                 "Capelin",
                                                                 "Argentine (Atl)",
                                                                 "Northern Sand Lance",
                                                                 "Mackerel(Atlantic)"), ]
Biomass_NoForage_TOP10 <- Biomass_NoForage %>%
                          filter(ID %in% "WSS") %>%
                          group_by(Species) %>%
                          summarize(mn=mean(BIOMASS)) %>%
                          arrange(desc(mn)) %>%
                          top_n(n = 15)  
Biomass_NoForage_TOP10
#write.csv(Biomass_q_TOP10, file = "C:/RProjects/UseIndicators/data/SpeciesBiomass/BiomassWSS_q_TOP10_NoForage.csv",row.names=FALSE)
Top10 <- as.character(Biomass_NoForage_TOP10$Species) 
Top10 <- dQuote(Top10) 
toString(Top10)  # copy paste this putput to the melt functions below
 
# *- - -* ESS 
BTop10_ess_noForage <- Biomass_NoForage[Biomass_NoForage$ID %in% c('ESS'), ]
# ------ q NO FORAGE FISH! (make sure you read different csv file! and filters)
# BTop15_ess_noForage <- BTop10_ess_noForage[BTop10_ess_noForage$Species %in% c("Redfish Unseparated", "American Plaice", "Cod(Atlantic)", "Haddock", "Silver Hake", "Yellowtail Flounder", "Pollock", "Thorny Skate", "White Hake", "Witch Flounder", "Short-Fin Squid", "Pandalus Borealis", "Sea Cucumbers", "Longhorn Sculpin", "Spiny Dogfish"), ]
# BTop10_ess_noForage <- BTop10_ess_noForage[BTop10_ess_noForage$Species %in% c("Redfish Unseparated", "American Plaice", "Cod(Atlantic)", "Haddock", "Silver Hake", "Yellowtail Flounder", "Pollock", "Thorny Skate", "White Hake", "Witch Flounder"), ]
# BTop5_ess_noForage <- BTop10_ess_noForage[BTop10_ess_noForage$Species %in% c("Redfish Unseparated", "American Plaice", "Cod(Atlantic)", "Haddock", "Silver Hake"), ]
#------ NO q NO FORAGE FISH! (make sure you read different csv file! and filters)
BTop15_ess_noForage <- BTop10_ess_noForage[BTop10_ess_noForage$Species %in% c("Redfish Unseparated", "Cod(Atlantic)", "Haddock", "American Plaice", "Silver Hake", "Pollock", "Yellowtail Flounder", "Thorny Skate", "Short-Fin Squid", "White Hake", "Pandalus Borealis", "Sea Cucumbers", "Spiny Dogfish", "Cucumaria Frondosa", "Witch Flounder"), ] 
BTop10_ess_noForage <- BTop10_ess_noForage[BTop10_ess_noForage$Species %in% c("Redfish Unseparated", "Cod(Atlantic)", "Haddock", "American Plaice", "Silver Hake", "Pollock", "Yellowtail Flounder", "Thorny Skate", "Short-Fin Squid", "White Hake"), ]
BTop5_ess_noForage <- BTop10_ess_noForage[BTop10_ess_noForage$Species %in% c("Redfish Unseparated", "Cod(Atlantic)", "Haddock", "American Plaice", "Silver Hake"), ]

# *- - -* WSS 
BTop10_wss_noForage <- Biomass_NoForage[Biomass_NoForage$ID %in% c('WSS'), ]
# ----- q NO FORAGE FISH! (make sure you read different csv file! and filters) 
# BTop15_wss_noForage <- BTop10_wss_noForage[BTop10_wss_noForage$Species %in% c("Redfish Unseparated", "Spiny Dogfish", "Haddock", "Silver Hake", "Pollock", "White Hake", "Cod(Atlantic)", "Spider/(Queen,Snow)Unid", "Winter Flounder", "American Plaice", "Short-Fin Squid", "Barndoor Skate", "Thorny Skate", "Longhorn Sculpin", "American Lobster"), ]
# BTop10_wss_noForage <- BTop10_wss_noForage[BTop10_wss_noForage$Species %in% c("Redfish Unseparated", "Spiny Dogfish", "Haddock", "Silver Hake", "Pollock", "White Hake", "Cod(Atlantic)", "Spider/(Queen,Snow)Unid", "Winter Flounder", "American Plaice"), ]
# BTop5_wss_noForage <- BTop10_wss_noForage[BTop10_wss_noForage$Species %in% c("Redfish Unseparated", "Spiny Dogfish", "Haddock", "Silver Hake", "Pollock"), ]
# ------ NO q NO FORAGE FISH! (make sure you read different csv file! and filters) 
BTop15_wss_noForage <- BTop10_wss_noForage[BTop10_wss_noForage$Species %in% c('Spiny Dogfish', 'Redfish Unseparated', 'Haddock', 'Pollock', 'White Hake', 'Cod(Atlantic)', 'Spider/(Queen,Snow)Unid', 'Silver Hake', 'Short-Fin Squid', 'American Lobster', 'Thorny Skate', 'Winter Flounder', 'Cusk', 'Monkfish,Goosefish,Angler', 'American Plaice'), ] 
BTop10_wss_noForage <- BTop10_wss_noForage[BTop10_wss_noForage$Species %in% c('Spiny Dogfish', 'Redfish Unseparated', 'Haddock', 'Pollock', 'White Hake', 'Cod(Atlantic)', 'Spider/(Queen,Snow)Unid', 'Silver Hake', 'Short-Fin Squid', 'American Lobster'), ]
BTop5_wss_noForage <- BTop10_wss_noForage[BTop10_wss_noForage$Species %in% c('Spiny Dogfish', 'Redfish Unseparated', 'Haddock', 'Pollock', 'White Hake'), ]


 #Becareful here! have to read a different part of the code to produce nonq which is commented
head(Biomass_ess_q)
head(Biomass_minusForage_q_ess)
names(Biomass_minusForage_q_ess)[names(Biomass_minusForage_q_ess) == 'Biomass_NOForage_q_ess'] <- 'Biomass_minusForage'
names(Biomass_minusForage_q_wss)[names(Biomass_minusForage_q_wss) == 'Biomass_NOForage_q_wss'] <- 'Biomass_minusForage'

head(Biomass_ess_noq)
head(Biomass_minusForage_Noq_ess)
names(Biomass_minusForage_Noq_ess)[names(Biomass_minusForage_Noq_ess) == 'Biomass_NOForage_Noq_ess'] <- 'Biomass_minusForage'
names(Biomass_minusForage_Noq_wss)[names(Biomass_minusForage_Noq_wss) == 'Biomass_NOForage_Noq_wss'] <- 'Biomass_minusForage'


# pdf("C:/RProjects/UseIndicators/output/figures/decoupled/TopSpp_BiomassCommunity_q_NoForageFish.pdf", width=16,height=10)
# PlotDecoupledIndi_with_area2(BTop15_ess_noForage, Biomass_minusForage_q_ess)
# PlotDecoupledIndi_with_area2(BTop10_ess_noForage, Biomass_minusForage_q_ess)
# PlotDecoupledIndi_with_area2(BTop5_ess_noForage, Biomass_minusForage_q_ess)
# PlotDecoupledIndi_with_area2(BTop15_wss_noForage, Biomass_minusForage_q_wss)
# PlotDecoupledIndi_with_area2(BTop10_wss_noForage, Biomass_minusForage_q_wss)
# PlotDecoupledIndi_with_area2(BTop5_wss_noForage, Biomass_minusForage_q_wss)
# dev.off()

#Becareful here! have to read a different part of the code to produce nonq which is commented
pdf("C:/RProjects/UseIndicators/output/figures/decoupled/TopSpp_BiomassCommunity_No_q_NoForageFish.pdf", width=16,height=10)
 PlotDecoupledIndi_with_area2(BTop15_ess_noForage, Biomass_minusForage_Noq_ess)
 PlotDecoupledIndi_with_area2(BTop10_ess_noForage, Biomass_minusForage_Noq_ess)
 PlotDecoupledIndi_with_area2(BTop5_ess_noForage, Biomass_minusForage_Noq_ess)
 PlotDecoupledIndi_with_area2(BTop15_wss_noForage, Biomass_minusForage_Noq_wss)
 PlotDecoupledIndi_with_area2(BTop10_wss_noForage, Biomass_minusForage_Noq_wss)
 PlotDecoupledIndi_with_area2(BTop5_wss_noForage, Biomass_minusForage_Noq_wss)
dev.off()




# The following constructs a data.frame  (that is then used to construct a plot) that contains the mean, 
# sd, se, and approximate 95% confidence interval for biomass of the community by year.  
BiomassAll <- BiomassPerSpecies %>%
  filter(ID %in% "ESS") %>%
  group_by(YEAR) %>%
  summarize(n=n(),mn=mean(BIOMASS),sd=sd(BIOMASS)) %>%
  mutate(se=sd/sqrt(n),LCI=mn+qnorm(0.025)*se,UCI=mn+qnorm(0.975)*se)
BiomassAll
with(BiomassAll,plotCI(YEAR,mn,ui=UCI,li=LCI,pch=16,ylab="Biomass ESS",xlab="Year"))

# -------- Step 4: Decouple and plot Biomass per TL 2, 3 and 4 *****  #  ####

#Find top 10 spp based on biomass 





# --------Step 4. Read and decouple biomass indicators per functional group

setwd("C:/RProjects/ExtractIndicators/output/Estimated Indicators/12-07/Qadj/stratifiedLevel")

BCod <- read.csv("resourcePotentialesswss10BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BCod$Species <- "Cod"

Haddock <- read.csv("resourcePotentialesswss11BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
Haddock$Species <- "Haddock"

WhiteHake <- read.csv("resourcePotentialesswss12BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
WhiteHake$Species <- "WhiteHake"

RedHake <- read.csv("resourcePotentialesswss13BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
RedHake$Species <- "RedHake/Squirrel"

SilverHake <- read.csv("resourcePotentialesswss14BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
SilverHake$Species <- "SilverHake"


BCusk <- read.csv("resourcePotentialesswss15BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BCusk$Species <- "Cusk"

BPollock <- read.csv("resourcePotentialesswss16BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BPollock$Species <- "Pollock"

Tomcod <- read.csv("resourcePotentialesswss17BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
Tomcod$Species <- "Tomcod"

Hake <- read.csv("resourcePotentialesswss18BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
Hake$Species <- "Hake"

OffshoreHake <- read.csv("resourcePotentialesswss19BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
OffshoreHake$Species <- "OffshoreHake"

RedFishUnseparated <- read.csv("resourcePotentialesswss23BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
RedFishUnseparated$Species <- "RedFishUnseparated"

BHalibut <- read.csv("resourcePotentialesswss30BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BHalibut$Species <- "Halibut"

Turbot_GreenlandHalibut <- read.csv("resourcePotentialesswss31BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
Turbot_GreenlandHalibut$Species <- "Turbot/GreenlandHalibut"

#
AmericanPlaice  <- read.csv("resourcePotentialesswss40BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
AmericanPlaice$Species <- "AmericanPlaice"

WitchFlounder   <- read.csv("resourcePotentialesswss41BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
WitchFlounder$Species <- "WitchFlounder"

YellowtailFlounder <- read.csv("resourcePotentialesswss42BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
YellowtailFlounder$Species <- "YellowtailFlounder"  

WinterFlounder <- read.csv("resourcePotentialesswss43BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
WinterFlounder$Species <- "WinterFlounder"

GulfStreamFlounder <- read.csv("resourcePotentialesswss44BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
GulfStreamFlounder$Species <- "GulfStreamFlounder"

EyedFlounder <- read.csv("resourcePotentialesswss45BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
EyedFlounder$Species <- "EyedFlounder"

FlounderUnidentified <- read.csv("resourcePotentialesswss49BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
FlounderUnidentified$Species <- "FlounderUnidentified"

StripedAtlanticWolffish <- read.csv("resourcePotentialesswss50BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
StripedAtlanticWolffish$Species <- "StripedAtlanticWolffish"

#
BHerring <- read.csv("resourcePotentialesswss60BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BHerring$Species <- "Herring"

BShad <- read.csv("resourcePotentialesswss61BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BShad$Species <- "Shad"

BAlewife <- read.csv("resourcePotentialesswss62BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BAlewife$Species <- "Alewife"

BSmelt <- read.csv("resourcePotentialesswss63BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BSmelt$Species <- "Smelt"

BCapelin <- read.csv("resourcePotentialesswss64BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BCapelin$Species <- "Capelin"

BMackerel <- read.csv("resourcePotentialesswss70BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BMackerel$Species <- "Mackerel"

ArcticCod <- read.csv("resourcePotentialesswss110BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
ArcticCod$Species <- "ArcticCod"

SpottedHake <- read.csv("resourcePotentialesswss111BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
SpottedHake$Species <- "SpottedHake"

LongfinHake <- read.csv("resourcePotentialesswss112BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
LongfinHake$Species <- "LongfinHake"

BlueAntimoraHake <- read.csv("resourcePotentialesswss113BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BlueAntimoraHake$Species <- "BlueAntimoraHake"

FourbeardRockling <- read.csv("resourcePotentialesswss114BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
FourbeardRockling$Species <- "FourbeardRockling"

ThreebeardRockling <- read.csv("resourcePotentialesswss115BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
ThreebeardRockling$Species <- "ThreebeardRockling"

SilverRockling <- read.csv("resourcePotentialesswss116BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
SilverRockling$Species <- "SilverRockling"

BlueWhiting <- read.csv("resourcePotentialesswss117BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BlueWhiting$Species <- "BlueWhiting"

GreenlandCod <- read.csv("resourcePotentialesswss118BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
GreenlandCod$Species <- "GreenlandCod"

LongfinHake <- read.csv("resourcePotentialesswss112BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
LongfinHake$Species <- "LongfinHake"

#
SmoothFlounder <- read.csv("resourcePotentialesswss140BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
SmoothFlounder$Species <- "SmoothFlounder"

SummerFlounder <- read.csv("resourcePotentialesswss141BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
SummerFlounder$Species <- "SummerFlounder"

FourspotFlounder <- read.csv("resourcePotentialesswss142BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
FourspotFlounder$Species <- "FourspotFlounder"

BrillWindowpane <- read.csv("resourcePotentialesswss143BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BrillWindowpane$Species <- "Brill/Windowpane"

BArgentine <- read.csv("resourcePotentialesswss160BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BArgentine$Species <- "Argentine"

#
BarndoorSkate <- read.csv("resourcePotentialesswss200BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BarndoorSkate$Species<- "BarndoorSkate"

ThornySkate <- read.csv("resourcePotentialesswss201BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
ThornySkate$Species <- "ThornySkate"

SmoothSkate <- read.csv("resourcePotentialesswss202BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
SmoothSkate$Species <- "SmoothSkate"

LittleSkate <- read.csv("resourcePotentialesswss203BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
LittleSkate$Species <- "LittleSkate"

WinterSkate <- read.csv("resourcePotentialesswss204BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
WinterSkate$Species <- "WinterSkate"

SpinytailSkate <- read.csv("resourcePotentialesswss205BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
SpinytailSkate$Species <- "SpinytailSkate"

BrierSkate <- read.csv("resourcePotentialesswss206BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BrierSkate$Species <- "BrierSkate"

RoundSkate <- read.csv("resourcePotentialesswss207BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
RoundSkate$Species <- "RoundSkate"

SoftSkate <- read.csv("resourcePotentialesswss208BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
SoftSkate$Species <- "SoftSkate"

ShorttailSkate <- read.csv("resourcePotentialesswss209BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
ShorttailSkate$Species <- "ShorttailSkate"

ArcticSkate <- read.csv("resourcePotentialesswss210BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
ArcticSkate$Species <- "ArcticSkate"

Skates <- read.csv("resourcePotentialesswss211BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
Skates$Species <- "Skates"

#
SpinyDogfish <- read.csv("resourcePotentialesswss220BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
SpinyDogfish$Species <- "SpinyDogfish"

LonghornSculpin <- read.csv("resourcePotentialesswss300BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
LonghornSculpin$Species <- "LonghornSculpin"

ShorthornSculpin <- read.csv("resourcePotentialesswss301BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
ShorthornSculpin$Species <- "ShorthornSculpin"

MailedSculpin <- read.csv("resourcePotentialesswss304BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
MailedSculpin$Species <- "MailedSculpin"

SeaRaven <- read.csv("resourcePotentialesswss320BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
SeaRaven$Species <- "SeaRaven"

RockGrenadier <- read.csv("resourcePotentialesswss414BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
RockGrenadier$Species <- "RockGrenadier"  

#
BSandLance <- read.csv("resourcePotentialesswss610BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BSandLance$Species <- "SandLance"

Butterfish <- read.csv("resourcePotentialesswss701BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
Butterfish$Species <- "Butterfish"

Monkfish_Goosefish_Angler <- read.csv("resourcePotentialesswss400BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
Monkfish_Goosefish_Angler$Species <- "Monkfish/Goosefish/Angler"

RosefishBlackBelly <- read.csv("resourcePotentialesswss123BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
RosefishBlackBelly$Species <- "RosefishBlackBelly" 

Lumpfish <- read.csv("resourcePotentialesswss501BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
Lumpfish$Species <- "Lumpfish"

Seasnail <- read.csv("resourcePotentialesswss505BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
Seasnail$Species <- "Seasnail"

NorthernHagfish <- read.csv("resourcePotentialesswss241BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
NorthernHagfish$Species <- "NorthernHagfish"

OceanPout <- read.csv("resourcePotentialesswss640BIOMASS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
OceanPout$Species <- "OceanPout"


# F U N C T I O N A L   G R O U P S 
BMediumBenthivore_spp <- rbind(Haddock, AmericanPlaice, WitchFlounder, YellowtailFlounder, WinterFlounder, RosefishBlackBelly, FourspotFlounder,
                               BrillWindowpane, SmoothSkate, LittleSkate, ShorthornSculpin, MailedSculpin, RockGrenadier, 
                               Lumpfish, Seasnail, FourbeardRockling, ThreebeardRockling, NorthernHagfish, OceanPout)
BMediumBenthivore_spp$X <- NULL
write.csv(BMediumBenthivore_spp, "C:/RProjects/UseIndicators/data/decoupled q esswss/BMediumBenthivore_spp.csv", row.names = F)


BFlatfish_spp <- rbind(BHalibut, Turbot_GreenlandHalibut, AmericanPlaice, WitchFlounder, YellowtailFlounder, WinterFlounder,
                       GulfStreamFlounder, EyedFlounder, FlounderUnidentified, SmoothFlounder, SummerFlounder, FourspotFlounder, BrillWindowpane)
BFlatfish_spp$X <- NULL
write.csv(BFlatfish_spp, "C:/RProjects/UseIndicators/data/decoupled q esswss/BFlatfish_spp.csv", row.names = F)


BSkates_spp <- rbind(BarndoorSkate, ThornySkate, SmoothSkate, LittleSkate, WinterSkate, SpinytailSkate, BrierSkate, RoundSkate, SoftSkate, ShorttailSkate,
                     ArcticSkate, Skates)
BSkates_spp$X <- NULL
write.csv(BSkates_spp, "C:/RProjects/UseIndicators/data/decoupled q esswss/BSkates_spp.csv", row.names = F)

BLargeBenthivore_spp <- rbind(BarndoorSkate, StripedAtlanticWolffish)
BLargeBenthivore_spp$X <- NULL
write.csv(BLargeBenthivore_spp, "C:/RProjects/UseIndicators/data/decoupled q esswss/BLargeBenthivore_spp.csv", row.names = F)

BPlanktivores_spp <- rbind(BHerring, BShad, BAlewife, BMackerel, BArgentine, BSandLance, BCapelin, Butterfish)
BPlanktivores_spp$X <- NULL
write.csv(BPlanktivores_spp, "C:/RProjects/UseIndicators/data/decoupled q esswss/BPlanktivores_spp.csv", row.names = F)

BClupeids_spp <- rbind(BHerring, BShad, BAlewife, BSmelt)
BClupeids_spp$X <- NULL
write.csv(BClupeids_spp, "C:/RProjects/UseIndicators/data/decoupled q esswss/BClupeids_spp.csv", row.names = F)

BForage_spp <- rbind(BHerring, BShad, BAlewife, BSmelt, BCapelin, BSandLance, BArgentine)
BForage_spp$X <- NULL
write.csv(BForage_spp, "C:/RProjects/UseIndicators/data/decoupled q esswss/BForage_spp.csv", row.names = F)

BPiscivores_spp <- rbind(BCod, WhiteHake, BCusk, BPollock, BHalibut, Turbot_GreenlandHalibut, LongfinHake, 
                         ThornySkate, WinterSkate, SpinyDogfish, LonghornSculpin, SeaRaven, Monkfish_Goosefish_Angler)
BPiscivores_spp$X <- NULL
write.csv(BPiscivores_spp, "C:/RProjects/UseIndicators/data/decoupled q esswss/BPiscivores_spp.csv", row.names = F)

BGadoids_spp <- rbind(BCod, Haddock, WhiteHake, RedHake, SilverHake, BCusk, BPollock, Tomcod, Hake, OffshoreHake, ArcticCod, SpottedHake, 
                      LongfinHake, BlueAntimoraHake, FourbeardRockling, ThreebeardRockling, SilverRockling, BlueWhiting, GreenlandCod)
BGadoids_spp$X <- NULL
write.csv(BGadoids_spp, "C:/RProjects/UseIndicators/data/decoupled q esswss/BGadoids_spp.csv", row.names = F)

BZoopiscivores_spp <- rbind(RedHake, SilverHake, OffshoreHake, RedFishUnseparated)
BZoopiscivores_spp$X <- NULL
write.csv(BZoopiscivores_spp, "C:/RProjects/UseIndicators/data/decoupled q esswss/BZoopiscivores_spp.csv", row.names = F)

#indicatorSpaceTime(path=path,indicator='speciesRichness',ags=c('ALL','BIOMASS'),groups='esswss',qadjusted=T)

#Read all csv files extracted in this section:
# Indidir <- "C:/RProjects/ExtractIndicators/output/Estimated Indicators/12-06/Qadj/stratifiedLevel/"
# Indifiles <- list.files(path = paste(Indidir, sep=""), 
#                        pattern = "\\.csv$", full.names = F)


# -------- Step 2 - old version: Plot decoupled indicators  *****  #  ####
BSkates_spp <- read.csv("C:/RProjects/UseIndicators/data/decoupled q esswss/BSkates_spp.csv",header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BSkates <- esswss_melt[esswss_melt$variable %in% c("BiomassSkates"), ]

BClupeids_spp <- read.csv("C:/RProjects/UseIndicators/data/decoupled q esswss/BClupeids_spp.csv",header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BClupeids <- esswss_melt[esswss_melt$variable %in% c("BiomassClupeids"), ]

BForage_spp <- read.csv("C:/RProjects/UseIndicators/data/decoupled q esswss/BForage_spp.csv",header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BForage <- esswss_melt[esswss_melt$variable %in% c("BiomassForage"), ]

BPlanktivores_spp <- read.csv("C:/RProjects/UseIndicators/data/decoupled q esswss/BPlanktivores_spp.csv",header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BPlanktivores <- esswss_melt[esswss_melt$variable %in% c("BTGPlanktivore"), ]

BPiscivores_spp <- read.csv("C:/RProjects/UseIndicators/data/decoupled q esswss/BPiscivores_spp.csv",header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BPiscivores <- esswss_melt[esswss_melt$variable %in% c("BTGPiscivore"), ]

BGadoids_spp <- read.csv("C:/RProjects/UseIndicators/data/decoupled q esswss/BGadoids_spp.csv",header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BGadoids <- esswss_melt[esswss_melt$variable %in% c("BiomassGadoids"), ]

BFlatfish_spp <- read.csv("C:/RProjects/UseIndicators/data/decoupled q esswss/BFlatfish_spp.csv",header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BFlatfish <- esswss_melt[esswss_melt$variable %in% c("BiomassFlatfish"), ]

BZoopiscivores_spp <- read.csv("C:/RProjects/UseIndicators/data/decoupled q esswss/BZoopiscivores_spp.csv",header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BZoopiscivores <- esswss_melt[esswss_melt$variable %in% c("BTGZoopiscivore"), ]

BLargeBenthivore_spp <- read.csv("C:/RProjects/UseIndicators/data/decoupled q esswss/BLargeBenthivore_spp.csv",header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BLargeBenthivore <- esswss_melt[esswss_melt$variable %in% c("BTGLargeBenthivore"), ]

BMediumBenthivore_spp <- read.csv("C:/RProjects/UseIndicators/data/decoupled q esswss/BMediumBenthivore_spp.csv",header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BMediumBenthivore <- esswss_melt[esswss_melt$variable %in% c("BTGMediumBenthivore"), ]

pdf("C:/RProjects/UseIndicators/output/figures/decoupled/decoupledIndi.pdf", width=14,height=8)
# PlotIndi_with_line(BClupeids)
# PlotDecoupledIndi(BClupeids_spp)
PlotDecoupledIndi_with_area(BClupeids_spp, BClupeids)
PlotDecoupledIndi_with_area(BForage_spp, BForage)
PlotDecoupledIndi_with_area(BPlanktivores_spp, BPlanktivores)
PlotDecoupledIndi_with_area(BPiscivores_spp, BPiscivores)
PlotDecoupledIndi_with_area(BGadoids_spp, BGadoids)
PlotDecoupledIndi_with_area(BSkates_spp, BSkates)
PlotDecoupledIndi_with_area(BLargeBenthivore_spp, BLargeBenthivore)
PlotDecoupledIndi_with_area(BMediumBenthivore_spp, BMediumBenthivore)
PlotDecoupledIndi_with_area(BFlatfish_spp, BFlatfish)
PlotDecoupledIndi_with_area(BZoopiscivores_spp, BZoopiscivores)
dev.off()


