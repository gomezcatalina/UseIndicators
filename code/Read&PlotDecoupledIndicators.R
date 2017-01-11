rm(list = ls())
# Read and Plot decoupled Indicators
source('C:/RProjects/UseIndicators/code/IndiFunctions.R')    
library(plyr)
library(reshape)
library(dplyr)
library(FSAdata)
library(plotrix)
library(ggplot2)
library(gridExtra)
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
# 
# import.list <- ldply(filenames, read_csv_filename)
# import.list$X <- NULL
# str(import.list)
# allowedVars <- c("Spp")
# BiomassPerSpecies <- addNewData("C:/RProjects/ExtractIndicators/extra info/variableNameRecode2.csv", import.list, allowedVars)
#write.csv(BiomassPerSpecies, "C:/RProjects/UseIndicators/data/SpeciesBiomass/BiomassPerSpp_q.csv", row.names = F)

## The following steps separate datframe so species code numbers are in a single column
# library(dplyr)
# library(tidyr)
# BiomassPerSpecies_esswss <- BiomassPerSpecies[BiomassPerSpecies$ID %in% c('ESS', 'WSS'), ]
# a <- BiomassPerSpecies_esswss %>%
#   separate(Species, into = c("a", "Species"), sep = "esswss")
# a$a <- NULL
# a <- a %>%
#   separate(Species, into = c("Species", "b"), sep = "BIOMASS")
# a$b <- NULL
# BiomassPerSpecies_nafo <- BiomassPerSpecies[BiomassPerSpecies$ID %in% c('4VN', '4VS', '4X', '4W'), ]
# c <- BiomassPerSpecies_nafo %>%
#   separate(Species, into = c("a", "Species"), sep = "nafo")
# c$a <- NULL
# c <- c %>%
#   separate(Species, into = c("Species", "b"), sep = "BIOMASS")
# c$b <- NULL
# BiomassPerSpecies_q <- rbind(a, c)
# names(BiomassPerSpecies_q)[names(BiomassPerSpecies_q) == 'Species'] <- 'SppCodes'
# BiomassPerSpecies_q$Species <- as.numeric(BiomassPerSpecies_q$Species)
# str(BiomassPerSpecies_q)
# write.csv(BiomassPerSpecies_q, "C:/RProjects/UseIndicators/data/SpeciesBiomass/BiomassPerSpp_q_coded.csv", row.names = F)


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



# -------- Step 3: Read biomass of all species from step 1 and 2 q corrected *****  #  ####
                    # ******* WARNING******#
                    #******** USE BiomassPerSpp_q here instead of BiomassPerSpp_No_q****#
                
#rm(list = ls())
BiomassPerSpecies <- read.csv("C:/RProjects/UseIndicators/data/SpeciesBiomass/BiomassPerSpp_q.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BiomassPerSpecies$Spp <- factor(BiomassPerSpecies$Spp)
BiomassPerSpecies$Species <- NULL
names(BiomassPerSpecies)[names(BiomassPerSpecies) == 'Spp'] <- 'Species'


# -------- Step 4. Decouple Biomass of the community q corrected  *****  #  ####
Indi_q <- read.csv("C:/RProjects/UseIndicators/data/esswsssetq.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
esswss_melt_q <- melt(Indi_q, id=c('YEAR', 'ID'))
Biomass_esswss_q <- esswss_melt_q[esswss_melt_q$variable %in% c("Biomass"), ]
Biomass_ess_q <- Biomass_esswss_q[Biomass_esswss_q$ID %in% c('ESS'), ]
Biomass_wss_q <- Biomass_esswss_q[Biomass_esswss_q$ID %in% c('WSS'), ]

Biomass_TOP10 <-BiomassPerSpecies %>%
  filter(ID %in% "WSS") %>%
  group_by(Species) %>%
  summarize(mn=mean(BIOMASS)) %>%
  arrange(desc(mn)) %>%
  top_n(n = 10)  
Biomass_TOP10

#write.csv(Biomass_TOP10, file = "C:/RProjects/UseIndicators/data/SpeciesBiomass/BiomassWSS_q_TOP10.csv",row.names=FALSE)
Top10 <- as.character(Biomass_TOP10$Species) 
Top10 <- dQuote(Top10) 
toString(Top10)  # copy paste this putput to the melt functions below

# *---* ESS 
BTop10_ess <- BiomassPerSpecies[BiomassPerSpecies$ID %in% c('ESS'), ]
BTop10_ess <- BTop10_ess[BTop10_ess$Species %in% c('Herring(Atlantic)', 'Northern Sand Lance', 'Redfish Unseparated', 'American Plaice', 'Cod(Atlantic)', 'Mackerel(Atlantic)', 'Haddock', 'Silver Hake', 'Yellowtail Flounder', 'Capelin'), ]
BTop10_ess$Species <- factor(BTop10_ess$Species, levels=c('Herring(Atlantic)', 'Northern Sand Lance', 'Redfish Unseparated', 'American Plaice', 'Cod(Atlantic)', 'Mackerel(Atlantic)', 'Haddock', 'Silver Hake', 'Yellowtail Flounder', 'Capelin'))
BTop5_ess <- BTop10_ess[BTop10_ess$Species %in% c('Herring(Atlantic)', 'Northern Sand Lance', 'Redfish Unseparated', 'American Plaice', 'Cod(Atlantic)'), ]

# *---* WSS 
BTop10_wss <- BiomassPerSpecies[BiomassPerSpecies$ID %in% c('WSS'), ]
BTop10_wss <- BTop10_wss[BTop10_wss$Species %in% c('Herring(Atlantic)', 'Redfish Unseparated', 'Argentine (Atl)', 'Spiny Dogfish', 'Haddock', 'Silver Hake', 'Pollock', 'Alewife', 'Shad American', 'White Hake'), ]
BTop10_wss$Species <- factor(BTop10_wss$Species, levels=c('Herring(Atlantic)', 'Redfish Unseparated', 'Argentine (Atl)', 'Spiny Dogfish', 'Haddock', 'Silver Hake', 'Pollock', 'Alewife', 'Shad American', 'White Hake'))
BTop5_wss <- BTop10_wss[BTop10_wss$Species %in% c('Herring(Atlantic)', 'Redfish Unseparated', 'Argentine (Atl)', 'Spiny Dogfish', 'Haddock'), ]

#----- Plot 
pdf("C:/RProjects/UseIndicators/output/figures/decoupled/TopSpp_BiomassCommunity_q.pdf", width=16,height=10)
grid.arrange(PlotDecoupledIndi_with_area(BTop5_ess, Biomass_ess_q), PlotDecoupledIndi_with_area(BTop5_wss, Biomass_wss_q)) 
grid.arrange(PlotDecoupledIndi_with_area(BTop10_ess, Biomass_ess_q), PlotDecoupledIndi_with_area(BTop10_wss, Biomass_wss_q)) 
dev.off()



# -------- Step 5. Decouple Biomass of the community without forage fish q corrected  *****  #  ####

# *********** 1) Create and/or read new indicator: Biomass of the community without forage fish *****

# Part a) Sum of all forage species I extracted on December 19 2016 : "Herring(Atlantic)",  "Shad American",  "Alewife",  "Rainbow Smelt",  "Capelin",  "Argentine (Atl)",  "Northern Sand Lance", "Mackerel(Atlantic)".
# 
#Biomass_Forage <- BiomassPerSpecies[BiomassPerSpecies$Spp %in% c("Herring(Atlantic)",
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

# # Q
Biomass_ess_Forage_Q <- read.csv("C:/RProjects/UseIndicators/data/SpeciesBiomass/BiomassCommunity_without forage fish/Biomass_ess_q_Forage.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
Biomass_ess_Forage_Q <- melt(Biomass_ess_Forage_Q, id=c('YEAR', 'ID'))
Biomass_wss_Forage_Q <- read.csv("C:/RProjects/UseIndicators/data/SpeciesBiomass/BiomassCommunity_without forage fish/Biomass_wss_q_Forage.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
Biomass_wss_Forage_Q <- melt(Biomass_wss_Forage_Q, id=c('YEAR', 'ID'))
# 
Biomass_NOForage_q_ess <- Biomass_ess_q$value - Biomass_ess_Forage_Q$value
Biomass_NOForage_q_wss <- Biomass_wss_q$value - Biomass_wss_Forage_Q$value
# 
Biomass_minusForage_q_ess <- Biomass_ess_Forage_Q  %>% mutate(Biomass_NOForage_q_ess)
Biomass_minusForage_q_wss <- Biomass_wss_Forage_Q  %>% mutate(Biomass_NOForage_q_wss)


# *********** 2) Decouple *****
Biomass_NoForage <- BiomassPerSpecies[!BiomassPerSpecies$Species %in% c("Herring(Atlantic)",
                                                                 "Shad American",
                                                                 "Alewife",
                                                                 "Rainbow Smelt",
                                                                 "Capelin",
                                                                 "Argentine (Atl)",
                                                                 "Northern Sand Lance",
                                                                 "Mackerel(Atlantic)"), ]

Biomass_NoForage_TOP10 <- Biomass_NoForage %>%
                          filter(ID %in% "ESS") %>%
                          group_by(Species) %>%
                          summarize(mn=mean(BIOMASS)) %>%
                          arrange(desc(mn)) %>%
                          top_n(n = 10)  
Biomass_NoForage_TOP10
#write.csv(Biomass_q_TOP10, file = "C:/RProjects/UseIndicators/data/SpeciesBiomass/BiomassWSS_q_TOP10_NoForage.csv",row.names=FALSE)
Top10 <- as.character(Biomass_NoForage_TOP10$Species) 
Top10 <- dQuote(Top10) 
toString(Top10)  # copy paste this putput to the melt functions below
 
# *- - -* ESS 
BTop10_ess_noForage <- Biomass_NoForage[Biomass_NoForage$ID %in% c('ESS'), ]
head(BTop10_ess_noForage)
BTop10_ess_noForage <- BTop10_ess_noForage[BTop10_ess_noForage$Species %in% c("Redfish Unseparated", "American Plaice", "Cod(Atlantic)", "Haddock", "Silver Hake", "Yellowtail Flounder", "Pollock", "Thorny Skate", "White Hake", "Witch Flounder"), ]
BTop10_ess_noForage$Species <- factor(BTop10_ess_noForage$Species, levels=c("Redfish Unseparated", "American Plaice", "Cod(Atlantic)", "Haddock", "Silver Hake", "Yellowtail Flounder", "Pollock", "Thorny Skate", "White Hake", "Witch Flounder"))
BTop5_ess_noForage <- BTop10_ess_noForage[BTop10_ess_noForage$Species %in% c("Redfish Unseparated", "American Plaice", "Cod(Atlantic)", "Haddock", "Silver Hake"), ]
# *- - -* WSS 
BTop10_wss_noForage <- Biomass_NoForage[Biomass_NoForage$ID %in% c('WSS'), ]
BTop10_wss_noForage <- BTop10_wss_noForage[BTop10_wss_noForage$Species %in% c("Redfish Unseparated", "Spiny Dogfish", "Haddock", "Silver Hake", "Pollock", "White Hake", "Cod(Atlantic)", "Spider/(Queen,Snow)Unid", "Winter Flounder", "American Plaice"), ]
BTop10_wss_noForage$Species <- factor(BTop10_wss_noForage$Species, levels=c("Redfish Unseparated", "Spiny Dogfish", "Haddock", "Silver Hake", "Pollock", "White Hake", "Cod(Atlantic)", "Spider/(Queen,Snow)Unid", "Winter Flounder", "American Plaice"))
BTop5_wss_noForage <- BTop10_wss_noForage[BTop10_wss_noForage$Species %in% c("Redfish Unseparated", "Spiny Dogfish", "Haddock", "Silver Hake", "Pollock"), ]
# *- - -* Plot
head(Biomass_ess_q)
head(Biomass_minusForage_q_ess)
names(Biomass_minusForage_q_ess)[names(Biomass_minusForage_q_ess) == 'Biomass_NOForage_q_ess'] <- 'Biomass_minusForage'
names(Biomass_minusForage_q_wss)[names(Biomass_minusForage_q_wss) == 'Biomass_NOForage_q_wss'] <- 'Biomass_minusForage'

pdf("C:/RProjects/UseIndicators/output/figures/decoupled/TopSpp_BiomassCommunity_q_NoForageFish.pdf", width=18,height=10)
grid.arrange(PlotDecoupledIndi_with_area2(BTop5_ess_noForage, Biomass_minusForage_q_ess), PlotDecoupledIndi_with_area2(BTop5_wss_noForage, Biomass_minusForage_q_wss)) 
grid.arrange(PlotDecoupledIndi_with_area2(BTop10_ess_noForage, Biomass_minusForage_q_ess), PlotDecoupledIndi_with_area2(BTop10_wss_noForage, Biomass_minusForage_q_wss))
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



# -------- Step 6: Decouple Biomass of invertebrates *****  #  ####
BiomassInvertebrates_esswss_q <- esswss_melt_q[esswss_melt_q$variable %in% c("BiomassInvertebrates"), ] 
BiomassInvertebrates_ess_q <- BiomassInvertebrates_esswss_q[BiomassInvertebrates_esswss_q$ID %in% c('ESS'), ]
BiomassInvertebrates_wss_q <- BiomassInvertebrates_esswss_q[BiomassInvertebrates_esswss_q$ID %in% c('WSS'), ]

BiomassPerSpecies_coded <- read.csv("C:/RProjects/UseIndicators/data/SpeciesBiomass/BiomassPerSpp_q_coded.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BiomassInverts <- BiomassPerSpecies_coded[BiomassPerSpecies_coded$SppCodes %in% c(2000:8999),]
BiomassInverts <- BiomassInverts[BiomassInverts$YEAR %in% c(1999:2015),]

BiomassInverts$Spp <- factor(BiomassInverts$Spp)
BiomassInverts$SppCodes <- NULL
names(BiomassInverts)[names(BiomassInverts) == 'Spp'] <- 'Species'

BiomassInverts_TOP10 <-BiomassInverts %>%
  filter(ID %in% "ESS") %>%
  group_by(Species) %>%
  summarize(mn=mean(BIOMASS)) %>%
  arrange(desc(mn)) %>%
  top_n(n = 15)  
BiomassInverts_TOP10

#write.csv(BiomassInverts_TOP10, file = "C:/RProjects/UseIndicators/data/SpeciesBiomass/BiomassInverts_ESS_TOP15.csv",row.names=FALSE)
InvertsTop10 <- as.character(BiomassInverts_TOP10$Species) 
InvertsTop10 <- dQuote(InvertsTop10) 
toString(InvertsTop10)  # copy paste this putput to the melt functions below

# *---* ESS 
BInvertsTop10_ess <- BiomassInverts[BiomassInverts$ID %in% c('ESS'), ]
BInvertsTop10_ess <- BInvertsTop10_ess[BInvertsTop10_ess$Species %in% c("Pandalus Borealis", "Sea Cucumbers", "Cucumaria Frondosa", "Short-Fin Squid", "Snow Crab (Queen)", "Pandalus Montagui", "S. Droebachiensis", "Asteroidea S.C.", "American Lobster", "Basket Stars"), ]
BInvertsTop10_ess$Species <- factor(BInvertsTop10_ess$Species, levels=c("Pandalus Borealis", "Sea Cucumbers", "Cucumaria Frondosa", "Short-Fin Squid", "Snow Crab (Queen)", "Pandalus Montagui", "S. Droebachiensis", "Asteroidea S.C.", "American Lobster", "Basket Stars"))
BInvertsTop5_ess <- BInvertsTop10_ess[BInvertsTop10_ess$Species %in% c("Pandalus Borealis", "Sea Cucumbers", "Cucumaria Frondosa", "Short-Fin Squid", "Snow Crab (Queen)"), ]
# *---* WSS 
BInvertsTop10_wss <- BiomassInverts[BiomassInverts$ID %in% c('WSS'), ]
BInvertsTop10_wss <- BInvertsTop10_wss[BInvertsTop10_wss$Species %in% c("American Lobster", "Short-Fin Squid","Pandalus Montagui", "Sponges", "Sea Scallop", "Jonah Crab", "Asteroidea S.C.", "Pandalus Borealis", "Sea Cucumbers", "Atl Rock Crab"), ]
BInvertsTop10_wss$Species <- factor(BInvertsTop10_wss$Species, levels=c("American Lobster", "Short-Fin Squid","Pandalus Montagui", "Sponges", "Sea Scallop", "Jonah Crab", "Asteroidea S.C.", "Pandalus Borealis", "Sea Cucumbers", "Atl Rock Crab"))
BInvertsTop5_wss <- BInvertsTop10_wss[BInvertsTop10_wss$Species %in% c("American Lobster", "Short-Fin Squid","Pandalus Montagui", "Sponges", "Sea Scallop"), ]

pdf("C:/RProjects/UseIndicators/output/figures/decoupled/TopSpp_BiomassInvertebrates.pdf", width=16,height=10)
grid.arrange(PlotDecoupledIndi_with_area(BInvertsTop5_ess, BiomassInvertebrates_ess_q), PlotDecoupledIndi_with_area(BInvertsTop5_wss, BiomassInvertebrates_wss_q))
grid.arrange(PlotDecoupledIndi_with_area(BInvertsTop10_ess, BiomassInvertebrates_ess_q), PlotDecoupledIndi_with_area(BInvertsTop10_wss, BiomassInvertebrates_wss_q))
dev.off()


# -------- Step 7: Decouple TL 2 *****  #  ####
BiomassTL2_esswss_q <- esswss_melt_q[esswss_melt_q$variable %in% c("BiomassTL2"), ] 
BiomassTL2_ess_q <- BiomassTL2_esswss_q[BiomassTL2_esswss_q$ID %in% c('ESS'), ]
BiomassTL2_wss_q <- BiomassTL2_esswss_q[BiomassTL2_esswss_q$ID %in% c('WSS'), ]

BiomassTL2 <- BiomassPerSpecies_coded[BiomassPerSpecies_coded$SppCodes %in% c(2873,2800,4328,3212,2411,4316,6100,2541,2990,6300,4300,
                                                                              3550,4390,3100,6200,1900,2509,2524,2874,2870,8320,4700,
                                                                              3221,4310,2900,2500,2516,2417,2416,2400,2000,2005,2970,
                                                                              2215,6511,6999,2330,3143,2811,2820,4431,2531,4323,4344,
                                                                              2559,2300,4332,2521,8400,2906,4342,4322,2980,2510,2600,
                                                                              2319,2312,2320,3501,5200,2611,4999,4000,2565,6115,2555,
                                                                              2556,2700,4331,3131,3130,4221,2523,4350,2941,3112,2560,
                                                                              2562,2200,2211,2212,2210,2221,3144,4250,5300,3451,6121,
                                                                              4301,6411,6500,3999,4320,8530,6600,6900,3200,1827,8318,
                                                                              2967,1823,4321,4400,5100,6400,8300,3000,2499,2100,4200,
                                                                              2519,6000,2310,2313,2316,8600,4355,2520,3142,1810,4110,4211,4210,3199,4354),]


BiomassTL2$Spp <- factor(BiomassTL2$Spp)
BiomassTL2$SppCodes <- NULL
names(BiomassTL2)[names(BiomassTL2) == 'Spp'] <- 'Species'

BiomassTL2_TOP10 <- BiomassTL2 %>%
  filter(ID %in% "ESS") %>%
  group_by(Species) %>%
  summarize(mn=mean(BIOMASS)) %>%
  arrange(desc(mn)) %>%
  top_n(n = 15)  
BiomassTL2_TOP10

write.csv(BiomassTL2_TOP10, file = "C:/RProjects/UseIndicators/data/SpeciesBiomass/BiomassTL2_WSS_TOP15.csv",row.names=FALSE)
BiomassTL2Top10 <- as.character(BiomassTL2_TOP10$Species) 
BiomassTL2Top10 <- dQuote(BiomassTL2Top10) 
toString(BiomassTL2Top10)  # copy paste this putput to the melt functions below

BiomassTL2Top10_ess <- BiomassTL2[BiomassTL2$ID %in% c('ESS'), ]
BiomassTL2Top15_ess <- BiomassTL2Top10_ess[BiomassTL2Top10_ess$Species %in% c("Pandalus Borealis", "Sea Cucumbers", "Pandalus Montagui", "S. Droebachiensis", "Pandalus Sp.", "Asteroidea S.C.", "Basket Stars", "Sponges", "Sea Urchins", "Sea Scallop", "Northern Stone Crab", "Sand Dollars", "Hyas Coarctatus", "Iceland Scallop", "Purple Sunstar"), ]
BiomassTL2Top10_ess <- BiomassTL2Top10_ess[BiomassTL2Top10_ess$Species %in% c("Pandalus Borealis", "Sea Cucumbers", "Pandalus Montagui", "S. Droebachiensis", "Pandalus Sp.", "Asteroidea S.C.", "Basket Stars", "Sponges", "Sea Urchins", "Sea Scallop"), ]
BiomassTL2Top5_ess <- BiomassTL2Top10_ess[BiomassTL2Top10_ess$Species %in% c("Pandalus Borealis", "Sea Cucumbers", "Pandalus Montagui", "S. Droebachiensis", "Pandalus Sp."), ]

BiomassTL2Top10_wss <- BiomassTL2[BiomassTL2$ID %in% c('WSS'), ]
BiomassTL2Top10_wss <- BiomassTL2Top10_wss[BiomassTL2Top10_wss$Species %in% c("Krill Shrimp", "Pandalus Montagui", "Sponges", "Bryozoans P.", "Sea Scallop", "Pandalus Sp.", "Pandalus Borealis", "Asteroidea S.C.", "Sea Cucumbers", "Barnacles"), ]
BiomassTL2Top5_wss <- BiomassTL2Top10_wss[BiomassTL2Top10_wss$Species %in% c("Krill Shrimp", "Pandalus Montagui", "Sponges", "Bryozoans P.", "Sea Scallop"), ]

pdf("C:/RProjects/UseIndicators/output/figures/decoupled/TopSpp_BiomassTL2.pdf", width=16,height=10)
#PlotDecoupledIndi_with_area(BiomassTL2Top15_ess, BiomassTL2_ess_q)
PlotDecoupledIndi_with_area(BiomassTL2Top10_ess, BiomassTL2_ess_q)
PlotDecoupledIndi_with_area(BiomassTL2Top5_ess, BiomassTL2_ess_q)
PlotDecoupledIndi_with_area(BiomassTL2Top5_wss, BiomassTL2_wss_q)
PlotDecoupledIndi_with_area(BiomassTL2Top10_wss, BiomassTL2_wss_q)
dev.off()


# -------- Step 8: Decouple TL 3 *****  #  ####
BiomassTL3_esswss_q <- esswss_melt_q[esswss_melt_q$variable %in% c("BiomassTL3"), ] 
BiomassTL3_ess_q <- BiomassTL3_esswss_q[BiomassTL3_esswss_q$ID %in% c('ESS'), ]
BiomassTL3_wss_q <- BiomassTL3_esswss_q[BiomassTL3_esswss_q$ID %in% c('WSS'), ]

BiomassTL3 <- BiomassPerSpecies_coded[BiomassPerSpecies_coded$SppCodes %in% c(),]


BiomassTL3$Spp <- factor(BiomassTL3$Spp)
BiomassTL3$SppCodes <- NULL
names(BiomassTL3)[names(BiomassTL3) == 'Spp'] <- 'Species'

BiomassTL3_TOP10 <- BiomassTL3 %>%
  filter(ID %in% "WSS") %>%
  group_by(Species) %>%
  summarize(mn=mean(BIOMASS)) %>%
  arrange(desc(mn)) %>%
  top_n(n = 15)  
BiomassTL3_TOP10

write.csv(BiomassTL3_TOP10, file = "C:/RProjects/UseIndicators/data/SpeciesBiomass/BiomassTL3_WSS_TOP15.csv",row.names=FALSE)
BiomassTL3Top10 <- as.character(BiomassTL3_TOP10$Species) 
BiomassTL3Top10 <- dQuote(BiomassTL3Top10) 
toString(BiomassTL3Top10)  # copy paste this putput to the melt functions below

BiomassTL3Top10_ess <- BiomassTL3[BiomassTL3$ID %in% c('ESS'), ]
BiomassTL3Top15_ess <- BiomassTL3Top10_ess[BiomassTL3Top10_ess$Species %in% c("Herring(Atlantic)", "Northern Sand Lance", "Redfish Unseparated", "American Plaice", "Cod(Atlantic)", "Mackerel(Atlantic)", "Haddock", "Silver Hake", "Yellowtail Flounder", "Capelin", "Argentine (Atl)", "Pollock", "Thorny Skate", "Witch Flounder", "Short-Fin Squid"), ]
BiomassTL3Top10_ess <- BiomassTL3Top10_ess[BiomassTL3Top10_ess$Species %in% c("Herring(Atlantic)", "Northern Sand Lance", "Redfish Unseparated", "American Plaice", "Cod(Atlantic)", "Mackerel(Atlantic)", "Haddock", "Silver Hake", "Yellowtail Flounder", "Capelin"), ]
BiomassTL3Top5_ess <- BiomassTL3Top10_ess[BiomassTL3Top10_ess$Species %in% c("Herring(Atlantic)", "Northern Sand Lance", "Redfish Unseparated", "American Plaice", "Cod(Atlantic)"), ]

BiomassTL3Top10_wss <- BiomassTL3[BiomassTL3$ID %in% c('WSS'), ]
BiomassTL3Top10_wss <- BiomassTL3Top10_wss[BiomassTL3Top10_wss$Species %in% c("Herring(Atlantic)", "Redfish Unseparated", "Argentine (Atl)", "Haddock", "Silver Hake", "Pollock", "Alewife", "Shad American", "Cod(Atlantic)", "Mackerel(Atlantic)"), ]
BiomassTL3Top5_wss <- BiomassTL3Top10_wss[BiomassTL3Top10_wss$Species %in% c("Herring(Atlantic)", "Redfish Unseparated", "Argentine (Atl)", "Haddock", "Silver Hake"), ]

pdf("C:/RProjects/UseIndicators/output/figures/decoupled/TopSpp_BiomassTL3.pdf", width=16,height=10)
#PlotDecoupledIndi_with_area(BiomassTL3Top15_ess, BiomassTL3_ess_q)
PlotDecoupledIndi_with_area(BiomassTL3Top10_ess, BiomassTL3_ess_q)
PlotDecoupledIndi_with_area(BiomassTL3Top5_ess, BiomassTL3_ess_q)
PlotDecoupledIndi_with_area(BiomassTL3Top5_wss, BiomassTL3_wss_q)
PlotDecoupledIndi_with_area(BiomassTL3Top10_wss, BiomassTL3_wss_q)
dev.off()


# -------- Step 9: Decouple TL 4 *****  #  ####
BiomassTL4_esswss_q <- esswss_melt_q[esswss_melt_q$variable %in% c("BiomassTL4"), ] 
BiomassTL4_ess_q <- BiomassTL4_esswss_q[BiomassTL4_esswss_q$ID %in% c('ESS'), ]
BiomassTL4_wss_q <- BiomassTL4_esswss_q[BiomassTL4_esswss_q$ID %in% c('WSS'), ]

BiomassTL4 <- BiomassPerSpecies_coded[BiomassPerSpecies_coded$SppCodes %in% c(190,704,192,221,33,231,71,10,15,747,246,30,592,400,19,16,230,320,14,172,220,72,234,1004,73,31,12,32,191),]

BiomassTL4$Spp <- factor(BiomassTL4$Spp)
BiomassTL4$SppCodes <- NULL
names(BiomassTL4)[names(BiomassTL4) == 'Spp'] <- 'Species'

BiomassTL4_TOP10 <- BiomassTL4 %>%
  filter(ID %in% "ESS") %>%
  group_by(Species) %>%
  summarize(mn=mean(BIOMASS)) %>%
  arrange(desc(mn)) %>%
  top_n(n = 10)  
BiomassTL4_TOP10

#write.csv(BiomassTL4_TOP10, file = "C:/RProjects/UseIndicators/data/SpeciesBiomass/BiomassTL4_ESS_TOP15.csv",row.names=FALSE)
BiomassTL4Top10 <- as.character(BiomassTL4_TOP10$Species) 
BiomassTL4Top10 <- dQuote(BiomassTL4Top10) 
toString(BiomassTL4Top10)  # copy paste this putput to the melt functions below

BiomassTL4Top10_ess <- BiomassTL4[BiomassTL4$ID %in% c('ESS'), ]
BiomassTL4Top10_ess <- BiomassTL4Top10_ess[BiomassTL4Top10_ess$Species %in% c("Cod(Atlantic)", "Silver Hake", "Pollock", "White Hake", "Spiny Dogfish", "Turbot,Greenland Halibut", "Monkfish,Goosefish,Angler", "Halibut(Atlantic)", "Sea Raven", "Cusk"), ]
BiomassTL4Top10_ess$Species <- factor(BiomassTL4Top10_ess$Species, levels=c("Cod(Atlantic)", "Silver Hake", "Pollock", "White Hake", "Spiny Dogfish", "Turbot,Greenland Halibut", "Monkfish,Goosefish,Angler", "Halibut(Atlantic)", "Sea Raven", "Cusk"))
BiomassTL4Top5_ess <- BiomassTL4Top10_ess[BiomassTL4Top10_ess$Species %in% c("Cod(Atlantic)", "Silver Hake", "Pollock", "White Hake", "Spiny Dogfish"), ]

BiomassTL4Top10_wss <- BiomassTL4[BiomassTL4$ID %in% c('WSS'), ]
BiomassTL4Top10_wss <- BiomassTL4Top10_wss[BiomassTL4Top10_wss$Species %in% c("Spiny Dogfish", "Silver Hake", "Pollock", "White Hake", "Cod(Atlantic)", "Cusk", "Sea Raven", "Monkfish,Goosefish,Angler", "Halibut(Atlantic)", "Off-Shore Hake"), ]
BiomassTL4Top10_wss$Species <- factor(BiomassTL4Top10_wss$Species, levels=c("Spiny Dogfish", "Silver Hake", "Pollock", "White Hake", "Cod(Atlantic)", "Cusk", "Sea Raven", "Monkfish,Goosefish,Angler", "Halibut(Atlantic)", "Off-Shore Hake"))
BiomassTL4Top5_wss <- BiomassTL4Top10_wss[BiomassTL4Top10_wss$Species %in% c("Spiny Dogfish", "Silver Hake", "Pollock", "White Hake", "Cod(Atlantic)"), ]

pdf("C:/RProjects/UseIndicators/output/figures/decoupled/TopSpp_BiomassTL4.pdf", width=16,height=10)
#PlotDecoupledIndi_with_area(BiomassTL4Top15_ess, BiomassTL4_ess_q)
PlotDecoupledIndi_with_area(BiomassTL4Top10_ess, BiomassTL4_ess_q)
PlotDecoupledIndi_with_area(BiomassTL4Top5_ess, BiomassTL4_ess_q)
PlotDecoupledIndi_with_area(BiomassTL4Top5_wss, BiomassTL4_wss_q)
PlotDecoupledIndi_with_area(BiomassTL4Top10_wss, BiomassTL4_wss_q)
dev.off()

# -------- Step 10: Decouple Biomass q corrected groundfish *****  #  ####
BiomassGroundfish_esswss_q <- esswss_melt_q[esswss_melt_q$variable %in% c("BiomassGroundfish"), ] 
BiomassGroundfish_ess_q <- BiomassGroundfish_esswss_q[BiomassGroundfish_esswss_q$ID %in% c('ESS'), ]
BiomassGroundfish_wss_q <- BiomassGroundfish_esswss_q[BiomassGroundfish_esswss_q$ID %in% c('WSS'), ]
BiomassPerSpecies_coded <- read.csv("C:/RProjects/UseIndicators/data/SpeciesBiomass/BiomassPerSpp_q_coded.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BiomassGroundfish <- BiomassPerSpecies_coded[BiomassPerSpecies_coded$SppCodes %in% c(10:22,24:59,140,141,142,143,110,111,112,113,114,115,
                                                                                     116,117,118,200,201,202,203,204,205,206,207,208,209,210,
                                                                                     211,220,221,300,301,304,310,320,340,350,400,620:650),]
BiomassGroundfish$Spp <- factor(BiomassGroundfish$Spp)
BiomassGroundfish$SppCodes <- NULL
names(BiomassGroundfish)[names(BiomassGroundfish) == 'Spp'] <- 'Species'

BiomassGroundfish_TOP10 <-BiomassGroundfish %>%
 filter(ID %in% "WSS") %>%
  group_by(Species) %>%
  summarize(mn=mean(BIOMASS)) %>%
  arrange(desc(mn)) %>%
  top_n(n = 10)  
BiomassGroundfish_TOP10
#write.csv(BiomassGroundfish_TOP10, file = "C:/RProjects/UseIndicators/data/SpeciesBiomass/BiomassGroundfish_WSS_TOP10.csv",row.names=FALSE)
BiomassGroundfishTop10 <- as.character(BiomassGroundfish_TOP10$Species) 
BiomassGroundfishTop10 <- dQuote(BiomassGroundfishTop10) 
toString(BiomassGroundfishTop10)  # copy paste this putput to the melt functions below

# *---* ESS 
BGroundfishTop10_ess <- BiomassGroundfish[BiomassGroundfish$ID %in% c('ESS'), ]
BGroundfishTop10_ess <- BGroundfishTop10_ess[BGroundfishTop10_ess$Species %in% c("American Plaice", "Cod(Atlantic)", "Haddock", "Silver Hake", "Yellowtail Flounder", "Pollock", "Thorny Skate", "White Hake", "Witch Flounder", "Longhorn Sculpin"), ]
BGroundfishTop10_ess$Species <- factor(BGroundfishTop10_ess$Species, levels=c("American Plaice", "Cod(Atlantic)", "Haddock", "Silver Hake", "Yellowtail Flounder", "Pollock", "Thorny Skate", "White Hake", "Witch Flounder", "Longhorn Sculpin"))
BGroundfishTop5_ess <- BGroundfishTop10_ess[BGroundfishTop10_ess$Species %in% c("American Plaice", "Cod(Atlantic)", "Haddock", "Silver Hake", "Yellowtail Flounder"), ]
# *---* WSS 
BGroundfishTop10_wss <- BiomassGroundfish[BiomassGroundfish$ID %in% c('WSS'), ]
BGroundfishTop10_wss <- BGroundfishTop10_wss[BGroundfishTop10_wss$Species %in% c("Spiny Dogfish", "Haddock", "Silver Hake", "Pollock", "White Hake", "Cod(Atlantic)", "Winter Flounder", "American Plaice", "Barndoor Skate", "Thorny Skate"), ]
BGroundfishTop10_wss$Species <- factor(BGroundfishTop10_wss$Species, levels=c("Spiny Dogfish", "Haddock", "Silver Hake", "Pollock", "White Hake", "Cod(Atlantic)", "Winter Flounder", "American Plaice", "Barndoor Skate", "Thorny Skate"))
BGroundfishTop5_wss <- BGroundfishTop10_wss[BGroundfishTop10_wss$Species %in% c("Spiny Dogfish", "Haddock", "Silver Hake", "Pollock", "White Hake"), ]

pdf("C:/RProjects/UseIndicators/output/figures/decoupled/TopSpp_BiomassGroundfish.pdf", width=16,height=10)
grid.arrange(PlotDecoupledIndi_with_area(BGroundfishTop5_ess, BiomassGroundfish_ess_q), PlotDecoupledIndi_with_area(BGroundfishTop5_wss, BiomassGroundfish_wss_q))
grid.arrange(PlotDecoupledIndi_with_area(BGroundfishTop10_ess, BiomassGroundfish_ess_q), PlotDecoupledIndi_with_area(BGroundfishTop10_wss, BiomassGroundfish_wss_q))
dev.off()


# -------- Step 11: Decouple Biomass q corrected per functional groups *****  #  ####
names(BiomassPerSpecies_coded)[names(BiomassPerSpecies_coded) == 'Spp'] <- 'Species'
BiomassPerSpecies_coded <- BiomassPerSpecies_coded[BiomassPerSpecies_coded$ID %in% c("ESS", "WSS"),]

BSkates_spp <- BiomassPerSpecies_coded[BiomassPerSpecies_coded$SppCodes %in% c(200,201,202,203,204,205,206,207,208,209,210,211),]
BSkates <- esswss_melt_q[esswss_melt_q$variable %in% c("BiomassSkates"), ]
BSkates_spp_TOP10 <- BSkates_spp  %>%  group_by(Species) %>% summarize(mn=mean(BIOMASS)) %>% arrange(desc(mn)) %>% top_n(n = 10)  
#write.csv(BSkates_spp_TOP10, file = "C:/RProjects/UseIndicators/data/SpeciesBiomass/BSkates_spp_TOP10.csv",row.names=FALSE)
BSkates_sppTop10 <- as.character(BSkates_spp_TOP10$Species) 
BSkates_sppTop10 <- dQuote(BSkates_sppTop10) 
toString(BSkates_sppTop10) 
BSkates_spp$Species <- factor(BSkates_spp$Species, levels=c("Thorny Skate", "Barndoor Skate", "Winter Skate", "Little Skate", "Smooth Skate", "Spinytail Skate", "Skates (Ns)", "Round Skate", "Arctic Skate", "Brier Skate", "Jensen'S Skate", "Soft Skate"))

BClupeids_spp <- BiomassPerSpecies_coded[BiomassPerSpecies_coded$SppCodes %in% c(60,61,62,63),]
BClupeids <- esswss_melt_q[esswss_melt_q$variable %in% c("BiomassClupeids"), ]
BClupeids_spp_TOP10 <- BClupeids_spp  %>%  group_by(Species) %>% summarize(mn=mean(BIOMASS)) %>% arrange(desc(mn)) %>% top_n(n = 10)  
BClupeids_sppTop10 <- as.character(BClupeids_spp_TOP10$Species) 
BClupeids_sppTop10 <- dQuote(BClupeids_sppTop10) 
toString(BClupeids_sppTop10) 
BClupeids_spp$Species <- factor(BClupeids_spp$Species, levels=c("Herring(Atlantic)", "Alewife", "Shad American", "Rainbow Smelt"))

BForage_spp <- BiomassPerSpecies_coded[BiomassPerSpecies_coded$SppCodes %in% c(60,61,62,63,64,610,160),]
BForage <- esswss_melt_q[esswss_melt_q$variable %in% c("BiomassForage"), ]
BForage_spp_TOP10 <- BForage_spp  %>%  group_by(Species) %>% summarize(mn=mean(BIOMASS)) %>% arrange(desc(mn)) %>% top_n(n = 10)  
BForage_sppTop10 <- as.character(BForage_spp_TOP10$Species) 
BForage_sppTop10 <- dQuote(BForage_sppTop10) 
toString(BForage_sppTop10) 
BForage_spp$Species <- factor(BForage_spp$Species, levels=c("Herring(Atlantic)", "Northern Sand Lance", "Argentine (Atl)", "Capelin", "Alewife", "Shad American", "Rainbow Smelt"))

BPlanktivores_spp <- BiomassPerSpecies_coded[BiomassPerSpecies_coded$SppCodes %in% c(60,61,62,70,160,610,701,64),]
BPlanktivores <- esswss_melt_q[esswss_melt_q$variable %in% c("BTGPlanktivore"), ]
BPlanktivores_spp_TOP10 <- BPlanktivores_spp  %>%  group_by(Species) %>% summarize(mn=mean(BIOMASS)) %>% arrange(desc(mn)) %>% top_n(n = 10)  
BPlanktivores_sppTop10 <- as.character(BPlanktivores_spp_TOP10$Species) 
BPlanktivores_sppTop10 <- dQuote(BPlanktivores_sppTop10) 
toString(BPlanktivores_sppTop10) 
BPlanktivores_spp$Species <- factor(BPlanktivores_spp$Species, levels=c("Herring(Atlantic)", "Northern Sand Lance", "Argentine (Atl)", "Mackerel(Atlantic)", "Capelin", "Alewife", "Shad American", "Butterfish"))

BPiscivores_spp <- BiomassPerSpecies_coded[BiomassPerSpecies_coded$SppCodes %in% c(10,12,15,16,30,31,112,201,204,220,300,320,400),]
BPiscivores <- esswss_melt_q[esswss_melt_q$variable %in% c("BTGPiscivore"), ]
BPiscivores_spp_TOP10 <- BPiscivores_spp  %>%  group_by(Species) %>% summarize(mn=mean(BIOMASS)) %>% arrange(desc(mn)) %>% top_n(n = 10)  
BPiscivores_sppTop10 <- as.character(BPiscivores_spp_TOP10$Species) 
BPiscivores_sppTop10 <- dQuote(BPiscivores_sppTop10) 
toString(BPiscivores_sppTop10) 
BPiscivores_spp$Species <- factor(BPiscivores_spp$Species, levels=c("Spiny Dogfish", "Cod(Atlantic)", "Pollock", "White Hake", "Thorny Skate", "Longhorn Sculpin", "Turbot,Greenland Halibut", "Monkfish,Goosefish,Angler", "Winter Skate", "Cusk"))

BGadoids_spp <- BiomassPerSpecies_coded[BiomassPerSpecies_coded$SppCodes %in% c(10,11,12,13,14,15,16,17,18,19,110,111,112,113,114,115,116,117,118),]
BGadoids <- esswss_melt_q[esswss_melt_q$variable %in% c("BiomassGadoids"), ]
BGadoids_spp_TOP10 <- BGadoids_spp  %>%  group_by(Species) %>% summarize(mn=mean(BIOMASS)) %>% arrange(desc(mn)) %>% top_n(n = 10)  
BGadoids_sppTop10 <- as.character(BGadoids_spp_TOP10$Species) 
BGadoids_sppTop10 <- dQuote(BGadoids_sppTop10) 
toString(BGadoids_sppTop10) 
BGadoids_spp$Species <- factor(BGadoids_spp$Species, levels=c("Haddock", "Silver Hake", "Cod(Atlantic)", "Pollock", "White Hake", "Squirrel Or Red Hake", "Cusk", "Longfin Hake", "Off-Shore Hake", "Fourbeard Rockling"))

BFlatfish_spp <- BiomassPerSpecies_coded[BiomassPerSpecies_coded$SppCodes %in% c(30,31,40,41,42,43,44,45,49,140,141,142,143),]
BFlatfish <- esswss_melt_q[esswss_melt_q$variable %in% c("BiomassFlatfish"), ]
BFlatfish_spp_TOP10 <- BFlatfish_spp  %>%  group_by(Species) %>% summarize(mn=mean(BIOMASS)) %>% arrange(desc(mn)) %>% top_n(n = 10)  
BFlatfish_sppTop10 <- as.character(BFlatfish_spp_TOP10$Species) 
BFlatfish_sppTop10 <- dQuote(BFlatfish_sppTop10) 
toString(BFlatfish_sppTop10) 
BFlatfish_spp$Species <- factor(BFlatfish_spp$Species, levels=c("American Plaice", "Yellowtail Flounder", "Winter Flounder", "Witch Flounder", "Turbot,Greenland Halibut", "Halibut(Atlantic)", "Gulf Stream Flounder", "Brill/Windowpane", "Fourspot Flounder", "Summer Flounder"))

BZoopiscivores_spp <- BiomassPerSpecies_coded[BiomassPerSpecies_coded$SppCodes %in% c(13,14,19,23),]
BZoopiscivores <- esswss_melt_q[esswss_melt_q$variable %in% c("BTGZoopiscivore"), ]
BZoopiscivores_spp_TOP10 <- BZoopiscivores_spp  %>%  group_by(Species) %>% summarize(mn=mean(BIOMASS)) %>% arrange(desc(mn)) %>% top_n(n = 10)  
BZoopiscivores_sppTop10 <- as.character(BZoopiscivores_spp_TOP10$Species) 
BZoopiscivores_sppTop10 <- dQuote(BZoopiscivores_sppTop10) 
toString(BZoopiscivores_sppTop10) 
BZoopiscivores_spp$Species <- factor(BZoopiscivores_spp$Species, levels=c("Redfish Unseparated", "Silver Hake", "Squirrel Or Red Hake", "Off-Shore Hake"))

BLargeBenthivore_spp <- BiomassPerSpecies_coded[BiomassPerSpecies_coded$SppCodes %in% c(50,200),]
BLargeBenthivore <- esswss_melt_q[esswss_melt_q$variable %in% c("BTGLargeBenthivore"), ]
BLargeBenthivore_spp_TOP10 <- BLargeBenthivore_spp  %>%  group_by(Species) %>% summarize(mn=mean(BIOMASS)) %>% arrange(desc(mn)) %>% top_n(n = 10)  
BLargeBenthivore_sppTop10 <- as.character(BLargeBenthivore_spp_TOP10$Species) 
BLargeBenthivore_sppTop10 <- dQuote(BLargeBenthivore_sppTop10) 
toString(BLargeBenthivore_sppTop10) 
BLargeBenthivore_spp$Species <- factor(BLargeBenthivore_spp$Species, levels=c("Barndoor Skate", "Striped Atl Wolffish"))

BMediumBenthivore_spp <- BiomassPerSpecies_coded[BiomassPerSpecies_coded$SppCodes %in% c(11,241,40,41,42,43,123,142,143,202,203,301,304,414,501,505,640,114,115),]
BMediumBenthivore <- esswss_melt_q[esswss_melt_q$variable %in% c("BTGMediumBenthivore"), ]
BMediumBenthivore_spp_TOP10 <- BMediumBenthivore_spp  %>%  group_by(Species) %>% summarize(mn=mean(BIOMASS)) %>% arrange(desc(mn)) %>% top_n(n = 10)  
BMediumBenthivore_sppTop10 <- as.character(BMediumBenthivore_spp_TOP10$Species) 
BMediumBenthivore_sppTop10 <- dQuote(BMediumBenthivore_sppTop10) 
toString(BMediumBenthivore_sppTop10) 
BMediumBenthivore_spp$Species <- factor(BMediumBenthivore_spp$Species, levels=c("Haddock", "American Plaice", "Yellowtail Flounder", "Winter Flounder", "Witch Flounder", "Lumpfish", "Atlantic Hagfish", "Rosefish(Black Belly)", "Little Skate", "Smooth Skate"))


pdf("C:/RProjects/UseIndicators/output/figures/decoupled/BiomassPerFunctionalGroup_q.pdf", width=20,height=8)
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


# -------- Step 12 Decouple Biomass of the community NO Q corrected  *****  #  ####
#no q corerected **********************************************************************************************************  
rm(list = ls())
source('C:/RProjects/UseIndicators/code/IndiFunctions.R')    
library(plyr)
library(reshape)
library(dplyr)
library(FSAdata)
library(plotrix)
library(ggplot2)
library(gridExtra)
theme_set(theme_bw())

Indi_noq <- read.csv("C:/RProjects/UseIndicators/data/esswssnonq.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
esswss_melt_noq <- melt(Indi_noq, id=c('YEAR', 'ID'))
Biomass_esswss_noq <- esswss_melt_noq[esswss_melt_noq$variable %in% c("Biomass"), ]
Biomass_ess_noq <- Biomass_esswss_noq[Biomass_esswss_noq$ID %in% c('ESS'), ]
Biomass_wss_noq <- Biomass_esswss_noq[Biomass_esswss_noq$ID %in% c('WSS'), ]

BiomassPerSpecies <- read.csv("C:/RProjects/UseIndicators/data/SpeciesBiomass/BiomassPerSpp_No_q.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BiomassPerSpecies$Spp <- factor(BiomassPerSpecies$Spp)
BiomassPerSpecies$Species <- NULL
names(BiomassPerSpecies)[names(BiomassPerSpecies) == 'Spp'] <- 'Species'

Biomass_TOP10 <-BiomassPerSpecies %>%
  filter(ID %in% "WSS") %>%
  group_by(Species) %>%
  summarize(mn=mean(BIOMASS)) %>%
  arrange(desc(mn)) %>%
  top_n(n = 10)  
Biomass_TOP10
#write.csv(Biomass_TOP10, file = "C:/RProjects/UseIndicators/data/SpeciesBiomass/BiomassWSS_noq_TOP10.csv",row.names=FALSE)
Top10 <- as.character(Biomass_TOP10$Species) 
Top10 <- dQuote(Top10) 
toString(Top10)  # copy paste this putput to the melt functions below

# *---* ESS 
BTop10_ess <- BiomassPerSpecies[BiomassPerSpecies$ID %in% c('ESS'), ]
BTop10_ess <- BTop10_ess[BTop10_ess$Species %in% c("Redfish Unseparated", "Cod(Atlantic)", "Haddock", "American Plaice", "Herring(Atlantic)", "Silver Hake", "Pollock", "Yellowtail Flounder", "Thorny Skate", "Short-Fin Squid"), ]
BTop10_ess$Species <- factor(BTop10_ess$Species, levels=c("Redfish Unseparated", "Cod(Atlantic)", "Haddock", "American Plaice", "Herring(Atlantic)", "Silver Hake", "Pollock", "Yellowtail Flounder", "Thorny Skate", "Short-Fin Squid"))
BTop5_ess <- BTop10_ess[BTop10_ess$Species %in% c("Redfish Unseparated", "Cod(Atlantic)", "Haddock", "American Plaice", "Herring(Atlantic)"), ]

# *---* WSS 
BTop10_wss <- BiomassPerSpecies[BiomassPerSpecies$ID %in% c('WSS'), ]
BTop10_wss <- BTop10_wss[BTop10_wss$Species %in% c("Herring(Atlantic)", "Spiny Dogfish", "Redfish Unseparated", "Haddock", "Pollock", "White Hake", "Cod(Atlantic)", "Spider/(Queen,Snow)Unid", "Silver Hake", "Short-Fin Squid"), ]
BTop10_wss$Species <- factor(BTop10_wss$Species, levels=c("Herring(Atlantic)", "Spiny Dogfish", "Redfish Unseparated", "Haddock", "Pollock", "White Hake", "Cod(Atlantic)", "Spider/(Queen,Snow)Unid", "Silver Hake", "Short-Fin Squid"))
BTop5_wss <- BTop10_wss[BTop10_wss$Species %in% c("Herring(Atlantic)", "Spiny Dogfish", "Redfish Unseparated", "Haddock", "Pollock"), ]

#----- Plot 
pdf("C:/RProjects/UseIndicators/output/figures/decoupled/TopSpp_BiomassCommunity_noq2.pdf", width=16,height=10)
grid.arrange(PlotDecoupledIndi_with_area(BTop5_ess, Biomass_ess_noq), PlotDecoupledIndi_with_area(BTop5_wss, Biomass_wss_noq)) 
grid.arrange(PlotDecoupledIndi_with_area(BTop10_ess, Biomass_ess_noq), PlotDecoupledIndi_with_area(BTop10_wss, Biomass_wss_noq)) 
dev.off()


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
  top_n(n = 10)  
Biomass_NoForage_TOP10
#write.csv(Biomass_q_TOP10, file = "C:/RProjects/UseIndicators/data/SpeciesBiomass/BiomassWSS_q_TOP10_NoForage.csv",row.names=FALSE)
Top10 <- as.character(Biomass_NoForage_TOP10$Species) 
Top10 <- dQuote(Top10) 
toString(Top10)  # copy paste this putput to the melt functions below

# *- - -* ESS 
BTop10_ess_noForage <- Biomass_NoForage[Biomass_NoForage$ID %in% c('ESS'), ]
BTop10_ess_noForage <- BTop10_ess_noForage[BTop10_ess_noForage$Species %in% c("Redfish Unseparated", "Cod(Atlantic)", "Haddock", "American Plaice", "Silver Hake", "Pollock", "Yellowtail Flounder", "Thorny Skate", "Short-Fin Squid", "White Hake"), ]
BTop10_ess_noForage$Species <- factor(BTop10_ess_noForage$Species, levels=c("Redfish Unseparated", "Cod(Atlantic)", "Haddock", "American Plaice", "Silver Hake", "Pollock", "Yellowtail Flounder", "Thorny Skate", "Short-Fin Squid", "White Hake"))
BTop5_ess_noForage <- BTop10_ess_noForage[BTop10_ess_noForage$Species %in% c("Redfish Unseparated", "Cod(Atlantic)", "Haddock", "American Plaice", "Silver Hake"), ]

# *- - -* WSS 
BTop10_wss_noForage <- Biomass_NoForage[Biomass_NoForage$ID %in% c('WSS'), ]
BTop10_wss_noForage <- BTop10_wss_noForage[BTop10_wss_noForage$Species %in% c('Spiny Dogfish', 'Redfish Unseparated', 'Haddock', 'Pollock', 'White Hake', 'Cod(Atlantic)', 'Spider/(Queen,Snow)Unid', 'Silver Hake', 'Short-Fin Squid', 'American Lobster'), ]
BTop10_wss_noForage$Species <- factor(BTop10_wss_noForage$Species, levels=c('Spiny Dogfish', 'Redfish Unseparated', 'Haddock', 'Pollock', 'White Hake', 'Cod(Atlantic)', 'Spider/(Queen,Snow)Unid', 'Silver Hake', 'Short-Fin Squid', 'American Lobster'))
BTop5_wss_noForage <- BTop10_wss_noForage[BTop10_wss_noForage$Species %in% c('Spiny Dogfish', 'Redfish Unseparated', 'Haddock', 'Pollock', 'White Hake'), ]

# *- - -* Plot
 head(Biomass_ess_noq)
 
 # NoQ
 Biomass_ess_Forage_noQ <- read.csv("C:/RProjects/UseIndicators/data/SpeciesBiomass/BiomassCommunity_without forage fish/Biomass_ess_NOq_Forage.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
 Biomass_ess_Forage_noQ <- melt(Biomass_ess_Forage_noQ, id=c('YEAR', 'ID'))
 Biomass_wss_Forage_noQ <- read.csv("C:/RProjects/UseIndicators/data/SpeciesBiomass/BiomassCommunity_without forage fish/Biomass_wss_NOq_Forage.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
 Biomass_wss_Forage_noQ <- melt(Biomass_wss_Forage_noQ, id=c('YEAR', 'ID'))
 
 Biomass_NOForage_Noq_ess <- Biomass_ess_noq$value - Biomass_ess_Forage_noQ$value
 Biomass_NOForage_Noq_wss <- Biomass_wss_noq$value - Biomass_wss_Forage_noQ$value
 
 Biomass_minusForage_Noq_ess <- Biomass_ess_Forage_noQ  %>% mutate(Biomass_NOForage_Noq_ess)
 Biomass_minusForage_Noq_wss <- Biomass_wss_Forage_noQ  %>% mutate(Biomass_NOForage_Noq_wss)
 
 names(Biomass_minusForage_Noq_ess)[names(Biomass_minusForage_Noq_ess) == 'Biomass_NOForage_Noq_ess'] <- 'Biomass_minusForage'
 names(Biomass_minusForage_Noq_wss)[names(Biomass_minusForage_Noq_wss) == 'Biomass_NOForage_Noq_wss'] <- 'Biomass_minusForage'

pdf("C:/RProjects/UseIndicators/output/figures/decoupled/TopSpp_BiomassCommunity_No_q_NoForageFish.pdf", width=18,height=10)
grid.arrange(PlotDecoupledIndi_with_area2(BTop5_ess_noForage, Biomass_minusForage_Noq_ess), PlotDecoupledIndi_with_area2(BTop5_wss_noForage, Biomass_minusForage_Noq_wss)) 
grid.arrange(PlotDecoupledIndi_with_area2(BTop10_ess_noForage, Biomass_minusForage_Noq_ess), PlotDecoupledIndi_with_area2(BTop10_wss_noForage, Biomass_minusForage_Noq_wss))
dev.off()








# -------- OLD CODE - RECYCLE OR DELETE *****  #  ####



# -------- Step 9. Read and decouple biomass indicators per functional group -- old script fix based on what i did with BiomassInvertebrates *****  #  ####

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


# -------- Step 8 - old version: Plot decoupled indicators  *****  #  ####



