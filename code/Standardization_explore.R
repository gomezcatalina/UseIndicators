#write.csv(ESS_Q, "outputs/data/ESS_Q_trial.csv") 
ESS_Q <- read.csv("outputs/data/ESS_Q_trial.csv",header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)

#The problem: 
ESS_Q$FPInvertebrates.L_s # all NAs for all datasets in all extractions 

#Fishing pressure without standardizing is fine:
ESS_Q$FPInvertebrates.L

#I wonderf if there is something wrong in the process of doing the standardization?

# I tried using another indicator:
head(ESS_Q$LargeFishIndicator)

LFI_s <- scale(ESS_Q$LargeFishIndicator, center = TRUE, scale = TRUE)
head(LFI_s)

LFI_s2 <- (ESS_Q$LargeFishIndicator - mean(ESS_Q$LargeFishIndicator))/sd(ESS_Q$LargeFishIndicator)
head(LFI_s2)

#Up to that point all makes sense; however the same indicator standardized in your dataset is different:
head(ESS_Q$LargeFishIndicator_s)

#I wonder if there is something off in the global standardization... when I look FPInvertebratesl is weird:
FP_s <- scale(ESS_Q$FPInvertebrates.L, center = TRUE, scale = TRUE)
FP_s #This one gives the right value

FP_s2 <- (ESS_Q$FPInvertebrates.L - mean(ESS_Q$FPInvertebrates.L))/sd(ESS_Q$FPInvertebrates.L)
FP_s2 #This one replicates your error - which is the function used in STAR.R

#THOUGHTS? I am bit puzzled about this... ideas are welcome!


