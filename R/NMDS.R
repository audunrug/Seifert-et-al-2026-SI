setwd("/Users/katri/OneDrive/Documents/Master/Master project")
library(readxl)
library(dplyr)
library(ggplot2)
library(lme4)
library(MuMIn)
library(rstatix)
library(tidyr)
library(openxlsx)
library(vegan)
library(fossil)
library(qwraps2)
library(brms)
library(caret)
library(pROC)
library(tidyverse)

#----creating dataset----
WF <- read_excel("Master_fixed_removed.xlsx", sheet = "Sheet1")


WF$Operation_start<-as.factor(WF$Operation_start)

names(WF)[names(WF) == "Abundance_(x/20)"] <- "Abundance"
names(WF)[names(WF) == "Bare soil"] <- "Bare_soil"


###Subsetting vascular plants
WFvasc<-subset(WF, Classification=="V")
WFvasc<-na.omit(WFvasc)

WFvasc$Rud<-as.numeric(WFvasc$Rud)
WFvasc$Abundance<-as.numeric(WFvasc$Abundance)
summary(WFvasc)

####calculating weighted ruderality
WFvasc_summary <- 
  WFvasc %>% 
  group_by(Site, Plot) %>% 
  summarise(weighted_ruderality = weighted.mean(Rud,Abundance))

WFvasc_summary

WFvasc_final <- left_join(WFvasc, WFvasc_summary)

WFvasc_final$weighted_ruderality100<-WFvasc_final$weighted_ruderality/100

### getting rubin codes 
WF[c('Genus2', 'Species_Name')] <- str_split_fixed(WF$Species, ' ', 2) # ATHY fil not added probably due to how it is split 
WF$Rubin1 = substr(WF$Genus2, 1, 4)
WF$Rubin2 = substr(WF$Species_Name, 1, 3)
WF$Rubin <- paste(WF$Rubin1,WF$Rubin2)

WF$PlotID <- paste(WF$Site, WF$Plot, sep = ". ")
WFvasc_final$PlotID <- paste(WFvasc_final$Site, WFvasc_final$Plot, sep = ". ")

?create.matrix

WF1<- as.data.frame(WF)

WF1<-subset(WF1, Classification!="L")

summary(WF1)

WF1$PlotID<-as.vector(WF1$PlotID)

#making environmental data for the envfit 

ellena <- read_excel("ENV.xlsx", sheet = "Comb")

ellena2<-ellena[,1:11]

###Correct nmds to use as plots with cerastium fontanum have less than 4 species 

WFO<-subset(WF1, PlotID!="1. 25")
WFO<-subset(WFO, PlotID!="1. 1")




ellena2 <- ellena2[-c(1,9), ]

CF <- create.matrix(WFO, tax.name = "Rubin",
                    locality = "PlotID",
                    time.col = NULL,
                    time = NULL,
                    abund = TRUE,
                    abund.col = "Abundance")

abundance.matrixCF <- as.data.frame(t(CF))


abundance.matrixCF <- add_column(Site = rep(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                              2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
                                              
                                              3,3,3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3),each = 1), .before = 'Agro can',abundance.matrixCF)


abundance.matrixCF <- add_column(Buffer = rep(c(1,1,1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3,3, 1, 3,1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3,3, 1, 3,1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3,3, 1, 3), each =1), .before = 'Agro can',abundance.matrixCF)

abundance.matrixCF<-cbind(abundance.matrixCF,ellena2)

a.mO<-abundance.matrixCF[,3:159]
SiteO<-abundance.matrixCF[,1]

BufferO<-abundance.matrixCF[,2]

abundance.matrixCF$Site<-factor(abundance.matrixCF$Site)

site_dummies <- model.matrix(~ Site - 1, data = abundance.matrixCF)

site_dummies_df <- as.data.frame(site_dummies)

# Step 4: Combine with your original environmental data
ENVCF<-abundance.matrixCF[,160:170]

ENVCF <- cbind(ENVCF, site_dummies_df)

ENVCF <- subset(ENVCF, select = -weighted.ruderality)

ENVCF <- subset(ENVCF, select = -Site1)

ENVCF <- subset(ENVCF, select = -Site2)

ENVCF <- subset(ENVCF, select = -Site3)


AllCF<-metaMDS(a.mO,
               distance = "bray",
               k = 3,
               trymax = 250)
AllCF



bufscf<-factor(rep(c( 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3,3, 1, 3,1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3,3, 1, 3,1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3,3, 1, 3), each = 1))

print(colnames(ENVCF))

colnames(ENVCF) <- c("Light", "Moisture", "Nutrients", "Reactivity", "Road distance", "Distance to Turbine", "Altitude","Open water", "Bare Soil", "Bare Rock" )

ENVCF
fitcf<-envfit(AllCF,ENVCF,permu=999)
fitcf
plot(fitcf,p.max=0.05, col="black")

# colours from Lukas if i want to change to the exact ones #FDe735ff", "#75d054ff", "#2c728eff"

cols<-c("#4682B4","#AADC32","#FDE725")
light<-c("#7BAFD1","#C9E96F","#FFF27F")

pchvec <- c(21, 24, 22)
pchvec <- as.numeric(as.factor(Site))



par(mfrow = c(1, 1))

library(ggplot2)
png("NMDSnopointsnosite20.12.png", width = 30, height = 20, units = "cm", res = 400)

plot(AllCF,type = "n", xlim = c(-1.5, 1.3), ylim = c(-1.5, 1.6))

points(AllCF, display = "sites",                          # Filled circle
bg = light[bufscf],  # Fill color from your 'light' palette
col = "black",                     # Border color
cex = 1,pch = pchvec[SiteO]) 

#points(AllCF, display="species", pch = 3, col = "wheat4", cex = 0.3)
#ordipointlabel(AllCF, display="species", col="brown", cex = 1, add = TRUE)


ordiellipse(AllCF, BufferO,kind = "sd", conf = 0.95, lwd = 3, col = cols)
#ordiellipse(buff10, Site10,kind = "sd", conf = 0.95, lwd = 2, col = cols, fill = TRUE, alpha = 0.3)
#ordispider(AllCF,BufferO, label = F, col = cols)
legend("topright", legend =c('Distance 0-10 m', 'Distance 10-50 m', 'Distance 50-120 m'), pch=21,  pt.cex = c(1.8, 1.8, 1.8), cex=1, bty='n',
       col = cols, pt.bg = cols)
legend("top", legend =c('F4', 'Y12', 'S19'), pch=pchvec,  pt.cex = c(1.8, 1.6, 1.8), cex=1, bty='n',
       col = "Black", pt.bg = "Black", ncol = 3)
legend("topleft", legend = "Bray curtis, stress = 0.1796")
title("NMDS ordination of all sites combined", cex=4)
plot(fitcf,p.max=0.05, col="Black", cex = 1.5)



dev.off()

