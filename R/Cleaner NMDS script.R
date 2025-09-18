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

ellena2<-ellena[,5:11]

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

"a.mO<-abundance.matrixCF[,3:159]
SiteO<-abundance.matrixCF[,1]

BufferO<-abundance.matrixCF[,2]"

abundance.matrixCF$Site<-factor(abundance.matrixCF$Site)

site_dummies <- model.matrix(~ Site - 1, data = abundance.matrixCF)

site_dummies_df <- as.data.frame(site_dummies)

# Step 4: Combine with your original environmental data
ENVCF <- cbind(ENVCF, site_dummies_df)

ENVCF<-abundance.matrixCF[,160:166]

ENVCF <- subset(ENVCF, select = -weighted.ruderality)


AllCF<-metaMDS(a.mO,
               distance = "bray",
               k = 3,
               trymax = 250)
AllCF



bufscf<-factor(rep(c( 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3,3, 1, 3,1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3,3, 1, 3,1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3,3, 1, 3), each = 1))

print(colnames(ENVCF))

colnames(ENVCF) <- c("Distance.Road", "Distance.Turbine", "Altitude","Open.water", "Bare.soil", "Bare.rock", "Froya" ,"Smola", "Ytre Vikna" )

ENVCF
fitcf<-envfit(AllCF,ENVCF,permu=999)
fitcf
plot(fitcf,p.max=0.05, col="black")


cols<-c("#56B4E9","#E69F00","#009E73")
light<-c("#F6C97A","#A6D8F0","#80D6B0")

par(mfrow = c(1, 1))

library(ggplot2)
png("NMDS03.08.png", width = 30, height = 20, units = "cm", res = 400)

plot(AllCF,type = "n", xlim = c(-1.7, 1.3), ylim = c(-1.7, 1.5))
text(AllCF, display = "sites", col=light[bufscf], cex=0.6)
#points(AllCF, display="species", pch = 3, col = "wheat4", cex = 0.3)
#ordipointlabel(AllCF, display="species", col="brown", cex = 1, add = TRUE)


ordiellipse(AllCF, BufferO,kind = "sd", conf = 0.95, lwd = 3, col = cols)
#ordiellipse(buff10, Site10,kind = "sd", conf = 0.95, lwd = 2, col = cols, fill = TRUE, alpha = 0.3)
#ordispider(AllCF,BufferO, label = F, col = cols)
legend("topright", legend =c('Distance 0-10 m', 'Distance 10-50 m', 'Distance 50-120 m'), pch=16, pt.cex=2, cex=1, bty='n',
       col = cols)
legend("topleft", legend = "Bray curtis, stress = 0.1796")
title("NMDS ordination of all sites combined", cex=4)
plot(fitcf,p.max=0.05, col="black", cex = 1.2)

dev.off()

