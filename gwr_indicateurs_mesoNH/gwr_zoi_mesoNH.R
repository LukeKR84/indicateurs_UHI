
# Load necessary packages 
library(cartography)
library(sf)
library(classInt)
library(spgwr)
library(ggplot2)
library(dplyr)

# Set working directory 
setwd("C:/Stage_IGN/indicateurs/gwr_indicateurs_mesoNH")

# Load zone of interest
zoi <- read_sf("C:/Stage_IGN/indicateurs/gwr_indicateurs_mesoNH/zoi_mesoNH_strat1.geojson")
# Fix the projection
st_crs(zoi) <- 2154

# Conversion en objet de type SpatialPolygonsDataFrame pour le package SPGWR
zoi_spatial <-as(zoi, 'Spatial')

summary(zoi_spatial)

##### GWR BSF TEB 1 to TEB 6 #####

# Réalisation d'une régression linéaire (lm) simple
# ml_BSF_TEB1 <-lm(zoi_spatial$bsf~zoi_spatial$TEB_T1)
# ml_BSF_TEB2 <-lm(zoi_spatial$bsf~zoi_spatial$TEB_T2)
# ml_BSF_TEB3 <-lm(zoi_spatial$bsf~zoi_spatial$TEB_T3)
# ml_BSF_TEB4 <-lm(zoi_spatial$bsf~zoi_spatial$TEB_T4)
# ml_BSF_TEB5 <-lm(zoi_spatial$bsf~zoi_spatial$TEB_T5)
# ml_BSF_TEB6 <-lm(zoi_spatial$bsf~zoi_spatial$TEB_T6)

ml_BSF_TEB1 <-lm(zoi$bsf~zoi_spatial$TEB_T1)
ml_BSF_TEB2 <-lm(zoi$bsf~zoi_spatial$TEB_T2)
ml_BSF_TEB3 <-lm(zoi$bsf~zoi_spatial$TEB_T3)
ml_BSF_TEB4 <-lm(zoi$bsf~zoi_spatial$TEB_T4)
ml_BSF_TEB5 <-lm(zoi$bsf~zoi_spatial$TEB_T5)
ml_BSF_TEB6 <-lm(zoi$bsf~zoi_spatial$TEB_T6)

summary(ml_BSF_TEB1)
summary(ml_BSF_TEB2)
summary(ml_BSF_TEB3)
summary(ml_BSF_TEB4)
summary(ml_BSF_TEB5)
summary(ml_BSF_TEB6)

# Dessin du graphique de corrélation entre les deux variables
plot(zoi_spatial$bsf~zoi_spatial$TEB_T1, xlab = "TEB_T1", ylab = "BSF range")
abline(ml_BSF_TEB1, col="red")
plot(zoi_spatial$bsf~zoi_spatial$TEB_T2, xlab = "TEB_T2", ylab = "BSF range")
abline(ml_BSF_TEB2, col="red")
plot(zoi_spatial$bsf~zoi_spatial$TEB_T3, xlab = "TEB_T3", ylab = "BSF range")
abline(ml_BSF_TEB3, col="red")
plot(zoi_spatial$bsf~zoi_spatial$TEB_T4, xlab = "TEB_T4", ylab = "BSF range")
abline(ml_BSF_TEB4, col="red")
plot(zoi_spatial$bsf~zoi_spatial$TEB_T5, xlab = "TEB_T5", ylab = "BSF range")
abline(ml_BSF_TEB5, col="red")
plot(zoi_spatial$bsf~zoi_spatial$TEB_T6, xlab = "TEB_T6", ylab = "BSF range")
abline(ml_BSF_TEB6, col="red")
# Stockage des résidus dans une variable du fond de carte
zoi$residus_1 <- ml_BSF_TEB1$residuals
zoi$residus_2 <- ml_BSF_TEB2$residuals
residus_3 <- ml_BSF_TEB3$residuals
residus_4 <- ml_BSF_TEB4$residuals
residus_5 <- ml_BSF_TEB5$residuals
residus_6 <- ml_BSF_TEB6$residuals

residus_1_data <- as.data.frame(residus_1)

# Code d'une autre source à adapter 

# Histogramme des fréquences des résidus
hist(epci$residus, breaks=40)

# Réglage des proportions, dimensions et marges du fichier image
figdim <- getFigDim(x = epci, width = 800, mar = c(0,0,1.2,0), res = 100)
png(filename = "carteGWR1.png", width = figdim[1], height = figdim[2], res = 100)
par(mar = c(0,0,1.2,0))

# Discrétisation par les quantilesen  6 classes
qt6<-classIntervals(var=epci$residus, n=6)

# Cartographie thématique des résidus, par quantiles et palette autour de zéro
couleurs<-carto.pal(pal1 = "green.pal", n1 = 3, pal2 = "red.pal", n2 = 3)
choroLayer(spdf=epci, df=epci@data, var="residus", breaks=qt6$brks, col=couleurs, border=FALSE)
dev.off()

# Recherche de la bande passante en utilisant une méthode de cross-validation basée sur une fonction gaussienne.
bp<-gwr.sel(epci$Mediane_niv_vie_2015~epci$Part_diplomes_sup_pop_2015, data=epci, method="cv")
bp
# Calcul de la GWR avec cette bp
g<-gwr(epci$Mediane_niv_vie_2015~epci$Part_diplomes_sup_pop_2015, data=epci, bandwidth=bp, hatmatrix=TRUE, se.fit=TRUE)

# Affichage des résultats
g

# Sockage des résultats dans une variable, puis dans les données attributaires
rg<-as.data.frame(g$SDF)
epci$rg=rg$epci.Part_diplomes_sup_pop_2015