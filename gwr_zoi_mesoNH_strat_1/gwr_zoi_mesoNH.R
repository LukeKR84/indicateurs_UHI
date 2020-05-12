
# Load necessary packages 
library(cartography)
library(sf)
library(classInt)
library(spgwr)
library(ggplot2)
library(dplyr)
library(GWmodel)

# Set working directory 
setwd("C:/Stage_IGN/indicateurs/gwr_indicateurs_mesoNH")

# Load zone of interest
zoi <- read_sf("C:/Stage_IGN/indicateurs/gwr_indicateurs_mesoNH/zoi_mesoNH_strat1.geojson")
# Fix the projection
st_crs(zoi) <- 2154
# Create different subsets based on each indicator (data sets without NAs in the indicator column)
zoi_bsf <- subset(zoi, (!is.na(zoi$bsf)))
zoi_hre <- subset(zoi, (!is.na(zoi$hre)))
zoi_isf <- subset(zoi, (!is.na(zoi$isf)))
zoi_psf <- subset(zoi, (!is.na(zoi$psf)))

# Conversion en objet de type SpatialPolygonsDataFrame pour le package SPGWR
zoi_bsf_spatial <-as(zoi_bsf, 'Spatial')
zoi_hre_spatial <-as(zoi_hre, 'Spatial')
zoi_isf_spatial <-as(zoi_isf, 'Spatial')
zoi_psf_spatial <-as(zoi_psf, 'Spatial')

summary(zoi_bsf_spatial)
summary(zoi_hre_spatial)
summary(zoi_isf_spatial)
summary(zoi_psf_spatial)

##### GWR BSF TEB 1 to TEB 6 #####

# Réalisation d'une régression linéaire (lm) simple
ml_BSF_TEB1 <-lm(zoi_bsf_spatial$bsf~zoi_bsf_spatial$TEB_T1)
ml_BSF_TEB2 <-lm(zoi_bsf_spatial$bsf~zoi_bsf_spatial$TEB_T2)
ml_BSF_TEB3 <-lm(zoi_bsf_spatial$bsf~zoi_bsf_spatial$TEB_T3)
ml_BSF_TEB4 <-lm(zoi_bsf_spatial$bsf~zoi_bsf_spatial$TEB_T4)
ml_BSF_TEB5 <-lm(zoi_bsf_spatial$bsf~zoi_bsf_spatial$TEB_T5)
ml_BSF_TEB6 <-lm(zoi_bsf_spatial$bsf~zoi_bsf_spatial$TEB_T6)

# Summaries
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
zoi_bsf_spatial$residus_1 <- ml_BSF_TEB1$residuals
zoi_bsf_spatial$residus_2 <- ml_BSF_TEB2$residuals
zoi_bsf_spatial$residus_3 <- ml_BSF_TEB3$residuals
zoi_bsf_spatial$residus_4 <- ml_BSF_TEB4$residuals
zoi_bsf_spatial$residus_5 <- ml_BSF_TEB5$residuals
zoi_bsf_spatial$residus_6 <- ml_BSF_TEB6$residuals


# Histogramme des fréquences des résidus
hist(zoi_bsf_spatial$residus_1, col="red", main="Histogram of BSF TEB T1 Residuals", xlab="residual range", xlim=c(-0.6,0.6),breaks=40)
hist(zoi_bsf_spatial$residus_2, col="red", main="Histogram of BSF TEB T2 Residuals", xlab="residual range", xlim=c(-0.6,0.6),breaks=40)
hist(zoi_bsf_spatial$residus_3, col="red", main="Histogram of BSF TEB T3 Residuals", xlab="residual range", xlim=c(-0.6,0.6),breaks=40)
hist(zoi_bsf_spatial$residus_4, col="red", main="Histogram of BSF TEB T4 Residuals", xlab="residual range", xlim=c(-0.6,0.6),breaks=40)
hist(zoi_bsf_spatial$residus_5, col="red", main="Histogram of BSF TEB T5 Residuals", xlab="residual range", xlim=c(-0.6,0.6),breaks=40)
hist(zoi_bsf_spatial$residus_6, col="red", main="Histogram of BSF TEB T6 Residuals", xlab="residual range", xlim=c(-0.6,0.6),breaks=40)

# Réglage des proportions, dimensions et marges du fichier image
# figdim <- getFigDim(x = zoi_bsf_spatial, width = 800, mar = c(0,0,1.2,0), res = 100)
# png(filename = "BSF GWR residuals TEB T1.png", width = figdim[1], height = figdim[2], res = 100)
# par(mar = c(0,0,1.2,0))

# Discrétisation par les quantilesen  6 classes
qt6<-classIntervals(var=zoi_bsf_spatial$residus_1, n=6)
# Deplacer le zéro
qt6$brks[4] <- 0

# Cartographie thématique des résidus, par quantiles et palette autour de zéro
couleurs<-carto.pal(pal1 = "green.pal", n1 = 3, pal2 = "red.pal", n2 = 3)
choroLayer(spdf=zoi_bsf_spatial, df=zoi_bsf_spatial@data, var="residus_1", breaks=qt6$brks, col=couleurs, border=FALSE,
           legend.pos = "topleft", legend.values.rnd = 2, legend.title.txt = "residuals", legend.frame = FALSE)

layoutLayer("BSF~TEB T1 Meso-NH Residuals", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Building data:IGN/BD Topo, Météo France", frame = F, scale = 5)

# Habillage avec superposition par les quartiers et communes de l'agglo.
plot(zoi$geometry, col = NA, border = FALSE , add = TRUE)

north(pos = "topright")
dev.off()

# Recherche de la bande passante en utilisant une méthode de cross-validation basée sur une fonction gaussienne.
bp <- gwr.sel(zoi_bsf_spatial$bsf~zoi_bsf_spatial$TEB_T1, data = zoi_bsf_spatial, method = "cv")
bp

# Calcul de la GWR avec cette bp (vrai calcul fait par Paul)
g_bsf <- gwr(zoi_bsf_spatial$bsf~zoi_bsf_spatial$TEB_T1, data = zoi_bsf_spatial, bandwidth = bp, hatmatrix = TRUE, se.fit = TRUE)

# Affichage des résultats
g_bsf

# g_bsf <- g (import de calcul fait par Paul et changement du nom de variable)
# rm(g) (pour ne pas le confondre avec le g de la PSF)

# Sockage des résultats dans une variable, puis dans les données attributaires
rg_bsf<-as.data.frame(g$SDF)
zoi_bsf_spatial$rg_bsf=rg_bsf$zoi_bsf_spatial.TEB_T1

# Préparation de la carte thématique : couleurs et discrétisations Jenks en 6 classes
hist(zoi_bsf_spatial$rg_bsf, col="red", main = "Gaussian Process Regression Model of the BSF (GWR)", xlab="coef. distribution", breaks=40)
fish_jks7<-classIntervals(zoi_bsf_spatial$rg_bsf, style="fisher", n=7)
# Deplacer le zéro
fish_jks7$brks[4] <- 0 
hist(zoi_bsf_spatial$rg_bsf, breaks=fish_jks7$brks)
# jks7$brks<-append(jks7$brks, 0, after=1) => empeche choroLayer de foncionner (elle pense ça rajoute des NAs)
# hist(zoi_psf_spatial$rg_psf, breaks=jks7$brks)
couleurs<-carto.pal(pal1 = "kaki.pal", n1 = 3, pal2 = "red.pal", n2 = 5)

# Dessin de la carte
choroLayer(spdf=zoi_bsf_spatial, df=zoi_bsf_spatial@data, var="rg_bsf", breaks=fish_jks7$brks, col=couleurs, border=FALSE, lwd=0.1, 
           legend.values.rnd = 2, legend.pos="topleft", legend.title.txt = "coefs.")

layoutLayer("BSF~TEB T1 Meso-NH Gaussian Process Regression (GWR) : coefs, non-adapted BW", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Building data:IGN/BD Topo, Météo France", frame = F, scale = 5)

# Habillage avec superposition par les USR du maillage de MApUCE.
plot(zoi_spatial, col = NA, border = FALSE, add = TRUE)

north(pos = "topright")
dev.off()

# Recherche d'une BP adaptative (avec le package GWmodel pour gagner du temps)
dm_bsf <- gw.dist(sp::coordinates(zoi_bsf_spatial))

bpa_bsf<- bw.gwr(bsf~TEB_T1, data = zoi_bsf_spatial, approach = "AIC", adaptive = T, dMat = dm_bsf)

# Affichage des résultats
bpa_bsf

# Recalcul de la GWR avec cette nouvelle BP
gwr2_bsf <-gwr.basic(bsf~TEB_T1, data = zoi_bsf_spatial, bw = bpa_bsf, adaptive = T)
gwr2_bsf

# Cartographie des coefficients adaptatifs
aic_bsf<-as.data.frame(gwr2_bsf$SDF)
zoi_bsf_spatial$aic_bsf=aic_bsf$TEB_T1

# Préparation de la carte thématique : couleurs et discrétisations Jenks en 6 classes
hist(zoi_bsf_spatial$aic_bsf, col="red", main = "Adapted BW model of the BSF - AIC corrected  (GWR)", xlab="coef. distribution", xlim= c(-10, 10), breaks=40)
fish_jks7<-classIntervals(zoi_bsf_spatial$aic_bsf, style="fisher", n=7)
# Deplacer le zéro
fish_jks7$brks[4] <- 0
hist(zoi_bsf_spatial$aic_bsf, breaks=fish_jks7$brks)

#couleurs<-carto.pal(pal1 = "harmo.pal", n1 = 6, pal2 = "turquoise.pal", n2 = 2)
couleurs<-carto.pal(pal1 = "kaki.pal", n1 = 3, pal2 = "red.pal", n2 = 4)

# Dessin de la carte
choroLayer(spdf=zoi_bsf_spatial, df=zoi_bsf_spatial@data, var="aic_bsf", breaks=fish_jks7$brks, col=couleurs, border=FALSE, lwd=0.1, 
           legend.values.rnd = 2, legend.pos="topleft", legend.title.txt = "coefs.")

layoutLayer("BSF~TEB T1 Meso-NH AIC corrected regression (GWR) : coefs, adapted BW", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Building data:IGN/BD Topo, Météo France", frame = F, scale = 5)

# Habillage avec superposition par les USR du maillage de MApUCE.
plot(zoi_spatial, col = NA, border = F, add = TRUE)

north(pos = "topright")
dev.off()



##### GWR HRE TEB 1 to TEB 6 #####

# Réalisation d'une régression linéaire (lm) simple
ml_HRE_TEB1 <-lm(zoi_hre_spatial$hre~zoi_hre_spatial$TEB_T1)
ml_HRE_TEB2 <-lm(zoi_hre_spatial$hre~zoi_hre_spatial$TEB_T2)
ml_HRE_TEB3 <-lm(zoi_hre_spatial$hre~zoi_hre_spatial$TEB_T3)
ml_HRE_TEB4 <-lm(zoi_hre_spatial$hre~zoi_hre_spatial$TEB_T4)
ml_HRE_TEB5 <-lm(zoi_hre_spatial$hre~zoi_hre_spatial$TEB_T5)
ml_HRE_TEB6 <-lm(zoi_hre_spatial$hre~zoi_hre_spatial$TEB_T6)

# ml_BSF_TEB1 <-lm(zoi$bsf~zoi_spatial$TEB_T1)
# ml_BSF_TEB2 <-lm(zoi$bsf~zoi_spatial$TEB_T2)
# ml_BSF_TEB3 <-lm(zoi$bsf~zoi_spatial$TEB_T3)
# ml_BSF_TEB4 <-lm(zoi$bsf~zoi_spatial$TEB_T4)
# ml_BSF_TEB5 <-lm(zoi$bsf~zoi_spatial$TEB_T5)
# ml_BSF_TEB6 <-lm(zoi$bsf~zoi_spatial$TEB_T6)

summary(ml_HRE_TEB1)
summary(ml_HRE_TEB2)
summary(ml_HRE_TEB3)
summary(ml_HRE_TEB4)
summary(ml_HRE_TEB5)
summary(ml_HRE_TEB6)

# Dessin du graphique de corrélation entre les deux variables
plot(zoi_hre_spatial$hre~zoi_hre_spatial$TEB_T1, xlab = "TEB_T1", ylab = "HRE range in metres")
abline(ml_HRE_TEB1, col="red")
plot(zoi_hre_spatial$hre~zoi_hre_spatial$TEB_T2, xlab = "TEB_T2", ylab = "HRE range in metres")
abline(ml_HRE_TEB2, col="red")
plot(zoi_hre_spatial$hre~zoi_hre_spatial$TEB_T3, xlab = "TEB_T3", ylab = "HRE range in metres")
abline(ml_HRE_TEB3, col="red")
plot(zoi_hre_spatial$hre~zoi_hre_spatial$TEB_T4, xlab = "TEB_T4", ylab = "HRE range in metres")
abline(ml_HRE_TEB4, col="red")
plot(zoi_hre_spatial$hre~zoi_hre_spatial$TEB_T5, xlab = "TEB_T5", ylab = "HRE range in metres")
abline(ml_HRE_TEB5, col="red")
plot(zoi_hre_spatial$hre~zoi_hre_spatial$TEB_T6, xlab = "TEB_T6", ylab = "HRE range in metres")
abline(ml_HRE_TEB6, col="red")
# Stockage des résidus dans une variable du fond de carte
zoi_hre_spatial$residus_1 <- ml_HRE_TEB1$residuals
zoi_hre_spatial$residus_2 <- ml_HRE_TEB2$residuals
zoi_hre_spatial$residus_3 <- ml_HRE_TEB3$residuals
zoi_hre_spatial$residus_4 <- ml_HRE_TEB4$residuals
zoi_hre_spatial$residus_5 <- ml_HRE_TEB5$residuals
zoi_hre_spatial$residus_6 <- ml_HRE_TEB6$residuals

# Histogramme des fréquences des résidus
hist(zoi_hre_spatial$residus_1, col="purple", main="Histogram of HRE TEB T1 Residuals", xlab="residual range", xlim=c(-10,50),breaks=40)
hist(zoi_hre_spatial$residus_2, col="purple", main="Histogram of HRE TEB T2 Residuals", xlab="residual range", xlim=c(-10,50),breaks=40)
hist(zoi_hre_spatial$residus_3, col="purple", main="Histogram of HRE TEB T3 Residuals", xlab="residual range", xlim=c(-10,50),breaks=40)
hist(zoi_hre_spatial$residus_4, col="purple", main="Histogram of HRE TEB T4 Residuals", xlab="residual range", xlim=c(-10,50),breaks=40)
hist(zoi_hre_spatial$residus_5, col="purple", main="Histogram of HRE TEB T5 Residuals", xlab="residual range", xlim=c(-10,50),breaks=40)
hist(zoi_hre_spatial$residus_6, col="purple", main="Histogram of HRE TEB T6 Residuals", xlab="residual range", xlim=c(-10,50),breaks=40)

# Réglage des proportions, dimensions et marges du fichier image
# figdim <- getFigDim(x = zoi_bsf_spatial, width = 800, mar = c(0,0,1.2,0), res = 100)
# png(filename = "BSF GWR residuals TEB T1.png", width = figdim[1], height = figdim[2], res = 100)
# par(mar = c(0,0,1.2,0))

# Discrétisation par les quantilesen  6 classes
qt6<-classIntervals(var=zoi_hre_spatial$residus_1, n=6)
# Deplacer le zéro
qt6$brks[4] <- 0

# Cartographie thématique des résidus, par quantiles et palette autour de zéro
couleurs<-carto.pal(pal1 = "sand.pal", n1 = 3, pal2 = "purple.pal", n2 = 3)
choroLayer(spdf=zoi_hre_spatial, df=zoi_hre_spatial@data, var="residus_1", breaks=qt6$brks, col=couleurs, border=FALSE,
           legend.pos = "topleft", legend.values.rnd = 2, legend.title.txt = "residuals", legend.frame = FALSE)

layoutLayer("HRE~TEB T1 Meso-NH Residuals", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Building data:IGN/BD Topo, Météo France", frame = F, scale = 5)

# Habillage avec superposition par les quartiers et communes de l'agglo.
plot(zoi$geometry, col = NA, border = FALSE , add = TRUE)

north(pos = "topright")
dev.off()

# Recherche de la bande passante en utilisant une méthode de cross-validation basée sur une fonction gaussienne.
bp_hre <- gwr.sel(zoi_hre_spatial$hre~zoi_hre_spatial$TEB_T1, data = zoi_hre_spatial, method = "cv")
bp_hre

# Calcul de la GWR avec cette bp
g_hre <- gwr(zoi_hre_spatial$hre~zoi_hre_spatial$TEB_T1, data = zoi_hre_spatial, bandwidth = bp_hre, hatmatrix = TRUE, se.fit = TRUE)

# Affichage des résultats
g_hre

# Sockage des résultats dans une variable, puis dans les données attributaires
rg_hre<-as.data.frame(g_hre$SDF)
zoi_hre_spatial$rg_hre=rg_hre$zoi_hre_spatial.TEB_T1

# Préparation de la carte thématique : couleurs et discrétisations Jenks en 6 classes
hist(zoi_hre_spatial$rg_hre, col="purple", main = "Gaussian Process Regression Model of the HRE (GWR)", xlab="coef. distribution", xlim= c(-40, 80), breaks=40)
fish_jks7<-classIntervals(zoi_hre_spatial$rg_hre, style="fisher", n=7)
# Deplacer le zéro
# jks7$brks[4] <- 0 => empeche choroLayer de foncionner (elle pense ça rajoute des NAs)
hist(zoi_hre_spatial$rg_hre, breaks=fish_jks7$brks)

fish_jks7$brks[4] <- 0
# jks7$brks<-append(jks7$brks, 0, after=1) => empeche choroLayer de foncionner (elle pense ça rajoute des NAs)
# hist(zoi_psf_spatial$rg_psf, breaks=jks7$brks)
couleurs<-carto.pal(pal1 = "sand.pal", n1 = 3, pal2 = "purple.pal", n2 = 4)

# Dessin de la carte
choroLayer(spdf=zoi_hre_spatial, df=zoi_hre_spatial@data, var="rg_hre", breaks=fish_jks7$brks, col=couleurs, border=FALSE, lwd=0.1, 
           legend.values.rnd = 2, legend.pos="topleft", legend.title.txt = "coefs.")

layoutLayer("HRE~TEB T1 Meso-NH Gaussian Process Regression (GWR) : coefs, non-adapted BW", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Building data:IGN/BD Topo, Météo France", frame = F, scale = 5)

# Habillage avec superposition par les USR du maillage de MApUCE.
plot(zoi_spatial, col = NA, border = FALSE, add = TRUE)

north(pos = "topright")
dev.off()

# Recherche d'une BP adaptative
bpa_hre<-gwr.sel(zoi_hre_spatial$hre~zoi_hre_spatial$TEB_T1, data = zoi_hre_spatial, method = "cv", adapt = TRUE)

# Affichage des résultats
bpa_hre

# Recalcul de la GWR avec cette nouvelle BP
ga_hre <-gwr(zoi_hre_spatial$hre~zoi_hre_spatial$TEB_T1, data = zoi_hre_spatial, adapt = bpa_hre, hatmatrix = TRUE, se.fit = TRUE)

# Cartographie des coefficients adaptatifs
rga_hre<-as.data.frame(ga_hre$SDF)
zoi_hre_spatial$rga_hre=rga_hre$zoi_hre_spatial.TEB_T1

# Préparation de la carte thématique : couleurs et discrétisations Jenks en 6 classes
hist(zoi_hre_spatial$rga_hre, col="purple", main = "Adapted Gaussian Process Regression Model of the HRE (GWR)", xlab="coef. distribution", breaks=40)
fish_jks7<-classIntervals(zoi_hre_spatial$rga_hre, style="fisher", n=7)
# Deplacer le zéro
fish_jks7$brks[5] <- 0
hist(zoi_hre_spatial$rga_hre, breaks=fish_jks8$brks)
# jks7$brks<-append(jks7$brks, 0, after=1) => empeche choroLayer de foncionner (elle pense ça rajoute des NAs)
hist(zoi_hre_spatial$rg_hre, breaks=fish_jks7$brks)
#couleurs<-carto.pal(pal1 = "harmo.pal", n1 = 6, pal2 = "turquoise.pal", n2 = 2)
couleurs<-carto.pal(pal1 = "sand.pal", n1 = 4, pal2 = "purple.pal", n2 = 3)

# Dessin de la carte
choroLayer(spdf=zoi_hre_spatial, df=zoi_hre_spatial@data, var="rga_hre", breaks=fish_jks7$brks, col=couleurs, border=FALSE, lwd=0.1, 
           legend.values.rnd = 2, legend.pos="topleft", legend.title.txt = "coefs.")

layoutLayer("HRE~TEB T1 Meso-NH Gaussian Process Regression (GWR) : coefs, adapted BW", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Building data:IGN/BD Topo, Météo France", frame = F, scale = 5)

# Habillage avec superposition par les USR du maillage de MApUCE.
plot(zoi_spatial, col = NA, border = FALSE, add = TRUE)

north(pos = "topright")
dev.off()


##### GWR ISF TEB 1 to TEB 6 #####

# Réalisation d'une régression linéaire (lm) simple
ml_ISF_TEB1 <-lm(zoi_isf_spatial$isf~zoi_isf_spatial$TEB_T1)
ml_ISF_TEB2 <-lm(zoi_isf_spatial$isf~zoi_isf_spatial$TEB_T2)
ml_ISF_TEB3 <-lm(zoi_isf_spatial$isf~zoi_isf_spatial$TEB_T3)
ml_ISF_TEB4 <-lm(zoi_isf_spatial$isf~zoi_isf_spatial$TEB_T4)
ml_ISF_TEB5 <-lm(zoi_isf_spatial$isf~zoi_isf_spatial$TEB_T5)
ml_ISF_TEB6 <-lm(zoi_isf_spatial$isf~zoi_isf_spatial$TEB_T6)

# Summaries
summary(ml_ISF_TEB1)
summary(ml_ISF_TEB2)
summary(ml_ISF_TEB3)
summary(ml_ISF_TEB4)
summary(ml_ISF_TEB5)
summary(ml_ISF_TEB6)

# Dessin du graphique de corrélation entre les deux variables
plot(zoi_isf_spatial$isf~zoi_isf_spatial$TEB_T1, xlab = "TEB_T1", ylab = "ISF range")
abline(ml_ISF_TEB1, col="red")
plot(zoi_isf_spatial$isf~zoi_isf_spatial$TEB_T2, xlab = "TEB_T2", ylab = "ISF range")
abline(ml_ISF_TEB2, col="red")
plot(zoi_isf_spatial$isf~zoi_isf_spatial$TEB_T3, xlab = "TEB_T3", ylab = "ISF range")
abline(ml_ISF_TEB3, col="red")
plot(zoi_isf_spatial$isf~zoi_isf_spatial$TEB_T4, xlab = "TEB_T4", ylab = "ISF range")
abline(ml_ISF_TEB4, col="red")
plot(zoi_isf_spatial$isf~zoi_isf_spatial$TEB_T5, xlab = "TEB_T5", ylab = "ISF range")
abline(ml_ISF_TEB5, col="red")
plot(zoi_isf_spatial$isf~zoi_isf_spatial$TEB_T6, xlab = "TEB_T6", ylab = "ISF range")
abline(ml_ISF_TEB6, col="red")

# Stockage des résidus dans une variable du fond de carte
zoi_isf_spatial$residus_1 <- ml_ISF_TEB1$residuals
zoi_isf_spatial$residus_2 <- ml_ISF_TEB2$residuals
zoi_isf_spatial$residus_3 <- ml_ISF_TEB3$residuals
zoi_isf_spatial$residus_4 <- ml_ISF_TEB4$residuals
zoi_isf_spatial$residus_5 <- ml_ISF_TEB5$residuals
zoi_isf_spatial$residus_6 <- ml_ISF_TEB6$residuals


# Histogramme des fréquences des résidus
hist(zoi_isf_spatial$residus_1, col="grey", main="Histogram of ISF TEB T1 Residuals", xlab="residual range", xlim=c(-0.2,0.8),breaks=40)
hist(zoi_isf_spatial$residus_2, col="grey", main="Histogram of ISF TEB T2 Residuals", xlab="residual range", xlim=c(-0.2,0.8),breaks=40)
hist(zoi_isf_spatial$residus_3, col="grey", main="Histogram of ISF TEB T3 Residuals", xlab="residual range", xlim=c(-0.2,0.8),breaks=40)
hist(zoi_isf_spatial$residus_4, col="grey", main="Histogram of ISF TEB T4 Residuals", xlab="residual range", xlim=c(-0.2,0.8),breaks=40)
hist(zoi_isf_spatial$residus_5, col="grey", main="Histogram of ISF TEB T5 Residuals", xlab="residual range", xlim=c(-0.2,0.8),breaks=40)
hist(zoi_isf_spatial$residus_6, col="grey", main="Histogram of ISF TEB T6 Residuals", xlab="residual range", xlim=c(-0.2,0.8),breaks=40)

# Réglage des proportions, dimensions et marges du fichier image
# figdim <- getFigDim(x = zoi_bsf_spatial, width = 800, mar = c(0,0,1.2,0), res = 100)
# png(filename = "BSF GWR residuals TEB T1.png", width = figdim[1], height = figdim[2], res = 100)
# par(mar = c(0,0,1.2,0))

# Discrétisation par les quantilesen  6 classes
qt6<-classIntervals(var=zoi_isf_spatial$residus_4, n=6)
# Deplacer le zéro
qt6$brks[5] <- 0

# Cartographie thématique des résidus, par quantiles et palette autour de zéro
couleurs<-carto.pal(pal1 = "turquoise.pal", n1 = 4, pal2 = "wine.pal", n2 = 2)
choroLayer(spdf=zoi_isf_spatial, df=zoi_isf_spatial@data, var="residus_4", breaks=qt6$brks, col=couleurs, border=FALSE,
           legend.pos = "topleft", legend.values.rnd = 2, legend.title.txt = "residuals", legend.frame = FALSE)

layoutLayer("ISF~TEB T4 Meso-NH Residuals", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Impervious data: Contributors OSM, Météo France", frame = F, scale = 5)

# Habillage avec superposition par les quartiers et communes de l'agglo.
plot(zoi$geometry, col = NA, border = FALSE , add = TRUE)

north(pos = "topright")
dev.off()

# Recherche de la bande passante en utilisant une méthode de cross-validation basée sur une fonction gaussienne.
bp_isf <- gwr.sel(zoi_isf_spatial$isf~zoi_isf_spatial$TEB_T1, data = zoi_isf_spatial, method = "cv")
bp_isf

# Calcul de la GWR avec cette bp
g_isf <- gwr(zoi_isf_spatial$isf~zoi_isf_spatial$TEB_T1, data = zoi_isf_spatial, bandwidth = bp_isf, hatmatrix = TRUE, se.fit = TRUE)

# Affichage des résultats
g_isf

# Sockage des résultats dans une variable, puis dans les données attributaires
rg_isf<-as.data.frame(g_isf$SDF)
zoi_isf_spatial$rg_isf=rg_isf$zoi_isf_spatial.TEB_T1

# Préparation de la carte thématique : couleurs et discrétisations Jenks en 6 classes
hist(zoi_isf_spatial$rg_isf, col="grey", main = "Gaussian Process Regression Model of the ISF (GWR)", xlab="coef. distribution", xlim=c(-10,5), breaks=40)
fish_jks7<-classIntervals(zoi_isf_spatial$rg_isf, style="fisher", n=7)
# Deplacer le zéro
fish_jks7$brks[5] <- 0 
hist(zoi_isf_spatial$rg_isf, breaks=fish_jks7$brks)
# jks7$brks<-append(jks7$brks, 0, after=1) => empeche choroLayer de foncionner (elle pense ça rajoute des NAs)
hist(zoi_psf_spatial$rg_psf, breaks=fish_jks7$brks)
couleurs<-carto.pal(pal1 = "turquoise.pal", n1 = 4, pal2 = "wine.pal", n2 = 3)

# Dessin de la carte
choroLayer(spdf=zoi_isf_spatial, df=zoi_isf_spatial@data, var="rg_isf", breaks=fish_jks7$brks, col=couleurs, border=FALSE, lwd=0.1, 
           legend.values.rnd = 2, legend.pos="topleft", legend.title.txt = "coefs.")

layoutLayer("ISF~TEB T1 Meso-NH Gaussian Process Regression (GWR) : coefs, non adapted BW", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Impervious data: Contributors OSM, Météo France", frame = F, scale = 5)

# Habillage avec superposition par les USR du maillage de MApUCE.
plot(zoi_spatial, col = NA, border = FALSE , add = TRUE)

north(pos = "topright")
dev.off()

# Recherche d'une BP adaptative (score : 88.49)
bpa_isf<-gwr.sel(zoi_isf_spatial$isf~zoi_isf_spatial$TEB_T1, data = zoi_isf_spatial, method = "cv", adapt = TRUE)

# Affichage des résultats
bpa_isf

# Choix d’une BPA de 94 points
#bpa_psf <-(94/2647)

# Recalcul de la GWR avec cette nouvelle BP
ga_isf <-gwr(zoi_isf_spatial$isf~zoi_isf_spatial$TEB_T1, data = zoi_isf_spatial, adapt = bpa_isf, hatmatrix = TRUE, se.fit = TRUE)

# Cartographie des coefficients adaptatifs
rga_isf<-as.data.frame(ga_isf$SDF)
zoi_isf_spatial$rga_isf=rga_isf$zoi_isf_spatial.TEB_T1

# Préparation de la carte thématique : couleurs et discrétisations Jenks en 6 classes
hist(zoi_isf_spatial$rga_isf, col="grey", main = "Adapted Gaussian Process Regression Model of the ISF (GWR)", xlab="coef. distribution", xlim=c(-50,20), breaks=40)
fish_jks7<-classIntervals(zoi_isf_spatial$rga_isf, style="fisher", n=7)
# Deplacer le zéro
fish_jks7$brks[5] <- 0 
hist(zoi_isf_spatial$rga_isf, breaks=fish_jks7$brks)
# jks7$brks<-append(jks7$brks, 0, after=1) => empeche choroLayer de foncionner (elle pense ça rajoute des NAs)
#hist(zoi_psf_spatial$rg_psf, breaks=jks7$brks)
#couleurs<-carto.pal(pal1 = "harmo.pal", n1 = 6, pal2 = "turquoise.pal", n2 = 2)
couleurs<-carto.pal(pal1 = "turquoise.pal", n1 = 4, pal2 = "wine.pal", n2 = 3)

# Dessin de la carte
choroLayer(spdf=zoi_isf_spatial, df=zoi_isf_spatial@data, var="rga_isf", breaks=fish_jks7$brks, col=couleurs, border=FALSE, lwd=0.1, 
           legend.values.rnd = 2, legend.pos="topleft", legend.title.txt = "coefs.")

layoutLayer("ISF~TEB T1 Meso-NH Gaussian Process Regression (GWR) : coefs, adapted BW", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Impervious data: Contributors OSM, Météo France", frame = F, scale = 5)

# Habillage avec superposition par les USR du maillage de MApUCE.
plot(zoi_spatial, col = NA, border = FALSE, add = TRUE)

north(pos = "topright")
dev.off()


##### GWR PSF TEB 1 to TEB 6 #####

# Réalisation d'une régression linéaire (lm) simple
ml_PSF_TEB1 <-lm(zoi_psf_spatial$psf~zoi_psf_spatial$TEB_T1)
ml_PSF_TEB2 <-lm(zoi_psf_spatial$psf~zoi_psf_spatial$TEB_T2)
ml_PSF_TEB3 <-lm(zoi_psf_spatial$psf~zoi_psf_spatial$TEB_T3)
ml_PSF_TEB4 <-lm(zoi_psf_spatial$psf~zoi_psf_spatial$TEB_T4)
ml_PSF_TEB5 <-lm(zoi_psf_spatial$psf~zoi_psf_spatial$TEB_T5)
ml_PSF_TEB6 <-lm(zoi_psf_spatial$psf~zoi_psf_spatial$TEB_T6)

# Summaries
summary(ml_PSF_TEB1)
summary(ml_PSF_TEB2)
summary(ml_PSF_TEB3)
summary(ml_PSF_TEB4)
summary(ml_PSF_TEB5)
summary(ml_PSF_TEB6)

# Dessin du graphique de corrélation entre les deux variables
plot(zoi_psf_spatial$psf~zoi_psf_spatial$TEB_T1, xlab = "TEB_T1", ylab = "PSF range")
abline(ml_PSF_TEB1, col="red")
plot(zoi_psf_spatial$psf~zoi_psf_spatial$TEB_T2, xlab = "TEB_T2", ylab = "PSF range")
abline(ml_PSF_TEB2, col="red")
plot(zoi_psf_spatial$psf~zoi_psf_spatial$TEB_T3, xlab = "TEB_T3", ylab = "PSF range")
abline(ml_PSF_TEB3, col="red")
plot(zoi_psf_spatial$psf~zoi_psf_spatial$TEB_T4, xlab = "TEB_T4", ylab = "PSF range")
abline(ml_PSF_TEB4, col="red")
plot(zoi_psf_spatial$psf~zoi_psf_spatial$TEB_T5, xlab = "TEB_T5", ylab = "PSF range")
abline(ml_PSF_TEB5, col="red")
plot(zoi_psf_spatial$psf~zoi_psf_spatial$TEB_T6, xlab = "TEB_T6", ylab = "PSF range")
abline(ml_PSF_TEB6, col="red")

# Stockage des résidus dans une variable du fond de carte
zoi_psf_spatial$residus_1 <- ml_PSF_TEB1$residuals
zoi_psf_spatial$residus_2 <- ml_PSF_TEB2$residuals
zoi_psf_spatial$residus_3 <- ml_PSF_TEB3$residuals
zoi_psf_spatial$residus_4 <- ml_PSF_TEB4$residuals
zoi_psf_spatial$residus_5 <- ml_PSF_TEB5$residuals
zoi_psf_spatial$residus_6 <- ml_PSF_TEB6$residuals


# Histogramme des fréquences des résidus
hist(zoi_psf_spatial$residus_1, col="green", main="Histogram of PSF TEB T1 Residuals", xlab="residual range", xlim=c(-0.4,1),breaks=40)
hist(zoi_psf_spatial$residus_2, col="green", main="Histogram of PSF TEB T2 Residuals", xlab="residual range", xlim=c(-0.4,1),breaks=40)
hist(zoi_psf_spatial$residus_3, col="green", main="Histogram of PSF TEB T3 Residuals", xlab="residual range", xlim=c(-0.4,1),breaks=40)
hist(zoi_psf_spatial$residus_4, col="green", main="Histogram of PSF TEB T4 Residuals", xlab="residual range", xlim=c(-0.4,1),breaks=40)
hist(zoi_psf_spatial$residus_5, col="green", main="Histogram of PSF TEB T5 Residuals", xlab="residual range", xlim=c(-0.4,1),breaks=40)
hist(zoi_psf_spatial$residus_6, col="green", main="Histogram of PSF TEB T6 Residuals", xlab="residual range", xlim=c(-0.4,1),breaks=40)

# Réglage des proportions, dimensions et marges du fichier image
# figdim <- getFigDim(x = zoi_bsf_spatial, width = 800, mar = c(0,0,1.2,0), res = 100)
# png(filename = "BSF GWR residuals TEB T1.png", width = figdim[1], height = figdim[2], res = 100)
# par(mar = c(0,0,1.2,0))

# Discrétisation par les quantilesen  6 classes
qt6<-classIntervals(var=zoi_psf_spatial$residus_1, n=6)
# Deplacer le zéro
qt6$brks[5] <- 0

# Cartographie thématique des résidus, par quantiles et palette autour de zéro
couleurs<-carto.pal(pal1 = "harmo.pal", n1 = 4, pal2 = "turquoise.pal", n2 = 2)
choroLayer(spdf=zoi_psf_spatial, df=zoi_psf_spatial@data, var="residus_1", breaks=qt6$brks, col=couleurs, border=FALSE,
           legend.pos = "topleft", legend.values.rnd = 2, legend.title.txt = "residuals", legend.frame = FALSE, legend.nodata = TRUE)

layoutLayer("PSF~TEB T1 Meso-NH Residuals", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Impervious data: Contributors OSM, Météo France", frame = F, scale = 5)

# Habillage avec superposition par les USR du maillage de MApUCE.
plot(zoi$geometry, col = NA, border = FALSE , add = TRUE)

north(pos = "topright")
dev.off()

# Recherche de la bande passante en utilisant une méthode de cross-validation basée sur une fonction gaussienne.
bp_psf <- gwr.sel(zoi_psf_spatial$psf~zoi_psf_spatial$TEB_T1, data = zoi_psf_spatial, method = "cv")
bp_psf

# Calcul de la GWR avec cette bp
g <- gwr(zoi_psf_spatial$psf~zoi_psf_spatial$TEB_T1, data = zoi_psf_spatial, bandwidth = bp_psf, hatmatrix = TRUE, se.fit = TRUE)

# Affichage des résultats
g

# Sockage des résultats dans une variable, puis dans les données attributaires
rg_psf<-as.data.frame(g$SDF)
zoi_psf_spatial$rg_psf=rg_psf$zoi_psf_spatial.TEB_T1

# Préparation de la carte thématique : couleurs et discrétisations Jenks en 6 classes
hist(zoi_psf_spatial$rg_psf, col="green", main = "Gaussian Process Regression Model of the PSF (GWR)", xlab="residual range", xlim=c(-2,2), breaks=40)
fish_jks7<-classIntervals(zoi_psf_spatial$rg_psf, style="fisher", n=7)
# Deplacer le zéro
fish_jks7$brks[5] <- 0 
hist(zoi_psf_spatial$rg_psf, breaks=jks7$brks)
# jks7$brks<-append(jks7$brks, 0, after=1) => empeche choroLayer de foncionner (elle pense ça rajoute des NAs)
hist(zoi_psf_spatial$rg_psf, breaks=jks7$brks)
couleurs<-carto.pal(pal1 = "harmo.pal", n1 = 4, pal2 = "turquoise.pal", n2 = 3)

# Dessin de la carte
choroLayer(spdf=zoi_psf_spatial, df=zoi_psf_spatial@data, var="rg_psf", breaks=fish_jks7$brks, col=couleurs, border=FALSE, lwd=0.1, 
           legend.values.rnd = 2, legend.pos="topleft", legend.title.txt = "coefs.")

layoutLayer("PSF~TEB T1 Meso-NH Gaussian Process Regression (GWR) : coefs, non adapted BW", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Pervious data: Contributors OSM, Météo France", frame = F, scale = 5)

# Habillage avec superposition par les USR du maillage de MApUCE.
plot(zoi_spatial, col = NA, border = "grey40" , add = TRUE)

north(pos = "topright")
dev.off()

# Recherche d'une BP adaptative
bpa_psf<-gwr.sel(zoi_psf_spatial$psf~zoi_psf_spatial$TEB_T1, data = zoi_psf_spatial, method = "cv", adapt = TRUE)

# Affichage des résultats
bpa_psf

# Choix d’une BPA de 94 points
bpa_psf <-(94/2647)

# Recalcul de la GWR avec cette nouvelle BP
ga_psf <-gwr(zoi_psf_spatial$psf~zoi_psf_spatial$TEB_T1, data = zoi_psf_spatial, adapt = bpa_psf, hatmatrix = TRUE, se.fit = TRUE)

# Cartographie des coefficients adaptatifs
rga_psf<-as.data.frame(ga_psf$SDF)
zoi_psf_spatial$rga_psf=rga_psf$zoi_psf_spatial.TEB_T1

# Préparation de la carte thématique : couleurs et discrétisations Jenks en 6 classes
hist(zoi_psf_spatial$rga_psf, col="green", main = "Adapted Gaussian Process Regression Model of the PSF (GWR)", xlab="coef. distribution", breaks=40)
fish_jks7<-classIntervals(zoi_psf_spatial$rga_psf, style="fisher", n=7)
# Deplacer le zéro
fish_jks7$brks[6] <- 0 
hist(zoi_psf_spatial$rga_psf, breaks=fish_jks7$brks)
# Régler les couleurs
# couleurs<-carto.pal(pal1 = "harmo.pal", n1 = 5, pal2 = "turquoise.pal", n2 = 2)
couleurs<-carto.pal(pal1 = "turquoise.pal", n1 = 7)

# Dessin de la carte
choroLayer(spdf=zoi_psf_spatial, df=zoi_psf_spatial@data, var="rga_psf", breaks=fish_jks7$brks, col=couleurs, border=FALSE, lwd=0.1, 
           legend.values.rnd = 2, legend.pos="topleft", legend.title.txt = "coefs.")

layoutLayer("PSF~TEB T1 Meso-NH Gaussian Process Regression (GWR) : coefs, adapted BW", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Pervious data: Contributors OSM, Météo France", frame = F, scale = 5)

# Habillage avec superposition par les USR du maillage de MApUCE.
plot(zoi_spatial, col = NA, border = "grey40", add = TRUE)

north(pos = "topright")
dev.off()
