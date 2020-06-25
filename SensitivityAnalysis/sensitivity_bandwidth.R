# Load necessary packages 
library(cartography)
library(sf)
library(classInt)
library(spgwr)
library(GWmodel)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

# Set working directory 
setwd("~/encadrement/stage_luke/indicateurs_UHI/")
# Load zone of interest
zoi <- read_sf("./gwr_zoi_mesoNH_strat_1/zoi_mesoNH_strat1.geojson")
# Fix the projection
st_crs(zoi) <- 2154
# Conversion of ZOI to a spatial object



#variable of interest 
voi <-  "bsf"

#remove NAs in voi
zoi <-  subset.data.frame(zoi,!is.na(zoi[,voi, drop=T]))
anyNA(zoi[,voi])
zoi_spatial<-as(zoi, 'Spatial')

# Conversion of indicators to SpatialPolygonsDataFrame objects for the package GWmodel


# Creation of a distrance matrix for the BSF spatial object
dm <- gw.dist(sp::coordinates(zoi_spatial))

# Bandwidth selection using the cross validation method and the Gaussian kernel
bp_auto_CV <- bw.gwr(bsf~TEB_T1, data = zoi_spatial, approach = "CV", kernel = "gaussian", dMat = dm, adaptive = F)
bp_auto_CV

bp_auto_AIC <- bw.gwr(bsf~TEB_T1, data = zoi_spatial, approach = "AIC", kernel = "gaussian", dMat = dm, adaptive = F)
bp_auto_AIC


kernels <-  c("gaussian", "exponential", "bisquare", "tricube", "boxcar")
approaches <-  c("CV", "AIC")

combinations <-  expand.grid(kernels,approaches) 
names(combinations) <-  c("kernel", "approach")


#perform bandwidth computation for each combination of parameter
combinations$bandwidth <-  NA

#library to map a function on several parameters list 
library(purrr)

ff <-  function(ker, app){
  cat("#####",ker, "  " ,app, "\n")
  bw.gwr(bsf~TEB_T1, data = zoi_spatial, approach = app, kernel = ker , dMat = dm, adaptive = F)
}

#mapping of the bandwidth function
xx <-  map2(combinations$kernel, combinations$approach,ff)
combinations$bandwidth <- unlist(xx)
rm(xx)


#function to perform gwr
makegwr <-  function(ker, bandw){
  cat("#####",ker, "  " ,bandw, "\n")
  gwr.basic(bsf~TEB_T1, data = zoi_spatial, bw = bandw, kernel = ker, adaptive = F, dMat = dm)
  }



combinations$R2 <-  NA
combinations$R2adj <-  NA
combinations$RSS <-  NA
combinations$AIC <- NA
combinations$AICC <- NA


for(i in 1:nrow(combinations)){
   cat(i, "\n")
  ker <-  combinations$kernel[i] %>% as.character()
  bandw <-  combinations$bandwidth[i]
  try(
    {
    result <-makegwr(ker, bandw)
  
  combinations$RSS[i] <-  result$GW.diagnostic$RSS.gw
  combinations$R2[i] <-  result$GW.diagnostic$gw.R2
  combinations$R2adj[i] <-  result$GW.diagnostic$gwR2.adj
  combinations$AIC[i] <-  result$GW.diagnostic$AIC
  combinations$AICC[i] <-  result$GW.diagnostic$AICc
  })
}

do_it <- function(){
bws <-  seq(from=50, to=1300, by=50)
  for (b in bws){
    cat("bandwidth=",b, "m\n")
    ker <-  lili$kernel %>% as.character()
    try(
      {
        result <-makegwr(ker, b)
        lili$bandwidth <- b
        lili$RSS <-  result$GW.diagnostic$RSS.gw
        lili$R2 <-  result$GW.diagnostic$gw.R2
        lili$R2adj <-  result$GW.diagnostic$gwR2.adj
        lili$AIC <-  result$GW.diagnostic$AIC
        lili$AICC <-  result$GW.diagnostic$AICc
    
  })
  combinations <- rbind(combinations,lili)
}
}


for(i in 1:10){
  lili <-  combinations[6,]
  do_it()
  
}

write.csv(combinations, "./SensitivityAnalysis/AS_bw_gwr.csv")


yy
# Sockage des résultats dans une variable, puis dans les données attributaires
g_bsf<-as.data.frame(g_bsf$SDF)
zoi_bsf_spatial$rg_bsf=rg_bsf$TEB_T1

# Histogram of the Coefficient values relating to the bsf~TEB_T1
qplot(zoi_bsf_spatial$rg_bsf, geom = "histogram", binwidth = 0.15, main = "GWR - BSF~TEB_T1 Coef. distribution", 
      xlab = "coef. distribution", ylab = "coef. count", fill=I("red"), col=I("black"),
      alpha=(.2), xlim=c(-3, 3))

# Preparation of fisher-jenks classification for the map legend 
fish_jks8<-classIntervals(zoi_bsf_spatial$rg_bsf, style="fisher", n=8)
# Deplacer le zéro
fish_jks8$brks[5] <- 0 
hist(zoi_bsf_spatial$rg_bsf, breaks=fish_jks8$brks)
# Colour prep
couleurs<-carto.pal(pal1 = "taupe.pal", n1 = 4, pal2 = "red.pal", n2 = 4)

# Dessin de la carte
choroLayer(spdf=zoi_bsf_spatial, df=zoi_bsf_spatial@data, var="rg_bsf", breaks=fish_jks8$brks, col=couleurs, border=FALSE, lwd=0.1, 
           legend.values.rnd = 2, legend.pos="topleft", legend.title.txt = "coefs.")

layoutLayer("BSF~TEB T1 Meso-NH Gaussian Process Regression (GWR) : coefs, non-adapted BW", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Building data:IGN/BD Topo, Météo France", frame = F, scale = 5)

# Habillage avec superposition par les USR du maillage de MApUCE.
plot(zoi$geometry, col = NA, border = F, add = TRUE)

north(pos = "topright")
dev.off()

# Recherche d'une BP adaptative
bpa_bsf<-bw.gwr(bsf~TEB_T1, data = zoi_bsf_spatial, approach = "AIC", adaptive = T, dMat = dm_bsf)

# Affichage des résultats
bpa_bsf

# Recalcul de la GWR avec cette nouvelle BP
gwr2_bsf <- gwr.basic(bsf~TEB_T1, data = zoi_bsf_spatial, bw = bpa_bsf, adaptive = T)

# Cartographie des coefficients adaptatifs
rga_bsf<-as.data.frame(gwr2_bsf$SDF)
zoi_bsf_spatial$rga_bsf=rga_bsf$TEB_T1

# Histogram of the Coefficient values relating to the bsf~TEB_T1
qplot(zoi_bsf_spatial$rga_bsf, geom = "histogram", binwidth = 1, main = "GWR - BSF~TEB_T1 adapted bandwidth Coef. distribution", 
      xlab = "coef. distribution", ylab = "coef. count", fill=I("red"), col=I("black"),
      alpha=(.2))

# Preparation of fisher-jenks classification for the map legend 
fish_jks7<-classIntervals(zoi_bsf_spatial$rga_bsf, style="fisher", n=7)
# Deplacer le zéro
fish_jks7$brks[4] <- 0 
# Prep of quantile legend values (gives a very different but biased presentation of the data)
qt7<-classIntervals(zoi_bsf_spatial$rga_bsf, style="quantile", n=7)
# Deplacer le zéro
qt7$brks[4] <- 0
# Histograms 
hist(zoi_bsf_spatial$rga_bsf, breaks=fish_jks7$brks)
hist(zoi_bsf_spatial$rga_bsf, breaks=qt7$brks)

#couleurs<-carto.pal(pal1 = "harmo.pal", n1 = 6, pal2 = "turquoise.pal", n2 = 2)
# couleurs<-carto.pal(pal1 = "purple.pal", n1 = 3, pal2 = "red.pal", n2 = 4)
couleurs <- brewer.pal(7, "Accent")

# Dessin de la carte
choroLayer(spdf=zoi_bsf_spatial, df=zoi_bsf_spatial@data, var="rga_bsf", breaks=fish_jks7$brks, col=couleurs, border=FALSE, lwd=0.1, 
           legend.values.rnd = 2, legend.pos="topleft", legend.title.txt = "coefs.")

layoutLayer("BSF~TEB T1 Meso-NH Gaussian Process Regression (GWR) : coefs, adapted BW", tabtitle = T, postitle = "center",
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

# Summaries of the linear models
summary(ml_HRE_TEB1)
summary(ml_HRE_TEB2)
summary(ml_HRE_TEB3)
summary(ml_HRE_TEB4)
summary(ml_HRE_TEB5)
summary(ml_HRE_TEB6)

# Residuals are included in the spatial object for the future BSF residual map
zoi_hre_spatial$residus_1 <- ml_HRE_TEB1$residuals

# GGPlot2 of the TEB_T1 model
ggplot(ml_HRE_TEB1, aes(x=zoi_hre_spatial$TEB_T1, y=zoi_hre_spatial$hre))+
  geom_point(colour="black", shape="diamond")+geom_smooth(colour="red", method="lm", fill="red")+
  ylab("Height of Roughness Elements")+
  xlab("Temperature TEB_T1")+
  theme_gray()

# Histogram of the HRE residuals
qplot(zoi_hre_spatial$residus_1, geom = "histogram", binwidth = 1, main = "HRE residual distribution", xlab = "residual distribution", ylab = "residual count",
      fill=I("purple"), col=I("black"), alpha=(.2))

# BSF Residuals Map

# Legend values = quantile range 6
qt6<-classIntervals(var=zoi_hre_spatial$residus_1, n=6)
# Deplacer le zéro
qt6$brks[4] <- 0

# Cartographie thématique des résidus, par quantiles et palette autour de zéro
couleurs<-carto.pal(pal1 = "sand.pal", n1 = 3, pal2 = "purple.pal", n2 = 4)
choroLayer(spdf=zoi_hre_spatial, df=zoi_hre_spatial@data, var="residus_1", breaks=qt6$brks, col=couleurs, border=FALSE,
           legend.pos = "topleft", legend.values.rnd = 2, legend.title.txt = "residuals", legend.frame = FALSE)

layoutLayer("HRE~TEB T1 Meso-NH Residuals Strat_2", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Building data:IGN/BD Topo, Météo France", frame = F, scale = 5)

# Habillage avec superposition par les quartiers et communes de l'agglo.
plot(zoi$geometry, col = NA, border = "grey40" , add = TRUE)

north(pos = "topright")
dev.off()

# GWR

# Creation of a distrance matrix for the BSF spatial object
dm_hre <- gw.dist(sp::coordinates(zoi_hre_spatial))

# Bandwidth selection using the cross validation method and the Gaussian kernel
bp_hre <- bw.gwr(hre~TEB_T1, data = zoi_hre_spatial, approach = "CV", kernel = "gaussian", dMat = dm_hre)
bp_hre

# GWR according to the automatically calculated bandwidth
g_hre <- gwr.basic(hre~TEB_T1, data = zoi_hre_spatial, bw = bp_hre, kernel = "gaussian", adaptive = F, dMat = dm_hre)
g_hre

# Sockage des résultats dans une variable, puis dans les données attributaires
rg_hre<-as.data.frame(g_hre$SDF)
zoi_hre_spatial$rg_hre=rg_hre$TEB_T1

# Histogram of the Coefficient values relating to the bsf~TEB_T1
qplot(zoi_hre_spatial$rg_hre, geom = "histogram", binwidth = 1, main = "GWR - HRE~TEB_T1 Coef. distribution", 
      xlab = "coef. distribution", ylab = "coef. count", fill=I("purple"), col=I("black"),
      alpha=(.2))

# Preparation of fisher-jenks classification for the map legend 
fish_jks7<-classIntervals(zoi_hre_spatial$rg_hre, style="fisher", n=7)
# Deplacer le zéro
fish_jks7$brks[4] <- 0 
hist(zoi_hre_spatial$rg_hre, breaks=fish_jks7$brks)
# Colour prep
couleurs<-carto.pal(pal1 = "sand.pal", n1 = 3, pal2 = "purple.pal", n2 = 4)

# Dessin de la carte
choroLayer(spdf=zoi_hre_spatial, df=zoi_hre_spatial@data, var="rg_hre", breaks=fish_jks7$brks, col=couleurs, border=FALSE, lwd=0.1, 
           legend.values.rnd = 2, legend.pos="topleft", legend.title.txt = "coefs.")

layoutLayer("HRE~TEB T1 Meso-NH Gaussian Process Regression (GWR) : coefs, non-adapted BW", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Building data:IGN/BD Topo, Météo France", frame = F, scale = 5)

# Habillage avec superposition par les USR du maillage de MApUCE.
plot(zoi$geometry, col = NA, border = "grey40", add = TRUE)

north(pos = "topright")
dev.off()

# Recherche d'une BP adaptative
bpa_hre<-bw.gwr(hre~TEB_T1, data = zoi_hre_spatial, approach = "AIC", adaptive = T, dMat = dm_hre)

# Affichage des résultats
bpa_hre

# Recalcul de la GWR avec cette nouvelle BP
gwr2_hre <- gwr.basic(hre~TEB_T1, data = zoi_hre_spatial, bw = bpa_hre, adaptive = T)
gwr2_hre

# Cartographie des coefficients adaptatifs
aic_hre<-as.data.frame(gwr2_hre$SDF)
zoi_hre_spatial$aic_hre=aic_hre$TEB_T1

# Histogram of the Coefficient values relating to the bsf~TEB_T1
qplot(zoi_hre_spatial$aic_hre, geom = "histogram", binwidth = 5, main = "GWR - HRE~TEB_T1 adapted bandwidth Coef. distribution", 
      xlab = "coef. distribution", ylab = "coef. count", fill=I("purple"), col=I("black"),
      alpha=(.2))

# Preparation of fisher-jenks classification for the map legend 
fish_jks8<-classIntervals(zoi_hre_spatial$aic_hre, style="fisher", n=8)
# Deplacer le zéro
fish_jks8$brks[4] <- 0 
# # Prep of quantile legend values (gives a very different but biased presentation of the data)
# qt7<-classIntervals(zoi_bsf_spatial$rga_bsf, style="quantile", n=7)
# # Deplacer le zéro
# qt7$brks[4] <- 0
# Histograms 
hist(zoi_hre_spatial$aic_hre, breaks=fish_jks8$brks)
# hist(zoi_bsf_spatial$rga_bsf, breaks=qt7$brks)

#couleurs<-carto.pal(pal1 = "harmo.pal", n1 = 6, pal2 = "turquoise.pal", n2 = 2)
couleurs<-carto.pal(pal1 = "sand.pal", n1 = 3, pal2 = "purple.pal", n2 = 5)
# couleurs <- brewer.pal(7, "Purples")

# Dessin de la carte
choroLayer(spdf=zoi_hre_spatial, df=zoi_hre_spatial@data, var="aic_hre", breaks=fish_jks8$brks, col=couleurs, border=FALSE, lwd=0.1, 
           legend.values.rnd = 2, legend.pos="topleft", legend.title.txt = "coefs.")

layoutLayer("HRE~TEB T1 Meso-NH AIC corrected regression (GWR) : coefs, adapted BW", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Building data:IGN/BD Topo, Météo France", frame = F, scale = 5)

# Habillage avec superposition par les USR du maillage de MApUCE.
plot(zoi_spatial, col = NA, border = F, add = TRUE)

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

# Residuals are included in the spatial object for the future BSF residual map
zoi_isf_spatial$residus_1 <- ml_ISF_TEB1$residuals

# GGPlot2 of the TEB_T1 model
ggplot(ml_ISF_TEB1, aes(x=zoi_isf_spatial$TEB_T1, y=zoi_isf_spatial$isf))+
  geom_point(colour="black", shape="diamond")+geom_smooth(colour="red", method="lm", fill="red")+
  ylab("Impervious Surface Fraction")+
  xlab("Temperature TEB_T1")+
  theme_gray()

# Histogram of the ISF residuals
qplot(zoi_isf_spatial$residus_1, geom = "histogram", binwidth = 0.02, main = "ISF residual distribution", xlab = "residual distribution", ylab = "residual count",
      fill=I("grey"), col=I("black"), alpha=(.2))

# ISF Residuals Map

# Legend values = quantile range 6
qt6<-classIntervals(var=zoi_isf_spatial$residus_1, n=6)
# Deplacer le zéro
qt6$brks[5] <- 0

# Cartographie thématique des résidus, par quantiles et palette autour de zéro
couleurs<-carto.pal(pal1 = "turquoise.pal", n1 = 4, pal2 = "wine.pal", n2 = 3)
choroLayer(spdf=zoi_isf_spatial, df=zoi_isf_spatial@data, var="residus_1", breaks=qt6$brks, col=couleurs, border=FALSE,
           legend.pos = "topleft", legend.values.rnd = 2, legend.title.txt = "residuals", legend.frame = FALSE)

layoutLayer("ISF~TEB T1 Meso-NH Residuals Strat_2", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Impervious data: contributors OSM, Météo France", frame = F, scale = 5)

# Habillage avec superposition par les quartiers et communes de l'agglo.
plot(zoi$geometry, col = NA, border = "grey40" , add = TRUE)

north(pos = "topright")
dev.off()

# GWR

# Creation of a distrance matrix for the ISF spatial object
dm_isf <- gw.dist(sp::coordinates(zoi_isf_spatial))

# Bandwidth selection using the cross validation method and the Gaussian kernel
bp_isf <- bw.gwr(isf~TEB_T1, data = zoi_isf_spatial, approach = "CV", kernel = "gaussian", dMat = dm_isf)
bp_isf
# bp_bsf_2 <- bw.gwr(bsf~TEB_T1, data = zoi_bsf_spatial, approach = "CV", kernel = "gaussian") # pour voir si même bp sans dMat, oui même résutat

# GWR according to the automatically calculated bandwidth
g_isf <- gwr.basic(isf~TEB_T1, data = zoi_isf_spatial, bw = bp_isf, kernel = "gaussian", adaptive = F, dMat = dm_isf)
g_isf

# Sockage des résultats dans une variable, puis dans les données attributaires
rg_isf<-as.data.frame(g_isf$SDF)
zoi_isf_spatial$rg_isf=rg_isf$TEB_T1

# Histogram of the Coefficient values relating to the isf~TEB_T1
qplot(zoi_isf_spatial$rg_isf, geom = "histogram", binwidth = 0.3, main = "GWR - ISF~TEB_T1 Coef. distribution", 
      xlab = "coef. distribution", ylab = "coef. count", fill=I("grey"), col=I("black"),
      alpha=(.2))

# Preparation of fisher-jenks classification for the map legend 
fish_jks8<-classIntervals(zoi_isf_spatial$rg_isf, style="fisher", n=8)
# Deplacer le zéro
fish_jks8$brks[5] <- 0 
hist(zoi_isf_spatial$rg_isf, breaks=fish_jks8$brks)
# Colour prep
couleurs<-carto.pal(pal1 = "turquoise.pal", n1 = 4, pal2 = "wine.pal", n2 = 4)

# Dessin de la carte
choroLayer(spdf=zoi_isf_spatial, df=zoi_isf_spatial@data, var="rg_isf", breaks=fish_jks8$brks, col=couleurs, border=FALSE, lwd=0.1, 
           legend.values.rnd = 2, legend.pos="topleft", legend.title.txt = "coefs.")

layoutLayer("ISF~TEB T1 Meso-NH Gaussian Process Regression (GWR) : coefs, non-adapted BW", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Impervious data: contributors OSM, Météo France", frame = F, scale = 5)

# Habillage avec superposition par les USR du maillage de MApUCE.
plot(zoi$geometry, col = NA, border = "grey40", add = TRUE)

north(pos = "topright")
dev.off()

# Recherche d'une BP adaptative
bpa_isf<-bw.gwr(isf~TEB_T1, data = zoi_isf_spatial, approach = "AIC", adaptive = T, dMat = dm_isf)

# Affichage des résultats
bpa_bsf

# Recalcul de la GWR avec cette nouvelle BP
gwr2_isf <- gwr.basic(isf~TEB_T1, data = zoi_isf_spatial, bw = bpa_isf, adaptive = T)
gwr2_isf

# Cartographie des coefficients adaptatifs
aic_isf<-as.data.frame(gwr2_isf$SDF)
zoi_isf_spatial$aic_isf=aic_isf$TEB_T1

# Histogram of the Coefficient values relating to the bsf~TEB_T1
qplot(zoi_isf_spatial$aic_isf, geom = "histogram", binwidth = 0.5, main = "GWR - ISF~TEB_T1 adapted bandwidth Coef. distribution", 
      xlab = "coef. distribution", ylab = "coef. count", fill=I("grey"), col=I("black"),
      alpha=(.2))

# Preparation of fisher-jenks classification for the map legend 
fish_jks7<-classIntervals(zoi_isf_spatial$aic_isf, style="fisher", n=7)
# Deplacer le zéro
fish_jks7$brks[5] <- 0 
# # Prep of quantile legend values (gives a very different but biased presentation of the data)
# qt7<-classIntervals(zoi_bsf_spatial$rga_bsf, style="quantile", n=7)
# # Deplacer le zéro
# qt7$brks[4] <- 0
# Histograms 
hist(zoi_isf_spatial$aic_isf, breaks=fish_jks7$brks)
# hist(zoi_bsf_spatial$rga_bsf, breaks=qt7$brks)

# Colour prep
couleurs<-carto.pal(pal1 = "turquoise.pal", n1 = 4, pal2 = "wine.pal", n2 = 3)
# couleurs <- brewer.pal(7, "Accent")

# Dessin de la carte
choroLayer(spdf=zoi_isf_spatial, df=zoi_isf_spatial@data, var="aic_isf", breaks=fish_jks7$brks, col=couleurs, border=FALSE, lwd=0.1, 
           legend.values.rnd = 2, legend.pos="topleft", legend.title.txt = "coefs.")

layoutLayer("ISF~TEB T1 Meso-NH AIC corrected regression (GWR) : coefs, adapted BW", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Impervious data: contributors OSM, Météo France", frame = F, scale = 5)

# Habillage avec superposition par les USR du maillage de MApUCE.
plot(zoi_spatial, col = NA, border = "grey40", add = TRUE)

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

# Residuals are included in the spatial object for the future BSF residual map
zoi_psf_spatial$residus_1 <- ml_PSF_TEB1$residuals

# GGPlot2 of the TEB_T1 model
ggplot(ml_PSF_TEB1, aes(x=zoi_psf_spatial$TEB_T1, y=zoi_psf_spatial$psf))+
  geom_point(colour="black", shape="diamond")+geom_smooth(colour="red", method="lm", fill="red")+
  ylab("Pervious Surface Fraction")+
  xlab("Temperature TEB_T1")+
  theme_gray()

# Histogram of the ISF residuals
qplot(zoi_psf_spatial$residus_1, geom = "histogram", binwidth = 0.02, main = "PSF residual distribution", xlab = "residual distribution", ylab = "residual count",
      fill=I("green"), col=I("black"), xlim= c(-0.5, 1), alpha=(.2))

# PSF Residuals Map

# Legend values = quantile range 6
qt6<-classIntervals(var=zoi_psf_spatial$residus_1, n=6)
# Deplacer le zéro
qt6$brks[5] <- 0

# Cartographie thématique des résidus, par quantiles et palette autour de zéro
couleurs<-carto.pal(pal1 = "harmo.pal", n1 = 4, pal2 = "turquoise.pal", n2 = 3)
choroLayer(spdf=zoi_psf_spatial, df=zoi_psf_spatial@data, var="residus_1", breaks=qt6$brks, col=couleurs, border=FALSE,
           legend.pos = "topleft", legend.values.rnd = 2, legend.title.txt = "residuals", legend.frame = FALSE)

layoutLayer("PSF~TEB T1 Meso-NH Residuals Strat_2", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Pervious data: contributors OSM, Météo France", frame = F, scale = 5)

# Habillage avec superposition par les quartiers et communes de l'agglo.
plot(zoi$geometry, col = NA, border = F , add = TRUE)

north(pos = "topright")
dev.off()

# GWR

# Creation of a distrance matrix for the ISF spatial object
dm_psf <- gw.dist(sp::coordinates(zoi_psf_spatial))

# Bandwidth selection using the cross validation method and the Gaussian kernel
bp_psf <- bw.gwr(psf~TEB_T1, data = zoi_psf_spatial, approach = "CV", kernel = "gaussian", dMat = dm_psf)
bp_psf
# bp_bsf_2 <- bw.gwr(bsf~TEB_T1, data = zoi_bsf_spatial, approach = "CV", kernel = "gaussian") # pour voir si même bp sans dMat, oui même résutat

# GWR according to the automatically calculated bandwidth
g_psf <- gwr.basic(psf~TEB_T1, data = zoi_psf_spatial, bw = bp_psf, kernel = "gaussian", adaptive = F, dMat = dm_psf)
g_psf

# Sockage des résultats dans une variable, puis dans les données attributaires
rg_psf<-as.data.frame(g_psf$SDF)
zoi_psf_spatial$rg_psf=rg_psf$TEB_T1

# Histogram of the Coefficient values relating to the isf~TEB_T1
qplot(zoi_psf_spatial$rg_psf, geom = "histogram", binwidth = 0.2, main = "GWR - PSF~TEB_T1 Coef. distribution", 
      xlab = "coef. distribution", ylab = "coef. count", fill=I("green"), col=I("black"),
      alpha=(.2))

# Preparation of fisher-jenks classification for the map legend 
fish_jks7<-classIntervals(zoi_psf_spatial$rg_psf, style="fisher", n=7)
# Deplacer le zéro
fish_jks7$brks[6] <- 0 
hist(zoi_isf_spatial$rg_isf, breaks=fish_jks8$brks)
# Colour prep
couleurs<-carto.pal(pal1 = "harmo.pal", n1 = 4, pal2 = "turquoise.pal", n2 = 4)

# Dessin de la carte
choroLayer(spdf=zoi_psf_spatial, df=zoi_psf_spatial@data, var="rg_psf", breaks=fish_jks7$brks, col=couleurs, border=FALSE, lwd=0.1, 
           legend.values.rnd = 2, legend.pos="topleft", legend.title.txt = "coefs.")

layoutLayer("PSF~TEB T1 Meso-NH Gaussian Process Regression (GWR) : coefs, non-adapted BW", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Pervious data: contributors OSM, Météo France", frame = F, scale = 5)

# Habillage avec superposition par les USR du maillage de MApUCE.
plot(zoi$geometry, col = NA, border = "grey40", add = TRUE)

north(pos = "topright")
dev.off()

# Recherche d'une BP adaptative
bpa_psf<-bw.gwr(psf~TEB_T1, data = zoi_psf_spatial, approach = "AIC", adaptive = T, dMat = dm_psf)

# Affichage des résultats
bpa_psf

# Recalcul de la GWR avec cette nouvelle BP
gwr2_psf <- gwr.basic(psf~TEB_T1, data = zoi_psf_spatial, bw = bpa_psf, adaptive = T)
gwr2_psf

# Cartographie des coefficients adaptatifs
aic_psf<-as.data.frame(gwr2_psf$SDF)
zoi_psf_spatial$aic_psf=aic_psf$TEB_T1

# Histogram of the Coefficient values relating to the bsf~TEB_T1
qplot(zoi_psf_spatial$aic_psf, geom = "histogram", binwidth = 0.2, main = "GWR - PSF~TEB_T1 adapted bandwidth Coef. distribution", 
      xlab = "coef. distribution", ylab = "coef. count", fill=I("green"), col=I("black"),
      alpha=(.2))

# Preparation of fisher-jenks classification for the map legend 
fish_jks7<-classIntervals(zoi_psf_spatial$aic_psf, style="fisher", n=7)
# Deplacer le zéro
fish_jks7$brks[6] <- 0 
# # Prep of quantile legend values (gives a very different but biased presentation of the data)
# qt7<-classIntervals(zoi_bsf_spatial$rga_bsf, style="quantile", n=7)
# # Deplacer le zéro
# qt7$brks[4] <- 0
# Histograms 
hist(zoi_psf_spatial$aic_psf, breaks=fish_jks7$brks)
# hist(zoi_bsf_spatial$rga_bsf, breaks=qt7$brks)

# Colour prep
couleurs<-carto.pal(pal1 = "harmo.pal", n1 = 5, pal2 = "turquoise.pal", n2 = 2)
# couleurs <- brewer.pal(7, "Accent")

# Dessin de la carte
choroLayer(spdf=zoi_psf_spatial, df=zoi_psf_spatial@data, var="aic_psf", breaks=fish_jks7$brks, col=couleurs, border=FALSE, lwd=0.1, 
           legend.values.rnd = 2, legend.pos="topleft", legend.title.txt = "coefs.")

layoutLayer("PSF~TEB T1 Meso-NH AIC corrected regression (GWR) : coefs, adapted BW", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Pervious data: contributors OSM, Météo France", frame = F, scale = 5)

# Habillage avec superposition par les USR du maillage de MApUCE.
plot(zoi_spatial, col = NA, border = F, add = TRUE)

north(pos = "topright")
dev.off()
