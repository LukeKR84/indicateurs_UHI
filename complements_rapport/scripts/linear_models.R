# Load necessary packages 
library(cartography)
library(sf)
library(classInt)
library(GWmodel)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

# Set working directory 
setwd("C:/Stage_IGN/indicateurs/gwr_master/quality_control_check")

# Load zones of interest
zoi_1 <- read_sf("C:/Stage_IGN/indicateurs/gwr_master/zoi_files/zoi_indicateurs_strat_1.geojson")
zoi_2 <- read_sf("C:/Stage_IGN/indicateurs/gwr_master/zoi_files/zoi_indicateurs_strat_2.geojson")

# Fix the projection
st_crs(zoi_1) <- 2154
st_crs(zoi_2) <- 2154

# Conversion of ZOI to a spatial object
zoi_spatial<-as(zoi_1, 'Spatial')

# Create different subsets based on each indicator (data sets without NAs in the indicator column)
zoi_bsf <- subset(zoi_spatial, (!is.na(zoi_spatial$bsf)))
zoi_hre <- subset(zoi_spatial, (!is.na(zoi_spatial$hre)))
zoi_isf <- subset(zoi_spatial, (!is.na(zoi_spatial$isf)))
zoi_psf <- subset(zoi_spatial, (!is.na(zoi_spatial$psf)))
zoi_asf <- subset(zoi_spatial, (!is.na(zoi_spatial$asf)))


summary(zoi_bsf)
summary(zoi_hre)
summary(zoi_isf)
summary(zoi_psf)
summary(zoi_asf)


# Linear models comparing temperature
lm_bsf <- lm(zoi_spatial$TEB_T1~zoi_spatial$bsf)
lm_hre <- lm(zoi_spatial$TEB_T1~zoi_spatial$hre)
lm_isf <- lm(zoi_spatial$TEB_T1~zoi_spatial$isf)
lm_psf <- lm(zoi_spatial$TEB_T1~zoi_spatial$psf)
lm_asf <- lm(zoi_spatial$TEB_T1~zoi_spatial$asf)


summary(lm_bsf)
summary(lm_hre)
summary(lm_isf)
summary(lm_psf)
summary(lm_asf)


# Residuals are included in the spatial object for the future BSF residual map
zoi_bsf$residus <- lm_bsf$residuals
zoi_hre$residus <- lm_hre$residuals
zoi_isf$residus <- lm_isf$residuals
zoi_psf$residus <- lm_psf$residuals
zoi_asf$residus <- lm_asf$residuals

# RÃ©glage des proportions, dimensions et marges du fichier image
png(filename = "lm_TEB_T1_hre_strat1.png", width = 800, height = 800, res = 100)
par(mar = c(0,0,1.2,0))

# GGPlot2 of the TEB_T1 model
ggplot(zoi_1, aes(x=zoi_1$TEB_T1, y=zoi_1$hre))+
  geom_point(colour="black", shape="diamond")+geom_smooth(colour="red", method="lm", fill="red")+
  ylab("Height of Roughnss Elements")+
  xlab("Temperature TEB_T1")+
  theme_gray()

dev.off()

# RÃ©glage des proportions, dimensions et marges du fichier image
png(filename = "histo_asf_residus_strat2.png", width = 800, height = 600, res = 100)
par(mar = c(0,0,1.2,0))

# Histogram of the BSF residuals
qplot(zoi_asf$residus, geom = "histogram", binwidth = 0.06, main = "ASF residual distribution", xlab = "residual distribution", ylab = "residual count",
      fill=I("blue"), col=I("white"), alpha=(.2))

# xlim=c(-0.5, 0.75)
dev.off()

# Residual Maps

# Legend values = quantile range 6
qt6<-classIntervals(zoi_bsf$residus, n=6)
# Deplacer le zéro
qt6$brks[4] <- 0

# Cartographie thématique des résidus, par quantiles et palette autour de zéro
couleurs<-carto.pal(pal1 = "green.pal", n1 = 3, pal2 = "red.pal", n2 = 3)

# RÃ©glage des proportions, dimensions et marges du fichier image
png(filename = "bsf_residuals_strat2.png", width = 800, height = 600, res = 100)
par(mar = c(0,0,1.2,0))

# Dessin de la carte
choroLayer(spdf=zoi_bsf, df=zoi_bsf@data, var="residus", breaks=qt6$brks, col=couleurs, border=FALSE, lwd=0.2,
           legend.values.rnd = 2, legend.pos="n")

legendChoro(pos = "topleft", title.txt = "Residuals", breaks = qt6$brks, col = couleurs, cex = 1, values.rnd = 2, nodata = T,
            nodata.txt = "NA", border = "black")

layoutLayer("TEB T1~BSF Strat 2 : residual spatial distribution", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Building data: IGN, Temperature data: Météo France", 
            frame = F, scale = 5, posscale = "bottomright")

# Habillage avec superposition par les USR du maillage de MApUCE.
plot(zoi_spatial, col = NA, border = "Grey40", add = TRUE)

north(pos = "topright")
dev.off()

Impervious data: contributors OSM
Building data: IGN