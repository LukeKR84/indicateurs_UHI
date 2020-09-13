# Load necessary packages 
library(cartography)
library(sf)
library(classInt)
library(GWmodel)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(viridis)
library(ggplot2)

# Set working directory 
setwd("C:/Stage_IGN/indicateurs/gwr_master/artificialSurfaceFraction")

# Load data
zoi_strat1 <- read_sf("C:/Stage_IGN/indicateurs/gwr_master/zoi_files/zoi_indicateurs_strat_1.geojson")

# Conversion of ZOI to a spatial object
zoiStrat1_spatial<-as(zoi_strat1, 'Spatial')

# Create different subset to test asf without NA values
zoi_asf <- subset(zoiStrat1_spatial, (!is.na(zoiStrat1_spatial$asf)))

# Creation of linear model and scatter plot
ml_asf <- lm(zoi_asf$TEB_T1~zoi_asf$asf)

summary(ml_asf)

ggplot(ml_asf, aes(x=zoi_asf$TEB_T1, y=zoi_asf$asf))+
  geom_point(colour="black", shape="diamond")+geom_smooth(colour="red", method="lm", fill="red")+
  ylab("Artificial Surface Fraction")+
  xlab("Temperature TEB_T1")+
  theme_gray()

# Histogram of residuals
zoi_asf$asf_residus <- ml_asf$residuals

qplot(zoi_asf$asf_residus, geom = "histogram", binwidth = 0.1, main = "ASF residual distribution", xlab = "residual distribution", ylab = "residual count",
      fill=I("blue"), col=I("white"), alpha=(.2))

# Residual Map

# Legend values = quantile range 6
qt6<-classIntervals(zoi_asf$asf_residus, n=6)
# Deplacer le zéro
qt6$brks[4] <- 0

# Cartographie thématique des résidus, par quantiles et palette autour de zéro
couleurs<-carto.pal(pal1 = "green.pal", n1 = 3, pal2 = "red.pal", n2 = 3)
choroLayer(spdf=zoi_asf, df=zoi_asf@data, var="asf_residus", breaks=qt6$brks, col=couleurs, border=FALSE,
           legend.pos = "topleft", legend.values.rnd = 2, legend.title.txt = "residuals", legend.frame = FALSE)

layoutLayer("TEB T1~ASF Meso-NH Residuals Strat_1", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Map data: IGN, Contributors OSM, Météo France", frame = F, scale = 5)

# Habillage avec superposition par les quartiers et communes de l'agglo.
plot(zoiStrat1_spatial, col = NA, border = "grey40" , add = TRUE)

north(pos = "topright")
dev.off()

# GWR analysis of asf with varying bandwidths

# Creation of distance matrix
dm_asf <- gw.dist(sp::coordinates(zoi_asf))


var1 <- zoi_asf$TEB_T1
var2 <- zoi_asf$asf
dataSet <- zoi_asf
dm <- dm_asf


bpna_calculator <- function(var1, var2, dataSet, bp, dm) {
  gauss_bpna <- gwr.basic(var1~var2, data = dataSet, bw = bp, kernel = "gaussian", dMat = dm)
  return(gauss_bpna)
}

bpRange <- seq(150, 900, by=50)

for (i in bpRange) {
  gwrCalcul <- bpna_calculator(var1 = var1, var2 = var2, dataSet = dataSet, bp = i,dm = dm)
  
  myObjectName <- paste0("rg_asf_", i)
  
  assign(myObjectName, gwrCalcul, envir = globalenv())
}


integrate_bpna <- function(x) {
  x <- as.data.frame(x$SDF)
  return(x)
}

rg_asf_150
rg_asf_200
rg_asf_250
rg_asf_300
rg_asf_350
rg_asf_400
rg_asf_450
rg_asf_500
rg_asf_550
rg_asf_600
rg_asf_650
rg_asf_700
rg_asf_750
rg_asf_800
rg_asf_850
rg_asf_900


rg_asf_data_1 = integrate_bpna(x = rg_asf_150)
rg_asf_data_2 = integrate_bpna(x = rg_asf_200)
rg_asf_data_3 = integrate_bpna(x = rg_asf_250)
rg_asf_data_4 = integrate_bpna(x = rg_asf_300)
rg_asf_data_5 = integrate_bpna(x = rg_asf_350)
rg_asf_data_6 = integrate_bpna(x = rg_asf_400)
rg_asf_data_7 = integrate_bpna(x = rg_asf_450)
rg_asf_data_8 = integrate_bpna(x = rg_asf_500)
rg_asf_data_9 = integrate_bpna(x = rg_asf_550)
rg_asf_data_10 = integrate_bpna(x = rg_asf_600)
rg_asf_data_11 = integrate_bpna(x = rg_asf_650)
rg_asf_data_12 = integrate_bpna(x = rg_asf_700)
rg_asf_data_13 = integrate_bpna(x = rg_asf_750)
rg_asf_data_14 = integrate_bpna(x = rg_asf_800)
rg_asf_data_15 = integrate_bpna(x = rg_asf_850)
rg_asf_data_16 = integrate_bpna(x = rg_asf_900)

zoi_asf$rg_150 = rg_asf_data_1$var2
zoi_asf$rg_200 = rg_asf_data_2$var2
zoi_asf$rg_250 = rg_asf_data_3$var2
zoi_asf$rg_300 = rg_asf_data_4$var2
zoi_asf$rg_350 = rg_asf_data_5$var2
zoi_asf$rg_400 = rg_asf_data_6$var2
zoi_asf$rg_450 = rg_asf_data_7$var2
zoi_asf$rg_500 = rg_asf_data_8$var2
zoi_asf$rg_550 = rg_asf_data_9$var2
zoi_asf$rg_600 = rg_asf_data_10$var2
zoi_asf$rg_650 = rg_asf_data_11$var2
zoi_asf$rg_700 = rg_asf_data_12$var2
zoi_asf$rg_750 = rg_asf_data_13$var2
zoi_asf$rg_800 = rg_asf_data_14$var2
zoi_asf$rg_850 = rg_asf_data_15$var2
zoi_asf$rg_900 = rg_asf_data_16$var2

# Basic histogram to determine classification type
hist(zoi_asf$rg_900, breaks=40)

# Preparation of fisher-jenks classification for the map legend 
fish_jks7<-classIntervals(zoi_asf$rg_900, style="fisher", n=7)
# Preparation of standard deviation classification for the map legend
sd7 <- classIntervals(zoi_isf$rg_1,style="sd", n=7)
# Deplacer le zéro
fish_jks7$brks[2] <- 0 
sd7$brks[3] <- 0
hist(zoi_asf$rg_150, breaks=fish_jks7$brks)

# File type
#png(filename = "bsf_bpna_150.png",width = 700, height = 700)

# Colour prep
couleurs<-carto.pal(pal1 = "blue.pal", n1 = 1, pal2 = "red.pal", n2 = 6)

# Dessin de la carte
choroLayer(spdf=zoi_asf, df=zoi_asf@data, var="rg_900", breaks=fish_jks7$brks, col=couleurs, border=FALSE, lwd=0.2, 
           legend.values.rnd = 2, legend.pos="topleft", legend.title.txt = "coefs.")

layoutLayer("TEB T1~ ASF Strat_1 (GWR) : coefs, non-adapted BW: 900m", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Pervious data: OSM Contributors, Météo France", frame = F, scale = 5)

# Habillage avec superposition par les USR du maillage de MApUCE.
plot(zoiStrat1_spatial, col = NA, border = "grey40", add = TRUE)

north(pos = "topright")
dev.off()

# GWR with an adapted bandwidth

bpa_calculator <- function(var1, var2, dataSet, bp, dm) {
  bi2_bpa <- gwr.basic(var1~var2, data = dataSet, bw = bp, adaptive=TRUE, kernel = "bisquare", dMat = dm)
  return(bi2_bpa)
}

bpa_range <- seq(15, 50, by=5)

var1 <- zoi_asf$TEB_T1
var2 <- zoi_asf$asf
dataSet <- zoi_asf
dm <- dm_asf

for (i in bpa_range) {
  gwrCalcul <- bpa_calculator(var1 = var1, var2 = var2, dataSet = dataSet, bp = i, dm = dm)
  
  myObjectName <- paste0("rbi2_asf_", i)
  
  assign(myObjectName, gwrCalcul, envir = globalenv())
}


integrate_bpa <- function(x) {
  x <- as.data.frame(x$SDF)
  return(x)
}

rga_asf_15
rga_asf_20
rga_asf_25
rga_asf_30
rga_asf_35
rga_asf_40
rga_asf_45
rga_asf_50

rga_asf_data_1 = integrate_bpa(x = rga_asf_15)
rga_asf_data_2 = integrate_bpa(x = rga_asf_20)
rga_asf_data_3 = integrate_bpa(x = rga_asf_25)
rga_asf_data_4 = integrate_bpa(x = rga_asf_30)
rga_asf_data_5 = integrate_bpa(x = rga_asf_35)
rga_asf_data_6 = integrate_bpa(x = rga_asf_40)
rga_asf_data_7 = integrate_bpa(x = rga_asf_45)
rga_asf_data_8 = integrate_bpa(x = rga_asf_50)

zoi_asf$rga_15 = rga_asf_data_1$var2
zoi_asf$rga_20 = rga_asf_data_2$var2
zoi_asf$rga_25 = rga_asf_data_3$var2
zoi_asf$rga_30 = rga_asf_data_4$var2
zoi_asf$rga_35 = rga_asf_data_5$var2
zoi_asf$rga_40 = rga_asf_data_6$var2
zoi_asf$rga_45 = rga_asf_data_7$var2
zoi_asf$rga_50 = rga_asf_data_8$var2

rbi2_asf_15
rbi2_asf_20
rbi2_asf_25
rbi2_asf_30
rbi2_asf_35
rbi2_asf_40
rbi2_asf_45
rbi2_asf_50

rbi2_asf_data_1 = integrate_bpa(x = rbi2_asf_15)
rbi2_asf_data_2 = integrate_bpa(x = rbi2_asf_20)
rbi2_asf_data_3 = integrate_bpa(x = rbi2_asf_25)
rbi2_asf_data_4 = integrate_bpa(x = rbi2_asf_30)
rbi2_asf_data_5 = integrate_bpa(x = rbi2_asf_35)
rbi2_asf_data_6 = integrate_bpa(x = rbi2_asf_40)
rbi2_asf_data_7 = integrate_bpa(x = rbi2_asf_45)
rbi2_asf_data_8 = integrate_bpa(x = rbi2_asf_50)

zoi_asf$rbi2_15 = rbi2_asf_data_1$var2
zoi_asf$rbi2_20 = rbi2_asf_data_2$var2
zoi_asf$rbi2_25 = rbi2_asf_data_3$var2
zoi_asf$rbi2_30 = rbi2_asf_data_4$var2
zoi_asf$rbi2_35 = rbi2_asf_data_5$var2
zoi_asf$rbi2_40 = rbi2_asf_data_6$var2
zoi_asf$rbi2_45 = rbi2_asf_data_7$var2
zoi_asf$rbi2_50 = rbi2_asf_data_8$var2

# Basic histogram to determine classification type
hist(zoi_asf$rbi2_30, breaks=40)

# Preparation of fisher-jenks classification for the map legend 
fish_jks7<-classIntervals(zoi_asf$rbi2_30, style="fisher", n=7)
# Preparation of standard deviation classification for the map legend
sd7 <- classIntervals(zoi_isf$rg_1,style="sd", n=7)
# Deplacer le zéro
fish_jks7$brks[3] <- 0 
sd7$brks[3] <- 0
hist(zoi_asf$rg_150, breaks=fish_jks7$brks)

# File type
#png(filename = "bsf_bpna_150.png",width = 700, height = 700)

# Colour prep
couleurs<-carto.pal(pal1 = "blue.pal", n1 = 2, pal2 = "red.pal", n2 = 5)

# Dessin de la carte
choroLayer(spdf=zoi_asf, df=zoi_asf@data, var="rbi2_30", breaks=fish_jks7$brks, col=couleurs, border=FALSE, lwd=0.2, 
           legend.values.rnd = 2, legend.pos="topleft", legend.title.txt = "coefs.")

layoutLayer("TEB T1~ ASF Strat_1 (GWR) : coefs, adapted BW: 30 neighbours (bi2)", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Pervious data: OSM Contributors, Météo France", frame = F, scale = 5)

# Habillage avec superposition par les USR du maillage de MApUCE.
plot(zoiStrat1_spatial, col = NA, border = "grey40", add = TRUE)

north(pos = "topright")
dev.off()
