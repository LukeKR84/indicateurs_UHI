# Load necessary packages 
library(cartography)
library(sf)
library(classInt)
library(GWmodel)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

# Set working directory 
setwd("C:/Stage_IGN/indicateurs/gwr_master/functionTest")
# Load zone of interest
zoi <- read_sf("C:/Stage_IGN/indicateurs/gwr_indicateurs_mesoNH_strat_2/zoi_mesoNH_strat2.geojson")

zoi_1 <- read_sf("C:/Stage_IGN/indicateurs/gwr_master/zoi_files/zoi_indicateurs_strat_1.geojson")
zoi_2 <- read_sf("C:/Stage_IGN/indicateurs/gwr_master/zoi_files/zoi_indicateurs_strat_2.geojson")


# Fix the projection
st_crs(zoi) <- 2154

# Conversion of ZOI to a spatial object
zoi_spatial<-as(zoi, 'Spatial')

# Create different subsets based on each indicator (data sets without NAs in the indicator column)
zoi_bsf <- subset(zoi_spatial, (!is.na(zoi_spatial$bsf)))
zoi_hre <- subset(zoi_spatial, (!is.na(zoi_spatial$hre)))
zoi_isf <- subset(zoi_spatial, (!is.na(zoi_spatial$isf)))
zoi_psf <- subset(zoi_spatial, (!is.na(zoi_spatial$psf)))

# Creation of a distrance matrix for the spatial object
dm_zoi <- gw.dist(sp::coordinates(zoi_spatial))
dm_bsf <- gw.dist(sp::coordinates(zoi_bsf))
dm_hre <- gw.dist(sp::coordinates(zoi_hre))
dm_isf <- gw.dist(sp::coordinates(zoi_isf))
dm_psf <- gw.dist(sp::coordinates(zoi_psf))

var1 <- zoi_bsf$TEB_T1
var2 <- zoi_bsf$bsf
dataSet <- zoi_bsf
dm <- dm_bsf


bpna_calculator <- function(var1, var2, dataSet, bp, dm) {
  gauss_bpna <- gwr.basic(var1~var2, data = dataSet, bw = bp, kernel = "gaussian", dMat = dm)
  return(gauss_bpna)
}

## Earlier experiments

# bsf_test1 <- bpna_calculator(var1 = var1, var2 = var2, dataSet = dataSet, bp = bp, dm = dm_bsf)

# bpName <- c("bp_1", "bp_2", "bp_3", "bp_4", "bp_5", "bp_6", "bp_7", "bp_8", "bp_9", "bp_10", "bp_11", "bp_12")
# dat <- data.frame(1:12)
# 
# for (bp in bpName) {
#   dat$name <- bpName
#   increment_bp <- seq(150, 700, by=50)
#   dat$bp_range <- increment_bp
# }
# 
bpRange <- seq(150, 700, by=50)

for (i in bpRange) {
  gwrCalcul <- bpna_calculator(var1 = var1, var2 = var2, dataSet = dataSet, bp = i,dm = dm)
  
  myObjectName <- paste0("rg_bsf_", i)
  
  assign(myObjectName, gwrCalcul, envir = globalenv())
}


integrate_bpna <- function(x) {
  x <- as.data.frame(x$SDF)
  return(x)
}

rg_bsf_data_1 = integrate_bpna(x = rg_bsf_150)
rg_bsf_data_2 = integrate_bpna(x = rg_bsf_200)
rg_bsf_data_3 = integrate_bpna(x = rg_bsf_250)
rg_bsf_data_4 = integrate_bpna(x = rg_bsf_300)
rg_bsf_data_5 = integrate_bpna(x = rg_bsf_350)
rg_bsf_data_6 = integrate_bpna(x = rg_bsf_400)
rg_bsf_data_7 = integrate_bpna(x = rg_bsf_450)
rg_bsf_data_8 = integrate_bpna(x = rg_bsf_500)
rg_bsf_data_9 = integrate_bpna(x = rg_bsf_550)
rg_bsf_data_10 = integrate_bpna(x = rg_bsf_600)
rg_bsf_data_11 = integrate_bpna(x = rg_bsf_650)
rg_bsf_data_12 = integrate_bpna(x = rg_bsf_700)

rg_hre_data_1 = integrate_bpna(x = rg_hre_150)
rg_hre_data_2 = integrate_bpna(x = rg_hre_200)
rg_hre_data_3 = integrate_bpna(x = rg_hre_250)
rg_hre_data_4 = integrate_bpna(x = rg_hre_300)
rg_hre_data_5 = integrate_bpna(x = rg_hre_350)
rg_hre_data_6 = integrate_bpna(x = rg_hre_400)
rg_hre_data_7 = integrate_bpna(x = rg_hre_450)
rg_hre_data_8 = integrate_bpna(x = rg_hre_500)
rg_hre_data_9 = integrate_bpna(x = rg_hre_550)
rg_hre_data_10 = integrate_bpna(x = rg_hre_600)
rg_hre_data_11 = integrate_bpna(x = rg_hre_650)
rg_hre_data_12 = integrate_bpna(x = rg_hre_700)

rg_isf_data_1 = integrate_bpna(x = rg_isf_150)
rg_isf_data_2 = integrate_bpna(x = rg_isf_200)
rg_isf_data_3 = integrate_bpna(x = rg_isf_250)
rg_isf_data_4 = integrate_bpna(x = rg_isf_300)
rg_isf_data_5 = integrate_bpna(x = rg_isf_350)
rg_isf_data_6 = integrate_bpna(x = rg_isf_400)
rg_isf_data_7 = integrate_bpna(x = rg_isf_450)
rg_isf_data_8 = integrate_bpna(x = rg_isf_500)
rg_isf_data_9 = integrate_bpna(x = rg_isf_550)
rg_isf_data_10 = integrate_bpna(x = rg_isf_600)
rg_isf_data_11 = integrate_bpna(x = rg_isf_650)
rg_isf_data_12 = integrate_bpna(x = rg_isf_700)

rg_psf_data_1 = integrate_bpna(x = rg_psf_150)
rg_psf_data_2 = integrate_bpna(x = rg_psf_200)
rg_psf_data_3 = integrate_bpna(x = rg_psf_250)
rg_psf_data_4 = integrate_bpna(x = rg_psf_300)
rg_psf_data_5 = integrate_bpna(x = rg_psf_350)
rg_psf_data_6 = integrate_bpna(x = rg_psf_400)
rg_psf_data_7 = integrate_bpna(x = rg_psf_450)
rg_psf_data_8 = integrate_bpna(x = rg_psf_500)
rg_psf_data_9 = integrate_bpna(x = rg_psf_550)
rg_psf_data_10 = integrate_bpna(x = rg_psf_600)
rg_psf_data_11 = integrate_bpna(x = rg_psf_650)
rg_psf_data_12 = integrate_bpna(x = rg_psf_700)

zoi_bsf$rg_1 = rg_bsf_data_1$var2
zoi_bsf$rg_2 = rg_bsf_data_2$var2
zoi_bsf$rg_3 = rg_bsf_data_3$var2
zoi_bsf$rg_4 = rg_bsf_data_4$var2
zoi_bsf$rg_5 = rg_bsf_data_5$var2
zoi_bsf$rg_6 = rg_bsf_data_6$var2
zoi_bsf$rg_7 = rg_bsf_data_7$var2
zoi_bsf$rg_8 = rg_bsf_data_8$var2
zoi_bsf$rg_9 = rg_bsf_data_9$var2
zoi_bsf$rg_10 = rg_bsf_data_10$var2
zoi_bsf$rg_11 = rg_bsf_data_11$var2
zoi_bsf$rg_12 = rg_bsf_data_12$var2

zoi_hre$rg_1 = rg_hre_data_1$var2
zoi_hre$rg_2 = rg_hre_data_2$var2
zoi_hre$rg_3 = rg_hre_data_3$var2
zoi_hre$rg_4 = rg_hre_data_4$var2
zoi_hre$rg_5 = rg_hre_data_5$var2
zoi_hre$rg_6 = rg_hre_data_6$var2
zoi_hre$rg_7 = rg_hre_data_7$var2
zoi_hre$rg_8 = rg_hre_data_8$var2
zoi_hre$rg_9 = rg_hre_data_9$var2
zoi_hre$rg_10 = rg_hre_data_10$var2
zoi_hre$rg_11 = rg_hre_data_11$var2
zoi_hre$rg_12 = rg_hre_data_12$var2

zoi_isf$rg_1 = rg_isf_data_1$var2
zoi_isf$rg_2 = rg_isf_data_2$var2
zoi_isf$rg_3 = rg_isf_data_3$var2
zoi_isf$rg_4 = rg_isf_data_4$var2
zoi_isf$rg_5 = rg_isf_data_5$var2
zoi_isf$rg_6 = rg_isf_data_6$var2
zoi_isf$rg_7 = rg_isf_data_7$var2
zoi_isf$rg_8 = rg_isf_data_8$var2
zoi_isf$rg_9 = rg_isf_data_9$var2
zoi_isf$rg_10 = rg_isf_data_10$var2
zoi_isf$rg_11 = rg_isf_data_11$var2
zoi_isf$rg_12 = rg_isf_data_12$var2

zoi_psf$rg_1 = rg_psf_data_1$var2
zoi_psf$rg_2 = rg_psf_data_2$var2
zoi_psf$rg_3 = rg_psf_data_3$var2
zoi_psf$rg_4 = rg_psf_data_4$var2
zoi_psf$rg_5 = rg_psf_data_5$var2
zoi_psf$rg_6 = rg_psf_data_6$var2
zoi_psf$rg_7 = rg_psf_data_7$var2
zoi_psf$rg_8 = rg_psf_data_8$var2
zoi_psf$rg_9 = rg_psf_data_9$var2
zoi_psf$rg_10 = rg_psf_data_10$var2
zoi_psf$rg_11 = rg_psf_data_11$var2
zoi_psf$rg_12 = rg_psf_data_12$var2

# Basic histogram to determine classification type
hist(zoi_psf$rg_12, breaks=40)

# Preparation of fisher-jenks classification for the map legend 
fish_jks7<-classIntervals(zoi_psf$rg_12, style="fisher", n=7)
# Preparation of standard deviation classification for the map legend
sd7 <- classIntervals(zoi_isf$rg_1,style="sd", n=7)
# Deplacer le zéro
fish_jks7$brks[6] <- 0 
sd7$brks[3] <- 0
hist(zoi_hre$rg_isf, breaks=fish_jks7$brks)

# File type
#png(filename = "bsf_bpna_150.png",width = 700, height = 700)

# Colour prep
couleurs<-carto.pal(pal1 = "blue.pal", n1 = 5, pal2 = "red.pal", n2 = 2)

# Dessin de la carte
choroLayer(spdf=zoi_psf, df=zoi_psf@data, var="rg_12", breaks=fish_jks7$brks, col=couleurs, border=FALSE, lwd=0.2, 
           legend.values.rnd = 2, legend.pos="topleft", legend.title.txt = "coefs.")

layoutLayer("TEB T1~ PSF (GWR) : coefs, non-adapted BW: 700m", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Pervious data: OSM Contributors, Météo France", frame = F, scale = 5)

# Habillage avec superposition par les USR du maillage de MApUCE.
plot(zoi_spatial, col = NA, border = "grey40", add = TRUE)

north(pos = "topright")
dev.off()

## Appels des fonctions 

rg_bsf_150
rg_bsf_200
rg_bsf_250
rg_bsf_300
rg_bsf_350
rg_bsf_400
rg_bsf_450
rg_bsf_500
rg_bsf_550
rg_bsf_600
rg_bsf_650
rg_bsf_700

rg_hre_150
rg_hre_200
rg_hre_250
rg_hre_300
rg_hre_350
rg_hre_400
rg_hre_450
rg_hre_500
rg_hre_550
rg_hre_600
rg_hre_650
rg_hre_700

rg_isf_150
rg_isf_200
rg_isf_250
rg_isf_300
rg_isf_350
rg_isf_400
rg_isf_450
rg_isf_500
rg_isf_550
rg_isf_600
rg_isf_650
rg_isf_700

rg_psf_150
rg_psf_200
rg_psf_250
rg_psf_300
rg_psf_350
rg_psf_400
rg_psf_450
rg_psf_500
rg_psf_550
rg_psf_600
rg_psf_650
rg_psf_700

# Multiple bandwidth GWR analysis of the MApUCE morphological indicators

# Set working directory
setwd("C:/Stage_IGN/indicateurs/gwr_master/mapuceFunctionTest")

# Load data
zoiMapuce_1 <- read_sf("C:/Users/riley/Documents/GitHub/indicateurs_UHI/ventilation_mesoNH_maillage/zone_strat1_full.geojson")

zoiMapuce_2 <- read_sf("C:/Users/riley/Documents/GitHub/indicateurs_UHI/ventilation_mesoNH_maillage/zone_strat2_full.geojson")

# Certain USR have duplicates which need to be eliminated
zoiMapuce_1 <- zoiMapuce_1[!duplicated(zoiMapuce_1$pk_usr), ]
names(zoiMapuce_1)
zoiMapuce_2 <- zoiMapuce_2[!duplicated(zoiMapuce_2$pk_usr), ]
names(zoiMapuce_2)
# Creation of spatial objects
zm1_spatial <- as(zoiMapuce_1, 'Spatial')
zm2_spatial <- as(zoiMapuce_2, 'Spatial')
# Creation of distance matrices 
dm_zm1 <- gw.dist(sp::coordinates(zm1_spatial))
dm_zm2 <- gw.dist(sp::coordinates(zm2_spatial))

var1 <- zm1_spatial$TEB_T1
var2 <- zm1_spatial$cntg_mn
dataSet <- zm1_spatial
dm <- dm_zm1



bpna_calculator <- function(var1, var2, dataSet, bp, dm) {
  gauss_bpna <- gwr.basic(var1~var2, data = dataSet, bw = bp, kernel = "gaussian", dMat = dm)
  return(gauss_bpna)
}

bpRange <- seq(150, 700, by=50)

for (i in bpRange) {
  gwrCalcul <- bpna_calculator(var1 = var1, var2 = var2, dataSet = dataSet, bp = i,dm = dm)
  
  myObjectName <- paste0("rg_contig_", i)
  
  assign(myObjectName, gwrCalcul, envir = globalenv())
}


integrate_bpna <- function(x) {
  x <- as.data.frame(x$SDF)
  return(x)
}

rg_flRat_data_1 = integrate_bpna(x = rg_flRat_150)
rg_flRat_data_2 = integrate_bpna(x = rg_flRat_200)
rg_flRat_data_3 = integrate_bpna(x = rg_flRat_250)
rg_flRat_data_4 = integrate_bpna(x = rg_flRat_300)
rg_flRat_data_5 = integrate_bpna(x = rg_flRat_350)
rg_flRat_data_6 = integrate_bpna(x = rg_flRat_400)
rg_flRat_data_7 = integrate_bpna(x = rg_flRat_450)
rg_flRat_data_8 = integrate_bpna(x = rg_flRat_500)
rg_flRat_data_9 = integrate_bpna(x = rg_flRat_550)
rg_flRat_data_10 = integrate_bpna(x = rg_flRat_600)
rg_flRat_data_11 = integrate_bpna(x = rg_flRat_650)
rg_flRat_data_12 = integrate_bpna(x = rg_flRat_700)

zm1_spatial$rg_flRat_150 = rg_flRat_data_1$var2
zm1_spatial$rg_flRat_200 = rg_flRat_data_2$var2
zm1_spatial$rg_flRat_250 = rg_flRat_data_3$var2
zm1_spatial$rg_flRat_300 = rg_flRat_data_4$var2
zm1_spatial$rg_flRat_350 = rg_flRat_data_5$var2
zm1_spatial$rg_flRat_400 = rg_flRat_data_6$var2
zm1_spatial$rg_flRat_450 = rg_flRat_data_7$var2
zm1_spatial$rg_flRat_500 = rg_flRat_data_8$var2
zm1_spatial$rg_flRat_550 = rg_flRat_data_9$var2
zm1_spatial$rg_flRat_600 = rg_flRat_data_10$var2
zm1_spatial$rg_flRat_650 = rg_flRat_data_11$var2
zm1_spatial$rg_flRat_700 = rg_flRat_data_12$var2

rg_flRat_150
rg_flRat_200
rg_flRat_250
rg_flRat_300
rg_flRat_350
rg_flRat_400
rg_flRat_450
rg_flRat_500
rg_flRat_550
rg_flRat_600
rg_flRat_650
rg_flRat_700

rg_blkHoles_data_1 = integrate_bpna(x = rg_blkHoles_150)
rg_blkHoles_data_2 = integrate_bpna(x = rg_blkHoles_200)
rg_blkHoles_data_3 = integrate_bpna(x = rg_blkHoles_250)
rg_blkHoles_data_4 = integrate_bpna(x = rg_blkHoles_300)
rg_blkHoles_data_5 = integrate_bpna(x = rg_blkHoles_350)
rg_blkHoles_data_6 = integrate_bpna(x = rg_blkHoles_400)
rg_blkHoles_data_7 = integrate_bpna(x = rg_blkHoles_450)
rg_blkHoles_data_8 = integrate_bpna(x = rg_blkHoles_500)
rg_blkHoles_data_9 = integrate_bpna(x = rg_blkHoles_550)
rg_blkHoles_data_10 = integrate_bpna(x = rg_blkHoles_600)
rg_blkHoles_data_11 = integrate_bpna(x = rg_blkHoles_650)
rg_blkHoles_data_12 = integrate_bpna(x = rg_blkHoles_700)

zm1_spatial$rg_blkHoles_150 = rg_blkHoles_data_1$var2
zm1_spatial$rg_blkHoles_200 = rg_blkHoles_data_2$var2
zm1_spatial$rg_blkHoles_250 = rg_blkHoles_data_3$var2
zm1_spatial$rg_blkHoles_300 = rg_blkHoles_data_4$var2
zm1_spatial$rg_blkHoles_350 = rg_blkHoles_data_5$var2
zm1_spatial$rg_blkHoles_400 = rg_blkHoles_data_6$var2
zm1_spatial$rg_blkHoles_450 = rg_blkHoles_data_7$var2
zm1_spatial$rg_blkHoles_500 = rg_blkHoles_data_8$var2
zm1_spatial$rg_blkHoles_550 = rg_blkHoles_data_9$var2
zm1_spatial$rg_blkHoles_600 = rg_blkHoles_data_10$var2
zm1_spatial$rg_blkHoles_650 = rg_blkHoles_data_11$var2
zm1_spatial$rg_blkHoles_700 = rg_blkHoles_data_12$var2

rg_blkHoles_150
rg_blkHoles_200
rg_blkHoles_250
rg_blkHoles_300
rg_blkHoles_350
rg_blkHoles_400
rg_blkHoles_450
rg_blkHoles_500
rg_blkHoles_550
rg_blkHoles_600
rg_blkHoles_650
rg_blkHoles_700

rg_contig_data_1 = integrate_bpna(x = rg_contig_150)
rg_contig_data_2 = integrate_bpna(x = rg_contig_200)
rg_contig_data_3 = integrate_bpna(x = rg_contig_250)
rg_contig_data_4 = integrate_bpna(x = rg_contig_300)
rg_contig_data_5 = integrate_bpna(x = rg_contig_350)
rg_contig_data_6 = integrate_bpna(x = rg_contig_400)
rg_contig_data_7 = integrate_bpna(x = rg_contig_450)
rg_contig_data_8 = integrate_bpna(x = rg_contig_500)
rg_contig_data_9 = integrate_bpna(x = rg_contig_550)
rg_contig_data_10 = integrate_bpna(x = rg_contig_600)
rg_contig_data_11 = integrate_bpna(x = rg_contig_650)
rg_contig_data_12 = integrate_bpna(x = rg_contig_700)

zm1_spatial$rg_contig_150 = rg_contig_data_1$var2
zm1_spatial$rg_contig_200 = rg_contig_data_2$var2
zm1_spatial$rg_contig_250 = rg_contig_data_3$var2
zm1_spatial$rg_contig_300 = rg_contig_data_4$var2
zm1_spatial$rg_contig_350 = rg_contig_data_5$var2
zm1_spatial$rg_contig_400 = rg_contig_data_6$var2
zm1_spatial$rg_contig_450 = rg_contig_data_7$var2
zm1_spatial$rg_contig_500 = rg_contig_data_8$var2
zm1_spatial$rg_contig_550 = rg_contig_data_9$var2
zm1_spatial$rg_contig_600 = rg_contig_data_10$var2
zm1_spatial$rg_contig_650 = rg_contig_data_11$var2
zm1_spatial$rg_contig_700 = rg_contig_data_12$var2

rg_contig_150
rg_contig_200
rg_contig_250
rg_contig_300
rg_contig_350
rg_contig_400
rg_contig_450
rg_contig_500
rg_contig_550
rg_contig_600
rg_contig_650
rg_contig_700

# Basic histogram to determine classification type
hist(zm1_spatial$rg_flRat_700, breaks=40)

# Preparation of fisher-jenks classification for the map legend 
fish_jks7<-classIntervals(zm1_spatial$rg_flRat_700, style="fisher", n=7)
# Preparation of standard deviation classification for the map legend
sd7 <- classIntervals(zoi_isf$rg_1,style="sd", n=7)
# Deplacer le zéro
fish_jks7$brks[2] <- 0 
sd7$brks[3] <- 0
hist(zoi_hre$rg_isf, breaks=fish_jks7$brks)


# Colour prep
couleurs<-carto.pal(pal1 = "blue.pal", n1 = 1, pal2 = "red.pal", n2 = 6)

# Dessin de la carte
choroLayer(spdf=zm1_spatial, df=zm1_spatial@data, var="rg_flRat_700", breaks=fish_jks7$brks, col=couleurs, border=FALSE, lwd=0.2, 
           legend.values.rnd = 2, legend.pos="topleft", legend.title.txt = "coefs.")

layoutLayer("TEB T1~ Floor Ratio Strat_1 (GWR) : coefs, non-adapted BW: 700m", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Pervious data: OSM Contributors, Météo France", frame = F, scale = 5)

# Habillage avec superposition par les USR du maillage de MApUCE.
plot(zm1_spatial, col = NA, border = "grey40", add = TRUE)

north(pos = "topright")
dev.off()
