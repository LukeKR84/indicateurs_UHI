# Load necessary packages 
library(cartography)
library(sf)
library(classInt)
library(GWmodel)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(ggpubr)

# Set working directory 
# setwd("C:/Stage_IGN/indicateurs/gwr_master/quality_control_check")
setwd("C:/Stage_IGN/indicateurs/gwr_master/zoi_files")

# Load zones of interest
zoi_1 <- read_sf("C:/Stage_IGN/indicateurs/gwr_master/zoi_files/zoi_indicateurs_strat_1.geojson")
zoi_2 <- read_sf("C:/Stage_IGN/indicateurs/gwr_master/zoi_files/zoi_indicateurs_strat_2.geojson")

# Load larger scale zone of interest for the map dimensions
zoi_frame <- read_sf("C:/Stage_IGN/indicateurs/lisa/zoi_autostat.geojson")

# Fix the projection
st_crs(zoi_1) <- 2154
st_crs(zoi_2) <- 2154

# Conversion of ZOI to a spatial object
zoi_spatial<-as(zoi_2, 'Spatial')

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

# GWR

# Creation of a distrance matrix for the spatial object
dm_zoi <- gw.dist(sp::coordinates(zoi_spatial))
dm_bsf <- gw.dist(sp::coordinates(zoi_bsf))
dm_hre <- gw.dist(sp::coordinates(zoi_hre))
dm_isf <- gw.dist(sp::coordinates(zoi_isf))
dm_psf <- gw.dist(sp::coordinates(zoi_psf))
dm_asf <- gw.dist(sp::coordinates(zoi_asf))

# GWR according to the 700m bandwidth
g_bsf <- gwr.basic(TEB_T1~bsf, data = zoi_bsf, bw = 700, kernel = "gaussian", adaptive = F, dMat = dm_bsf)
g_hre <- gwr.basic(TEB_T1~hre, data = zoi_hre, bw = 700, kernel = "gaussian", adaptive = F, dMat = dm_hre)
g_isf <- gwr.basic(TEB_T1~isf, data = zoi_isf, bw = 700, kernel = "gaussian", adaptive = F, dMat = dm_isf)
g_psf <- gwr.basic(TEB_T1~psf, data = zoi_psf, bw = 700, kernel = "gaussian", adaptive = F, dMat = dm_psf)
g_asf <- gwr.basic(TEB_T1~asf, data = zoi_asf, bw = 700, kernel = "gaussian", adaptive = F, dMat = dm_asf)

g_bsf
g_hre
g_isf
g_psf
g_asf

# Sockage des résultats dans une variable, puis dans les données attributaires
rg_bsf<-as.data.frame(g_bsf$SDF)
zoi_bsf$rg_bsf=rg_bsf$bsf
zoi_bsf$r2=rg_bsf$Local_R2
zoi_bsf$se=rg_bsf$bsf_SE

rg_hre<-as.data.frame(g_hre$SDF)
zoi_hre$rg_hre=rg_hre$hre
zoi_hre$r2=rg_hre$Local_R2
zoi_hre$se=rg_hre$hre_SE

rg_isf<-as.data.frame(g_isf$SDF)
zoi_isf$rg_isf=rg_isf$isf
zoi_isf$r2=rg_isf$Local_R2
zoi_isf$se=rg_isf$isf_SE

rg_psf<-as.data.frame(g_psf$SDF)
zoi_psf$rg_psf=rg_psf$psf
zoi_psf$r2=rg_psf$Local_R2
zoi_psf$se=rg_psf$psf_SE

rg_asf <- as.data.frame(g_asf$SDF)
zoi_asf$rg_asf=rg_asf$asf
zoi_asf$r2=rg_asf$Local_R2
zoi_asf$se=rg_asf$asf_SE

######## WRITE GEOJSON FILES FOR USE AS 3D MODELS #########

# Convert SPDF objects back to sf objects
zoi_bsf_sf <- st_as_sf(zoi_bsf)
zoi_hre_sf <- st_as_sf(zoi_hre)
zoi_isf_sf <- st_as_sf(zoi_isf)
zoi_psf_sf <- st_as_sf(zoi_psf)
zoi_asf_sf <- st_as_sf(zoi_asf)
# Fix the projection
st_crs(zoi_bsf_sf) <- 2154
st_crs(zoi_hre_sf) <- 2154
st_crs(zoi_isf_sf) <- 2154
st_crs(zoi_psf_sf) <- 2154
st_crs(zoi_asf_sf) <- 2154
# Write geojson files
st_write(zoi_bsf_sf, "r2_bsf_visu.geojson")
st_write(zoi_hre_sf, "r2_hre_visu.geojson")
st_write(zoi_isf_sf, "r2_isf_visu.geojson")
st_write(zoi_psf_sf, "r2_psf_visu.geojson")
st_write(zoi_asf_sf, "r2_asf_visu.geojson")

######## GWR COEF MAPS ##############################################################

# Basic histogram to determine classification type
hist(zoi_asf$rg_asf, breaks=40)

# Preparation of fisher-jenks classification for the map legend 
fish_jks7<-classIntervals(zoi_asf$rg_asf, style="fisher", n=7)
# Deplacer le zéro
fish_jks7$brks[2] <- 0 
# Preparation of standard deviation classification for the map legend
sd7 <- classIntervals(zoi_isf$rg_isf, style="sd", n=7)
# Manual breaks
#brks <- c(-0.50, -0.25, 0, 0.25, 0.5, 1, 2, 3.5)

couleurs<-carto.pal(pal1 = "blue.pal", n1 = 1, pal2 = "red.pal", n2 = 6)

# RÃ©glage des proportions, dimensions et marges du fichier image
png(filename = "asf_gwr_700_coefs.png", width = 800, height = 600, res = 100)
par(mar = c(0,0,1.2,0))

# Dessin de la carte
choroLayer(spdf=zoi_asf, df=zoi_asf@data, var="rg_asf", breaks=fish_jks7$brks, col=couleurs, border=FALSE, lwd=0.2,
           legend.values.rnd = 2, legend.pos="n")

legendChoro(pos = "topleft", title.txt = "Coef.", breaks = fish_jks7$brks, col = couleurs, cex = 1, values.rnd = 2, nodata = T,
            nodata.txt = "NA", border = "black")

layoutLayer("TEB T1~ASF Strat 2 (GWR) : Local Coef. values, non-adapted 700m BW", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Building data: IGN, Impervious data: contributors OSM, Temperature data: Météo France", 
            frame = F, scale = 5, posscale = "bottomright")

# Habillage avec superposition par les USR du maillage de MApUCE.
plot(zoi_spatial, col = NA, border = "Grey40", add = TRUE)

north(pos = "topright")
dev.off()

######## LOCAL R2 MAPS ###############################################################

# Basic histogram to determine classification type
hist(zoi_asf$r2, breaks=40)

# Preparation of fisher-jenks classification for the map legend 
fish_jks7<-classIntervals(zoi_asf$r2, style="fisher", n=7)
# Preparation of standard deviation classification for the map legend
sd7 <- classIntervals(zoi_psf$rg_psf, style="sd", n=7)

# Local R2 colours
couleurs <- brewer.pal(7, "YlOrBr")

# RÃ©glage des proportions, dimensions et marges du fichier image
png(filename = "asf_gwr_700_R2_locaux.png", width = 800, height = 600, res = 100)
par(mar = c(0,0,1.2,0))

# Dessin de la carte
choroLayer(spdf=zoi_asf, df=zoi_asf@data, var="r2", breaks=fish_jks7$brks, col=couleurs, border=FALSE, lwd=0.2,
            legend.values.rnd = 2, legend.pos="n")

legendChoro(pos = "topleft", title.txt = "Local R2", breaks = fish_jks7$brks, col = couleurs, cex = 1,  nodata = T,
            nodata.txt = "NA", border = "black")
           
layoutLayer("TEB T1~ASF (GWR) : Local R2 values, non-adapted 700m BW", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Building data: IGN, Impervious data: contributors OSM, Temperature data: Météo France", frame = F, scale = 5, posscale = "bottomright")

# Habillage avec superposition par les USR du maillage de MApUCE.
plot(zoi_spatial, col = NA, border = "Grey40", add = TRUE)

north(pos = "topright")
dev.off()

############## SE MAPS ################################################################

# Basic histogram to determine classification type
hist(zoi_asf$se, breaks=40)

# Preparation of fisher-jenks classification for the map legend 
fish_jks7<-classIntervals(zoi_asf$se, style="fisher", n=7)
# Just for the PSF
fish_jks7$brks[2] <- 0.06 
# Preparation of standard deviation classification for the map legend
sd7 <- classIntervals(zoi_psf$rg_psf, style="sd", n=7)

# Local SE colours
couleurs <- brewer.pal(7, "RdPu")

# RÃ©glage des proportions, dimensions et marges du fichier image
png(filename = "asf_gwr_700_SE_locaux.png", width = 800, height = 600, res = 100)
par(mar = c(0,0,1.2,0))

# Dessin de la carte
choroLayer(spdf=zoi_asf, df=zoi_asf@data, var="se", breaks=fish_jks7$brks, col=couleurs, border=FALSE, lwd=0.2,
           colNA = "grey", legend.pos="n")

legendChoro(pos = "topleft", title.txt = "Local SE", breaks = fish_jks7$brks, col = couleurs, cex = 1, values.rnd = 2,  nodata = T,
            nodata.txt = "NA", border = "black")

layoutLayer("TEB T1~ASF (GWR) : Local Standard Error values, non-adapted 700m BW", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Building data: IGN, Impervious data: contributors OSM, Temperature data: Météo France", frame = F, scale = 5, posscale = "bottomright")

# Habillage avec superposition par les USR du maillage de MApUCE.
plot(zoi_spatial, col = NA, border = "Grey40", add = TRUE)

north(pos = "topright")
dev.off()

############### LINE GRAPHS ##################################################################

# Largely unsuccessful

asf_data <- data.frame("Intercept_asf" = rg_asf$Intercept, "asf_coef" = rg_asf$asf, "asf_SE" = rg_asf$asf_SE, "asf_R2" = rg_asf$Local_R2)
bsf_data <- data.frame("Intercept_bsf" = rg_bsf$Intercept, "bsf_coef" = rg_bsf$bsf, "bsf_SE" = rg_bsf$bsf_SE, "bsf_R2" = rg_bsf$Local_R2)
hre_data <- data.frame("Intercept_hre" = rg_hre$Intercept, "hre_coef" = rg_hre$hre, "hre_SE" = rg_hre$hre_SE, "hre_R2" = rg_hre$Local_R2)
isf_data <- data.frame("Intercept_isf" = rg_isf$Intercept, "isf_coef" = rg_isf$isf, "isf_SE" = rg_isf$isf_SE, "isf_R2" = rg_isf$Local_R2)
psf_data <- data.frame("Intercept_psf" = rg_psf$Intercept, "psf_coef" = rg_psf$psf, "psf_SE" = rg_psf$psf_SE, "psf_R2" = rg_psf$Local_R2)

asf_data <- mutate(asf_data, id = rownames(asf_data))
bsf_data <- mutate(bsf_data, id = rownames(bsf_data))
hre_data <- mutate(hre_data, id = rownames(hre_data))
isf_data <- mutate(isf_data, id = rownames(isf_data))
psf_data <- mutate(psf_data, id = rownames(psf_data))

stat_data <- full_join(asf_data, bsf_data, by = "id")
stat_data2 <- full_join(isf_data, hre_data, by = "id") 
stat_data <- full_join(stat_data, stat_data2, by= "id")
stat_data <- full_join(stat_data, psf_data, by = "id")
rm(stat_data2)

ggplot(stat_data, aes())

hist(stat_data$bsf_SE, breaks = 40)

temp_data <- data.frame("pk_usr" = zoi_1$pk_usr, "TEB_T1" = zoi_1$TEB_T1)
temp_data <- mutate(temp_data, id = row.names(temp_data))

temp_data <- full_join(temp_data, stat_data, by = "id")

ggplot(temp_data, aes(x=pk_usr)) +
  geom_line(aes(y = asf_coef), colour = "darkred")

################ EXPERIMENTS ###################################################################

zoi_2_ex <- as(zoi_2, 'Spatial')
# Remove NA values in the indicator columns
zoi_2_ex$bsf[is.na(zoi_2_ex$bsf)] <- 0
zoi_2_ex$hre[is.na(zoi_2_ex$hre)] <- 0
zoi_2_ex$isf[is.na(zoi_2_ex$isf)] <- 0
zoi_2_ex$psf[is.na(zoi_2_ex$psf)] <- 0
zoi_2_ex$asf[is.na(zoi_2_ex$asf)] <- 0


g_bsf_ex <- gwr.basic(TEB_T1~bsf, data = zoi_2_ex, bw = 700, kernel = "gaussian", adaptive = F, dMat = dm_zoi)
g_hre_ex <- gwr.basic(TEB_T1~hre, data = zoi_2_ex, bw = 700, kernel = "gaussian", adaptive = F, dMat = dm_zoi)
g_isf_ex <- gwr.basic(TEB_T1~isf, data = zoi_2_ex, bw = 700, kernel = "gaussian", adaptive = F, dMat = dm_zoi)
g_psf_ex <- gwr.basic(TEB_T1~psf, data = zoi_2_ex, bw = 700, kernel = "gaussian", adaptive = F, dMat = dm_zoi)
g_asf_ex <- gwr.basic(TEB_T1~asf, data = zoi_2_ex, bw = 700, kernel = "gaussian", adaptive = F, dMat = dm_zoi)

g_bsf_ex
g_hre_ex
g_isf_ex
g_psf_ex
g_asf_ex

# Sockage des résultats dans une variable, puis dans les données attributaires
rg_bsf_ex<-as.data.frame(g_bsf_ex$SDF)
zoi_2_ex$rg_bsf=rg_bsf_ex$bsf
zoi_2_ex$r2_bsf=rg_bsf_ex$Local_R2
zoi_2_ex$se_bsf=rg_bsf_ex$bsf_SE

rg_hre_ex<-as.data.frame(g_hre_ex$SDF)
zoi_2_ex$rg_hre=rg_hre_ex$hre
zoi_2_ex$r2_hre=rg_hre_ex$Local_R2
zoi_2_ex$se_hre=rg_hre_ex$hre_SE

rg_isf_ex<-as.data.frame(g_isf_ex$SDF)
zoi_2_ex$rg_isf=rg_isf_ex$isf
zoi_2_ex$r2_isf=rg_isf_ex$Local_R2
zoi_2_ex$se_isf=rg_isf_ex$isf_SE

rg_psf_ex<-as.data.frame(g_psf_ex$SDF)
zoi_2_ex$rg_psf=rg_psf_ex$psf
zoi_2_ex$r2_psf=rg_psf_ex$Local_R2
zoi_2_ex$se_psf=rg_psf_ex$psf_SE

rg_asf_ex <- as.data.frame(g_asf_ex$SDF)
zoi_2_ex$rg_asf=rg_asf_ex$asf
zoi_2_ex$r2_asf=rg_asf_ex$Local_R2
zoi_2_ex$se_asf=rg_asf_ex$asf_SE

# Convert zoi_ex to a dataframe in order to plot data
zoi_2_gwr_data <- as.data.frame(zoi_2_ex)
# Convert 0s back to NAs
zoi_2_gwr_data$bsf[which(zoi_2_gwr_data$bsf == 0)] <- NA
zoi_2_gwr_data$hre[which(zoi_2_gwr_data$hre == 0)] <- NA
zoi_2_gwr_data$isf[which(zoi_2_gwr_data$isf == 0)] <- NA
zoi_2_gwr_data$psf[which(zoi_2_gwr_data$psf == 0)] <- NA
zoi_2_gwr_data$asf[which(zoi_2_gwr_data$asf == 0)] <- NA

# Where indicators are NA, subsequent GWR calculations are also modified to NA

zoi_2_gwr_data$rg_bsf <- ifelse(is.na(zoi_2_gwr_data$bsf) == T, zoi_2_gwr_data$rg_bsf == NA, zoi_2_gwr_data$rg_bsf)
zoi_2_gwr_data$r2_bsf <- ifelse(is.na(zoi_2_gwr_data$bsf) == T, zoi_2_gwr_data$r2_bsf == NA, zoi_2_gwr_data$r2_bsf)
zoi_2_gwr_data$se_bsf <- ifelse(is.na(zoi_2_gwr_data$bsf) == T, zoi_2_gwr_data$se_bsf == NA, zoi_2_gwr_data$se_bsf)

zoi_2_gwr_data$rg_hre <- ifelse(is.na(zoi_2_gwr_data$hre) == T, zoi_2_gwr_data$rg_hre == NA, zoi_2_gwr_data$rg_hre)
zoi_2_gwr_data$r2_hre <- ifelse(is.na(zoi_2_gwr_data$hre) == T, zoi_2_gwr_data$r2_hre == NA, zoi_2_gwr_data$r2_hre)
zoi_2_gwr_data$se_hre <- ifelse(is.na(zoi_2_gwr_data$hre) == T, zoi_2_gwr_data$se_hre == NA, zoi_2_gwr_data$se_hre)

zoi_2_gwr_data$rg_isf <- ifelse(is.na(zoi_2_gwr_data$isf) == T, zoi_2_gwr_data$rg_isf == NA, zoi_2_gwr_data$rg_isf)
zoi_2_gwr_data$r2_isf <- ifelse(is.na(zoi_2_gwr_data$isf) == T, zoi_2_gwr_data$r2_isf == NA, zoi_2_gwr_data$r2_isf)
zoi_2_gwr_data$se_isf <- ifelse(is.na(zoi_2_gwr_data$isf) == T, zoi_2_gwr_data$se_isf == NA, zoi_2_gwr_data$se_isf)

zoi_2_gwr_data$rg_psf <- ifelse(is.na(zoi_2_gwr_data$psf) == T, zoi_2_gwr_data$rg_psf == NA, zoi_2_gwr_data$rg_psf)
zoi_2_gwr_data$r2_psf <- ifelse(is.na(zoi_2_gwr_data$psf) == T, zoi_2_gwr_data$r2_psf == NA, zoi_2_gwr_data$r2_psf)
zoi_2_gwr_data$se_psf <- ifelse(is.na(zoi_2_gwr_data$psf) == T, zoi_2_gwr_data$se_psf == NA, zoi_2_gwr_data$se_psf)

zoi_2_gwr_data$rg_asf <- ifelse(is.na(zoi_2_gwr_data$asf) == T, zoi_2_gwr_data$rg_asf == NA, zoi_2_gwr_data$rg_asf)
zoi_2_gwr_data$r2_asf <- ifelse(is.na(zoi_2_gwr_data$asf) == T, zoi_2_gwr_data$r2_asf == NA, zoi_2_gwr_data$r2_asf)
zoi_2_gwr_data$se_asf <- ifelse(is.na(zoi_2_gwr_data$asf) == T, zoi_2_gwr_data$se_asf == NA, zoi_2_gwr_data$se_asf)

# ggplot(zoi_1_gwr_data, aes(x=se_asf)) +
#   geom_line(aes(y = TEB_T1), colour = "darkred")

 

#normalisation 1 
# zscore  (x - mean / sd(x))

normalize1 <-  function(x){
  mu <-  mean(x,na.rm = T)
  sigma  <-  sd(x, na.rm = T)
  return( (x - mu) / sigma)
}

#normalisation 2
# x- min / max _ min 
normalize2 <- function(x){
  mimi <-  min(x,na.rm = T)
  mama <-  max(x, na.rm = T)
  return ( ( x- mimi )/ (mama-mimi) )
}

zoi_1_gwr_data$norm1_TEB_T1  <-  normalize1(zoi_1_gwr_data$TEB_T1)
zoi_1_gwr_data$norm2_TEB_T1  <-  normalize2(zoi_1_gwr_data$TEB_T1)

ggplot(zoi_1_gwr_data, aes(x=norm2_TEB_T1)) +
     geom_line(aes(y = se_asf), colour = "darkred")

zoi_1_gwr_data$norm1_asf_se <- normalize1(zoi_1_gwr_data$se_asf)

ggplot(zoi_2_gwr_data, aes(x=TEB_T1)) +
  #geom_line(aes(y = se_asf), colour = "darkred") +
  geom_line(aes(y = se_psf), colour = "green")

ggplot(zoi_2_gwr_data, aes(x=se_asf)) +
  geom_line(colour ="green")

ggplot(data = zoi_2_gwr_data, aes(x=se_bsf)) +
  geom_boxplot(aes(y= TEB_T1, coulour = "red"))
