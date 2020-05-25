# Load libraries
library(sf)
library(spdep)
library(ggplot2)
library(maptools)
library(classInt)
library(gstat)


# Set working directory 
setwd("C:/Stage_IGN/indicateurs/lisa")

# Load zone of interest
zoi <- read_sf("C:/Stage_IGN/indicateurs/lisa/zoi_autostat.geojson")
# Fix the projection
#st_crs(zoi) <- 102110
#zoi <- st_transform(zoi, 2154)
st_crs(zoi)
summary(zoi)

# Conversion of ZOI to a spatial object
zoi_spatial<-as(zoi, 'Spatial')

# Create a spatial weights for neighbours list
nbUSR <- poly2nb(zoi_spatial, queen = T)

voisins_pond <- nb2listw(nbUSR, style = "W")

# Limit scientific notation for reasonably-sized values
options(scipen = 7)

##### CALCULATE THE MORAN'S I FOR THE BSF ####

# Global Moran
moran.test(zoi_spatial$bsf, voisins_pond, zero.policy = T, na.action = na.exclude)

# Results printed below :

# Moran I statistic standard deviate = 93.963, p-value < 2.2e-16
# alternative hypothesis: greater
# sample estimates:
#   Moran I statistic       Expectation          Variance 
# 0.6014085433     -0.0001085894      0.0000409808 

# Calculate the local Moran's I for the BSF
local_M_BSF <- localmoran(zoi_spatial$bsf, voisins_pond, zero.policy = T, na.action = na.exclude)
summary(local_M_BSF)

# Manually make a Moran plot to standardise the variables
zoi_spatial$s_bsf <- scale(zoi_spatial$bsf) # save to a new column
# Create a lagged variable
zoi_spatial$lag_sbsf <- lag.listw(voisins_pond, zoi_spatial$s_bsf, NAOK = TRUE)

summary(zoi_spatial$s_bsf)
summary(zoi_spatial$lag_sbsf)

plot(x = zoi_spatial$s_bsf, y = zoi_spatial$lag_sbsf, main = "Moran Scatterplot BSF")
abline(h = 0, v = 0)
abline(lm(zoi_spatial$lag_sbsf ~ zoi_spatial$s_bsf), lty = 3, lwd = 4, col = "red")

# Remove NA values in the columns to be used for the moran quadrant columns
zoi_spatial$s_bsf[is.na(zoi_spatial$s_bsf)] <- 0
zoi_spatial$lag_sbsf[is.na(zoi_spatial$lag_sbsf)] <- 0
local_M_BSF[is.na(local_M_BSF[, 5])] <- 0

# identify the moran plot quadrant for each observation
zoi_spatial$quad_sigBSF <- NA
zoi_spatial@data[(zoi_spatial$s_bsf >= 0 & zoi_spatial$lag_sbsf >= 0) & (local_M_BSF[, 5] <= 0.05), "quad_sigBSF"] <- 1
zoi_spatial@data[(zoi_spatial$s_bsf <= 0 & zoi_spatial$lag_sbsf <= 0) & (local_M_BSF[, 5] <= 0.05), "quad_sigBSF"] <- 2
zoi_spatial@data[(zoi_spatial$s_bsf >= 0 & zoi_spatial$lag_sbsf <= 0) & (local_M_BSF[, 5] <= 0.05), "quad_sigBSF"] <- 3
zoi_spatial@data[(zoi_spatial$s_bsf >= 0 & zoi_spatial$lag_sbsf <= 0) & (local_M_BSF[, 5] <= 0.05), "quad_sigBSF"] <- 4
zoi_spatial@data[(zoi_spatial$s_bsf <= 0 & zoi_spatial$lag_sbsf >= 0) & (local_M_BSF[, 5] <= 0.05), "quad_sigBSF"] <- 5  # WE ASSIGN A 5 TO ALL NON-SIGNIFICANT OBSERVATIONS

# Set the breaks for the thematic map classes
breaks <- seq(1, 5, 1)
# Set the corresponding labels for the thematic map classes
labels <- c("high-High", "low-Low", "High-Low", "Low-High", "Not Signif.")
# Set intervals
np <- findInterval(zoi_spatial$quad_sigBSF, breaks)
# Assign colours to each map class
colours <- c("red", "blue", "lightpink", "skyblue2", "white")
plot(zoi_spatial, col = colours[np], border = T)
mtext("BSF - Local Moran's I", cex = 1.5, side = 3, line = 1)
legend("topleft", legend = labels, fill = colours, bty = "n")
dev.off()
##### CALCULATE THE MORAN'S I FOR THE HRE ####

# Global Moran
moran.test(zoi_spatial$hre, voisins_pond, zero.policy = T, na.action = na.exclude)

# Results printed below

# Moran I statistic standard deviate = 100.9, p-value < 2.2e-16
# alternative hypothesis: greater
# sample estimates:
#   Moran I statistic       Expectation          Variance 
# 0.64566939973    -0.00010858942     0.00004096619 

# Calculate the local Moran's I for the BSF
local_M_HRE <- localmoran(zoi_spatial$hre, voisins_pond, zero.policy = T, na.action = na.exclude)
summary(local_M_HRE)

# Manually make a Moran plot to standardise the variables
zoi_spatial$s_hre <- scale(zoi_spatial$hre) # save to a new column
# Create a lagged variable
zoi_spatial$lag_shre <- lag.listw(voisins_pond, zoi_spatial$s_hre, NAOK = TRUE)

summary(zoi_spatial$s_hre)
summary(zoi_spatial$lag_shre)

plot(x = zoi_spatial$s_hre, y = zoi_spatial$lag_shre, main = "Moran Scatterplot HRE")
abline(h = 0, v = 0)
abline(lm(zoi_spatial$lag_shre ~ zoi_spatial$s_hre), lty = 3, lwd = 4, col = "red")

# Remove NA values in the columns to be used for the moran quadrant columns
zoi_spatial$s_hre[is.na(zoi_spatial$s_hre)] <- 0
zoi_spatial$lag_shre[is.na(zoi_spatial$lag_shre)] <- 0
local_M_HRE[is.na(local_M_HRE[, 5])] <- 0

# identify the moran plot quadrant for each observation
zoi_spatial$quad_sigHRE <- NA
zoi_spatial@data[(zoi_spatial$s_hre >= 0 & zoi_spatial$lag_shre >= 0) & (local_M_HRE[, 5] <= 0.05), "quad_sigHRE"] <- 1
zoi_spatial@data[(zoi_spatial$s_hre <= 0 & zoi_spatial$lag_shre <= 0) & (local_M_HRE[, 5] <= 0.05), "quad_sigHRE"] <- 2
zoi_spatial@data[(zoi_spatial$s_hre >= 0 & zoi_spatial$lag_shre <= 0) & (local_M_HRE[, 5] <= 0.05), "quad_sigHRE"] <- 3
zoi_spatial@data[(zoi_spatial$s_hre >= 0 & zoi_spatial$lag_shre <= 0) & (local_M_HRE[, 5] <= 0.05), "quad_sigHRE"] <- 4
zoi_spatial@data[(zoi_spatial$s_hre <= 0 & zoi_spatial$lag_shre >= 0) & (local_M_HRE[, 5] <= 0.05), "quad_sigHRE"] <- 5  # WE ASSIGN A 5 TO ALL NON-SIGNIFICANT OBSERVATIONS

# Set the breaks for the thematic map classes
breaks <- seq(1, 5, 1)
# Set the corresponding labels for the thematic map classes
labels <- c("high-High", "low-Low", "High-Low", "Low-High", "Not Signif.")
# Set intervals
np <- findInterval(zoi_spatial$quad_sigHRE, breaks)
# Assign colours to each map class
colours <- c("red", "blue", "lightpink", "skyblue2", "white")
plot(zoi_spatial, col = colours[np], border = T)
mtext("HRE - Local Moran's I", cex = 1.5, side = 3, line = 1)
legend("topleft", legend = labels, fill = colours, bty = "n")
dev.off()

##### CALCULATE THE MORAN'S I FOR THE ISF ####

# Global Moran
moran.test(zoi_spatial$isf, voisins_pond, zero.policy = T, na.action = na.exclude)

# Results printed below

# Moran I statistic standard deviate = 37.134, p-value < 2.2e-16
# alternative hypothesis: greater
# sample estimates:
#   Moran I statistic       Expectation          Variance 
# 0.21754986504    -0.00009396730     0.00003435254 

# Calculate the local Moran's I for the BSF
local_M_ISF <- localmoran(zoi_spatial$isf, voisins_pond, zero.policy = T, na.action = na.exclude)
summary(local_M_ISF)

# Manually make a Moran plot to standardise the variables
zoi_spatial$s_isf <- scale(zoi_spatial$isf) # save to a new column
# Create a lagged variable
zoi_spatial$lag_sisf <- lag.listw(voisins_pond, zoi_spatial$s_isf, NAOK = TRUE)

summary(zoi_spatial$s_isf)
summary(zoi_spatial$lag_sisf)

plot(x = zoi_spatial$s_isf, y = zoi_spatial$lag_sisf, main = "Moran Scatterplot ISF")
abline(h = 0, v = 0)
abline(lm(zoi_spatial$lag_sisf ~ zoi_spatial$s_isf), lty = 3, lwd = 4, col = "red")

# Remove NA values in the columns to be used for the moran quadrant columns
zoi_spatial$s_isf[is.na(zoi_spatial$s_isf)] <- 0
zoi_spatial$lag_sisf[is.na(zoi_spatial$lag_sisf)] <- 0
local_M_ISF[is.na(local_M_ISF[, 5])] <- 0

# identify the moran plot quadrant for each observation
zoi_spatial$quad_sigISF <- NA
zoi_spatial@data[(zoi_spatial$s_isf >= 0 & zoi_spatial$lag_sisf >= 0) & (local_M_ISF[, 5] <= 0.05), "quad_sigISF"] <- 1
zoi_spatial@data[(zoi_spatial$s_isf <= 0 & zoi_spatial$lag_sisf <= 0) & (local_M_ISF[, 5] <= 0.05), "quad_sigISF"] <- 2
zoi_spatial@data[(zoi_spatial$s_isf >= 0 & zoi_spatial$lag_sisf <= 0) & (local_M_ISF[, 5] <= 0.05), "quad_sigISF"] <- 3
zoi_spatial@data[(zoi_spatial$s_isf >= 0 & zoi_spatial$lag_sisf <= 0) & (local_M_ISF[, 5] <= 0.05), "quad_sigISF"] <- 4
zoi_spatial@data[(zoi_spatial$s_isf <= 0 & zoi_spatial$lag_sisf >= 0) & (local_M_ISF[, 5] <= 0.05), "quad_sigISF"] <- 5  # WE ASSIGN A 5 TO ALL NON-SIGNIFICANT OBSERVATIONS

# Set the breaks for the thematic map classes
breaks <- seq(1, 5, 1)
# Set the corresponding labels for the thematic map classes
labels <- c("high-High", "low-Low", "High-Low", "Low-High", "Not Signif.")
# Set intervals
np <- findInterval(zoi_spatial$quad_sigISF, breaks)
# Assign colours to each map class
colours <- c("red", "blue", "lightpink", "skyblue2", "white")
plot(zoi_spatial, col = colours[np], border = T)
mtext("ISF - Local Moran's I", cex = 1.5, side = 3, line = 1)
legend("topleft", legend = labels, fill = colours, bty = "n")
dev.off()

##### CALCULATE THE MORAN'S I FOR THE PSF ####

# Global Moran
moran.test(zoi_spatial$psf, voisins_pond, zero.policy = T, na.action = na.exclude)

# Results printed below
# Moran I statistic standard deviate = 37.89, p-value < 2.2e-16
# alternative hypothesis: greater
# sample estimates:
#   Moran I statistic       Expectation          Variance 
# 0.5592836307     -0.0003345601      0.0002181405 

# Calculate the local Moran's I for the BSF
local_M_PSF <- localmoran(zoi_spatial$psf, voisins_pond, zero.policy = T, na.action = na.exclude)
summary(local_M_PSF)

# Manually make a Moran plot to standardise the variables
zoi_spatial$s_psf <- scale(zoi_spatial$psf) # save to a new column
# Create a lagged variable
zoi_spatial$lag_spsf <- lag.listw(voisins_pond, zoi_spatial$s_psf, NAOK = TRUE)

summary(zoi_spatial$s_psf)
summary(zoi_spatial$lag_spsf)

plot(x = zoi_spatial$s_psf, y = zoi_spatial$lag_spsf, main = "Moran Scatterplot PSF")
abline(h = 0, v = 0)
abline(lm(zoi_spatial$lag_spsf ~ zoi_spatial$s_psf), lty = 3, lwd = 4, col = "red")

# Remove NA values in the columns to be used for the moran quadrant columns
zoi_spatial$s_psf[is.na(zoi_spatial$s_psf)] <- 0
zoi_spatial$lag_spsf[is.na(zoi_spatial$lag_spsf)] <- 0
local_M_PSF[is.na(local_M_PSF[, 5])] <- 0

# identify the moran plot quadrant for each observation
zoi_spatial$quad_sigPSF <- NA
zoi_spatial@data[(zoi_spatial$s_psf >= 0 & zoi_spatial$lag_spsf >= 0) & (local_M_PSF[, 5] <= 0.05), "quad_sigPSF"] <- 1
zoi_spatial@data[(zoi_spatial$s_psf <= 0 & zoi_spatial$lag_spsf <= 0) & (local_M_PSF[, 5] <= 0.05), "quad_sigPSF"] <- 2
zoi_spatial@data[(zoi_spatial$s_psf >= 0 & zoi_spatial$lag_spsf <= 0) & (local_M_PSF[, 5] <= 0.05), "quad_sigPSF"] <- 3
zoi_spatial@data[(zoi_spatial$s_psf >= 0 & zoi_spatial$lag_spsf <= 0) & (local_M_PSF[, 5] <= 0.05), "quad_sigPSF"] <- 4
zoi_spatial@data[(zoi_spatial$s_psf <= 0 & zoi_spatial$lag_spsf >= 0) & (local_M_PSF[, 5] <= 0.05), "quad_sigPSF"] <- 5  # WE ASSIGN A 5 TO ALL NON-SIGNIFICANT OBSERVATIONS

# Set the breaks for the thematic map classes
breaks <- seq(1, 5, 1)
# Set the corresponding labels for the thematic map classes
labels <- c("high-High", "low-Low", "High-Low", "Low-High", "Not Signif.")
# Set intervals
np <- findInterval(zoi_spatial$quad_sigPSF, breaks)
# Assign colours to each map class
colours <- c("red", "blue", "lightpink", "skyblue2", "white")
plot(zoi_spatial, col = colours[np], border = T)
mtext("PSF - Local Moran's I", cex = 1.5, side = 3, line = 1)
legend("topleft", legend = labels, fill = colours, bty = "n")
dev.off()

###### CALCULATE THE MORAN'S I FOR THE DEMOGRAPHIC INDICATORS ####

# Global Moran indicators 
moran.test(zoi_spatial$insee_individus, voisins_pond, zero.policy = T, na.action = na.exclude)
moran.test(zoi_spatial$insee_menages, voisins_pond, zero.policy = T, na.action = na.exclude)
moran.test(zoi_spatial$insee_men_coll, voisins_pond, zero.policy = T, na.action = na.exclude)
moran.test(zoi_spatial$insee_men_surf, voisins_pond, zero.policy = T, na.action = na.exclude)
moran.test(zoi_spatial$insee_surface_collectif, voisins_pond, zero.policy = T, na.action = na.exclude)
# Results - individus

# Moran I statistic standard deviate = 30.278, p-value < 2.2e-16
# alternative hypothesis: greater
# sample estimates:
#   Moran I statistic       Expectation          Variance 
# 0.19537107247    -0.00010484378     0.00004167934

# Results - menages

# Moran I statistic standard deviate = 32.194, p-value < 2.2e-16
# alternative hypothesis: greater
# sample estimates:
#   Moran I statistic       Expectation          Variance 
# 0.20748348014    -0.00010460251     0.00004157801

# Results - men_coll

# Moran I statistic standard deviate = 35.955, p-value < 2.2e-16
# alternative hypothesis: greater
# sample estimates:
#   Moran I statistic       Expectation          Variance 
# 0.23172388524    -0.00010460251     0.00004157277 


# Results - men_surf

# Moran I statistic standard deviate = 27.301, p-value < 2.2e-16
# alternative hypothesis: greater
# sample estimates:
#   Moran I statistic       Expectation          Variance 
# 0.17592689140    -0.00010460251     0.00004157549 

# Results - surf_collectif

# Moran I statistic standard deviate = 30.666, p-value < 2.2e-16
# alternative hypothesis: greater
# sample estimates:
#   Moran I statistic       Expectation          Variance 
# 0.19760990702    -0.00010460251     0.00004156732

# Calculate the local Moran's I 
local_M_indiv <- localmoran(zoi_spatial$insee_individus, voisins_pond, zero.policy = T, na.action = na.exclude)
local_M_menages <- localmoran(zoi_spatial$insee_menages, voisins_pond, zero.policy = T, na.action = na.exclude)
local_M_menCol <- localmoran(zoi_spatial$insee_men_coll, voisins_pond, zero.policy = T, na.action = na.exclude)
local_M_menSurf <- localmoran(zoi_spatial$insee_men_surf, voisins_pond, zero.policy = T, na.action = na.exclude)
local_M_surfCol <- localmoran(zoi_spatial$insee_surface_collectif, voisins_pond, zero.policy = T, na.action = na.exclude)
# Summaries
summary(local_M_indiv)
summary(local_M_menages)
summary(local_M_menCol)
summary(local_M_menSurf)
summary(local_M_surfCol)
# Manually make Moran plots to standardise the variables
# scale columns
zoi_spatial$s_indiv <- scale(zoi_spatial$insee_individus)
zoi_spatial$s_menages <- scale(zoi_spatial$insee_menages)
zoi_spatial$s_menCol <- scale(zoi_spatial$insee_men_coll)
zoi_spatial$s_menSurf <- scale(zoi_spatial$insee_men_surf)
zoi_spatial$s_surfCol <- scale(zoi_spatial$insee_surface_collectif)
# lagged variables
zoi_spatial$lag_sindiv <- lag.listw(voisins_pond, zoi_spatial$s_indiv, NAOK = T)
zoi_spatial$lag_smenages <- lag.listw(voisins_pond, zoi_spatial$s_menages, NAOK = T)
zoi_spatial$lag_smenCol <- lag.listw(voisins_pond, zoi_spatial$s_menCol, NAOK = T)
zoi_spatial$lag_smenSurf <- lag.listw(voisins_pond, zoi_spatial$s_menSurf, NAOK = T)
zoi_spatial$lag_ssurfCol <- lag.listw(voisins_pond, zoi_spatial$s_surfCol, NAOK = T)
# Plots : modifying the x and y axes variables for a different plot each time
plot(x = zoi_spatial$s_menages, y = zoi_spatial$lag_smenages, main = "Moran Scatterplot INSEE Menages")
abline(h = 0, v = 0)
abline(lm(zoi_spatial$lag_smenages ~ zoi_spatial$s_menages), lty = 3, lwd = 4, col = "red")

# Remove NA values in the columns to be used for the moran quadrant columns
# INSEE individus 
zoi_spatial$s_indiv[is.na(zoi_spatial$s_indiv)] <- 0
zoi_spatial$lag_sindiv[is.na(zoi_spatial$lag_sindiv)] <- 0
local_M_indiv[is.na(local_M_indiv[, 5])] <- 0
# INSEE menages
zoi_spatial$s_menages[is.na(zoi_spatial$s_menages)] <- 0
zoi_spatial$lag_smenages[is.na(zoi_spatial$lag_smenages)] <- 0
local_M_menages[is.na(local_M_menages[, 5])] <- 0
# INSEE menages collectifs
zoi_spatial$s_menCol[is.na(zoi_spatial$s_menCol)] <- 0
zoi_spatial$lag_smenCol[is.na(zoi_spatial$lag_smenCol)] <- 0
local_M_menCol[is.na(local_M_menCol[, 5])] <- 0
# INSEE surface menage
zoi_spatial$s_menSurf[is.na(zoi_spatial$s_menSurf)] <- 0
zoi_spatial$lag_smenSurf[is.na(zoi_spatial$lag_smenSurf)] <- 0
local_M_menSurf[is.na(local_M_menSurf[, 5])] <- 0
# INSEE surface menage collectifs 
zoi_spatial$s_surfCol[is.na(zoi_spatial$s_surfCol)] <- 0
zoi_spatial$lag_ssurfCol[is.na(zoi_spatial$lag_ssurfCol)] <- 0
local_M_surfCol[is.na(local_M_surfCol[, 5])] <- 0

# identify the moran plot quadrant for each observation
# INSEE individus 
zoi_spatial$quad_sigINDIV <- NA
zoi_spatial@data[(zoi_spatial$s_indiv >= 0 & zoi_spatial$lag_sindiv >= 0) & (local_M_indiv[, 5] <= 0.05), "quad_sigINDIV"] <- 1
zoi_spatial@data[(zoi_spatial$s_indiv <= 0 & zoi_spatial$lag_sindiv <= 0) & (local_M_indiv[, 5] <= 0.05), "quad_sigINDIV"] <- 2
zoi_spatial@data[(zoi_spatial$s_indiv >= 0 & zoi_spatial$lag_sindiv <= 0) & (local_M_indiv[, 5] <= 0.05), "quad_sigINDIV"] <- 3
zoi_spatial@data[(zoi_spatial$s_indiv >= 0 & zoi_spatial$lag_sindiv <= 0) & (local_M_indiv[, 5] <= 0.05), "quad_sigINDIV"] <- 4
zoi_spatial@data[(zoi_spatial$s_indiv <= 0 & zoi_spatial$lag_sindiv >= 0) & (local_M_indiv[, 5] <= 0.05), "quad_sigINDIV"] <- 5  # WE ASSIGN A 5 TO ALL NON-SIGNIFICANT OBSERVATIONS
# INSEE menages
zoi_spatial$quad_sigMENAG <- NA
zoi_spatial@data[(zoi_spatial$s_menages >= 0 & zoi_spatial$lag_smenages >= 0) & (local_M_menages[, 5] <= 0.05), "quad_sigMENAG"] <- 1
zoi_spatial@data[(zoi_spatial$s_menages <= 0 & zoi_spatial$lag_smenages <= 0) & (local_M_menages[, 5] <= 0.05), "quad_sigMENAG"] <- 2
zoi_spatial@data[(zoi_spatial$s_menages >= 0 & zoi_spatial$lag_smenages <= 0) & (local_M_menages[, 5] <= 0.05), "quad_sigMENAG"] <- 3
zoi_spatial@data[(zoi_spatial$s_menages >= 0 & zoi_spatial$lag_smenages <= 0) & (local_M_menages[, 5] <= 0.05), "quad_sigMENAG"] <- 4
zoi_spatial@data[(zoi_spatial$s_menages <= 0 & zoi_spatial$lag_smenages >= 0) & (local_M_menages[, 5] <= 0.05), "quad_sigMENAG"] <- 5 
# INSEE menages collectifs
zoi_spatial$quad_sigMENCOL <- NA
zoi_spatial@data[(zoi_spatial$s_menCol >= 0 & zoi_spatial$lag_smenCol >= 0) & (local_M_menCol[, 5] <= 0.05), "quad_sigMENCOL"] <- 1
zoi_spatial@data[(zoi_spatial$s_menCol <= 0 & zoi_spatial$lag_smenCol <= 0) & (local_M_menCol[, 5] <= 0.05), "quad_sigMENCOL"] <- 2
zoi_spatial@data[(zoi_spatial$s_menCol >= 0 & zoi_spatial$lag_smenCol <= 0) & (local_M_menCol[, 5] <= 0.05), "quad_sigMENCOL"] <- 3
zoi_spatial@data[(zoi_spatial$s_menCol >= 0 & zoi_spatial$lag_smenCol <= 0) & (local_M_menCol[, 5] <= 0.05), "quad_sigMENCOL"] <- 4
zoi_spatial@data[(zoi_spatial$s_menCol <= 0 & zoi_spatial$lag_smenCol >= 0) & (local_M_menCol[, 5] <= 0.05), "quad_sigMENCOL"] <- 5 
# INSEE surface menage
zoi_spatial$quad_sigSURFMEN <- NA
zoi_spatial@data[(zoi_spatial$s_menSurf >= 0 & zoi_spatial$lag_smenSurf >= 0) & (local_M_menSurf[, 5] <= 0.05), "quad_sigSURFMEN"] <- 1
zoi_spatial@data[(zoi_spatial$s_menSurf <= 0 & zoi_spatial$lag_smenSurf <= 0) & (local_M_menSurf[, 5] <= 0.05), "quad_sigSURFMEN"] <- 2
zoi_spatial@data[(zoi_spatial$s_menSurf >= 0 & zoi_spatial$lag_smenSurf <= 0) & (local_M_menSurf[, 5] <= 0.05), "quad_sigSURFMEN"] <- 3
zoi_spatial@data[(zoi_spatial$s_menSurf >= 0 & zoi_spatial$lag_smenSurf <= 0) & (local_M_menSurf[, 5] <= 0.05), "quad_sigSURFMEN"] <- 4
zoi_spatial@data[(zoi_spatial$s_menSurf <= 0 & zoi_spatial$lag_smenSurf >= 0) & (local_M_menSurf[, 5] <= 0.05), "quad_sigSURFMEN"] <- 5 
# INSEE surface menage collectifs 
zoi_spatial$quad_sigSURFCOL <- NA
zoi_spatial@data[(zoi_spatial$s_surfCol >= 0 & zoi_spatial$lag_ssurfCol >= 0) & (local_M_surfCol[, 5] <= 0.05), "quad_sigSURFCOL"] <- 1
zoi_spatial@data[(zoi_spatial$s_surfCol <= 0 & zoi_spatial$lag_ssurfCol <= 0) & (local_M_surfCol[, 5] <= 0.05), "quad_sigSURFCOL"] <- 2
zoi_spatial@data[(zoi_spatial$s_surfCol >= 0 & zoi_spatial$lag_ssurfCol <= 0) & (local_M_surfCol[, 5] <= 0.05), "quad_sigSURFCOL"] <- 3
zoi_spatial@data[(zoi_spatial$s_surfCol >= 0 & zoi_spatial$lag_ssurfCol <= 0) & (local_M_surfCol[, 5] <= 0.05), "quad_sigSURFCOL"] <- 4
zoi_spatial@data[(zoi_spatial$s_surfCol <= 0 & zoi_spatial$lag_ssurfCol >= 0) & (local_M_surfCol[, 5] <= 0.05), "quad_sigSURFCOL"] <- 5 

# Set the breaks for the thematic map classes
breaks <- seq(1, 5, 1)
# Set the corresponding labels for the thematic map classes
labels <- c("high-High", "low-Low", "High-Low", "Low-High", "Not Signif.")
# Set intervals
np <- findInterval(zoi_spatial$quad_sigMENCOL, breaks)
# Assign colours to each map class
colours <- c("red", "blue", "lightpink", "skyblue2", "white")
plot(zoi_spatial, col = colours[np], border = "grey40")
mtext("INSEE MENAGES COLLECTIFS - Local Moran's I", cex = 1.5, side = 3, line = 1)
legend("topleft", legend = labels, fill = colours, bty = "n")
dev.off()

###### CALCULATE THE MORAN'S I FOR THE MApUCE MORPHOLOGICAL INDICATORS ####

# Global Moran tests => results not very signifigant
moran.test(zoi_spatial$floor, voisins_pond)
moran.test(zoi_spatial$floor_ratio, voisins_pond)
moran.test(zoi_spatial$compac_mean_nw, voisins_pond)
moran.test(zoi_spatial$compac_mean_w, voisins_pond)
moran.test(zoi_spatial$contig_mean, voisins_pond)
moran.test(zoi_spatial$p_vol_ratio_mean, voisins_pond)
moran.test(zoi_spatial$b_vol, voisins_pond)
moran.test(zoi_spatial$b_vol_m, voisins_pond)
moran.test(zoi_spatial$b_holes_area_mean, voisins_pond)
moran.test(zoi_spatial$b_std_h_mean, voisins_pond)
moran.test(zoi_spatial$b_m_nw_compacity, voisins_pond)
moran.test(zoi_spatial$b_m_w_compacity, voisins_pond)

# Calculate the local Moran's I 
local_M_floor <- localmoran(zoi_spatial$floor, voisins_pond)
local_M_floorRatio <- localmoran(zoi_spatial$floor_ratio, voisins_pond)
local_M_compac_mean_nw <- localmoran(zoi_spatial$compac_mean_nw, voisins_pond)
local_M_compac_mean_w <- localmoran(zoi_spatial$compac_mean_w, voisins_pond)
local_M_contig_mean <- localmoran(zoi_spatial$contig_mean, voisins_pond)
local_M_p_vol_ratio_mean <- localmoran(zoi_spatial$p_vol_ratio_mean, voisins_pond)
local_M_b_vol <- localmoran(zoi_spatial$b_vol, voisins_pond)
local_M_b_vol_m <- localmoran(zoi_spatial$b_vol_m, voisins_pond)
local_M_b_holes_area_mean <- localmoran(zoi_spatial$b_holes_area_mean, voisins_pond)
local_M_b_std_h_mean <- localmoran(zoi_spatial$b_std_h_mean, voisins_pond)
local_M_b_m_nw_compacity <- localmoran(zoi_spatial$b_m_nw_compacity, voisins_pond)
local_M_b_m_w_compacity <- localmoran(zoi_spatial$b_m_w_compacity, voisins_pond)

summary(local_M_floor)
summary(local_M_floorRatio)
summary(local_M_compac_mean_nw)
summary(local_M_compac_mean_w)
summary(local_M_contig_mean)
summary(local_M_p_vol_ratio_mean)
summary(local_M_b_vol)
summary(local_M_b_vol_m)
summary(local_M_b_holes_area_mean)
summary(local_M_b_std_h_mean)
summary(local_M_b_m_nw_compacity)
summary(local_M_b_m_w_compacity)

# Manually make Moran plots to standardise the variables
# scale columns
zoi_spatial$s_floor <- scale(zoi_spatial$floor)
zoi_spatial$s_floorRatio <- scale(zoi_spatial$floor_ratio)
zoi_spatial$s_compac_mean_nw <- scale(zoi_spatial$compac_mean_nw)
zoi_spatial$s_compac_mean_w <- scale(zoi_spatial$compac_mean_w)
zoi_spatial$s_contig_mean <- scale(zoi_spatial$contig_mean)
zoi_spatial$s_p_vol_ratio_mean <- scale(zoi_spatial$p_vol_ratio_mean)
zoi_spatial$s_b_vol <- scale(zoi_spatial$b_vol)
zoi_spatial$s_b_vol_m <- scale(zoi_spatial$b_vol_m)
zoi_spatial$s_b_holes_area_mean <- scale(zoi_spatial$b_holes_area_mean)
zoi_spatial$s_b_std_h_mean <- scale(zoi_spatial$b_std_h_mean)
zoi_spatial$s_b_m_nw_compacity <- scale(zoi_spatial$b_m_nw_compacity)
zoi_spatial$s_b_m_w_compacity <- scale(zoi_spatial$b_m_w_compacity)
# lagged variables
zoi_spatial$lag_sfloor <- lag.listw(voisins_pond, zoi_spatial$s_floor )
zoi_spatial$lag_sfloorRatio <- lag.listw(voisins_pond, zoi_spatial$s_floorRatio)
zoi_spatial$lag_scompac_mean_nw <- lag.listw(voisins_pond, zoi_spatial$s_compac_mean_nw)
zoi_spatial$lag_scompac_mean_w <- lag.listw(voisins_pond, zoi_spatial$s_compac_mean_w)
zoi_spatial$lag_scontig_mean <- lag.listw(voisins_pond, zoi_spatial$s_contig_mean)
zoi_spatial$lag_sp_vol_ratio_mean <- lag.listw(voisins_pond, zoi_spatial$s_p_vol_ratio_mean)
zoi_spatial$lag_sb_vol <- lag.listw(voisins_pond, zoi_spatial$s_b_vol)
zoi_spatial$lag_sb_vol_m <- lag.listw(voisins_pond, zoi_spatial$s_b_vol_m)
zoi_spatial$lag_sb_holes_area_mean <- lag.listw(voisins_pond, zoi_spatial$s_b_holes_area_mean)
zoi_spatial$lag_sb_std_h_mean <- lag.listw(voisins_pond, zoi_spatial$s_b_std_h_mean)
zoi_spatial$lag_sb_m_nw_compacity <- lag.listw(voisins_pond, zoi_spatial$s_b_m_nw_compacity)
zoi_spatial$lag_sb_m_w_compacity <- lag.listw(voisins_pond, zoi_spatial$s_b_m_w_compacity)

# Plots : modifying the x and y axes variables for a different plot each time
plot(x = zoi_spatial$s_floorRatio, y = zoi_spatial$lag_sfloorRatio, main = "Moran Scatterplot - Floor Ratio")
abline(h = 0, v = 0)
abline(lm(zoi_spatial$lag_sfloorRatio ~ zoi_spatial$s_floorRatio), lty = 3, lwd = 4, col = "red")

# identify the moran plot quadrant for each observation
# Floor Area
zoi_spatial$quad_sigFLOOR <- NA
zoi_spatial@data[(zoi_spatial$s_floor >= 0 & zoi_spatial$lag_sfloor >= 0) & (local_M_floor[, 5] <= 0.05), "quad_sigFLOOR"] <- 1
zoi_spatial@data[(zoi_spatial$s_floor <= 0 & zoi_spatial$lag_sfloor <= 0) & (local_M_indiv[, 5] <= 0.05), "quad_sigFLOOR"] <- 2
zoi_spatial@data[(zoi_spatial$s_floor >= 0 & zoi_spatial$lag_sfloor <= 0) & (local_M_indiv[, 5] <= 0.05), "quad_sigFLOOR"] <- 3
zoi_spatial@data[(zoi_spatial$s_floor >= 0 & zoi_spatial$lag_sfloor <= 0) & (local_M_indiv[, 5] <= 0.05), "quad_sigFLOOR"] <- 4
zoi_spatial@data[(zoi_spatial$s_floor <= 0 & zoi_spatial$lag_sfloor >= 0) & (local_M_indiv[, 5] <= 0.05), "quad_sigFLOOR"] <- 5
# Floor ratio
zoi_spatial$quad_sigFLRAT <- NA
zoi_spatial@data[(zoi_spatial$s_floorRatio >= 0 & zoi_spatial$lag_sfloorRatio >= 0) & (local_M_floorRatio[, 5] <= 0.05), "quad_sigFLRAT"] <- 1
zoi_spatial@data[(zoi_spatial$s_floorRatio <= 0 & zoi_spatial$lag_sfloorRatio <= 0) & (local_M_floorRatio[, 5] <= 0.05), "quad_sigFLRAT"] <- 2
zoi_spatial@data[(zoi_spatial$s_floorRatio >= 0 & zoi_spatial$lag_sfloorRatio <= 0) & (local_M_floorRatio[, 5] <= 0.05), "quad_sigFLRAT"] <- 3
zoi_spatial@data[(zoi_spatial$s_floorRatio >= 0 & zoi_spatial$lag_sfloorRatio <= 0) & (local_M_floorRatio[, 5] <= 0.05), "quad_sigFLRAT"] <- 4
zoi_spatial@data[(zoi_spatial$s_floorRatio <= 0 & zoi_spatial$lag_sfloorRatio >= 0) & (local_M_floorRatio[, 5] <= 0.05), "quad_sigFLRAT"] <- 5
# Non-wieghted mean bulding compacity
zoi_spatial$quad_sigNWBC <- NA
zoi_spatial@data[(zoi_spatial$s_compac_mean_nw >= 0 & zoi_spatial$lag_scompac_mean_nw >= 0) & (local_M_compac_mean_nw[, 5] <= 0.05), "quad_sigNWBC"] <- 1
zoi_spatial@data[(zoi_spatial$s_compac_mean_nw <= 0 & zoi_spatial$lag_scompac_mean_nw <= 0) & (local_M_compac_mean_nw[, 5] <= 0.05), "quad_sigNWBC"] <- 2
zoi_spatial@data[(zoi_spatial$s_compac_mean_nw >= 0 & zoi_spatial$lag_scompac_mean_nw <= 0) & (local_M_compac_mean_nw[, 5] <= 0.05), "quad_sigNWBC"] <- 3
zoi_spatial@data[(zoi_spatial$s_compac_mean_nw >= 0 & zoi_spatial$lag_scompac_mean_nw <= 0) & (local_M_compac_mean_nw[, 5] <= 0.05), "quad_sigNWBC"] <- 4
zoi_spatial@data[(zoi_spatial$s_compac_mean_nw <= 0 & zoi_spatial$lag_scompac_mean_nw >= 0) & (local_M_compac_mean_nw[, 5] <= 0.05), "quad_sigNWBC"] <- 5
# Weighted mean bulding compacity
zoi_spatial$quad_sigWBC <- NA
zoi_spatial@data[(zoi_spatial$s_compac_mean_w >= 0 & zoi_spatial$lag_scompac_mean_w >= 0) & (local_M_compac_mean_w[, 5] <= 0.05), "quad_sigWBC"] <- 1
zoi_spatial@data[(zoi_spatial$s_compac_mean_w <= 0 & zoi_spatial$lag_scompac_mean_w <= 0) & (local_M_compac_mean_w[, 5] <= 0.05), "quad_sigWBC"] <- 2
zoi_spatial@data[(zoi_spatial$s_compac_mean_w >= 0 & zoi_spatial$lag_scompac_mean_w <= 0) & (local_M_compac_mean_w[, 5] <= 0.05), "quad_sigWBC"] <- 3
zoi_spatial@data[(zoi_spatial$s_compac_mean_w >= 0 & zoi_spatial$lag_scompac_mean_w <= 0) & (local_M_compac_mean_w[, 5] <= 0.05), "quad_sigWBC"] <- 4
zoi_spatial@data[(zoi_spatial$s_compac_mean_w <= 0 & zoi_spatial$lag_scompac_mean_w >= 0) & (local_M_compac_mean_w[, 5] <= 0.05), "quad_sigWBC"] <- 5
# Mean bulding contiguity
zoi_spatial$quad_sigCONTIG <- NA
zoi_spatial@data[(zoi_spatial$s_contig_mean >= 0 & zoi_spatial$lag_scontig_mean >= 0) & (local_M_contig_mean[, 5] <= 0.05), "quad_sigCONTIG"] <- 1
zoi_spatial@data[(zoi_spatial$s_contig_mean <= 0 & zoi_spatial$lag_scontig_mean <= 0) & (local_M_contig_mean[, 5] <= 0.05), "quad_sigCONTIG"] <- 2
zoi_spatial@data[(zoi_spatial$s_contig_mean >= 0 & zoi_spatial$lag_scontig_mean <= 0) & (local_M_contig_mean[, 5] <= 0.05), "quad_sigCONTIG"] <- 3
zoi_spatial@data[(zoi_spatial$s_contig_mean >= 0 & zoi_spatial$lag_scontig_mean <= 0) & (local_M_contig_mean[, 5] <= 0.05), "quad_sigCONTIG"] <- 4
zoi_spatial@data[(zoi_spatial$s_contig_mean <= 0 & zoi_spatial$lag_scontig_mean >= 0) & (local_M_contig_mean[, 5] <= 0.05), "quad_sigCONTIG"] <- 5
# Passive volume ratio mean
zoi_spatial$quad_sigPVRM <- NA
zoi_spatial@data[(zoi_spatial$s_p_vol_ratio_mean >= 0 & zoi_spatial$lag_sp_vol_ratio_mean >= 0) & (local_M_p_vol_ratio_mean[, 5] <= 0.05), "quad_sigPVRM"] <- 1
zoi_spatial@data[(zoi_spatial$s_p_vol_ratio_mean <= 0 & zoi_spatial$lag_sp_vol_ratio_mean <= 0) & (local_M_p_vol_ratio_mean[, 5] <= 0.05), "quad_sigPVRM"] <- 2
zoi_spatial@data[(zoi_spatial$s_p_vol_ratio_mean >= 0 & zoi_spatial$lag_sp_vol_ratio_mean <= 0) & (local_M_p_vol_ratio_mean[, 5] <= 0.05), "quad_sigPVRM"] <- 3
zoi_spatial@data[(zoi_spatial$s_p_vol_ratio_mean >= 0 & zoi_spatial$lag_sp_vol_ratio_mean <= 0) & (local_M_p_vol_ratio_mean[, 5] <= 0.05), "quad_sigPVRM"] <- 4
zoi_spatial@data[(zoi_spatial$s_p_vol_ratio_mean <= 0 & zoi_spatial$lag_sp_vol_ratio_mean >= 0) & (local_M_p_vol_ratio_mean[, 5] <= 0.05), "quad_sigPVRM"] <- 5
# Building volume
zoi_spatial$quad_sigBV <- NA
zoi_spatial@data[(zoi_spatial$s_b_vol >= 0 & zoi_spatial$lag_sb_vol >= 0) & (local_M_b_vol[, 5] <= 0.05), "quad_sigBV"] <- 1
zoi_spatial@data[(zoi_spatial$s_b_vol <= 0 & zoi_spatial$lag_sb_vol <= 0) & (local_M_b_vol[, 5] <= 0.05), "quad_sigBV"] <- 2
zoi_spatial@data[(zoi_spatial$s_b_vol >= 0 & zoi_spatial$lag_sb_vol <= 0) & (local_M_b_vol[, 5] <= 0.05), "quad_sigBV"] <- 3
zoi_spatial@data[(zoi_spatial$s_b_vol >= 0 & zoi_spatial$lag_sb_vol <= 0) & (local_M_b_vol[, 5] <= 0.05), "quad_sigBV"] <- 4
zoi_spatial@data[(zoi_spatial$s_b_vol <= 0 & zoi_spatial$lag_sb_vol >= 0) & (local_M_b_vol[, 5] <= 0.05), "quad_sigBV"] <- 5
# Mean building volume
zoi_spatial$quad_sigBVM <- NA
zoi_spatial@data[(zoi_spatial$s_b_vol_m >= 0 & zoi_spatial$lag_sb_vol_m >= 0) & (local_M_b_vol_m[, 5] <= 0.05), "quad_sigBVM"] <- 1
zoi_spatial@data[(zoi_spatial$s_b_vol_m <= 0 & zoi_spatial$lag_sb_vol_m <= 0) & (local_M_b_vol_m[, 5] <= 0.05), "quad_sigBVM"] <- 2
zoi_spatial@data[(zoi_spatial$s_b_vol_m >= 0 & zoi_spatial$lag_sb_vol_m <= 0) & (local_M_b_vol_m[, 5] <= 0.05), "quad_sigBVM"] <- 3
zoi_spatial@data[(zoi_spatial$s_b_vol_m >= 0 & zoi_spatial$lag_sb_vol_m <= 0) & (local_M_b_vol_m[, 5] <= 0.05), "quad_sigBVM"] <- 4
zoi_spatial@data[(zoi_spatial$s_b_vol_m <= 0 & zoi_spatial$lag_sb_vol_m >= 0) & (local_M_b_vol_m[, 5] <= 0.05), "quad_sigBVM"] <- 5
# Mean holes in blocks 
zoi_spatial$quad_sigMHB <- NA
zoi_spatial@data[(zoi_spatial$s_b_holes_area_mean >= 0 & zoi_spatial$lag_sb_holes_area_mean >= 0) & (local_M_b_holes_area_mean[, 5] <= 0.05), "quad_sigMHB"] <- 1
zoi_spatial@data[(zoi_spatial$s_b_holes_area_mean <= 0 & zoi_spatial$lag_sb_holes_area_mean <= 0) & (local_M_b_holes_area_mean[, 5] <= 0.05), "quad_sigMHB"] <- 2
zoi_spatial@data[(zoi_spatial$s_b_holes_area_mean >= 0 & zoi_spatial$lag_sb_holes_area_mean <= 0) & (local_M_b_holes_area_mean[, 5] <= 0.05), "quad_sigMHB"] <- 3
zoi_spatial@data[(zoi_spatial$s_b_holes_area_mean >= 0 & zoi_spatial$lag_sb_holes_area_mean <= 0) & (local_M_b_holes_area_mean[, 5] <= 0.05), "quad_sigMHB"] <- 4
zoi_spatial@data[(zoi_spatial$s_b_holes_area_mean <= 0 & zoi_spatial$lag_sb_holes_area_mean >= 0) & (local_M_b_holes_area_mean[, 5] <= 0.05), "quad_sigMHB"] <- 5
# Mean block height
zoi_spatial$quad_sigMBH <- NA
zoi_spatial@data[(zoi_spatial$s_b_std_h_mean >= 0 & zoi_spatial$lag_sb_std_h_mean >= 0) & (local_M_b_std_h_mean[, 5] <= 0.05), "quad_sigMBH"] <- 1
zoi_spatial@data[(zoi_spatial$s_b_std_h_mean <= 0 & zoi_spatial$lag_sb_std_h_mean <= 0) & (local_M_b_std_h_mean[, 5] <= 0.05), "quad_sigMBH"] <- 2
zoi_spatial@data[(zoi_spatial$s_b_std_h_mean >= 0 & zoi_spatial$lag_sb_std_h_mean <= 0) & (local_M_b_std_h_mean[, 5] <= 0.05), "quad_sigMBH"] <- 3
zoi_spatial@data[(zoi_spatial$s_b_std_h_mean >= 0 & zoi_spatial$lag_sb_std_h_mean <= 0) & (local_M_b_std_h_mean[, 5] <= 0.05), "quad_sigMBH"] <- 4
zoi_spatial@data[(zoi_spatial$s_b_std_h_mean <= 0 & zoi_spatial$lag_sb_std_h_mean >= 0) & (local_M_b_std_h_mean[, 5] <= 0.05), "quad_sigMBH"] <- 5
# Mean non-weighted block compacity
zoi_spatial$quad_sigMNWBC <- NA
zoi_spatial@data[(zoi_spatial$s_b_m_nw_compacity >= 0 & zoi_spatial$lag_sb_m_nw_compacity >= 0) & (local_M_b_m_nw_compacity[, 5] <= 0.05), "quad_sigMNWBC"] <- 1
zoi_spatial@data[(zoi_spatial$s_b_m_nw_compacity <= 0 & zoi_spatial$lag_sb_m_nw_compacity <= 0) & (local_M_b_m_nw_compacity[, 5] <= 0.05), "quad_sigMNWBC"] <- 2
zoi_spatial@data[(zoi_spatial$s_b_m_nw_compacity >= 0 & zoi_spatial$lag_sb_m_nw_compacity <= 0) & (local_M_b_m_nw_compacity[, 5] <= 0.05), "quad_sigMNWBC"] <- 3
zoi_spatial@data[(zoi_spatial$s_b_m_nw_compacity >= 0 & zoi_spatial$lag_sb_m_nw_compacity <= 0) & (local_M_b_m_nw_compacity[, 5] <= 0.05), "quad_sigMNWBC"] <- 4
zoi_spatial@data[(zoi_spatial$s_b_m_nw_compacity <= 0 & zoi_spatial$lag_sb_m_nw_compacity >= 0) & (local_M_b_m_nw_compacity[, 5] <= 0.05), "quad_sigMNWBC"] <- 5
# Mean weighted block compacity
zoi_spatial$quad_sigMWBC <- NA
zoi_spatial@data[(zoi_spatial$s_b_m_w_compacity >= 0 & zoi_spatial$lag_sb_m_w_compacity >= 0) & (local_M_b_m_w_compacity[, 5] <= 0.05), "quad_sigMWBC"] <- 1
zoi_spatial@data[(zoi_spatial$s_b_m_w_compacity <= 0 & zoi_spatial$lag_sb_m_w_compacity <= 0) & (local_M_b_m_w_compacity[, 5] <= 0.05), "quad_sigMWBC"] <- 2
zoi_spatial@data[(zoi_spatial$s_b_m_w_compacity >= 0 & zoi_spatial$lag_sb_m_w_compacity <= 0) & (local_M_b_m_w_compacity[, 5] <= 0.05), "quad_sigMWBC"] <- 3
zoi_spatial@data[(zoi_spatial$s_b_m_w_compacity >= 0 & zoi_spatial$lag_sb_m_w_compacity <= 0) & (local_M_b_m_w_compacity[, 5] <= 0.05), "quad_sigMWBC"] <- 4
zoi_spatial@data[(zoi_spatial$s_b_m_w_compacity <= 0 & zoi_spatial$lag_sb_m_w_compacity >= 0) & (local_M_b_m_w_compacity[, 5] <= 0.05), "quad_sigMWBC"] <- 5

# Set the breaks for the thematic map classes
breaks <- seq(1, 5, 1)
# Set the corresponding labels for the thematic map classes
labels <- c("high-High", "low-Low", "High-Low", "Low-High", "Not Signif.")
# Set intervals
np <- findInterval(zoi_spatial$quad_sigMWBC, breaks)
# Assign colours to each map class
colours <- c("red", "blue", "lightpink", "skyblue2", "white")
plot(zoi_spatial, col = colours[np], border = "grey40")
mtext("Mean weighted block compacity - Local Moran's I", cex = 1.5, side = 3, line = 1)
legend("topleft", legend = labels, fill = colours, bty = "n")
dev.off()
