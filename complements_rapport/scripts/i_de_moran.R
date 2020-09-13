# reference : https://rstudio-pubs-static.s3.amazonaws.com/4938_b5fc230d586c48b291627ff6ea484d2e.html

# Load libraries
library(sf)
library(spdep)
library(ggplot2)
library(maptools)
library(classInt)
library(gstat)
library(viridis)
library(tmap)
library(RColorBrewer)
library(cartography)

# Set working directory 
setwd("C:/Stage_IGN/indicateurs/lisa/scatterPlot")

# Load zone of interest
zoi <- read_sf("C:/Stage_IGN/indicateurs/lisa/zoi_autostat.geojson")

zoi_2 <- read_sf("C:/Stage_IGN/indicateurs/gwr_master/zoi_files/strat_2_et_mapuce.geojson")

zoi <- read_sf("C:/Stage_IGN/indicateurs/gwr_master/zoi_files/zoi_indicateurs_strat_1.geojson")

st_crs(zoi)
st_crs(zoi_2)

# Conversion of ZOI to a spatial object
zoi_spatial<-as(zoi, 'Spatial')
zoi_spatial2<-as(zoi_2, 'Spatial')

# Create a spatial weights for neighbours list
nbUSR <- poly2nb(zoi_spatial, queen = T)

voisins_pond <- nb2listw(nbUSR, style = "W")
# ZOI 2
nbUSR_2 <- poly2nb(zoi_spatial2, queen = T)

voisins_pond_2 <- nb2listw(nbUSR_2, style = "W")

# Limit scientific notation for reasonably-sized values
options(scipen = 7)



# Maps for final report

##### BSF #####

# Calculate the local Moran's I for the BSF
local_M_bsf <- localmoran(zoi$bsf, voisins_pond, zero.policy = T, na.action = na.exclude)
summary(local_M_bsf)

# Change zero values to NA so as to not affect mathematical calculations
# zoi <- transform(zoi, floor_ratio = ifelse(floor_ratio==0, floor_ratio==NA, floor_ratio))

# Manually make a Moran plot to standardise the variables

# Create a scale variable
zoi$s_bsf <- scale(zoi$bsf) # save to a new column
# Create a lagged variable
zoi$slag_bsf <- lag.listw(voisins_pond, zoi$s_bsf, NAOK = TRUE)



summary(zoi$s_bsf)
summary(zoi$slag_bsf)

coef(lm(slag_bsf ~ s_bsf, data = zoi))
# (Intercept)       s_bsf 
# -0.008489312  0.555241268 

# Test plot
plot(x = zoi$s_bsf, y = zoi$slag_bsf, main = "Moran Scatterplot BSF")
abline(h = 0, v = 0)
abline(lm(zoi$slag_bsf ~ zoi$s_bsf), lty = 3, lwd = 4, col = "red")

# Test using package function
zoi_bsf <- subset()
moran.plot(zoi$bsf, voisins_pond, labels = F, pch=19)


# Set image file proportions and dimensions
png(filename = "bsf_moran_scatter_plot.png", width = 800, height = 800, res = 100)
par(mar = c(0,0,1.2,0))
# Moran scatter plot
p <- ggplot(zoi, aes(s_bsf, slag_bsf)) +
  # Top right - light blue
  annotate("rect", xmin = Inf, xmax = 0, ymin = Inf, ymax = 0, fill = "#f19693") +
  # Bottom left - purple
  annotate("rect", xmin = - Inf, xmax = 0, ymin = - Inf, ymax = 0, fill = "#9193c4") +
  # Bottom right - pink
  annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = -Inf, fill = "#f8cbc8") +
  # Top left - light blue
  annotate("rect", xmin = 0, xmax = -Inf, ymin = Inf, ymax = 0, fill = "#aed3ed") +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  geom_point(alpha = 0.5, colour = "black", fill = "grey") +
  #lims(x=c(-1, 10), y=c(-1, 4)) +
  theme_minimal() +
  coord_fixed() +
  geom_abline(intercept = -0.008489312, slope = 0.555241268 , colour = "red", lwd =1) +
  ggtitle("Diagramme de Moran - BSF") +
  xlab("Indice de BSF")+
  ylab("Indice de BSF dans le voisinage")
p + coord_cartesian(xlim=c(-2,4), ylim = c(-2,2))

dev.off()

# Easier method, but the abline does not reach the full length of the plot
# geom_smooth(data = zoi, colour = "red", method = "lm", formula = y~x, se = F) +

# Compare the result with the spdep moran plot function
# moran.plot(zoi$floor_ratio, voisins_pond, labels = F, pch=19)


hist(zoi$s_bsf)
fishjks7 <- classIntervals(zoi$s_bsf, style = "fisher", n = 7)

zoi_map<-as(zoi, 'Spatial')

couleurs <- brewer.pal(7, "YlOrRd")

# Floor ratio - carte des indices locaux de Moran de BSF

# Réglage des proportions, dimensions et marges du fichier image
png(filename = "moran_indice_asso_bsf.png", width = 800, height = 600, res = 100)
par(mar = c(0,0,1.2,0))

# Dessin de la carte
choroLayer(spdf=zoi_map, df=zoi_map@data, var="s_bsf", breaks=fishjks7$brks, col=couleurs, border=FALSE, lwd=0.2,
           legend.values.rnd = 2, legend.pos="n")

legendChoro(pos = "topleft", title.txt = "Local I", breaks = fishjks7$brks, col = couleurs, cex = 0.8, values.rnd = 2, nodata = T,
            nodata.txt = "NA", border = "black")

layoutLayer("Local I de Moran - indice de BSF", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Building data: IGN", 
            frame = F, scale = 5, posscale = "bottomright")

# Habillage avec superposition par les USR du maillage de MApUCE.
plot(zoi_map, col = NA, border = "Grey40", add = TRUE)

north(pos = "topright")

dev.off()

# Psuedo P-value map

# Join the data
lmbsf_Data <- as.data.frame(local_M_bsf)

zoi$bsf_PV <- lmbsf_Data$`Pr(z > 0)`

hist(zoi$bsf_PV)
brks <- c(0, .001, .01, .05, .1, 1)

zoi_map<-as(zoi, 'Spatial')

couleurs <- rev(brewer.pal(9, "Blues"))

# Floor ratio - carte des indices locaux de Moran de BSF

# Réglage des proportions, dimensions et marges du fichier image
png(filename = "moran_p_values_bsf.png", width = 800, height = 600, res = 100)
par(mar = c(0,0,1.2,0))

# Dessin de la carte
choroLayer(spdf=zoi_map, df=zoi_map@data, var="bsf_PV", breaks=brks, col=couleurs, border=FALSE, lwd=0.2,
           legend.values.rnd = 2, legend.pos="n")

legendChoro(pos = "topleft", title.txt = "Local I", breaks = brks, col = couleurs, cex = 0.8, values.rnd = 3, nodata = T,
            nodata.txt = "NA", border = "black")

layoutLayer("Local I de Moran - p-values de BSF", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Building data: IGN", 
            frame = F, scale = 5, posscale = "bottomright")

# Habillage avec superposition par les USR du maillage de MApUCE.
plot(zoi_map, col = NA, border = "Grey40", add = TRUE)

north(pos = "topright")

dev.off()




# Create subsets for the quandrant map preparation
zoi_quad <- subset(zoi, (!is.na(zoi$s_bsf)))

zoi_map <- as(zoi_quad, 'Spatial')

zoi_map$slag_bsf[is.na(zoi_map$slag_bsf)] <- 0

local_M_bsf <- subset(local_M_bsf, (!is.na(local_M_bsf[, 1])))
summary(local_M_bsf)

local_M_bsf[is.nan(local_M_bsf[, 5])] <- 0
local_M_bsf[is.na(local_M_bsf[, 5])] <- 0


# BSF
zoi_map$quad_bsf <- NA
zoi_map@data[(zoi_map$s_bsf >= 0 & zoi_map$slag_bsf >= 0) & (local_M_bsf[, 5] <= 0.05), "quad_bsf"] <- 1
zoi_map@data[(zoi_map$s_bsf <= 0 & zoi_map$slag_bsf <= 0) & (local_M_bsf[, 5] <= 0.05), "quad_bsf"] <- 2
zoi_map@data[(zoi_map$s_bsf >= 0 & zoi_map$slag_bsf <= 0) & (local_M_bsf[, 5] <= 0.05), "quad_bsf"] <- 3
zoi_map@data[(zoi_map$s_bsf >= 0 & zoi_map$slag_bsf <= 0) & (local_M_bsf[, 5] <= 0.05), "quad_bsf"] <- 4
zoi_map@data[(zoi_map$s_bsf <= 0 & zoi_map$slag_bsf >= 0) & (local_M_bsf[, 5] <= 0.05), "quad_bsf"] <- 5

# Set the breaks for the thematic map classes
breaks <- seq(1, 5, 1)
# Set the corresponding labels for the thematic map classes
labels <- c("h-H", "l-L", "H-L", "L-H", "NS")
# Set intervals
np <- findInterval(zoi_map$quad_bsf, breaks)
# Assign colours to each map class
colours <- c("red", "blue", "lightpink", "skyblue2", "white")

png(filename = "bsf_moran_quadrantMap.png", width = 800, height = 600, res = 100)
par(mar = c(0,0,2.5,0))
# Plot the map
plot(zoi_map, col = colours[np], border = "grey40")
mtext("BSF - Les quadrants de l'I local de Moran", cex = 1.5, side = 3, line = 1)
legend("topleft",legend = labels, fill = colours, bty = "n")
dev.off()


##### HRE ####
# Calculate the local Moran's I for the HRE
local_M_hre <- localmoran(zoi$hre, voisins_pond, zero.policy = T, na.action = na.exclude)
summary(local_M_hre)

# Change zero values to NA so as to not affect mathematical calculations
# zoi <- transform(zoi, floor_ratio = ifelse(floor_ratio==0, floor_ratio==NA, floor_ratio))

# Manually make a Moran plot to standardise the variables

# Create a scale variable
zoi$s_hre <- scale(zoi$hre) # save to a new column
# Create a lagged variable
zoi$slag_hre <- lag.listw(voisins_pond, zoi$s_hre, NAOK = TRUE)

summary(zoi$s_hre)
summary(zoi$slag_hre)

coef(lm(slag_hre ~ s_hre, data = zoi))
# (Intercept)       s_hre 
# -0.01083743  0.60122416

# Test plot
plot(x = zoi$s_hre, y = zoi$slag_hre, main = "Moran Scatterplot HRE")
abline(h = 0, v = 0)
abline(lm(zoi$slag_hre ~ zoi$s_hre), lty = 3, lwd = 4, col = "red")



# Set image file proportions and dimensions
png(filename = "hre_moran_scatter_plot.png", width = 800, height = 800, res = 100)
par(mar = c(0,0,1.2,0))
# Moran scatter plot
p <- ggplot(zoi, aes(s_hre, slag_hre)) +
  # Top right - light blue
  annotate("rect", xmin = Inf, xmax = 0, ymin = Inf, ymax = 0, fill = "#f19693") +
  # Bottom left - purple
  annotate("rect", xmin = - Inf, xmax = 0, ymin = - Inf, ymax = 0, fill = "#9193c4") +
  # Bottom right - pink
  annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = -Inf, fill = "#f8cbc8") +
  # Top left - light blue
  annotate("rect", xmin = 0, xmax = -Inf, ymin = Inf, ymax = 0, fill = "#aed3ed") +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  geom_point(alpha = 0.5, colour = "black", fill = "grey") +
  #lims(x=c(-1, 10), y=c(-1, 4)) +
  theme_minimal() +
  coord_fixed() +
  geom_abline(intercept = -0.01083743, slope = 0.60122416 , colour = "red", lwd =1) +
  ggtitle("Diagramme de Moran - HRE") +
  xlab("Indice de HRE")+
  ylab("Indice de HRE dans le voisinage")
p + coord_cartesian(xlim=c(-2,10), ylim = c(-2,3))

dev.off()

# Easier method, but the abline does not reach the full length of the plot
# geom_smooth(data = zoi, colour = "red", method = "lm", formula = y~x, se = F) +

# Compare the result with the spdep moran plot function
# moran.plot(zoi$floor_ratio, voisins_pond, labels = F, pch=19)


hist(zoi$s_hre)
fishjks7 <- classIntervals(zoi$s_hre, style = "fisher", n = 7)

zoi_map<-as(zoi, 'Spatial')

couleurs <- brewer.pal(7, "YlOrRd")

# Floor ratio - carte des indices locaux de Moran de BSF

# Réglage des proportions, dimensions et marges du fichier image
png(filename = "moran_indice_asso_hre.png", width = 800, height = 600, res = 100)
par(mar = c(0,0,1.2,0))

# Dessin de la carte
choroLayer(spdf=zoi_map, df=zoi_map@data, var="s_hre", breaks=fishjks7$brks, col=couleurs, border=FALSE, lwd=0.2,
           legend.values.rnd = 2, legend.pos="n")

legendChoro(pos = "topleft", title.txt = "Local I", breaks = fishjks7$brks, col = couleurs, cex = 0.8, values.rnd = 2, nodata = T,
            nodata.txt = "NA", border = "black")

layoutLayer("Local I de Moran - indice de HRE", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Building data: IGN", 
            frame = F, scale = 5, posscale = "bottomright")

# Habillage avec superposition par les USR du maillage de MApUCE.
plot(zoi_map, col = NA, border = "Grey40", add = TRUE)

north(pos = "topright")

dev.off()

# Psuedo P-value map

# Join the data
lm_hre_Data <- as.data.frame(local_M_hre)

zoi$hre_PV <- lm_hre_Data$`Pr(z > 0)`

hist(zoi$hre_PV)
brks <- c(0, .001, .01, .05, .1, 1)

zoi_map<-as(zoi, 'Spatial')

couleurs <- rev(brewer.pal(9, "Blues"))


# Carte de p-values HRE

# Réglage des proportions, dimensions et marges du fichier image
png(filename = "moran_p_values_hre.png", width = 800, height = 600, res = 100)
par(mar = c(0,0,1.2,0))

# Dessin de la carte
choroLayer(spdf=zoi_map, df=zoi_map@data, var="hre_PV", breaks=brks, col=couleurs, border=FALSE, lwd=0.2,
           legend.values.rnd = 2, legend.pos="n")

legendChoro(pos = "topleft", title.txt = "Local I", breaks = brks, col = couleurs, cex = 0.8, values.rnd = 3, nodata = T,
            nodata.txt = "NA", border = "black")

layoutLayer("Local I de Moran - p-values de HRE", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Building data: IGN", 
            frame = F, scale = 5, posscale = "bottomright")

# Habillage avec superposition par les USR du maillage de MApUCE.
plot(zoi_map, col = NA, border = "Grey40", add = TRUE)

north(pos = "topright")

dev.off()


# Create subsets for the quandrant map preparation
zoi_quad <- subset(zoi, (!is.na(zoi$s_hre)))

zoi_map <- as(zoi_quad, 'Spatial')

zoi_map$slag_hre[is.na(zoi_map$slag_hre)] <- 0

local_M_hre <- subset(local_M_hre, (!is.na(local_M_hre[, 1])))

summary(local_M_hre)

local_M_hre[is.nan(local_M_hre[, 5])] <- 0
local_M_hre[is.na(local_M_hre[, 5])] <- 0
summary(local_M_hre)


# HRE
zoi_map$quad_hre <- NA
zoi_map@data[(zoi_map$s_hre >= 0 & zoi_map$slag_hre >= 0) & (local_M_hre[, 5] <= 0.05), "quad_hre"] <- 1
zoi_map@data[(zoi_map$s_hre <= 0 & zoi_map$slag_hre <= 0) & (local_M_hre[, 5] <= 0.05), "quad_hre"] <- 2
zoi_map@data[(zoi_map$s_hre >= 0 & zoi_map$slag_hre <= 0) & (local_M_hre[, 5] <= 0.05), "quad_hre"] <- 3
zoi_map@data[(zoi_map$s_hre >= 0 & zoi_map$slag_hre <= 0) & (local_M_hre[, 5] <= 0.05), "quad_hre"] <- 4
zoi_map@data[(zoi_map$s_hre <= 0 & zoi_map$slag_hre >= 0) & (local_M_hre[, 5] <= 0.05), "quad_hre"] <- 5

# Set the breaks for the thematic map classes
breaks <- seq(1, 5, 1)
# Set the corresponding labels for the thematic map classes
labels <- c("h-H", "l-L", "H-L", "L-H", "NS")
# Set intervals
np <- findInterval(zoi_map$quad_hre, breaks)
# Assign colours to each map class
colours <- c("red", "blue", "lightpink", "skyblue2", "white")

png(filename = "hre_moran_quadrantMap.png", width = 800, height = 600, res = 100)
par(mar = c(0,0,2.5,0))
# Plot the map
plot(zoi_map, col = colours[np], border = "grey40")
mtext("HRE - Les quadrants de l'I local de Moran", cex = 1.5, side = 3, line = 1)
legend("topleft",legend = labels, fill = colours, bty = "n")
dev.off()

##### ISF #####
# Calculate the local Moran's I for the HRE
local_M_isf <- localmoran(zoi$isf, voisins_pond, zero.policy = T, na.action = na.exclude)
summary(local_M_isf)

# Change zero values to NA so as to not affect mathematical calculations
# zoi <- transform(zoi, floor_ratio = ifelse(floor_ratio==0, floor_ratio==NA, floor_ratio))

# Manually make a Moran plot to standardise the variables

# Create a scale variable
zoi$s_isf <- scale(zoi$isf) # save to a new column
# Create a lagged variable
zoi$slag_isf <- lag.listw(voisins_pond, zoi$s_isf, NAOK = TRUE)

summary(zoi$s_isf)
summary(zoi$slag_isf)

coef(lm(slag_isf ~ s_isf, data = zoi))
# (Intercept)       s_isf 
# -0.06275137  0.22502135 

# Test plot
plot(x = zoi$s_isf, y = zoi$slag_isf, main = "Moran Scatterplot ISF")
abline(h = 0, v = 0)
abline(lm(zoi$slag_isf ~ zoi$s_isf), lty = 3, lwd = 4, col = "red")



# Set image file proportions and dimensions
png(filename = "isf_moran_scatter_plot.png", width = 800, height = 800, res = 100)
par(mar = c(0,0,1.2,0))
# Moran scatter plot
p <- ggplot(zoi, aes(s_isf, slag_isf)) +
  # Top right - light blue
  annotate("rect", xmin = Inf, xmax = 0, ymin = Inf, ymax = 0, fill = "#f19693") +
  # Bottom left - purple
  annotate("rect", xmin = - Inf, xmax = 0, ymin = - Inf, ymax = 0, fill = "#9193c4") +
  # Bottom right - pink
  annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = -Inf, fill = "#f8cbc8") +
  # Top left - light blue
  annotate("rect", xmin = 0, xmax = -Inf, ymin = Inf, ymax = 0, fill = "#aed3ed") +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  geom_point(alpha = 0.5, colour = "black", fill = "grey") +
  #lims(x=c(-1, 10), y=c(-1, 4)) +
  theme_minimal() +
  coord_fixed() +
  geom_abline(intercept = -0.06275137, slope = 0.22502135 , colour = "red", lwd =1) +
  ggtitle("Diagramme de Moran - ISF") +
  xlab("Indice de ISF")+
  ylab("Indice de ISF dans le voisinage")
p + coord_cartesian(xlim=c(-1, 8), ylim = c(-1,4))

dev.off()

# Easier method, but the abline does not reach the full length of the plot
# geom_smooth(data = zoi, colour = "red", method = "lm", formula = y~x, se = F) +

# Compare the result with the spdep moran plot function
# moran.plot(zoi$floor_ratio, voisins_pond, labels = F, pch=19)


hist(zoi$s_isf)
fishjks7 <- classIntervals(zoi$s_isf, style = "fisher", n = 7)

zoi_map<-as(zoi, 'Spatial')

couleurs <- brewer.pal(7, "YlOrRd")

# Floor ratio - carte des indices locaux de Moran de BSF

# Réglage des proportions, dimensions et marges du fichier image
png(filename = "moran_indice_asso_isf.png", width = 800, height = 600, res = 100)
par(mar = c(0,0,1.2,0))

# Dessin de la carte
choroLayer(spdf=zoi_map, df=zoi_map@data, var="s_isf", breaks=fishjks7$brks, col=couleurs, border=FALSE, lwd=0.2,
           colNA = "black", legend.values.rnd = 2, legend.pos="n")

legendChoro(pos = "topleft", title.txt = "Local I", breaks = fishjks7$brks, col = couleurs, cex = 0.8, values.rnd = 2, nodata = T,
            nodata.txt = "NA", nodata.col = "black", border = "black")

layoutLayer("Local I de Moran - indice de ISF", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Impervious data: OSM", 
            frame = F, scale = 5, posscale = "bottomright")

# Habillage avec superposition par les USR du maillage de MApUCE.
plot(zoi_map, col = NA, border = "Grey40", add = TRUE)

north(pos = "topright")

dev.off()

# Psuedo P-value map

# Join the data
lm_isf_Data <- as.data.frame(local_M_isf)

zoi$isf_PV <- lm_isf_Data$`Pr(z > 0)`

hist(zoi$isf_PV)
brks <- c(0, .001, .01, .05, .1, 1)

zoi_map<-as(zoi, 'Spatial')

couleurs <- rev(brewer.pal(9, "Blues"))


# Carte de p-values HRE

# Réglage des proportions, dimensions et marges du fichier image
png(filename = "moran_p_values_isf.png", width = 800, height = 600, res = 100)
par(mar = c(0,0,1.2,0))

# Dessin de la carte
choroLayer(spdf=zoi_map, df=zoi_map@data, var="isf_PV", breaks=brks, col=couleurs, border=FALSE, lwd=0.2,
           legend.values.rnd = 2, legend.pos="n")

legendChoro(pos = "topleft", title.txt = "Local I", breaks = brks, col = couleurs, cex = 0.8, values.rnd = 3, nodata = T,
            nodata.txt = "NA", border = "black")

layoutLayer("Local I de Moran - p-values de HRE", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Impervious data: OSM", 
            frame = F, scale = 5, posscale = "bottomright")

# Habillage avec superposition par les USR du maillage de MApUCE.
plot(zoi_map, col = NA, border = "Grey40", add = TRUE)

north(pos = "topright")

dev.off()



# Create subsets for the quandrant map preparation
zoi_quad <- subset(zoi, (!is.na(zoi$s_isf)))
summary(zoi_quad)
zoi_map <- as(zoi_quad, 'Spatial')

zoi_map$slag_isf[is.na(zoi_map$slag_isf)] <- 0
summary(zoi_map)
local_M_isf <- subset(local_M_isf, (!is.na(local_M_isf[, 1])))
summary(local_M_isf)

local_M_psf[is.nan(local_M_psf[, 5])] <- 0
local_M_psf[is.na(local_M_psf[, 5])] <- 0
summary(local_M_psf)


# ISF
zoi_map$quad_isf <- NA
zoi_map@data[(zoi_map$s_isf >= 0 & zoi_map$slag_isf >= 0) & (local_M_isf[, 5] <= 0.05), "quad_isf"] <- 1
zoi_map@data[(zoi_map$s_isf <= 0 & zoi_map$slag_isf <= 0) & (local_M_isf[, 5] <= 0.05), "quad_isf"] <- 2
zoi_map@data[(zoi_map$s_isf >= 0 & zoi_map$slag_isf <= 0) & (local_M_isf[, 5] <= 0.05), "quad_isf"] <- 3
zoi_map@data[(zoi_map$s_isf >= 0 & zoi_map$slag_isf <= 0) & (local_M_isf[, 5] <= 0.05), "quad_isf"] <- 4
zoi_map@data[(zoi_map$s_isf <= 0 & zoi_map$slag_isf >= 0) & (local_M_isf[, 5] <= 0.05), "quad_isf"] <- 5

# Set the breaks for the thematic map classes
breaks <- seq(1, 5, 1)
# Set the corresponding labels for the thematic map classes
labels <- c("h-H", "l-L", "H-L", "L-H", "NS")
# Set intervals
np <- findInterval(zoi_map$quad_isf, breaks)
# Assign colours to each map class
colours <- c("red", "blue", "lightpink", "skyblue2", "white")

png(filename = "isf_moran_quadrantMap.png", width = 800, height = 600, res = 100)
par(mar = c(0,0,2.5,0))
# Plot the map
plot(zoi_map, col = colours[np], border = "grey40")
mtext("ISF - Les quadrants de l'I local de Moran", cex = 1.5, side = 3, line = 1)
legend("topleft",legend = labels, fill = colours, bty = "n")
dev.off()

##### PSF ####

# Calculate the local Moran's I for the PSF
local_M_psf <- localmoran(zoi$psf, voisins_pond, zero.policy = T, na.action = na.exclude)
summary(local_M_psf)

# Change zero values to NA so as to not affect mathematical calculations
# zoi <- transform(zoi, floor_ratio = ifelse(floor_ratio==0, floor_ratio==NA, floor_ratio))

# Manually make a Moran plot to standardise the variables

# Create a scale variable
zoi$s_psf <- scale(zoi$psf) # save to a new column
# Create a lagged variable
zoi$slag_psf <- lag.listw(voisins_pond, zoi$s_psf, NAOK = TRUE)

summary(zoi$s_psf)
summary(zoi$slag_psf)

coef(lm(slag_psf ~ s_psf, data = zoi))
# (Intercept)       s_psf 
# -0.04830803  0.75517633 

# Test plot
plot(x = zoi$s_psf, y = zoi$slag_psf, main = "Moran Scatterplot PSF")
abline(h = 0, v = 0)
abline(lm(zoi$slag_psf ~ zoi$s_psf), lty = 3, lwd = 4, col = "red")



# Set image file proportions and dimensions
png(filename = "psf_moran_scatter_plot.png", width = 800, height = 800, res = 100)
par(mar = c(0,0,1.2,0))
# Moran scatter plot
p <- ggplot(zoi, aes(s_psf, slag_psf)) +
  # Top right - light blue
  annotate("rect", xmin = Inf, xmax = 0, ymin = Inf, ymax = 0, fill = "#f19693") +
  # Bottom left - purple
  annotate("rect", xmin = - Inf, xmax = 0, ymin = - Inf, ymax = 0, fill = "#9193c4") +
  # Bottom right - pink
  annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = -Inf, fill = "#f8cbc8") +
  # Top left - light blue
  annotate("rect", xmin = 0, xmax = -Inf, ymin = Inf, ymax = 0, fill = "#aed3ed") +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  geom_point(alpha = 0.5, colour = "black", fill = "grey") +
  #lims(x=c(-1, 10), y=c(-1, 4)) +
  theme_minimal() +
  coord_fixed() +
  geom_abline(intercept = -0.04830803, slope = 0.75517633 , colour = "red", lwd =1) +
  ggtitle("Diagramme de Moran - PSF") +
  xlab("Indice de PSF")+
  ylab("Indice de PSF dans le voisinage")
p + coord_cartesian(xlim=c(-1, 4), ylim = c(-1,3))

dev.off()

# Easier method, but the abline does not reach the full length of the plot
# geom_smooth(data = zoi, colour = "red", method = "lm", formula = y~x, se = F) +

# Compare the result with the spdep moran plot function
# moran.plot(zoi$floor_ratio, voisins_pond, labels = F, pch=19)


hist(zoi$s_psf)
fishjks7 <- classIntervals(zoi$s_psf, style = "fisher", n = 7)

zoi_map<-as(zoi, 'Spatial')

couleurs <- brewer.pal(7, "YlOrRd")

# Floor ratio - carte des indices locaux de Moran de BSF

# Réglage des proportions, dimensions et marges du fichier image
png(filename = "moran_indice_asso_psf.png", width = 800, height = 600, res = 100)
par(mar = c(0,0,1.2,0))

# Dessin de la carte
choroLayer(spdf=zoi_map, df=zoi_map@data, var="s_psf", breaks=fishjks7$brks, col=couleurs, border=FALSE, lwd=0.2,
           legend.values.rnd = 2, legend.pos="n")

legendChoro(pos = "topleft", title.txt = "Local I", breaks = fishjks7$brks, col = couleurs, cex = 0.8, values.rnd = 2, nodata = T,
            nodata.txt = "NA", border = "black")

layoutLayer("Local I de Moran - indice de PSF", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Pervious data: OSM", 
            frame = F, scale = 5, posscale = "bottomright")

# Habillage avec superposition par les USR du maillage de MApUCE.
plot(zoi_map, col = NA, border = "Grey40", add = TRUE)

north(pos = "topright")

dev.off()

# Psuedo P-value map

# Join the data
lm_psf_Data <- as.data.frame(local_M_psf)

zoi$psf_PV <- lm_psf_Data$`Pr(z > 0)`

hist(zoi$psf_PV)
brks <- c(0, .001, .01, .05, .1, 1)

zoi_map<-as(zoi, 'Spatial')

couleurs <- rev(brewer.pal(9, "Blues"))


# Carte de p-values HRE

# Réglage des proportions, dimensions et marges du fichier image
png(filename = "moran_p_values_psf.png", width = 800, height = 600, res = 100)
par(mar = c(0,0,1.2,0))

# Dessin de la carte
choroLayer(spdf=zoi_map, df=zoi_map@data, var="psf_PV", breaks=brks, col=couleurs, border=FALSE, lwd=0.2,
           legend.values.rnd = 2, legend.pos="n")

legendChoro(pos = "topleft", title.txt = "Local I", breaks = brks, col = couleurs, cex = 0.8, values.rnd = 3, nodata = T,
            nodata.txt = "NA", border = "black")

layoutLayer("Local I de Moran - p-values de HRE", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Pervious data: OSM", 
            frame = F, scale = 5, posscale = "bottomright")

# Habillage avec superposition par les USR du maillage de MApUCE.
plot(zoi_map, col = NA, border = "Grey40", add = TRUE)

north(pos = "topright")

dev.off()


# Create subsets for the quandrant map preparation
zoi_quad <- subset(zoi, (!is.na(zoi$s_psf)))
summary(zoi_quad)
zoi_map <- as(zoi_quad, 'Spatial')

zoi_map$slag_psf[is.na(zoi_map$slag_psf)] <- 0
summary(zoi_map)
local_M_psf <- subset(local_M_psf, (!is.na(local_M_psf[, 1])))
summary(local_M_psf)

local_M_psf[is.nan(local_M_psf[, 5])] <- 0
local_M_psf[is.na(local_M_psf[, 5])] <- 0
summary(local_M_psf)


# PSF
zoi_map$quad_psf <- NA
zoi_map@data[(zoi_map$s_psf >= 0 & zoi_map$slag_psf >= 0) & (local_M_psf[, 5] <= 0.05), "quad_psf"] <- 1
zoi_map@data[(zoi_map$s_psf <= 0 & zoi_map$slag_psf <= 0) & (local_M_psf[, 5] <= 0.05), "quad_psf"] <- 2
zoi_map@data[(zoi_map$s_psf >= 0 & zoi_map$slag_psf <= 0) & (local_M_psf[, 5] <= 0.05), "quad_psf"] <- 3
zoi_map@data[(zoi_map$s_psf >= 0 & zoi_map$slag_psf <= 0) & (local_M_psf[, 5] <= 0.05), "quad_psf"] <- 4
zoi_map@data[(zoi_map$s_psf <= 0 & zoi_map$slag_psf >= 0) & (local_M_psf[, 5] <= 0.05), "quad_psf"] <- 5

# Set the breaks for the thematic map classes
breaks <- seq(1, 5, 1)
# Set the corresponding labels for the thematic map classes
labels <- c("h-H", "l-L", "H-L", "L-H", "NS")
# Set intervals
np <- findInterval(zoi_map$quad_psf, breaks)
# Assign colours to each map class
colours <- c("red", "blue", "lightpink", "skyblue2", "white")

png(filename = "psf_moran_quadrantMap.png", width = 800, height = 600, res = 100)
par(mar = c(0,0,2.5,0))
# Plot the map
plot(zoi_map, col = colours[np], border = "grey40")
mtext("PSF - Les quadrants de l'I local de Moran", cex = 1.5, side = 3, line = 1)
legend("topleft",legend = labels, fill = colours, bty = "n")
dev.off()


##### TEB_T1 STRAT_1 & 2 ####

##### strat 1 #####

# Calculate the local Moran's I for the BSF
local_M_strat1_TEB <- localmoran(zoi$TEB_T1, voisins_pond, zero.policy = T, na.action = na.exclude)
summary(local_M_strat1_TEB)

# Change zero values to NA so as to not affect mathematical calculations
# zoi <- transform(zoi, floor_ratio = ifelse(floor_ratio==0, floor_ratio==NA, floor_ratio))

# Manually make a Moran plot to standardise the variables

# Create a scale variable
zoi$s_teb <- scale(zoi$TEB_T1) # save to a new column
# Create a lagged variable
zoi$slag_teb <- lag.listw(voisins_pond, zoi$s_teb)

summary(zoi$s_teb)
summary(zoi$slag_teb)

coef(lm(slag_teb ~ s_teb, data = zoi))
# (Intercept)       s_teb 
# -0.002626851  0.988949563 

# Test plot
plot(x = zoi$s_teb, y = zoi$slag_teb, main = "Moran Scatterplot TEB_T1 Strat_1")
abline(h = 0, v = 0)
abline(lm(zoi$slag_teb ~ zoi$s_teb), lty = 3, lwd = 4, col = "red")
dev.off()

# Test using package function
zoi_bsf <- subset()
moran.plot(zoi$TEB_T1, voisins_pond, labels = F, pch=19)
dev.off()

# Set image file proportions and dimensions
png(filename = "temp_strat_1_moran_scatter_plot.png", width = 800, height = 800, res = 100)
par(mar = c(0,0,1.2,0))
# Moran scatter plot
p <- ggplot(zoi, aes(s_teb, slag_teb)) +
  # Top right - light blue
  annotate("rect", xmin = Inf, xmax = 0, ymin = Inf, ymax = 0, fill = "#f19693") +
  # Bottom left - purple
  annotate("rect", xmin = - Inf, xmax = 0, ymin = - Inf, ymax = 0, fill = "#9193c4") +
  # Bottom right - pink
  annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = -Inf, fill = "#f8cbc8") +
  # Top left - light blue
  annotate("rect", xmin = 0, xmax = -Inf, ymin = Inf, ymax = 0, fill = "#aed3ed") +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  geom_point(alpha = 0.5, colour = "black", fill = "grey") +
  #lims(x=c(-1, 10), y=c(-1, 4)) +
  theme_minimal() +
  coord_fixed() +
  geom_abline(intercept = -0.002626851, slope = 0.988949563 , colour = "red", lwd =1) +
  ggtitle("Diagramme de Moran - TEB_T1 (Stratégie 1)") +
  xlab("Indice de TEB_T1")+
  ylab("Indice de TEB_T1 dans le voisinage")
p + coord_cartesian(xlim=c(-2,2), ylim = c(-2,2))

dev.off()

# Easier method, but the abline does not reach the full length of the plot
# geom_smooth(data = zoi, colour = "red", method = "lm", formula = y~x, se = F) +

# Compare the result with the spdep moran plot function
# moran.plot(zoi$floor_ratio, voisins_pond, labels = F, pch=19)


hist(zoi$s_teb)
equal_int7 <- classIntervals(zoi$s_teb, style = "equal", n = 7)

zoi_map<-as(zoi, 'Spatial')

couleurs <- brewer.pal(7, "YlOrRd")

# Floor ratio - carte des indices locaux de Moran de BSF

# Réglage des proportions, dimensions et marges du fichier image
png(filename = "moran_indice_asso_teb_strat_1.png", width = 800, height = 600, res = 100)
par(mar = c(0,0,1.2,0))

# Dessin de la carte
choroLayer(spdf=zoi_map, df=zoi_map@data, var="s_teb", breaks=equal_int7$brks, col=couleurs, border=FALSE, lwd=0.2,
           legend.values.rnd = 2, legend.pos="n")

legendChoro(pos = "topleft", title.txt = "Local I", breaks = equal_int7$brks, col = couleurs, cex = 0.8, values.rnd = 2, nodata = F,
            nodata.txt = "NA", border = "black")

layoutLayer("Local I de Moran - indice de TEB_T1 (Stratégie 1)", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Temperature data: Météo-France", 
            frame = F, scale = 5, posscale = "bottomright")

# Habillage avec superposition par les USR du maillage de MApUCE.
plot(zoi_map, col = NA, border = "Grey40", add = TRUE)

north(pos = "topright")

dev.off()

Psuedo P-value map

# Join the data
lm_teb_Data_1 <- as.data.frame(local_M_strat1_TEB)

zoi$teb_PV <- lm_teb_Data_1$`Pr(z > 0)`

hist(zoi$teb_PV)
brks <- c(0, .001, .01, .05, .1, 1)

zoi_map<-as(zoi, 'Spatial')

couleurs <- rev(brewer.pal(9, "Blues"))


# Carte de p-values TEB

# Réglage des proportions, dimensions et marges du fichier image
png(filename = "moran_p_values_teb_strat_1.png", width = 800, height = 600, res = 100)
par(mar = c(0,0,1.2,0))

# Dessin de la carte
choroLayer(spdf=zoi_map, df=zoi_map@data, var="teb_PV", breaks=brks, col=couleurs, border=FALSE, lwd=0.2,
           legend.values.rnd = 2, legend.pos="n")

legendChoro(pos = "topleft", title.txt = "Local I", breaks = brks, col = couleurs, cex = 0.8, values.rnd = 3, nodata = F,
            nodata.txt = "NA", border = "black")

layoutLayer("Local I de Moran - p-values de TEB_T1 (Stratégie 1)", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Temperature data: Météo-France", 
            frame = F, scale = 5, posscale = "bottomright")

# Habillage avec superposition par les USR du maillage de MApUCE.
plot(zoi_map, col = NA, border = "Grey40", add = TRUE)

north(pos = "topright")

dev.off()


# Create subsets for the quandrant map preparation

zoi_map <- as(zoi, 'Spatial')


# TEB_T1 strat 1
zoi_map$quad_teb <- NA
zoi_map@data[(zoi_map$s_teb >= 0 & zoi_map$slag_teb >= 0) & (local_M_strat1_TEB[, 5] <= 0.05), "quad_teb"] <- 1
zoi_map@data[(zoi_map$s_teb <= 0 & zoi_map$slag_teb <= 0) & (local_M_strat1_TEB[, 5] <= 0.05), "quad_teb"] <- 2
zoi_map@data[(zoi_map$s_teb >= 0 & zoi_map$slag_teb <= 0) & (local_M_strat1_TEB[, 5] <= 0.05), "quad_teb"] <- 3
zoi_map@data[(zoi_map$s_teb >= 0 & zoi_map$slag_teb <= 0) & (local_M_strat1_TEB[, 5] <= 0.05), "quad_teb"] <- 4
zoi_map@data[(zoi_map$s_teb <= 0 & zoi_map$slag_teb >= 0) & (local_M_strat1_TEB[, 5] <= 0.05), "quad_teb"] <- 5

# Set the breaks for the thematic map classes
breaks <- seq(1, 5, 1)
# Set the corresponding labels for the thematic map classes
labels <- c("h-H", "l-L", "H-L", "L-H", "NS")
# Set intervals
np <- findInterval(zoi_map$quad_teb, breaks)
# Assign colours to each map class
colours <- c("red", "blue", "lightpink", "skyblue2", "white")

png(filename = "teb_1_strat1_moran_quadrantMap.png", width = 800, height = 600, res = 100)
par(mar = c(0,0,2.5,0))
# Plot the map
plot(zoi_map, col = colours[np], border = "grey40")
mtext("TEB_T1 (Stratégie 1) - Les quadrants de l'I local de Moran", cex = 1.5, side = 3, line = 1)
legend("topleft",legend = labels, fill = colours, bty = "n")
dev.off()


##### strat 2 #####

# Calculate the local Moran's I for the BSF
local_M_strat2_TEB <- localmoran(zoi_2$TEB_T1, voisins_pond_2, zero.policy = T)
summary(local_M_strat2_TEB)

# Change zero values to NA so as to not affect mathematical calculations
# zoi <- transform(zoi, floor_ratio = ifelse(floor_ratio==0, floor_ratio==NA, floor_ratio))

# Manually make a Moran plot to standardise the variables

# Create a scale variable
zoi_2$s_teb <- scale(zoi_2$TEB_T1) # save to a new column
# Create a lagged variable
zoi_2$slag_teb <- lag.listw(voisins_pond_2, zoi_2$s_teb)

summary(zoi_2$s_teb)
summary(zoi_2$slag_teb)

coef(lm(slag_teb ~ s_teb, data = zoi_2))
# (Intercept)       s_teb 
# -0.003569998  0.974453060 

# Test plot
plot(x = zoi_2$s_teb, y = zoi_2$slag_teb, main = "Moran Scatterplot TEB_T1 Strat_2")
abline(h = 0, v = 0)
abline(lm(zoi_2$slag_teb ~ zoi_2$s_teb), lty = 3, lwd = 4, col = "red")
dev.off()

# Test using package function
moran.plot(zoi_2$TEB_T1, voisins_pond_2, labels = F, pch=19)
dev.off()

# Set image file proportions and dimensions
png(filename = "temp_strat_2_moran_scatter_plot.png", width = 800, height = 800, res = 100)
par(mar = c(0,0,1.2,0))
# Moran scatter plot
p <- ggplot(zoi_2, aes(s_teb, slag_teb)) +
  # Top right - light blue
  annotate("rect", xmin = Inf, xmax = 0, ymin = Inf, ymax = 0, fill = "#f19693") +
  # Bottom left - purple
  annotate("rect", xmin = - Inf, xmax = 0, ymin = - Inf, ymax = 0, fill = "#9193c4") +
  # Bottom right - pink
  annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = -Inf, fill = "#f8cbc8") +
  # Top left - light blue
  annotate("rect", xmin = 0, xmax = -Inf, ymin = Inf, ymax = 0, fill = "#aed3ed") +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  geom_point(alpha = 0.5, colour = "black", fill = "grey") +
  #lims(x=c(-1, 10), y=c(-1, 4)) +
  theme_minimal() +
  coord_fixed() +
  geom_abline(intercept = -0.003569998, slope = 0.974453060 , colour = "red", lwd =1) +
  ggtitle("Diagramme de Moran - TEB_T1 (Stratégie 2)") +
  xlab("Indice de TEB_T1")+
  ylab("Indice de TEB_T1 dans le voisinage")
p + coord_cartesian(xlim=c(-2.5,2), ylim = c(-2,2))

dev.off()

# Easier method, but the abline does not reach the full length of the plot
# geom_smooth(data = zoi, colour = "red", method = "lm", formula = y~x, se = F) +

# Compare the result with the spdep moran plot function
# moran.plot(zoi$floor_ratio, voisins_pond, labels = F, pch=19)


hist(zoi_2$s_teb)
equal_int7 <- classIntervals(zoi_2$s_teb, style = "equal", n = 7)

zoi_map<-as(zoi_2, 'Spatial')

couleurs <- brewer.pal(7, "YlOrRd")

# Floor ratio - carte des indices locaux de Moran de BSF

# Réglage des proportions, dimensions et marges du fichier image
png(filename = "moran_indice_asso_teb_strat_2.png", width = 800, height = 600, res = 100)
par(mar = c(0,0,1.2,0))

# Dessin de la carte
choroLayer(spdf=zoi_map, df=zoi_map@data, var="s_teb", breaks=equal_int7$brks, col=couleurs, border=FALSE, lwd=0.2,
           legend.values.rnd = 2, legend.pos="n")

legendChoro(pos = "topleft", title.txt = "Local I", breaks = equal_int7$brks, col = couleurs, cex = 0.8, values.rnd = 2, nodata = F,
            nodata.txt = "NA", border = "black")

layoutLayer("Local I de Moran - indice de TEB_T1 (Stratégie 2)", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Temperature data: Météo-France", 
            frame = F, scale = 5, posscale = "bottomright")

# Habillage avec superposition par les USR du maillage de MApUCE.
plot(zoi_map, col = NA, border = "Grey40", add = TRUE)

north(pos = "topright")

dev.off()

# Join the data
lm_teb_Data_2 <- as.data.frame(local_M_strat2_TEB)

zoi$teb_PV <- lm_teb_Data_1$`Pr(z > 0)`

hist(zoi$teb_PV)
brks <- c(0, .001, .01, .05, .1, 1)

zoi_map<-as(zoi, 'Spatial')

couleurs <- rev(brewer.pal(9, "Blues"))


# Carte de p-values TEB

# Réglage des proportions, dimensions et marges du fichier image
png(filename = "moran_p_values_teb_strat_2.png", width = 800, height = 600, res = 100)
par(mar = c(0,0,1.2,0))

# Dessin de la carte
choroLayer(spdf=zoi_map, df=zoi_map@data, var="teb_PV", breaks=brks, col=couleurs, border=FALSE, lwd=0.2,
           legend.values.rnd = 2, legend.pos="n")

legendChoro(pos = "topleft", title.txt = "Local I", breaks = brks, col = couleurs, cex = 0.8, values.rnd = 3, nodata = F,
            nodata.txt = "NA", border = "black")

layoutLayer("Local I de Moran - p-values de TEB_T1 (Stratégie 2)", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Temperature data: Météo-France", 
            frame = F, scale = 5, posscale = "bottomright")

# Habillage avec superposition par les USR du maillage de MApUCE.
plot(zoi_map, col = NA, border = "Grey40", add = TRUE)

north(pos = "topright")

dev.off()



# Create subsets for the quandrant map preparation

zoi_map <- as(zoi_2, 'Spatial')


# TEB_T1 strat 2
zoi_map$quad_teb <- NA
zoi_map@data[(zoi_map$s_teb >= 0 & zoi_map$slag_teb >= 0) & (local_M_strat2_TEB[, 5] <= 0.05), "quad_teb"] <- 1
zoi_map@data[(zoi_map$s_teb <= 0 & zoi_map$slag_teb <= 0) & (local_M_strat2_TEB[, 5] <= 0.05), "quad_teb"] <- 2
zoi_map@data[(zoi_map$s_teb >= 0 & zoi_map$slag_teb <= 0) & (local_M_strat2_TEB[, 5] <= 0.05), "quad_teb"] <- 3
zoi_map@data[(zoi_map$s_teb >= 0 & zoi_map$slag_teb <= 0) & (local_M_strat2_TEB[, 5] <= 0.05), "quad_teb"] <- 4
zoi_map@data[(zoi_map$s_teb <= 0 & zoi_map$slag_teb >= 0) & (local_M_strat2_TEB[, 5] <= 0.05), "quad_teb"] <- 5

# Set the breaks for the thematic map classes
breaks <- seq(1, 5, 1)
# Set the corresponding labels for the thematic map classes
labels <- c("h-H", "l-L", "H-L", "L-H", "NS")
# Set intervals
np <- findInterval(zoi_map$quad_teb, breaks)
# Assign colours to each map class
colours <- c("red", "blue", "lightpink", "skyblue2", "white")

png(filename = "teb_1_strat2_moran_quadrantMap.png", width = 800, height = 600, res = 100)
par(mar = c(0,0,2.5,0))
# Plot the map
plot(zoi_map, col = colours[np], border = "grey40")
mtext("TEB_T1 (Stratégie 2) - Les quadrants de l'I local de Moran", cex = 1.5, side = 3, line = 1)
legend("topleft",legend = labels, fill = colours, bty = "n")
dev.off()
