library(viridis)
library(ggplot2)
library(classInt)
library(sf)
library(RColorBrewer)
library(cartography)

# Set working directory 
setwd("C:/Stage_IGN/indicateurs/plot_indicators")

# Load ZOI files
zoi_strat1 <- read_sf("C:/Stage_IGN/indicateurs/gwr_master/zoi_files/zoi_indicateurs_strat_1.geojson")

zoi_strat2 <- read_sf("C:/Stage_IGN/indicateurs/gwr_master/zoi_files/zoi_indicateurs_strat_2.geojson")
# ZOI plots for final report
zoi <- read_sf("C:/Stage_IGN/indicateurs/lisa/zoi_autostat.geojson")
st_crs(zoi)
summary(zoi)

##### ZOI REPORT INDICATORS ####

# BSF
png("BSF_brute.png", width = 800, height = 800)
ggplot(data = zoi) + 
  geom_sf(aes(fill = bsf)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "BSF") +
  scale_fill_viridis(option = "viridis")
dev.off()

# HRE
png("HRE_brute.png", width = 800, height = 800)
ggplot(data = zoi) + 
  geom_sf(aes(fill = hre)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "HRE") +
  scale_fill_viridis(option = "magma")
dev.off()

# ISF
png("ISF_brute.png", width = 800, height = 800)
ggplot(data = zoi) + 
  geom_sf(aes(fill = isf)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "ISF") +
  scale_fill_viridis(option = "plasma")
dev.off()

# PSF
png("PSF_brute.png", width = 800, height = 800)
ggplot(data = zoi) + 
  geom_sf(aes(fill = psf)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "PSF") +
  scale_fill_gradient2(low = "blue", mid = "yellow", high = "green")
dev.off()

##### STRAT 1 INDICATORS ####

# BSF
png("Strat1_BSF.png", width = 800, height = 800)
ggplot(data = zoi_strat1) + 
  geom_sf(aes(fill = bsf)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "BSF") +
  scale_fill_gradientn(colours = rev(brewer.pal(11, "RdBu")))
dev.off()

# HRE
png("Strat1_HRE.png", width = 800, height = 800)
ggplot(data = zoi_strat1) + 
  geom_sf(aes(fill = hre)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "HRE") +
  scale_fill_viridis(option = "magma")
dev.off()

# ISF
png("Strat1_ISF.png", width = 800, height = 800)
ggplot(data = zoi_strat1) + 
  geom_sf(aes(fill = isf)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "ISF") +
  scale_fill_viridis(option = "plasma")
dev.off()

# PSF
png("Strat1_PSF_bis.png", width = 800, height = 800)
ggplot(data = zoi_strat1) + 
  geom_sf(aes(fill = psf)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "PSF") +
  scale_fill_gradient2(low = "blue", mid = "yellow", high = "green")
dev.off()

# VSF
png("Strat1_VSF.png", width = 800, height = 800)
ggplot(data = zoi_strat1) + 
  geom_sf(aes(fill = vsf)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "VSF") +
  scale_fill_gradient2(low = "yellow", mid = "blue", high = "green")
dev.off()

# ASF
png("Strat1_ASF.png", width = 800, height = 800)
ggplot(data = zoi_strat1) + 
  geom_sf(aes(fill = asf)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "ASF") +
  scale_fill_viridis(option = "inferno")
dev.off()

# Chiffres d'INSEE

# INSEE individus
png("Strat1_insee_individus.png", width = 800, height = 800)
ggplot(data = zoi_strat1) + 
  geom_sf(aes(fill = ins_ndv)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "INSEE individus") +
  # scale_fill_viridis(option = "magma")
  scale_fill_gradientn(colours = rev(brewer.pal(11, "RdYlBu")))
dev.off()

# INSEE menages
png("Strat1_insee_menages.png", width = 800, height = 800)
ggplot(data = zoi_strat1) + 
  geom_sf(aes(fill = ins_mng)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "INSEE ménages") +
  # scale_fill_viridis(option = "magma")
  scale_fill_gradientn(colours = rev(brewer.pal(11, "RdYlGn")))
dev.off()

# INSEE menages
png("Strat1_insee_menages_collectif.png", width = 800, height = 800)
ggplot(data = zoi_strat1) + 
  geom_sf(aes(fill = ins_mn_c)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "INSEE ménages collectif") +
  # scale_fill_viridis(option = "magma")
  scale_fill_gradientn(colours = rev(brewer.pal(11, "PiYG")))
dev.off()

# INSEE menages
png("Strat1_insee_menages_surf.png", width = 800, height = 800)
ggplot(data = zoi_strat1) + 
  geom_sf(aes(fill = ins_mn_s)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "ménages surfaces cumulées en m2") +
  # scale_fill_viridis(option = "magma")
  scale_fill_gradientn(colours = rev(brewer.pal(11, "Spectral")))
dev.off()

# INSEE menages
png("Strat1_insee_surfaces_collectifs.png", width = 800, height = 800)
ggplot(data = zoi_strat1) + 
  geom_sf(aes(fill = ins_sr_)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "surf. logements collectifs cumulées en m2") +
  # scale_fill_viridis(option = "magma")
  scale_fill_gradientn(colours = rev(brewer.pal(11, "BrBG")))
dev.off()

##### STRAT 2 INDICATORS ####

# BSF
png("Strat2_BSF.png", width = 800, height = 800)
ggplot(data = zoi_strat2) + 
  geom_sf(aes(fill = bsf)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "BSF") +
  scale_fill_gradientn(colours = rev(brewer.pal(11, "RdBu")))
dev.off()

# HRE
png("Strat2_HRE.png", width = 800, height = 800)
ggplot(data = zoi_strat2) + 
  geom_sf(aes(fill = hre)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "HRE") +
  scale_fill_viridis(option = "magma")
dev.off()

# ISF
png("Strat2_ISF.png", width = 800, height = 800)
ggplot(data = zoi_strat2) + 
  geom_sf(aes(fill = isf)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "ISF") +
  scale_fill_viridis(option = "plasma")
dev.off()

# PSF
png("Strat2_PSF_bis.png", width = 800, height = 800)
ggplot(data = zoi_strat2) + 
  geom_sf(aes(fill = psf)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "PSF") +
  scale_fill_gradient2(low = "blue", mid = "yellow", high = "green")
dev.off()

# VSF
png("Strat2_VSF.png", width = 800, height = 800)
ggplot(data = zoi_strat2) + 
  geom_sf(aes(fill = vsf)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "VSF") +
  scale_fill_gradient2(low = "yellow", mid = "blue", high = "green")
dev.off()

# ASF
png("Strat2_ASF.png", width = 800, height = 800)
ggplot(data = zoi_strat2) + 
  geom_sf(aes(fill = asf)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "ASF") +
  scale_fill_viridis(option = "inferno")
dev.off()


# Chiffres d'INSEE

# INSEE individus
png("Strat2_insee_individus.png", width = 800, height = 800)
ggplot(data = zoi_strat2) + 
  geom_sf(aes(fill = ins_ndv)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "INSEE individus") +
  # scale_fill_viridis(option = "magma")
  scale_fill_gradientn(colours = rev(brewer.pal(11, "RdYlBu")))
dev.off()

# INSEE menages
png("Strat2_insee_menages.png", width = 800, height = 800)
ggplot(data = zoi_strat2) + 
  geom_sf(aes(fill = ins_mng)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "INSEE ménages") +
  # scale_fill_viridis(option = "magma")
  scale_fill_gradientn(colours = rev(brewer.pal(11, "RdYlGn")))
dev.off()

# INSEE menages
png("Strat2_insee_menages_collectif.png", width = 800, height = 800)
ggplot(data = zoi_strat2) + 
  geom_sf(aes(fill = ins_mn_c)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "INSEE ménages collectif") +
  # scale_fill_viridis(option = "magma")
  scale_fill_gradientn(colours = rev(brewer.pal(11, "PiYG")))
dev.off()

# INSEE menages
png("Strat2_insee_menages_surf.png", width = 800, height = 800)
ggplot(data = zoi_strat2) + 
  geom_sf(aes(fill = ins_mn_s)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "ménages surfaces cumulées en m2") +
  # scale_fill_viridis(option = "magma")
  scale_fill_gradientn(colours = rev(brewer.pal(11, "Spectral")))
dev.off()

# INSEE menages
png("Strat2_insee_surfaces_collectifs.png", width = 800, height = 800)
ggplot(data = zoi_strat2) + 
  geom_sf(aes(fill = ins_sr_)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "surf. logements collectifs cumulées en m2") +
  # scale_fill_viridis(option = "magma")
  scale_fill_gradientn(colours = rev(brewer.pal(11, "BrBG")))
dev.off()

##### MApUCE INDICATEURS ####

zoi_mapuce <- read_sf("C:/Stage_IGN/indicateurs/lisa/zoi_autostat.geojson")

# Change zero values to NA so as to not affect mathematical calculations
zoi_mapuce <- transform(zoi_mapuce, floor_ratio = ifelse(floor_ratio==0, floor_ratio==NA, floor_ratio))

# Floor ratio
png("Mapuce_floor_ratio.png", width = 800, height = 800)
ggplot(data = zoi_mapuce) + 
  geom_sf(aes(fill = floor_ratio)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "Floor ratio") +
  scale_fill_viridis(option = "inferno")
dev.off()

##### MAPS ####

# Using the cartography package

# create a spacial object

zoi_map<-as(zoi_strat1, 'Spatial')

hist(zoi_map$psf, breaks = 40)
dev.off()
fishjks7 <- classIntervals(zoi_map$psf, style = "fisher", n = 7)

couleurs <- brewer.pal(7, "Reds") # BSF
couleurs <- brewer.pal(7, "Purples") # HRE
couleurs <- brewer.pal(7, "Blues") # ISF
couleurs <- brewer.pal(7, "YlGn") # PSF



# Floor ratio - carte des indices locaux de Moran de BSF

# R�glage des proportions, dimensions et marges du fichier image
png(filename = "carte_psf_brute.png", width = 800, height = 600, res = 100)
par(mar = c(0,0,1.2,0))

# Dessin de la carte
choroLayer(spdf=zoi_map, df=zoi_map@data, var="psf", breaks=fishjks7$brks, col=couleurs, border=FALSE, lwd=0.2,
           colNA = "grey",legend.values.rnd = 2, legend.pos="n")

legendChoro(pos = "topleft", title.txt = "PSF", breaks = fishjks7$brks, col = couleurs, cex = 0.8, values.rnd = 2, nodata = T,
            nodata.txt = "NA", nodata.col = "grey", border = "black")

layoutLayer("Valeurs brutes de PSF", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Pervious data: OSM", 
            frame = F, scale = 5, posscale = "bottomright")

# Habillage avec superposition par les USR du maillage de MApUCE.
plot(zoi_strat1$geometry, col = NA, border = "Grey40", add = TRUE)

north(pos = "topright")

dev.off()

##### TEB_T1 Maps - Strat 1 et 2 #####

# create a spacial object

zoi_map<-as(zoi_strat2, 'Spatial')

hist(zoi_map$TEB_T1, breaks = 40)

breaks <- c(297, 297.5, 298, 298.5, 299, 299.5, 300, 300.5, 300.8)

couleurs <- rev(brewer.pal(8, "RdBu")) # TEB_T1 Cold hot


# R�glage des proportions, dimensions et marges du fichier image
png(filename = "carte_teb_T1_brute_strat_2.png", width = 800, height = 600, res = 100)
par(mar = c(0,0,1.2,0))

# Dessin de la carte
choroLayer(spdf=zoi_map, df=zoi_map@data, var="TEB_T1", breaks=breaks, col=couleurs, border=FALSE, lwd=0.2,
           legend.values.rnd = 2, legend.pos="n")

legendChoro(pos = "topleft", title.txt = "TEB_T1", breaks = breaks, col = couleurs, cex = 0.8, values.rnd = 2,
            nodata = F, border = "black")

layoutLayer("Valeurs brutes de PSF (Strat�gie 2)", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Temperature data: M�t�o-France", 
            frame = F, scale = 5, posscale = "bottomright")

# Habillage avec superposition par les USR du maillage de MApUCE.
plot(zoi_map, col = NA, border = "Grey40", add = TRUE)

north(pos = "topright")

dev.off()
