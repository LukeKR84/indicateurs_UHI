library(sf)
library(dplyr)
library(ggplot2)
library(leaflet)
library(viridis)
library(stringr)
library(rgdal)
library(classInt)
library(cartography)

## Set working directory
setwd("C:/Stage_IGN/indicateurs/data_indicators_paris")

## Load data to define the zone of interest (ZOI) as defined by Mapuce

dfUSR <-  read_sf("C:/Stage_IGN/indicateurs/mapuce/data/paris/usr/usr_mapuce_paris.geojson")
st_crs(dfUSR) # Projection Lambert93
names(dfUSR)

paris <-  dfUSR %>% filter(startsWith(code_insee,"75"))
dep92 <-  dfUSR %>% filter(startsWith(code_insee,"92"))
dep93<-  dfUSR %>% filter(startsWith(code_insee,"93"))
dep94 <-  dfUSR %>% filter(startsWith(code_insee,"94"))

IdF <-  rbind(paris, dep92, dep93, dep94)

dev.off()
plot(IdF$geometry, border="lightgray",lwd=0.2 )                 
plot(paris$geometry, add=T, border="orange", lwd=0.2)

# ZOI : points mesoNH
dfMeso <- read.csv("C:/Stage_IGN/indicateurs/mesoNH/data_mesoNH_point_O.csv")
dfMeso_sf <-  st_as_sf(dfMeso, coords = c("longitude", "latitude"))
# assuming data is on WGS 84
st_crs(dfMeso_sf) <- 4326
dfMeso_sf <-  st_transform(dfMeso_sf, 2154)
# only temperature THT variables
temperature_sf <-  dfMeso_sf [,33:64]
rm(dfMeso, dfMeso_sf)

# recompute enveloppe / extent with projected coordinates
emprise_sf <-  st_bbox(temperature_sf)
emprise_sf <-  st_as_sfc(emprise_sf)
# enlarge bbox to get approx. one extra cube width around the corner points
emprise_sf <- st_buffer(emprise_sf, dist=400) 
emprise_sf <-  st_as_sfc(st_bbox(emprise_sf)) 
# convert back to sf object
emprise_sf <-  emprise_sf %>% st_geometry %>% st_sf()

dev.off()
plot(IdF$geometry, border="lightgray",lwd=0.2 )                 
plot(paris$geometry, add=T, border="orange", lwd=0.2)
plot(temperature_sf[,1], add=T)
plot(emprise_sf, add=T)

#intersection entre l'emprise et la région parisienne
zoneEtude <-  st_intersection(emprise_sf, IdF)
plot(zoneEtude$geometry, border="gray")
plot(temperature_sf[,1], add=T, pch=16)
names(zoneEtude)

st_write(zoneEtude, "zoneEtudeUSR.shp")

## Load parisian building data in order to calculate the building surface fraction

batiParis <- read_sf("C:/Stage_IGN/indicateurs/paris_data/osm/nouvelles_paris/building_lambert93_indicators.geojson")
# names(batiParis)
# [1] "id_build"                      "id_source"                     "height_wall"                   "height_roof"                  
# [5] "nb_lev"                        "type"                          "main_use"                      "zindex"                       
# [9] "ind_perimeter"                 "ind_area"                      "ind_volume"                    "ind_floor_area"               
# [13] "ind_total_facade_length"       "ind_contiguity"                "ind_common_wall_fraction"      "ind_number_building_neighbor" 
# [17] "ind_area_concavity"            "ind_form_factor"               "ind_raw_compactness"           "ind_perimeter_convexity"      
# [21] "ind_minimum_building_spacing"  "ind_road_distance"             "ind_likelihood_large_building" "geometry"       
st_crs(batiParis) # Check that the CRS is RGF93 => OK, we can proceed with the geoprocessing operations
# Coordinate Reference System:
#   EPSG: 2154 
# proj4string: "+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

# Intersection with the emprise and the parisian buildings
ZoneBati <- st_intersection(emprise_sf, batiParis)

dev.off()
plot(zoneEtude$geometry, border="gray")
plot(ZoneBati$geometry, add=T, border="pink") # Les bâtis ne couvrent pas totalement la zone d'étude
dev.off()
plot(zoneEtude$geometry, border="gray")
plot(batiParis$geometry, add=T, border="brown") # On voit bien que la couche des bâtis ne couvre pas les extrêmes nord et sud de la zone
dev.off()

# Preliminary study taking the zoneEtude with north and south extremes as defined by the parisian buildings layer
emprise_bati <- st_bbox(ZoneBati)
emprise_bati <-  st_as_sfc(emprise_bati)
emprise_bati <-  emprise_bati %>% st_geometry %>% st_sf()
# intersection avec zoneEtude
zonePreanalyse <- st_intersection(emprise_bati, zoneEtude)

plot(zonePreanalyse$geometry, border="gray")
plot(ZoneBati$geometry, add=T, border="purple") # Ce n'est pas parfait, il y a toujours une petite bande au sud non couverte, mais
dev.off()                                       # pour notre étude préalable ce n'est pas très grave

# Calculation of the area of the zonePreanalyse
areaZone <- sum(st_area(zonePreanalyse$geometry))
attributes(areaZone) # Pour vérifier que les unités sont en mètres 
areaZone <- as_data_frame(areaZone2)

##  Calculation of the building surface fraction (BSF) for the zonePreanalyse
bsf <- (sum(ZoneBati$ind_area)/areaZone) # Approx 27% de la zone est couvert par les emprises de bâtiments
# 1 0.2699454 [1/m^2]

## Load roads and impervious zones data to calculate the impervious surface fraction (ISF)
# The data have already been projected in Lambert93 in QGIS

# Impervious surfaces such as parking lots and artificial surfaces
imp_sur <- read_sf("C:/Stage_IGN/indicateurs/paris_data/osm/nouvelles_paris/imperviousRGF93.geojson")
# Parisian roads
routesParis <- read_sf("C:/Stage_IGN/indicateurs/paris_data/osm/osm_paris/road_lambert93.geojson")
# Calculation of road lengths in order to obtain the road area further on
routesParis$length <- st_length(routesParis$geometry)
names(routesParis)
# Area calculation
routesParis$area <- (routesParis$width*routesParis$length)
names(routesParis)
# Spatial join of both layers
surfaceImpermeable <- st_join(routesParis, imp_sur)
names(surfaceImpermeable) # Pas de NA pour les surfaces imperméables hors routes où on penserait les trouver dans les champs comme "type"
# Intersection with zonePreanalyse
zoneImpermeable <- st_intersection(emprise_bati, surfaceImpermeable)
names(zoneImpermeable)
# Plot of the impermeable zone
plot(zonePreanalyse$geometry, border="gray")
plot(zoneImpermeable$geometry, add=T, border="red")

## Calculation of the ISF
somme_surfImp <- sum(zoneImpermeable$area)
somme_surfImp <- as_data_frame(somme_surfImp)
isf <- (somme_surfImp/areaZone) # Appox 15% de la zone est couvert par les surfaces artificialisées 
# 0.1472286 [1/m]

## Load vegetation data to calculate the pervious surface fraction (PSF)
# The geojson file has already been projected in Lambert93 in QGIS
surfacePermeable <- read_sf("C:/Stage_IGN/indicateurs/paris_data/osm/nouvelles_paris/vegetation_lambert93.geojson")
surfacePermeable$area <- st_area(surfacePermeable$geometry)
names(surfacePermeable)
# Intersection with the ZonePreanalyse
zonePermeable <- st_intersection(emprise_bati, surfacePermeable)
# Plot of the permeable zone
dev.off()
plot(zonePreanalyse$geometry, border="gray")
plot(zonePermeable$geometry, add=T, border="green")
## Calculation of the PSF
psf <- sum(zonePermeable$area)/areaZone$value
psf <- as_data_frame(psf) # Approx 20% de la zone est couvert par les surfaces perméables 
# 0.20332