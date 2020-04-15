library(sf)
library(ggplot2)
library(viridis)
library(raster)
library(dplyr)
library(classInt)
library(cartography)
library(tidyverse)

setwd("C:/Stage_IGN/indicateurs/mapuce_usr_indicateurs_paris")

mapuceUSR <-  read_sf("C:/Stage_IGN/indicateurs/mapuce/data/paris/usr/usr_mapuce_paris.geojson")
st_crs(mapuceUSR) # Projection Lambert93
names(mapuceUSR)

# Delete fields relating to the urban fabric which will be recalculatd according to different layers
drops <- c("vegetation_surface", "route_surface", "route_longueur", "trottoir_longueur", "hydro_surface", "hydro_longueur", "floor",
           "floor_ratio", "compac_mean_nw", "compac_mean_w", "contig_mean", "contig_std", "main_dir_std", "h_mean", "h_std", "p_vol_ratio_mean",
           "b_area", "b_vol", "b_vol_m", "build_numb", "min_m_dist", "mean_m_dist", "mean_std_dist", "b_holes_area_mean", "b_std_h_mean",
           "b_m_nw_compacity", "b_m_w_compacity", "b_std_compacity", "dist_to_center", "build_dens", "hydro_dens", "veget_dens", "road_dens",
           "ext_env_area", "ba", "bgh", "icif", "icio", "id", "local", "pcif", "pcio", "pd", "psc", "typo_maj", "typo_second")

USR_reduced <- mapuceUSR[ , !(names(mapuceUSR) %in% drops)]

# Paris USR : points mesoNH
dfMeso <- read.csv("C:/Stage_IGN/indicateurs/mesoNH/data_mesoNH_point_O.csv")
dfMeso_sf <-  st_as_sf(dfMeso, coords = c("longitude", "latitude"))
# assuming data is on WGS 84
st_crs(dfMeso_sf) <- 4326
dfMeso_sf <-  st_transform(dfMeso_sf, 2154)
# only temperature TEB variables
temperature_sf <-  dfMeso_sf [,79:90]
rm(dfMeso, dfMeso_sf)

# recompute enveloppe / extent with projected coordinates
emprise_sf <-  st_bbox(temperature_sf)
emprise_sf <-  st_as_sfc(emprise_sf)
#venlarge bbox to get approx. one extra cube width around the corner points
emprise_sf <- st_buffer(emprise_sf, dist=400) 
emprise_sf <-  st_as_sfc(st_bbox(emprise_sf)) 
#vconvert back to sf object
emprise_sf <-  emprise_sf %>% st_geometry %>% st_sf()

# Zone d'étude extracted from mapuceUSR via an intersection with the emprise_sf layer
zoneEtude <- st_intersection(emprise_sf, USR_reduced)
names(zoneEtude)

# Load parisian building data in order to calculate the building surface fraction
batiParis <- read_sf("C:/Stage_IGN/indicateurs/paris_data/osm/nouvelles_paris/building_lambert93_indicators.geojson")
# Check that the CRS is RGF93
st_crs(batiParis) # OK, we can proceed with the geoprocessing operations
# Intersection with the emprise and the parisian buildings
ZoneBati <- st_intersection(emprise_sf, batiParis)
# Preliminary study taking the zoneEtude with north and south extremes as defined by the parisian buildings layer
emprise_bati <- st_bbox(ZoneBati)
emprise_bati <-  st_as_sfc(emprise_bati)
emprise_bati <-  emprise_bati %>% st_geometry %>% st_sf()
# intersection avec zoneEtude
zonePreanalyse <- st_intersection(emprise_bati, zoneEtude)
# Plot to see if the zonePreanalyse and the ZoneBati correspond
plot(zonePreanalyse$geometry, border="gray")
plot(ZoneBati$geometry, add=T, border="purple") # Ce n'est pas parfait, il y a toujours une petite bande au sud non couverte, mais
dev.off()                                       # pour notre étude préalable ce n'est pas très grave

# Calculate the area for each USR in the zone Preanalyse
zonePreanalyse$area_usr <- st_area(zonePreanalyse$geometry) 
# Intersection between builings and the zonePreanalyse
experiment_1 <- st_intersection(zonePreanalyse, ZoneBati)
st_write(experiment_1, "experiment_1.shp")
# Union of buildings and the USR
# experiment_2 <- st_union(zonePreanalyse, ZoneBati)

# experiment_1 processed in GQIS with the tool "dissolve with stats", resulting layer reloaded
buildings_usr <- read_sf("C:/Stage_IGN/indicateurs/mapuce_usr_indicateurs_paris/dissolve_exp1.shp")
# Drop non essential columns 
non_essentials <- c("ins_ndv", "ins_mng", "ins_mn_c", "ins_mn_s", "ins_sr_", "area_sr", "id_buld", "id_sorc", "hght_wl", "hght_rf",
                    "nb_lev", "type", "main_us", "zindex", "ind_prm", "ind_are", "ind_vlm", "ind_fl_", "ind_t__", "ind_cnt", "ind_c__",
                    "ind_n__","ind_r_c", "ind_fr_", "ind_rw_", "ind_pr_",  "ind_m__", "ind_rd_", "ind_l__")
buildings_usr <- buildings_usr[ , !(names(buildings_usr) %in% non_essentials)]
# calculate building areas by usr
plot(zonePreanalyse$geometry, border="gray")

buildings_usr$b_area <- st_area(buildings_usr$geometry)
plot(buildings_usr, border="red", add=T)

names(buildings_usr)
# Following attemps at dataframe joins unsuccessful
# st_crs(buildings_usr) <- 4326
# buildings_usr <- st_transform(buildings_usr, 2154)
# buildingsZone <- rbind(zonePreanalyse, buildings_usr, na = T)
# buildings_usr <- as.numeric(buildings_usr)

# export of shapefiles to manage the join in QGIS
st_write(buildings_usr, "buildingsTest.shp")
st_write(zonePreanalyse, "zoneTest.shp")
# import of shapefile after successful attribute join
bsf_test <- read_sf("C:/Stage_IGN/indicateurs/mapuce_usr_indicateurs_paris/bsf_test.shp")
st_crs(bsf_test) <- 102110
bsf_test <- st_transform(bsf_test, 2154)
bsf_test$bsf <- (bsf_test$j_b_area/bsf_test$area_sr)

# Plot the USR (only the background colour is plotted)
plot(st_geometry(bsf_test), col=NA, border=NA, bg="white")

bks <- getBreaks(v = bsf_test$bsf, method = "q6")
cols <- carto.pal(pal1 = "red.pal", n1 = 6)

# Plot the BSF
choroLayer(x= bsf_test, var = "bsf", breaks = bks, col = cols,
           border = "white", lwd = 0.3, legend.pos = "topleft", legend.values.rnd = 1,
           legend.title.txt = "By %", add = T)

layoutLayer("Building Surface Fraction of the RSU according to OSM data", tabtitle = T,
            author = "Luke Riley", sources = "MApUCE", frame = F, scale = 5)

north(pos = "topright")
dev.off()

# Load green spaces in order to calculate the permeable surface fraction
greenSpace <- read_sf("C:/Stage_IGN/indicateurs/paris_data/osm/nouvelles_paris/vegetation_lambert93.geojson")
crs(greenSpace)
greenSpace
# Load tree data from Paris Opendata 
trees <- read_sf("C:/Stage_IGN/indicateurs/paris_data/arbres/les-arbres.geojson")
crs(trees)
st_crs(trees) <- 4326
trees <- st_transform(trees, 2154)
# Intersection between the green spaces and the trees
treesGS <- st_intersection(greenSpace, trees)
# rm(vegetation)
# Plot to view the ensemble
# plot(zonePreanalyse$geometry, border="gray")
# plot(vegetation$geometry, add = T, border = "green") # le résultat n'est pas exploitable
# vegetation <- st_union(greenSpace, treesGS) => st_union fait planter mon ordi à chaque fois !

# Intersection between green spaces and the zonePreanalyse 
vegetationZone <- st_intersection(emprise_bati, greenSpace) # Comme st_union ne fonctionne pas, on prend que les espaces verts
st_write(vegetationZone, "vegetationZone.shp")
# Intersection between the vegetationZone and the zonePreanalyse
experiment_2 <- st_intersection(zonePreanalyse, vegetationZone)
st_write(experiment_2, "experiment_2.shp")

inessential <- c("insee_individus", "insee_menages", "insee_men_coll", "insee_men_surf", "insee_surface_collectif", "area_usr", "id_veget", "id_source", "type", "height_class")
experiment_2 <- experiment_2[ , !(names(experiment_2) %in% inessential)]
st_write(experiment_2, "experiment_2a.shp")

experiment_2 <- experiment_2 %>% group_by(experiment_2$pk_usr)  %>% summarise(experiment_2$geometry) # code pas fini 
