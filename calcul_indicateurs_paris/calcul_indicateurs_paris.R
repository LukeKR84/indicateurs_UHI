library(sf)
library(dplyr)
library(classInt)
library(cartography)

# Set working directory
setwd("C:/Stage_IGN/indicateurs/calcul_indicateurs_paris")

# Load zone of interest
zoi <- read_sf("C:/Users/riley/Documents/GitHub/indicateurs_UHI/zoneEtudeUSR.shp")
# Fix the projection
st_crs(zoi) <- 2154
# Check names
names(zoi)
# Drop unnecessary columns and only retain columns useful for calculating and comparing the indicators
drops <- c("vgttn_s", "rt_srfc", "rt_lngr", "trttr_l", "hydr_sr", "hydr_ln", "floor", "flor_rt", "cmpc_mn_n", "cmpc_mn_w", "cntg_mn",   
           "cntg_st", "mn_dr_s", "h_mean", "h_std", "p_vl_r_", "b_area", "b_vol", "b_vol_m", "bld_nmb", "min_m_dst", "men_m_dst", "mn_std_",
           "b_hls__", "b_std__", "b_m_nw_", "b_m_w_c", "b_std_c", "dst_t_c", "bld_dns", "hydr_dn", "vgt_dns", "rod_dns", "ext_nv_", "ba", "bgh",      
           "icif", "icio", "id", "local", "pcif", "pcio", "pd", "psc", "typo_mj", "typ_scn","ins_ndv", "ins_mng", "ins_mn_c", "ins_mn_s", "ins_sr_")
zoi <- zoi[ , !(names(zoi) %in% drops)]
# Certain USR have duplicates which need to be eliminated
zoi <- zoi[!duplicated(zoi$pk_usr), ]
# Plot to view the ZOI's geometry
plot(zoi$geometry, border = "gray")
# Calculate the area for each USR
zoi$usr_area <- st_area(zoi$geometry)

# Load buildings layer 
bati_paris <- read_sf("C:/Users/riley/Documents/GitHub/april_1st/indicateurs_UHI-master/data_bati/bati_zoneEtude_hauteur_corrigees.shp")
# Fix the projection
st_crs(bati_paris) <- 102110
bati_paris <- st_transform(bati_paris, 2154)

# Intersection between the ZOI and the buildings
BatiUSR <- st_intersection(zoi, bati_paris)
# Plot the buildings
# plot(BatiUSR$geometry, border = "red")
# Remove the duplicated buildings
BatiUSR <- BatiUSR[!duplicated(BatiUSR$ID), ]
# Calculate the area for each building 
BatiUSR$b_area <- st_area(BatiUSR$geometry)
# Calculate the mean height for each usr by dissolving the buildings into the usr and calculating the height each time
BatiMoyHauteur <- BatiUSR %>% group_by(pk_usr) %>% summarise(m_height = mean(HAUTEUR))

# Dissolve the building areas into each USR, so that we now have the total building area for each USR 
BatiUSR <- BatiUSR %>% group_by(pk_usr)  %>% summarise(b_area = sum(b_area)) 
# Export the dissolved values in separate non spatial data frames
area_data <- data.frame("pk_usr" = BatiUSR$pk_usr, "b_area" = BatiUSR$b_area)
height_data <- data.frame("pk_usr" = BatiMoyHauteur$pk_usr, "m_height" = BatiMoyHauteur$m_height)
# Join the data frames
batiUSR_data <- full_join(area_data, height_data, by = "pk_usr")

# Calculate the bsf
zone_area <- data.frame("pk_usr" = zoi$pk_usr, "usr_area" = zoi$usr_area)
batiUSR_data <- full_join(zone_area, batiUSR_data, by = "pk_usr")
batiUSR_data$bsf <- (batiUSR_data$b_area/batiUSR_data$usr_area)

# Calculate the height of roughness elements (HRE)
batiUSR_data$hre <- ((batiUSR_data$b_area * batiUSR_data$m_height)/batiUSR_data$usr_area)

# Prepare data frame for join with ZOI
drops <- c("usr_area")
batiUSR_data <- batiUSR_data[ , !(names(batiUSR_data) %in% drops)]
names(batiUSR_data)
# Remove duplicated columns
batiUSR_data <- batiUSR_data[!duplicated(batiUSR_data$pk_usr), ]
# Join building data to ZOI
zoi <- full_join(zoi, batiUSR_data, by = "pk_usr")

# Load roads layer
roads <- read_sf("C:/Stage_IGN/indicateurs/paris_data/osm/nouvelles_paris/roadsZoneEtude.shp")
# Fix the projection 
st_crs(roads) <- 2154
# Drop unnecessary columns
names(roads)
drops <- c("code", "fclass", "ref", "oneway", "maxspeed", "layer", "bridge", "tunnel")
roads <- roads[ , !(names(roads) %in% drops)]
# intersection with the ZOI
roadsZone <- st_intersection(zoi, roads)
# Plot of the roadsZone
# plot(roadsZone$geometry, border = "blue")
# Dissolve the roads and their widths into each USR
roadsZone <- roadsZone %>% group_by(pk_usr)  %>% select(osm_id, width)
# Calculate the length for each stretch of road inside the USRs
roadsZone$length <- st_length(roadsZone$geometry)
# Calculate the area for each stretch of road inside the USRs
roadsZone$r_area <- (roadsZone$length*roadsZone$width)
# Remove the duplicated columns
roadsZone <- roadsZone[!duplicated(roadsZone[c('pk_usr', 'osm_id')]), ]
# Dissolve the road area measurements into each USR
roadZoneArea <- roadsZone %>% group_by(pk_usr) %>% summarise(r_area = sum(r_area))
# Export the dissolved values in a separate non spatial data frame)
road_data <- data.frame("pk_usr" = roadZoneArea$pk_usr, "r_area" = roadZoneArea$r_area)
# Join usr area data
road_data <- full_join(zone_area, road_data, by ="pk_usr")
# Calculate the road surface fraction (RSF)
road_data$rsf <- (road_data$r_area/road_data$usr_area)
# Remove the duplicated columns
road_data <- road_data[!duplicated(road_data$pk_usr), ]
# Some RSF are greater than one, but the USR area is so small their value to the study is not important
# Transformation of these examples into NA so that they will not affect the study
road_data$rsf <- as.numeric(road_data$rsf)
road_data$rsf[which(road_data$rsf >= 1)] <- NA
# Prepare data for join with the ZOI
road_data_2 <- data.frame("pk_usr" = road_data$pk_usr, "r_area" = road_data$r_area, "rsf" = road_data$rsf)
zoi <- full_join(zoi, road_data_2, by = "pk_usr")

# Load impervious zones layer
imperviousZone <- read_sf("C:/Stage_IGN/indicateurs/paris_data/osm/nouvelles_paris/imperviousZoneEtudeGroupe.shp")
# Fix the projection
st_crs(imperviousZone) <- 2154
# Drop unnecessary columns
names(imperviousZone)
drops <- c("id_source", "osm_id", "code", "fclass", "name", "layer", "path")
imperviousZone <- imperviousZone[ , !(names(imperviousZone) %in% drops)]
# Intersection with the ZOI
imperviousUSR <- st_intersection(zoi, imperviousZone)
# Check the data frame for duplicated lines
imperviousUSR <- imperviousUSR[!duplicated(imperviousUSR$pk_usr), ]
# Calculate the area for each USR
imperviousUSR$i_area <- st_area(imperviousUSR$geometry)
# Export the impervious area data in a separate non-spatial data frame
imperviousUSR_data <- data.frame("pk_usr" = imperviousUSR$pk_usr, "i_area" = imperviousUSR$i_area)
# Join the road data to the imperviousUSR_data data frame
imperviousUSR_data <- full_join(imperviousUSR_data, road_data, by = "pk_usr")
# Transform area columns into numerical columns
imperviousUSR_data$i_area_num <- as.numeric(imperviousUSR_data$i_area)
imperviousUSR_data$r_area_num <- as.numeric(imperviousUSR_data$r_area)
# Remove NA values in the _num columns
imperviousUSR_data$i_area_num[is.na(imperviousUSR_data$i_area_num)] <- 0
imperviousUSR_data$r_area_num[is.na(imperviousUSR_data$r_area_num)] <- 0
# Calculate the impervious surface total area
imperviousUSR_data$imp_area <- (imperviousUSR_data$i_area_num + imperviousUSR_data$r_area_num)
# Convert the imp_area to metres squared
units(imperviousUSR_data$imp_area) <- "m^2"
# Calculate the impervious surface fraction (ISF)
imperviousUSR_data$isf <- (imperviousUSR_data$imp_area/imperviousUSR_data$usr_area)
# Remove the NaN values
imperviousUSR_data$isf[is.nan(imperviousUSR_data$isf)] <- 0
# Create a numeric column 
imperviousUSR_data$isf_num <- as.numeric(imperviousUSR_data$isf)

# Some fractions are greater than one which is incorrect, but the USR are less than 10m^2 in area, so their value is not significant
# They will be given default vaules of NA 
imperviousUSR_data$isf_num[which(imperviousUSR_data$isf_num >= 1)] <- NA
# Convert imp_area back to m^2
units(imperviousUSR_data$imp_area) <- "m^2"
# Export of the isf and impervious area in a separate data frame
impervious_surface_USRdata <- data.frame("pk_usr" = imperviousUSR_data$pk_usr, "imp_area" = imperviousUSR_data$imp_area, "isf" = imperviousUSR_data$isf_num) 
# Join impervious surface data to the ZOI
zoi <- full_join(zoi, impervious_surface_USRdata, by ="pk_usr")


# # Load vegetation layer
vegetation <- read_sf("C:/Stage_IGN/indicateurs/paris_data/osm/nouvelles_paris/veg_combined.shp")
# Fix the projection
st_crs(vegetation) <- 2154
names(vegetation)
drops <- c("code", "fclass", "name", "id_veget", "id_source", "type", "height_cla", "layer", "path")
vegetation <- vegetation[ , !(names(vegetation) %in% drops)]
# Intersection with the ZOI
permUSR <- st_intersection(zoi, vegetation)
names(permUSR)
drops <- c("code_ns", "dcomirs", "usr_area", "b_area", "m_height", "bsf", "hre", "osm_id", "v_area")
permUSR <- permUSR[ , !(names(permUSR) %in% drops)]
# Calculate the vegetation area
permUSR$v_area <- st_area(permUSR$geometry)
# Export the dissolved values in a separate non spatial data frame
permUSR_data <- data.frame("pk_usr" = permUSR$pk_usr, "v_area" = permUSR$v_area)
# Join usr area data
permUSR_data <- full_join(zone_area, permUSR_data, by ="pk_usr")
# Export the b_area and imp_area columns from the ZOI in a separate data frame
impUSR_data <- data.frame("pk_usr" = zoi$pk_usr, "b_area" = zoi$b_area, "imp_area" = zoi$imp_area)
# Join impUSR_data tp the permUSR data
permUSR_data <- full_join(permUSR_data, impUSR_data, by ="pk_usr")
# Create numeric columns of imp_area and b_area
permUSR_data$b_area_num <- as.numeric(permUSR_data$b_area)
permUSR_data$imp_area_num <- as.numeric(permUSR_data$imp_area)
# Remove NA values in the _num columns
permUSR_data$b_area_num[is.na(permUSR_data$b_area_num)] <- 0
permUSR_data$imp_area_num[is.na(permUSR_data$imp_area_num)] <- 0
# Create total impervious area column
permUSR_data$t_imp_area <- (permUSR_data$b_area_num + permUSR_data$imp_area_num)
# Calculate the vegetation surface fraction (VSF)
permUSR_data$vsf <- (permUSR_data$v_area/permUSR_data$usr_area)
# Create numeric v_area and vsf columns
permUSR_data$v_area_num <- as.numeric(permUSR_data$v_area)
permUSR_data$vsf_num <- as.numeric(permUSR_data$vsf)
# Subtract impervious area from the vegetation area for the USR where the vsf is greater than 0.9 to create pervious area column
permUSR_data <- transform(permUSR_data, p_area = ifelse(vsf_num>=0.9, v_area_num - t_imp_area, v_area_num))
# Convert the p_area to metres squared
units(permUSR_data$p_area) <- "m^2"
# Calculate the pervious surface fraction (PSF)
permUSR_data$psf <- (permUSR_data$p_area/permUSR_data$usr_area)
# Remove the duplicated columns
permUSR_data <- permUSR_data[!duplicated(permUSR_data$pk_usr), ]
perviousSurface <- data.frame("pk_usr" = permUSR_data$pk_usr, "p_area" = permUSR_data$p_area, "psf" = permUSR_data$psf)
# Join pervious surface data to ZOI
zoi <- full_join(zoi, perviousSurface, by = "pk_usr")

# Export the ZOI
st_write(zoi, "zoi.shp")


# Mapping the surface fractions

# BSF

# Plot the USR (only the background colour is plotted)
plot(st_geometry(zoi), col=NA, border=NA, bg="white")


cols <- carto.pal(pal1 = "red.pal", n1 = 6)

# Plot the BSF
choroLayer(x= zoi, var = "bsf", breaks = c(0,0.2,0.4,0.6,0.8,1.0), col = cols,
           border = "white", lwd = 0.3, legend.pos = "topleft", legend.values.rnd = 1,
           legend.title.txt = "BSF", add = T)

layoutLayer("Building Surface Fraction of the MApUCE RSU Grid", tabtitle = T, postitle = "center",
            author = "Luke Riley", sources = "MApUCE, IGN/BD Topo", frame = F, scale = 5)

north(pos = "topright")
dev.off()

# HRE (Quantile method)

# Plot the USR (only the background colour is plotted)
plot(st_geometry(zoi), col=NA, border=NA, bg="white")

bks <- getBreaks(v = zoi$hre, method = "q6")
cols <- carto.pal(pal1 = "harmo.pal", n1 = 6)

# Plot the HRE
choroLayer(x= zoi, var = "hre", breaks = bks, col = cols,
           border = "white", lwd = 0.3, legend.pos = "topleft", legend.values.rnd = 1,
           legend.title.txt = "HRE", add = T)

layoutLayer("Height of Roughness Elements of the MApUCE RSU Grid", tabtitle = T, postitle = "center",
            author = "Luke Riley", sources = "MApUCE, IGN/BD Topo", frame = F, scale = 5)

north(pos = "topright")
dev.off()

# HRE (manually defined breaks)

# Plot the USR (only the background colour is plotted)
plot(st_geometry(zoi), col=NA, border=NA, bg="white")

cols <- carto.pal(pal1 = "harmo.pal", n1 = 6)

# Plot the HRE
choroLayer(x= zoi, var = "hre", breaks = c(0,5,10,15,20,30,45), col = cols,
           border = "white", lwd = 0.3, legend.pos = "topleft", legend.values.rnd = 1,
           legend.title.txt = "HRE", add = T)

layoutLayer("Height of Roughness Elements of the MApUCE RSU Grid", tabtitle = T, postitle = "center",
            author = "Luke Riley", sources = "MApUCE, IGN/BD Topo", frame = F, scale = 5)

north(pos = "topright")
dev.off()

# ISF

# Plot the USR (only the background colour is plotted)
plot(st_geometry(zoi), col=NA, border=NA, bg="white")


cols <- carto.pal(pal1 = "kaki.pal", n1 = 6)

# Plot the ISF
choroLayer(x= zoi, var = "isf", breaks = c(0.01,0.2,0.4,0.6,0.8,1.0), col = cols,
           border = "white", lwd = 0.3, legend.pos = "topleft", legend.values.rnd = 2,
           legend.title.txt = "ISF", add = T)

layoutLayer("Impervious Surface Fraction of the MApUCE RSU Grid", tabtitle = T, postitle = "center",
            author = "Luke Riley", sources = "MApUCE, Contributeurs Open Street Map", frame = F, scale = 5)

north(pos = "topright")
dev.off()

# ISF (Jenks Method)

# Plot the USR (only the background colour is plotted)
plot(st_geometry(zoi), col=NA, border=NA, bg="white")
# Legend intervals
jenks <- getBreaks(v = zoi$isf, nclass = 7, method = "fisher-jenks")
# Change the lowest value to 0.01 and the highest value to one
jenks[1] <- 0.01
jenks[8] <- 1.00
# Colours
cols <- carto.pal(pal1 = "kaki.pal", n1 = 7)

# Plot the ISF
choroLayer(x= zoi, var = "isf", breaks = jenks, col = cols,
           border = "white", lwd = 0.3, legend.pos = "topleft", legend.values.rnd = 2,
           legend.title.txt = "ISF", add = T)

layoutLayer("Impervious Surface Fraction of the MApUCE RSU Grid", tabtitle = T, postitle = "center",
            author = "Luke Riley", sources = "MApUCE, Contributeurs Open Street Map", frame = F, scale = 5)

north(pos = "topright")
dev.off()


# PSF

# Plot the USR (only the background colour is plotted)
plot(st_geometry(zoi), col=NA, border=NA, bg="white")


cols <- carto.pal(pal1 = "green.pal", n1 = 6)

# Plot the PSF
choroLayer(x= zoi, var = "psf", breaks = c(0,0.2,0.4,0.6,0.8,1.0), col = cols,
           border = "white", lwd = 0.3, legend.pos = "topleft", legend.values.rnd = 1,
           legend.title.txt = "PSF", add = T)

layoutLayer("Pervious Surface Fraction of the MApUCE RSU Grid", tabtitle = T, postitle = "center",
            author = "Luke Riley", sources = "MApUCE, Contributeurs Open Street Map", frame = F, scale = 5)

north(pos = "topright")
dev.off()

# RSF

# Plot the USR (only the background colour is plotted)
plot(st_geometry(zoi), col=NA, border=NA, bg="white")


cols <- carto.pal(pal1 = "blue.pal", n1 = 8)

# Plot the RSF
choroLayer(x= zoi, var = "rsf", breaks = c(0.01,0.02,0.05,0.1,0.4,0.6,1.0), col = cols,
           border = "white", lwd = 0.3, legend.pos = "topleft", legend.values.rnd = 2,
           legend.title.txt = "RSF", add = T)

layoutLayer("Road Surface Fraction of the MApUCE RSU Grid", tabtitle = T, postitle = "center",
            author = "Luke Riley", sources = "MApUCE, Contributeurs Open Street Map", frame = F, scale = 5)

north(pos = "topright")
dev.off()

# RSF

# Plot the USR (only the background colour is plotted)
plot(st_geometry(zoi), col=NA, border=NA, bg="white")


cols <- carto.pal(pal1 = "blue.pal", n1 = 6)

# Plot the RSF
choroLayer(x= zoi, var = "rsf", breaks = c(0,0.2,0.4,0.6,0.8,1.0), col = cols,
           border = "white", lwd = 0.3, legend.pos = "topleft", legend.values.rnd = 2,
           legend.title.txt = "RSF", add = T)

layoutLayer("Road Surface Fraction of the MApUCE RSU Grid", tabtitle = T, postitle = "center",
            author = "Luke Riley", sources = "MApUCE, Contributeurs Open Street Map", frame = F, scale = 5)

north(pos = "topright")
dev.off()