library(sf)
library(dplyr)
library(viridis)
library(ggplot2)

# Set working directory
setwd("C:/Stage_IGN/geoviz/agregation")

# Load zone of interest
zoi <- read_sf("C:/Stage_IGN/indicateurs/gwr_master/zoi_files/zoi_indicateurs_strat_2.geojson")
# Fix the projection
st_crs(zoi) <- 2154

# Add new column, temp_zone
zoi$temp_zone <- NA

zoi$temp_zone[which(zoi$TEB_T1 <= 298)] <- 1
zoi$temp_zone[which(zoi$TEB_T1 > 298 & zoi$TEB_T1 <= 298.5)] <- 2
zoi$temp_zone[which(zoi$TEB_T1 > 298.5 & zoi$TEB_T1 <= 299)] <- 3
zoi$temp_zone[which(zoi$TEB_T1 > 299 & zoi$TEB_T1 <= 299.5)] <- 4
zoi$temp_zone[which(zoi$TEB_T1 > 299.5 & zoi$TEB_T1 <= 300)] <- 5
zoi$temp_zone[which(zoi$TEB_T1 > 300 & zoi$TEB_T1 <= 300.5)] <- 6
zoi$temp_zone[which(zoi$TEB_T1 > 300.5)] <- 7


st_write(zoi, "zoi_tempZones_2.geojson")

# Create a new zone with bigger polygons corresponding to the temp zones

#one geometry is not valid 
zone <- st_make_valid(zoi)

#affecting temperature class 
zone_homogenes <- zone %>% group_by(temp_zone) %>% summarise(geometry=st_union(geometry)) 

# Basic plot
plot(zone_homogenes$geometry)
# More complete plot 
png("Strat2_tempZones_agregees.png", width = 800, height = 800)
ggplot(data = zone_homogenes) + 
  geom_sf(aes(fill = temp_zone)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "Temp zones") +
  scale_fill_viridis(option = "magma")
dev.off()

# Create data frame for the purposes of aggregation and calculation
palette_indicateurs <- as.data.frame(zone)

# Average temperature for each zone
ave_temp <- palette_indicateurs %>% group_by(temp_zone) %>% summarise(ave_temp=mean(TEB_T1))

# Create subsets for indicators
p_indiv <- subset(palette_indicateurs, (!is.na(palette_indicateurs$ins_ndv)))
indiv_temp <- p_indiv %>% group_by(temp_zone) %>% summarise(insee_indiv=sum(ins_ndv))

p_menages <- subset(palette_indicateurs, (!is.na(palette_indicateurs$ins_mng)))
menages_temp <- p_menages %>% group_by(temp_zone) %>% summarise(insee_menages=sum(ins_mng))

p_men_coll <- subset(palette_indicateurs, (!is.na(palette_indicateurs$ins_mn_c)))
men_col_temp <- p_men_coll %>% group_by(temp_zone) %>% summarise(insee_men_col=sum(ins_mn_c))

p_men_surf <- subset(palette_indicateurs, (!is.na(palette_indicateurs$ins_mn_s)))
men_surf_temp <- p_men_surf %>% group_by(temp_zone) %>% summarise(insee_men_surf=sum(ins_mn_s))

p_men_surf_coll <- subset(palette_indicateurs, (!is.na(palette_indicateurs$ins_sr_)))
men_surf_coll_temp <- p_men_surf_coll %>% group_by(temp_zone) %>% summarise(insee_men_surf_col=sum(ins_sr_))

mean_usr_area <- palette_indicateurs %>% group_by(temp_zone) %>% summarise(mean_usr_area=mean(usr_area))

p_b_area <- subset(palette_indicateurs, (!is.na(palette_indicateurs$b_area)))
b_area_temp <- p_b_area %>% group_by(temp_zone) %>% summarise(b_area=sum(b_area))
mean_b_area_temp <- p_b_area %>% group_by(temp_zone) %>% summarise(mean_b_area=mean(b_area))

p_bsf <- subset(palette_indicateurs, (!is.na(palette_indicateurs$bsf)))
bsf_temp <- p_bsf %>% group_by(temp_zone) %>% summarise(bsf=mean(bsf))

mean_b_height <- subset(palette_indicateurs, (!is.na(palette_indicateurs$m_height)))
mean_b_height_temp <- mean_b_height %>% group_by(temp_zone) %>% summarise(mean_b_height=mean(m_height))

p_hre <- subset(palette_indicateurs, (!is.na(palette_indicateurs$hre)))
hre_temp <- p_hre %>% group_by(temp_zone) %>% summarise(hre=mean(hre))

p_r_area <- subset(palette_indicateurs, (!is.na(palette_indicateurs$r_area)))
r_area_temp <- p_r_area %>% group_by(temp_zone) %>% summarise(r_area=sum(r_area))
mean_r_area_temp <- p_r_area %>% group_by(temp_zone) %>% summarise(mean_r_area=mean(r_area))

p_rsf <- subset(palette_indicateurs, (!is.na(palette_indicateurs$rsf)))
rsf_temp <- p_rsf %>% group_by(temp_zone) %>% summarise(rsf=mean(rsf))

p_imp_area <- subset(palette_indicateurs, (!is.na(palette_indicateurs$imp_area)))
imp_area_temp <- p_imp_area %>% group_by(temp_zone) %>% summarise(imp_area=sum(imp_area))
mean_imp_area_temp <- p_imp_area %>% group_by(temp_zone) %>% summarise(mean_imp_area=mean(imp_area))

p_isf <- subset(palette_indicateurs, (!is.na(palette_indicateurs$isf)))
isf_temp <- p_isf %>% group_by(temp_zone) %>% summarise(isf=mean(isf))

p_p_area <- subset(palette_indicateurs, (!is.na(palette_indicateurs$p_area)))
p_area_temp <- p_p_area %>% group_by(temp_zone) %>% summarise(p_area=sum(p_area))
mean_p_area_temp <- p_p_area %>% group_by(temp_zone) %>% summarise(mean_p_area=mean(p_area))

p_psf <- subset(palette_indicateurs, (!is.na(palette_indicateurs$psf)))
psf_temp <- p_psf %>% group_by(temp_zone) %>% summarise(psf=mean(psf))

# Calculate the area in m2 for the temp zones
zone_homogenes$zone_area <- st_area(zone_homogenes$geometry)

# Collect temp zone data into one sf file
zone_homogenes <- full_join(zone_homogenes, ave_temp, by ="temp_zone")
zone_homogenes <- full_join(zone_homogenes, indiv_temp, by ="temp_zone")
zone_homogenes <- full_join(zone_homogenes, menages_temp, by ="temp_zone")
zone_homogenes <- full_join(zone_homogenes, men_col_temp, by ="temp_zone")
zone_homogenes <- full_join(zone_homogenes, men_surf_temp, by ="temp_zone")
zone_homogenes <- full_join(zone_homogenes, men_surf_coll_temp, by ="temp_zone")
zone_homogenes <- full_join(zone_homogenes, mean_usr_area, by ="temp_zone")
zone_homogenes <- full_join(zone_homogenes, b_area_temp, by ="temp_zone")
zone_homogenes <- full_join(zone_homogenes, mean_b_area_temp, by ="temp_zone")
zone_homogenes <- full_join(zone_homogenes, bsf_temp, by ="temp_zone")
zone_homogenes <- full_join(zone_homogenes, mean_b_height_temp, by ="temp_zone")
zone_homogenes <- full_join(zone_homogenes, hre_temp, by ="temp_zone")
zone_homogenes <- full_join(zone_homogenes, mean_r_area_temp, by ="temp_zone")
zone_homogenes <- full_join(zone_homogenes, rsf_temp, by ="temp_zone")
zone_homogenes <- full_join(zone_homogenes, imp_area_temp, by ="temp_zone")
zone_homogenes <- full_join(zone_homogenes, mean_imp_area_temp, by ="temp_zone")
zone_homogenes <- full_join(zone_homogenes, isf_temp, by ="temp_zone")
zone_homogenes <- full_join(zone_homogenes, p_area_temp, by ="temp_zone")
zone_homogenes <- full_join(zone_homogenes, mean_p_area_temp, by ="temp_zone")
zone_homogenes <- full_join(zone_homogenes, psf_temp, by ="temp_zone")

# Export the data as a geojson file
st_write(zone_homogenes, "strat2_tempZones.geojson")

zones_homogenes_centroids <- st_centroid(zone_homogenes)
st_write(zones_homogenes_centroids, "R_tempzone_centroids.geojson")
