library(sf)
library(ggplot2)
library(dplyr)
library(viridis)
library(stringr)


setwd("~/encadrement/stage_luke/")



dfUSR <-  read_sf("./données Mapuce_backup/usr_mapuce_paris.geojson")
st_crs(dfUSR)
names(dfUSR)




paris <-  dfUSR %>% filter(startsWith(code_insee,"75"))
dep92 <-  dfUSR %>% filter(startsWith(code_insee,"92"))
dep93<-  dfUSR %>% filter(startsWith(code_insee,"93"))
dep94 <-  dfUSR %>% filter(startsWith(code_insee,"94"))


IdF <-  rbind(paris, dep92, dep93, dep94)

dev.off()
plot(IdF$geometry, border="lightgray",lwd=0.2 )                 
plot(paris$geometry, add=T, border="orange", lwd=0.2)






# zone d'études : points mesoNH
dfMeso <- read.csv("./indicateurs_UHI/data_mesoNH_point_O.csv")
dfMeso_sf <-  st_as_sf(dfMeso, coords = c("longitude", "latitude"))
#assuming data is on WGS 84
st_crs(dfMeso_sf) <- 4326
dfMeso_sf <-  st_transform(dfMeso_sf, 2154)
#only temperature THT variables
temperature_sf <-  dfMeso_sf [,33:64]
rm(dfMeso, dfMeso_sf)

# recompute enveloppe / extent with projected coordinates
emprise_sf <-  st_bbox(temperature_sf)
emprise_sf <-  st_as_sfc(emprise_sf)
#enlarge bbox to get approx. one extra cube width around the corner points
emprise_sf <- st_buffer(emprise_sf, dist=400) 
emprise_sf <-  st_as_sfc(st_bbox(emprise_sf)) 
#convert back to sf object
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

st_write(zoneEtude, "./indicateurs_UHI/zoneEtudeUSR.shp")


## UHI 


uhi <- st_read("./UHI_2scenarios/Paris_icu_hot_summer_1.shp")  


uhi <-  st_transform(uhi, 2154)

dev.off()
par(mar=c(0,0,0,0))
plot(uhi[1], border=NA, lwd=0.1)
plot(zoneEtude$geometry, border= "white", add=TRUE)

plot(zoneEtude$geometry,lwd=0.3)
plot(uhi[1], border=NA, lwd=0.1, add=T, alpha=0.9)
plot(zoneEtude$geometry,lwd=0.3, add=T)









