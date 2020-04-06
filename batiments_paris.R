library(sf)
library(ggplot2)
library(dplyr)
library(viridis)
library(stringr)


setwd("~/encadrement/stage_luke/indicateurs_UHI/")



zoneEtude <-  read_sf("./USR_MAPUCE_Paris/zoneEtudeUSR.shp")
st_crs(dfUSR) <-  2154
names(dfUSR)






# zone d'études : points mesoNH
dfMeso <- read.csv("./data_mesoNH_point_O.csv")
dfMeso_sf <-  st_as_sf(dfMeso, coords = c("longitude", "latitude"))
#assuming data is on WGS 84
st_crs(dfMeso_sf) <- 4326
dfMeso_sf <-  st_transform(dfMeso_sf, 2154)
#only temperature THT variables
temperature_sf <-  dfMeso_sf [,33:64]
rm(dfMeso, dfMeso_sf)



# recompute enveloppe / extent with projected coordinates
emprise <-  st_bbox(temperature_sf)
emprise_sf <-  st_as_sfc(emprise)
#enlarge bbox to get approx. one extra cube width around the corner points
emprise_sf <- st_buffer(emprise_sf, dist=400) 
emprise_sf <-  st_as_sfc(st_bbox(emprise_sf)) 
#convert back to sf object
emprise_sf <-  emprise_sf %>% st_geometry %>% st_sf()





uhi <- st_read("./UHI_2scenarios/Paris_icu_hot_summer_1.shp")  
uhi <-  st_transform(uhi, 2154)
uhi <- st_intersection(emprise_sf,uhi)


par(mar=c(0,0,0,0))

dev.off()
par(mar=c(0,0,0,0))
plot(uhi[1], border=NA, lwd=0.1)
plot(zoneEtude$geometry, border= "white", add=TRUE)

dev.off()
plot(zoneEtude$geometry,lwd=0.3)
plot(uhi[1], border=NA, lwd=0.1, add=T, alpha=0.9)
plot(zoneEtude$geometry,lwd=0.3, add=T)




# couche bati BD TOPO 2017

pathBatiParis <-  "../BDTOPO/BDT_2-2_SHP_LAMB93_D075-ED171/E_BATI/BATI_INDIFFERENCIE.SHP"
pathBati92 <-  "../BDTOPO/BDT_2-2_SHP_LAMB93_D092-ED171/E_BATI/BATI_INDIFFERENCIE.SHP"
pathBati93 <-  "../BDTOPO/BDT_2-2_SHP_LAMB93_D093-ED171/E_BATI/BATI_INDIFFERENCIE.SHP"
pathBati94 <-  "../BDTOPO/BDT_2-2_SHP_LAMB93_D094-ED171/E_BATI/BATI_INDIFFERENCIE.SHP"


gen_batiZOneEtude <-  function(pathParis, path92, path93, path94, emprise){
batiParis <-  st_read(pathParis,stringsAsFactors=F  ) 
st_crs(batiParis) <- 2154
batiParisZoneEtude <- st_crop(batiParis,emprise)
rm(batiParis)

bati92 <-  st_read(path92,stringsAsFactors=F)
st_crs(bati92) <- 2154
bati92ZoneEtude <-  st_crop(bati92, emprise)
rm(bati92)

bati93 <-  st_read(path93,stringsAsFactors=F)
st_crs(bati93) <- 2154
bati93ZoneEtude <- st_crop(bati93, emprise) 
rm(bati93)

bati94 <-  st_read(path94,stringsAsFactors=F)
st_crs(bati94) <- 2154
bati94ZoneEtude <- st_crop(bati94, emprise) 
rm(bati94)

z1 <-  rbind(batiParisZoneEtude, bati92ZoneEtude)
rm(batiParisZoneEtude, bati92ZoneEtude)
z2 <-  rbind(bati93ZoneEtude, bati94ZoneEtude)
rm(bati93ZoneEtude, bati94ZoneEtude)
ZoneEtudeBati <-  rbind(z1,z2)
rm(z1,z2)

ZoneEtudeBati$ORIGIN_BAT <-  NULL
ZoneEtudeBati$PREC_PLANI <-  NULL
ZoneEtudeBati$PREC_ALTI <-  NULL
ZoneEtudeBati$ID <- row.names(ZoneEtudeBati) %>% as.numeric()
return(ZoneEtudeBati)
}


ZoneEtudeBati <-  gen_batiZOneEtude(pathBatiParis,pathBati92,pathBati93,pathBati94, emprise)

# st_write(ZoneEtudeBati,"./data_bati/bati_zoneEtude.shp")




# Beaucou de hauteurs sont abhérentes : correction endogène 




