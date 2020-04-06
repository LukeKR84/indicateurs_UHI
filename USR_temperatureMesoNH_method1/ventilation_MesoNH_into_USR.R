library(sf)
library(ggplot2)
library(viridis)
library(raster)
library(dplyr)

setwd("~/encadrement/stage_luke/")



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
emprise <-  st_bbox(temperature_sf)
emprise_sf <-  st_as_sfc(emprise)
#enlarge bbox to get approx. one extra cube width around the corner points
emprise_sf <- st_buffer(emprise_sf, dist=400) 
emprise_sf <-  st_as_sfc(st_bbox(emprise_sf)) 
#convert back to sf object
emprise_sf <-  emprise_sf %>% st_geometry %>% st_sf()




zoneEtude <- st_read("./indicateurs_UHI/zoneEtudeUSR.shp")
st_crs(zoneEtude) <- 2154 


dev.off()
par(mar=c(0,0,0,0))
plot(zoneEtude$geometry, border= "grey")
plot(temperature_sf[1], add= T, pch=16)




#  raster strategy 


#  create raster from mesoNH points 

library(sp)
# conversion to SP format
SP_temperature_sf <-  as_Spatial(temperature_sf)
SP_temp_pts <- SpatialPoints(SP_temperature_sf)
"raster creation"


# order is xmin xmax ymin y max
emp <- st_bbox(emprise_sf)
exten_meso <- extent( emp[c(1,3,2,4)])

raster_emprise <-  raster(nrow=22,ncol=26,exten_meso)
zvalues_names <- names(temperature_sf)[1:32]
rasterMeso <- rasterize(SP_temperature_sf, raster_emprise,field=zvalues_names)
plot(rasterMeso, col=sf.colors())
# vectorisation
poly_raster <-  as(rasterMeso, "SpatialPolygonsDataFrame")
# cast back to sf object
poly_raster <-  st_as_sf(poly_raster)

#free 
rm(SP_temp_pts)
rm(SP_temperature_sf)

dev.off()
par(mar=c(0,0,0,0))
plot(zoneEtude$geometry, border="grey")
plot(poly_raster[1], pal=sf.colors(n = 13,alpha=0.4), add=T, border="white")
plot(temperature_sf[1], add=T, pch=16)


#  perimeter
library(lwgeom)
zoneEtude$perimeter <- st_perimeter(zoneEtude) %>% as.numeric()
#area
zoneEtude$surfaceUSR <-  st_area(zoneEtude) %>% as.numeric()

#certians USR sont très allongés ou tès large 
qplot(zoneEtude$perimeter)
# bigPerim <-  zoneEtude %>%   filter(perimeter > 2*sqrt( 408017.4 ))
bigPerim <-  zoneEtude %>%   filter(perimeter > 2555)

bigArea <-  zoneEtude %>%  filter(surfaceUSR > 408017)

#aire d'une case du raster   408017.4 
poly_raster %>% st_area()
# perimetre d'une case de raster 2555
poly_raster %>% st_perimeter()



dev.off()
par(mar=c(0,0,0,0))
plot(zoneEtude$geometry, border="darkgrey", lwd=0.2)
plot(poly_raster[1], pal=sf.colors(n = 13,alpha=0.2), add=T, border="white", lwd=0.3)
plot(bigArea$geometry, add=T, col="palegreen", border=NA)
plot(bigPerim$geometry, add=T, border="firebrick", lwd=0.9)


zoneDelicates <-  rbind(bigArea, bigPerim)
rm(bigArea, bigPerim)
st_write(zoneDelicates, "./indicateurs_UHI/USR_MAPUCE_Paris/zonesDelicates.geojson")



zoneUSRregulieres <-  zoneEtude %>%  filter( ! (pk_usr %in% zoneDelicates$pk_usr )) 

plot(zoneUSRregulieres$geometry, border="gray")
plot(temperature_sf[1], add=T, pch=16)

# determiner les USR à moins d'une unité de distance interpoints de mesoNH'



#distance entre les points non nulle (maximale) => 627  
st_distance(temperature_sf) %>%  floor() %>% unique() %>% sort() 
# on prend 650 pour les cas aux bords de la zone 
seuil <-  650 




displayUSR <-  function(usr, voisins=NULL){
  dev.off()
  par(mar=c(0,0,0,0))
  plot(zoneUSRregulieres$geometry, border="grey")
  plot(usr$geometry, add=T, col="orange")

  if(! is.null(voisins)){
    plot(voisins[1], add=T, pch=19, col="blue")
    }
}

displayUSR(myusr)


nearest_mesoNH <-  function(usr){
  neigh <- st_distance(temperature_sf,usr) %>% as.vector() %>%  sort() 
  if(length(neigh)>=4){
    return(neigh[1:4])
  }
  else{
    return(NA)
  }
}

nearest_mesoNH(myusr)  


zoneUSRregulieres$TEMP <-  NA

for (i in 1:nrow(zoneUSRregulieres)){
  myusr <- zoneUSRregulieres[i,]
idxVoisins <-  st_is_within_distance(myusr, temperature_sf, seuil)
idxVoisins <- idxVoisins %>% unlist()
THT_Voisins <-temperature_sf[idxVoisins,"THT_1"]  
dist_i_Voisins <-  st_distance(myusr,THT_Voisins,by_element = T)


#if a temperature point is above the usr geometry distance is zero
idxPointAudessus <-  match(0,dist_i_Voisins)

if(! is.na(idxPointAudessus)){
  #temperature is the one of the above mesoNH point
 THT_moy <- THT_Voisins[idxPointAudessus, "THT_1", drop=T]  
} 
else{
THT_moy <-  sum(as.vector(1/dist_i_Voisins) * THT_Voisins$THT_1)  /   (sum(1/dist_i_Voisins) %>%  as.vector())
}


zoneUSRregulieres[i,"TEMP"] <-  THT_moy

}




st_write(zoneUSRregulieres, "./indicateurs_UHI/USR_temperatureMesoNH_method1/zoneEtude_temp_method1.geojson")

dev.off()
plot(zoneDelicates$geometry, col="palegreen")
plot(zoneUSRregulieres["TEMP"], add=T)






