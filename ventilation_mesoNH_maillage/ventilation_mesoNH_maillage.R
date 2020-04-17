library(sf)
library(ggplot2)
library(viridis)
library(raster)
library(sp)
library(tidyverse)
library(viridisLite)
library(viridis)
library(units)

setwd("~/encadrement/stage_luke/")


create_MesoNH_sf <-  function(path, p_colnames) {
  dfMeso <- read.csv(path)
  dfMeso_sf <-  st_as_sf(dfMeso, coords = c("longitude", "latitude"))
  #assuming data is on WGS 84
  st_crs(dfMeso_sf) <- 4326
  dfMeso_sf <-  st_transform(dfMeso_sf, 2154)
  warning("on suppose que le CRS des points MesoNH est 4326 (WGS84)", call. = F)
  # matching columns names
  
  colIdx <-  match(p_colnames, names(dfMeso_sf))
  if (anyNA(colIdx)) {
    message("Error: No such columns")
  }
  else{
    dfMeso_sf  <- dfMeso_sf[, colIdx]
    dfMeso_sf <-  dfMeso_sf %>%  filter_at(vars(column_names), all_vars(.<999))
    warning(" On suppose que la valeur des points MesoNH de la bordure exterieure sont à 999", call.=F)
    return(dfMeso_sf)
  }
}


create_extentRectangle <-  function(points , buff_width=0){
# recompute enveloppe / extent with projected coordinates
  emprise_sf <-  st_as_sfc(st_bbox(points))
  #enlarge bbox to get approx. one extra cube width around the corner points
  emprise_sf <- st_buffer(emprise_sf, dist=buff_width) 
  # to get a sharp edge square
  emprise_sf <-  st_as_sfc(st_bbox(emprise_sf))
return(emprise_sf)  
}




trim_zone_to_mesoNH_extent <-  function(mesoPoints_sf, zone){
  extent <-  create_extentRectangle(mesoPoints_sf)
  trimmed_zone <-  st_intersection(zone, extent)
  return(trimmed_zone)
}



prepare_zone <- function(path_maillage, path_meso, p_colnames){
  dfMeso <- create_MesoNH_sf(path_to_mesoNH_CSVFile, p_colnames)
  maillage_zone <- st_read(path_to_maillage)
  st_crs(maillage_zone) <- 2154 
  warning("on suppose que le CRS du maillage est 2154" , call.=F)
  zone <-  trim_zone_to_mesoNH_extent(dfMeso,maillage_zone)
  return(zone)
}




strategie1 <-  function(maillage, meso_sf, seuil = 650, column_names) {
  seuil <-  650
  nentites <-  nrow(maillage)
  for (i in 1:nentites) {
    myusr <- maillage[i, ]
    idxVoisins <-  st_is_within_distance(myusr, meso_sf, seuil)
    idxVoisins <- idxVoisins %>% unlist()
    THT_Voisins <- meso_sf[idxVoisins, column_names]
    dist_i_Voisins <-  st_distance(myusr, THT_Voisins, by_element = T) %>% as.numeric()
    #if a temperature point is above the usr geometry distance is zero
    idxPointAudessus <-  match(0, dist_i_Voisins)
    THT_moy <-  NA
    if (!is.na(idxPointAudessus)) {
      #temperature is those of the above mesoNH point
      THT_moy <- THT_Voisins[idxPointAudessus, column_names, drop = T] 
    }
    else{
      deno <-  (sum(1 / dist_i_Voisins))
      THT_moy <- colSums ( (1 / dist_i_Voisins) * THT_Voisins[,column_names, drop=T] )  / deno 
      }
    
    maillage[i, column_names] <-  THT_moy
    
    
    if(i %% 100 == 0){
      cat(i, "/", nentites, "\n")
    }
  }
  return(maillage)
}


#### 

#Methode 2  : echantilloner des points dans la géométrie de la maille
#déterminer les cases du raster MesoNH dans lesquelles se trouvent ces points
# aggreger les temperatures de ces points  par la moyenne


create_MesoNH_raster_sf <- function(meso_sf, column_names, aggreg = mean) {
    # conversion to SP format
    SP_temperature <-  as_Spatial(meso_sf)
    #raster creation
    
    # emprise coords order is xmin xmax ymin y max
    emp <- st_bbox(meso_sf)
    exten_meso <- extent(emp[c(1, 3, 2, 4)])
    
    #raster creation
    raster_emprise <-  raster(nrow = 20, ncol = 24, exten_meso)
    rasterMeso <- rasterize(SP_temperature, raster_emprise, field = column_names, fun=aggreg)
    
    # vectorisation
    poly_raster <-  as(rasterMeso, "SpatialPolygonsDataFrame")
    # cast back to sf object
    poly_raster <-  st_as_sf(poly_raster)
    st_crs(poly_raster) <- 2154
    return(poly_raster)
  }




# certaines geometries sont trop petites : moins de 10m^2
strategie2 <- function(zoneEtude, poly_raster, aggreg,column_names){
  zoneEtude <- zoneEtude %>%  filter( (st_area(.) %>% as.numeric()) > 10 )
  
    zoneEtude[,column_names] <-  NA
  nentites <-  nrow(zoneEtude)
  for (i in 1:nentites) {
    myusr <- zoneEtude[i, ]
      rndpts <-  st_sample(myusr$geometry, size = 10,  type = "random")
      idxCell <-  st_within(rndpts, poly_raster) %>%  unlist()
      temper <- sapply(poly_raster[idxCell, column_names, drop=T], aggreg)
      zoneEtude[i, column_names] <-  temper
    
    
    if(i %% 100 == 0){
      cat(i, "/", nentites, "\n")
    }
  }
  
  return(zoneEtude)
}

#  variante de la strategie 2 en utilisant le voronoi des points plutot que le raster .
# le quadrillage va donc suivre les points (meilleure découpage de  l'espace couvert par chaque point)
strategie3 <- function(meso_sf, zone, buff_dist=0, column_names){
  
  rect <-  create_extentRectangle(meso_sf, buff_dist)
  vor <-  st_voronoi(st_union(meso_sf), rect)
  vor <-  st_collection_extract(vor)
  vor <-  st_intersection(vor, rect)
  vor <-  st_as_sf(vor)
  vor <-  st_join(vor, meso_sf)
  result <-  strategie2(zoneEtude = zone,poly_raster = vor, mean,column_names)
  return(result)
  
}






path_to_mesoNH_CSVFile <- "./indicateurs_UHI/data_mesoNH_point_O.csv"
path_to_maillage <- "./indicateurs_UHI/zoneEtudeUSR.shp"
column_names <-paste0("TEB_T", 1:6)

zone <-  prepare_zone(path_to_maillage, path_to_mesoNH_CSVFile, column_names)
meso_sf <-  create_MesoNH_sf(path_to_mesoNH_CSVFile,column_names)

strat1 <-  strategie1(maillage = zone,meso_sf = meso_sf,seuil = 650,column_names)
polyMesoNH <-  create_MesoNH_raster_sf(meso_sf,column_names) 
strat2 <-strategie2(zone, polyMesoNH,mean, column_names)



# png("./indicateurs_UHI/USR_temperatureMesoNH_method1/strategie1.png",width = 800,height = 800)
par(mar=c(0,0,0,0))
plot(strat1["TEB_T1"],main="ventilation TEB_T1 sur USR (s1)")
dev.off()

png("./indicateurs_UHI/USR_temperatureMesoNH_method1/strategie2.png",width = 800,height = 800)
par(mar=c(0,0,0,0))
plot(strat2["TEB_T1"],main="ventilation TEB_T1 sur USR (s2)")
dev.off()


png("./indicateurs_UHI/USR_temperatureMesoNH_method1/superpos_raster_zone.png",width = 800,height = 800)
par(mar=c(0,0,0,0))
plot(zone$geometry, lwd=0.2, main="Raster TEB_T1 sur maillage USR")
plot(polyMesoNH["TEB_T1"], pal=sf.colors(alpha = 0.7), add=T, border=NA)
plot(zone$geometry, lwd=0.2, add=T)


dev.off()





st_write(strat1,"./indicateurs_UHI/ventilation_mesoNH_maillage//zone_strat1_full.geojson")
st_write(strat2,"./indicateurs_UHI/ventilation_mesoNH_maillage//zone_strat2_full.geojson")


# on garde uniquement les attributs TEB_T



st_write(strat1 %>%  select(column_names) ,"./indicateurs_UHI/ventilation_mesoNH_maillage/zone_strat1_TEB_only.geojson")
st_write(strat2 %>%  select(column_names) ,"./indicateurs_UHI/ventilation_mesoNH_maillage/zone_strat2_TEB_only.geojson")








  
strat3 <-  strategie3(meso_sf, zone, 400, column_names)  
strat3 <-  st_as_sf(strat3)

st_write(strat3 %>%  select(column_names) ,"./indicateurs_UHI/ventilation_mesoNH_maillage/zone_strat3_TEB_only.shp")
st_write(strat3,"./indicateurs_UHI/ventilation_mesoNH_maillage//zone_strat3_full.geojson")


  
##################################################################
# implémentation alternatives de startegie 1 pour voir le gain de performances


strat1_apply <-  function(maille, seuil = 650, column_names) {
  idxVoisins <-  st_is_within_distance(maille, meso_sf, seuil)
  idxVoisins <- idxVoisins %>% unlist()
  THT_Voisins <- meso_sf[idxVoisins, column_names]
  dist_i_Voisins <-  st_distance(myusr, THT_Voisins, by_element = T) %>% as.numeric()
  #if a temperature point is above the usr geometry distance is zero
  idxPointAudessus <-  match(0, dist_i_Voisins)
  THT_moy <-  NA
  if (!is.na(idxPointAudessus)) {
    #temperature is those of the above mesoNH point
    THT_moy <- THT_Voisins[idxPointAudessus, column_names, drop = T] 
  }
  else{
    deno <-  (sum(1 / dist_i_Voisins))
    THT_moy <- colSums ( (1 / dist_i_Voisins) * THT_Voisins[,column_names, drop=T] )  / deno 
  }
  
  return(THT_moy)
}



system.time(
yy <-  split(tutu, 1:nrow(tutu)) %>%    map(strat1_apply, seuil=650, column_names)
)


zz <- purrr::map(tutu, strat1_apply, 650, column_names)


strat1_sideEffects <-  function(i, maillage){
  myusr <- maillage[i, ]
  idxVoisins <-  st_is_within_distance(myusr, meso_sf, seuil)
  idxVoisins <- idxVoisins %>% unlist()
  THT_Voisins <- meso_sf[idxVoisins, column_names]
  dist_i_Voisins <-  st_distance(myusr, THT_Voisins, by_element = T) %>% as.numeric()
  #if a temperature point is above the usr geometry distance is zero
  idxPointAudessus <-  match(0, dist_i_Voisins)
  THT_moy <-  NA
  if (!is.na(idxPointAudessus)) {
    #temperature is those of the above mesoNH point
    THT_moy <- THT_Voisins[idxPointAudessus, column_names, drop = T] 
  }
  else{
    deno <-  (sum(1 / dist_i_Voisins))
    THT_moy <- colSums ( (1 / dist_i_Voisins) * THT_Voisins[,column_names, drop=T] )  / deno 
  }
  
  maillage[i, column_names] <-  THT_moy  
  return(NULL)
}



system.time(
zz <- lapply(1:nrow(tutu), strat1_sideEffects,maillage=zone)
)



