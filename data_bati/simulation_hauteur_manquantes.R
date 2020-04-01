library(sf)
library(ggplot2)
library(dplyr)
library(viridis)
library(stringr)


setwd("~/encadrement/stage_luke/indicateurs_UHI/")


ZoneEtudeBati  <- st_read("./data_bati/bati_zoneEtude.shp")


missingH <-  ZoneEtudeBati %>%  filter(HAUTEUR ==0)  
nrow(missingH)

# add an attribute
missingH$HAUTEUR_SIMU <-  NA



simulateHbymean <-  function(missingH,buff_dist){
  missingH <-  missingH %>%  filter(is.na(HAUTEUR_SIMU))  
  cat(nrow(missingH), "bati à traiter")
  centroMissingH <-  st_centroid(missingH)
  #define circular neighborhoood 
  neigh_missingH <-  st_buffer(centroMissingH,buff_dist)
  
  regularBuildings_centroids <-  st_centroid(ZoneEtudeBati) %>% filter( ! (ID %in%  missingH$ID))
  
  regularBuildings_H <-  regularBuildings_centroids %>% select(HAUTEUR)
  
  # aggregate heights by the mean  of surrounding heights in neighborhood
  neighbors_mean_H <-  aggregate(regularBuildings_H, neigh_missingH ,mean, na.rm=T)
  missingH$HAUTEUR_SIMU <- neighbors_mean_H$HAUTEUR  
  return(missingH)
}


missingH <-  ZoneEtudeBati %>%  filter( HAUTEUR ==0)  
missingH$HAUTEUR_SIMU <-  NA
missingH <-  simulateHbymean(missingH, 20)
missingH %>%as.data.frame() %>%   filter(is.na(HAUTEUR_SIMU)) %>%  summarise(n())


result <-  missingH %>%  filter(!(is.na(HAUTEUR_SIMU)))
missingH <-  simulateHbymean(missingH, 30)
missingH %>%as.data.frame() %>%   filter(is.na(HAUTEUR_SIMU)) %>%  summarise(n())
result <- rbind(result,  missingH %>%  filter(!(is.na(HAUTEUR_SIMU))))


missingH <-  simulateHbymean(missingH, 50)
missingH %>%as.data.frame() %>%   filter(is.na(HAUTEUR_SIMU)) %>%  summarise(n())
result <- rbind(result,  missingH %>%  filter(!(is.na(HAUTEUR_SIMU))))


missingH <-  simulateHbymean(missingH, 100)
missingH %>%as.data.frame() %>%   filter(is.na(HAUTEUR_SIMU)) %>%  summarise(n())
result <- rbind(result,  missingH %>%  filter(!(is.na(HAUTEUR_SIMU))))

missingH <-  simulateHbymean(missingH, 150)
missingH %>%as.data.frame() %>%   filter(is.na(HAUTEUR_SIMU)) %>%  summarise(n())
result <- rbind(result,  missingH %>%  filter(!(is.na(HAUTEUR_SIMU))))

missingH <-  simulateHbymean(missingH, 200)
missingH %>%as.data.frame() %>%   filter(is.na(HAUTEUR_SIMU)) %>%  summarise(n())
result <- rbind(result,  missingH %>%  filter(!(is.na(HAUTEUR_SIMU))))


missingH <-  simulateHbymean(missingH, 250)
missingH %>%as.data.frame() %>%   filter(is.na(HAUTEUR_SIMU)) %>%  summarise(n())
result <- rbind(result,  missingH %>%  filter(!(is.na(HAUTEUR_SIMU))))

missingH <-  simulateHbymean(missingH, 300)
missingH %>%as.data.frame() %>%   filter(is.na(HAUTEUR_SIMU)) %>%  summarise(n())
result <- rbind(result,  missingH %>%  filter(!(is.na(HAUTEUR_SIMU))))


result
st_write(result,"./data_bati/bati_hauteur_simulees.shp")

ZoneEtudeBati$HAUTEUR_SIMU <-  NA

H_OK <- ZoneEtudeBati %>%  filter(HAUTEUR !=0) 



lengthOK <-  H_OK %>%  nrow()
length_notOK <- result %>%  nrow()  
length_notOK + lengthOK == nrow(ZoneEtudeBati)

result$HAUTEUR <-  result$HAUTEUR_SIMU

final_bati <-  rbind(H_OK, result)
final_bati$HAUTEUR_SIMU <-  NULL

st_write(final_bati,"./data_bati/bati_zoneEtude_hauteur_corrigees.shp")


