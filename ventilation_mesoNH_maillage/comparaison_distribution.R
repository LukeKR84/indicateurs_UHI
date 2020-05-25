library(tidyverse)
library(sf)
library(ggplot2)
library(viridis)
library(raster)
library(tidyverse)
library(viridisLite)
library(viridis)
library(units)

setwd("~/encadrement/stage_luke/")


s1 <-  st_read("indicateurs_UHI/ventilation_mesoNH_maillage/zone_strat1_TEB_only.geojson")
s2 <-  st_read("indicateurs_UHI/ventilation_mesoNH_maillage/zone_strat2_TEB_only.geojson")
s3 <-  st_read("indicateurs_UHI/ventilation_mesoNH_maillage/zone_strat3_TEB_only.geojson")
ddr <- st_read("~/tmp/polyRasterMesoNH.geojson")
imran <-  st_read("./ventil_Imran/zoneEtudeUSR.shp")
st_crs(imran) <- 2154
plot(imran$geometry)
class(imran)
class(s1)
class(s2)
class(s3)

polyMesoNH %>% as.data.frame() %>%  ggplot(aes(x=TEB_T2))+ geom_density()


ddr$strategie <- "MesoNH"
s1$strategie <- "Strat 1"
s2$strategie <- "Strat 2"
s3$strategie <- "Strat 3"
imran$strategie <-  "Strat Imran"


colnames(imran)[57] <-  "TEB_T1"

mydf <- rbind( s1[, c("TEB_T1", "strategie"), drop=T],
 s2[, c("TEB_T1", "strategie"), drop=T],
 s3[, c("TEB_T1", "strategie"), drop=T],
 imran[, c("TEB_T1", "strategie"), drop=T],
ddr[, c("TEB_T1", "strategie"), drop=T]
)

nrows(mydf)



ggplot(mydf, aes(x=TEB_T1, color=strategie))+ geom_density(size=2)






library(entropy)




pointsTEBsampling <-  function(nbpoints=10000, data_sf){
  pts <-  st_sample(data_sf,nbpoints) %>% st_as_sf()
  pts <- st_join(pts,data_sf)
  return(pts)
}

rect <-   create_extentRectangle(meso_sf)

pts_S1 <-  pointsTEBsampling(100000, s1)
pts_S2 <-  pointsTEBsampling(100000, s2)
pts_S3<-  pointsTEBsampling(100000, s3)
pts_ddr<-  pointsTEBsampling(100000, ddr)

plot(pts_ddr["TEB_T1"], pch=16)

KL.empirical(pts_S1$TEB_T1,pts_ddr$TEB_T1)


