library(sf)
library(dplyr)
library(rayshader)
library(viridis)
  library(raster)
library(png)
library(ggplot2)
library(reshape2)
library(rgl)


setwd("/home/paulchapron/encadrement/stageURCLIM/indicateurs_UHI/ventilation_mesoNH_maillage")



zone <- read_sf("./zone_strat1_full.geojson")

#one geometry is not valid 
zone <- st_make_valid(zone)


#affecting temperature class 
classes_temp <-  seq(from=297, to=301, by=0.5 )
zone$temp_class  <- cut(zone$TEB_T1, breaks = classes_temp, labels=F)
zone_homogenes <- zone %>% group_by(temp_class) %>% summarise(geometry=st_union(geometry))



#export to PNG temporary file 
fifi <- tempfile()
png(fifi,width = 400, height = 400)
plot(zone_homogenes["temp_class"], border=NA, pal = viridis_pal(), main =NULL, key.pos=NULL, setParUsrBB=TRUE)
dev.off()





#raster layer for elevation  , elevation as color value
localtif <-  raster::raster(fifi)
elmat <-  raster_to_matrix(localtif,verbose = T)

#trim the raster to remove png borders  
offset <- 20
elmat <-  elmat[seq(from= offset, to= nrow(elmat)-offset), seq(from= offset, to= ncol(elmat)-offset)]


#fill the whitehole (value = 8) with surrounding value : 2
elmat[which(elmat==8)] <-  2


# check that borders have been trimmed
ggplot(melt(elmat), aes(x=Var1, y=Var2, fill=value))+
  geom_tile()

# to increase z values range NB : is not linear ! 
elmat <-  elmat ** 2


# gray 3D rendering 
elmat %>% 
  sphere_shade(progbar = TRUE,texture = "bw", sunangle = 45) %>%
  plot_3d(elmat, zoom=1,
          solid=FALSE,
          lineantialias = TRUE
          )

rgl.close()

#
# png overlay file for coloration

zone2 <- read_sf("../calcul_indicateurs_paris/zoi.shp")
#one geometry is not valid 
zone2 <- st_make_valid(zone)

fifi2 <- tempfile()
png(fifi2,width = 400, height = 400)
st_crs(zone2) <-  2154
st_crs(zone_homogenes) <-  2154 
zone_homogenes <-  st_transform(zone_homogenes,crs = 2154)
zone2 <-  st_transform(zone2,crs = 2154)
plot(zone2["bsf"], border=NA, pal = viridis_pal(), main =NULL, key.pos=NULL, setParUsrBB=TRUE, reset=FALSE)
# plot(zone_homogenes$geometry, border="orange", main =NULL, key.pos=NULL, setParUsrBB=TRUE, add=T)
dev.off()


#remove borders
myoverlay <-  readPNG(fifi2)
offset <- 20
myoverlay <-  myoverlay[seq(from= offset, to= nrow(myoverlay)-offset), seq(from= offset, to= ncol(myoverlay)-offset),]



#final render 
elmat %>% 
  sphere_shade(progbar = TRUE,texture = "bw", sunangle = 45) %>%
   add_overlay(myoverlay,alphacolor = "#FFFFFF") %>% 
  plot_3d(elmat, zoom=1,
         solid=TRUE,
          lineantialias = TRUE
  )


rgl.close()
