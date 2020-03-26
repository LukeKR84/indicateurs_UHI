library(sf)
library(dplyr)
library(ggplot2)
library(leaflet)
library(viridis)

setwd("~/encadrement/stage_luke/indicateurs_UHI/")





# mesoNH data loading
dfMeso <- read.csv("./data_mesoNH_point_O.csv")
names(dfMeso)


#conversion to spatial object
dfMeso_sf <-  st_as_sf(dfMeso, coords = c("longitude", "latitude"))
#only temperature THT variables
temperature_sf <-  dfMeso_sf [,33:64]


#display points pattern 
plot(temperature_sf[,1])

str(temperature_sf)

#spatial extent
emprise <-  st_bbox(temperature_sf)
emprise_sf <-  st_as_sfc(emprise)




#display on leaflet map
mypalette <- colorNumeric(palette = "viridis", domain = c(min(temperature_sf$THT_1, na.rm = T), max(temperature_sf$THT_1, na.rm = F)))
p1 <-leaflet(temperature_sf) %>% 
     addTiles( urlTemplate = '//{s}.tile.openstreetmap.fr/hot/{z}/{x}/{y}.png') %>% 
     addCircleMarkers(lng=st_coordinates(temperature_sf)[,1] ,
                 lat=st_coordinates(temperature_sf)[,2], 
                 radius=5,
                 stroke = T,
                 color="black",
                 weight = 1,
                  fillOpacity = 0.7,
               fillColor = ~mypalette(temperature_sf$THT_1)
                  )
p1


# display paris intra muros 

quartiers_sf <-  st_read("./data_vecteur/quartier_paris.shp")
voierie_sf <-  st_read("./data_vecteur/voie.shp") 
voierie_sf <-  voierie_sf$geometry

#check that crs are the same
st_crs(quartiers_sf) == st_crs(voierie_sf)


#overlay temperature points on map
dev.off()
plot(voierie_sf, col = "lightgrey", lwd = 0.5)
plot(quartiers_sf$geometry, add=T, border= "darkgrey")


# focus on points spatial extent

# project everything in lambert 93
quartiers_sf <-  st_transform(quartiers_sf, 2154)
voierie_sf <-  st_transform(voierie_sf, 2154)
#dirty but crs of temperature points is NA
st_crs(temperature_sf) <-  4326
temperature_sf <-  st_transform(temperature_sf, 2154)
st_crs(temperature_sf)

# recompute enveloppe / extent with projected coordinates
emprise <-  st_bbox(temperature_sf)
emprise_sf <-  st_as_sfc(emprise)
st_crs(emprise_sf)

#enlarge bbox to get approx. one extra cube width around the corner points
emprise_sf <- st_buffer(emprise_sf, dist=400) 
emprise_sf <-  st_as_sfc(st_bbox(emprise_sf)) 
#convert back to sf object
emprise_sf <-  emprise_sf %>% st_geometry %>% st_sf()


dev.off()
plot(voierie_sf, col = "lightgrey", lwd = 0.5)
plot(quartiers_sf$geometry, add=T, border= "darkgrey")
plot(temperature_sf[1],pal = viridis_pal(), add=T,  pch = 16)
plot(emprise_sf, add=T, border="purple")







#intersection of layers with point pattern extent
voierie_emprise <-  st_intersection(emprise_sf, voierie_sf) %>% st_as_sf()
quartiers_emprise <-  st_intersection(emprise_sf, quartiers_sf) %>% st_as_sf()



dev.off()
plot(voierie_emprise$geometry, col = "lightgrey", lwd = 0.5)
plot(quartiers_emprise$geometry, add=T, border= "darkgrey")
plot(temperature_sf[1],pal = viridis_pal(), add=T,  pch = 16)



#cell size is something like the maximum of the minimum non zero deistance between points
st_distance(temperature_sf) %>% as.matrix() %>%  as.vector() %>% sort() %>% as_data_frame %>% filter(.>600) 
cellsize <-  640
grid <-  st_make_grid(emprise_sf, what = "polygons", cellsize =  cellsize)


#check that each point is covered by a cell :  1 in each cell
st_contains(grid, temperature_sf) %>% lengths 


#final plot 
dev.off()
par(mar= c(0,0,0,0))
plot(emprise_sf$geometry)
plot(voierie_emprise$geometry, col = "lightgrey", lwd = 0.5, add=T)
plot(quartiers_emprise$geometry, add=T, border= "darkgrey")
plot(grid, add=T, border="orange", lwd=0.2)
plot(temperature_sf[1], add=T, pal=viridis_pal(), pch=16)







