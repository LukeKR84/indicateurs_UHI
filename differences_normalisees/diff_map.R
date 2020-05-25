# Load necessary packages 
library(sf)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(viridis)


# Set working directory 
setwd("encadrement/stage_luke/indicateurs_UHI/")

# Load 
zoi <- read_sf("./gwr_zoi_mesoNH_strat_1/zoi_mesoNH_strat1.geojson")

# Fix the projection
st_crs(zoi) <- 2154

names(zoi)


#normalisation 1 
# zscore  (x - mean / )

normalize1 <-  function(x){
  mu <-  mean(x,na.rm = T)
  sigma  <-  sd(x, na.rm = T)
  return( (x - mu) / sigma)
}

#normalisation 2
# x- min / max _ min 
normalize2 <- function(x){
mimi <-  min(x,na.rm = T)
mama <-  max(x, na.rm = T)
return ( ( x- mimi )/ (mama-mimi) )
}

zoi$norm1_TEB_T1  <-  normalize1(zoi$TEB_T1)
zoi$norm2_TEB_T1  <-  normalize2(zoi$TEB_T1)


#display map of normalized absolute differences between normalized TEBT1 and var 

mapdiff <- function(var){
zoi$diff1 <- abs(zoi$norm1_TEB_T1 - normalize1(zoi[,var, drop=T]))
zoi$diff2 <-abs(zoi$norm2_TEB_T1 - normalize2(zoi[,var, drop=T]))


# plot2
filename <-  paste0("TEBT1_minus_", var,"_zscore.png")
png(filename = filename,width = 800, height = 800)
plot(zoi["diff1"], pal = viridis_pal() ,main = paste("TEB_T1 -" ,var, " (z-score) " ), border="darkgrey", lwd=0.2)
dev.off()

#plot 2
filename <-  paste0("TEBT1_minus_", var,"_MaxMin.png")
png(filename = filename,width = 800, height = 800)
plot(zoi["diff2"], pal = viridis_pal() ,main = paste("TEB_T1 -" ,var, "(min Max normalisation) " ), border="darkgrey", lwd=0.2)
dev.off()

}


setwd("./differences_normalisees/")
for (v in mycolumns){

  mapdiff(v)
}





