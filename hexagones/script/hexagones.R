library(sf)
library(ggplot2)
library(viridis)
library(sp)
library(dplyr)
library(RColorBrewer)

# Set working directory
setwd("C:/Stage_IGN/geoviz/agregation/hexagones")

# Load hexagons layer
hex650m <- read_sf("C:/Stage_IGN/geoviz/agregation/hexagones/650m_hexagones.shp")
st_crs(hex650m) <- 2154
# Create ID field and drop unnecessary columns
hex650m <- dplyr::mutate(hex650m, FID = row_number())
head(hex650m)
drops <- c("left", "bottom", "right", "top")
hex650m <- hex650m[ , !(names(hex650m) %in% drops)]
# Create area field
hex650m$surface <- st_area(hex650m$geometry)

# Load zoi strat_1
strat_1 <- read_sf("C:/Stage_IGN/indicateurs/gwr_master/zoi_files/zoi_indicateurs_strat_1.geojson")

# Intersect strat 1 with the hexagones
strat_1_hex <- st_intersection(strat_1, hex650m)

# Summarise key indicator information according to the hexagon FID
hextempData <- strat_1_hex %>% group_by(FID) %>% summarise(TEB_T1=mean(TEB_T1))
hextempData <- data.frame("FID"=hextempData$FID, "TEB_T1"=hextempData$TEB_T1)
# Create subsets for key indicators
hex_morpho <- subset(strat_1_hex, (!is.na(strat_1_hex$bsf)))
zoi_isf <- subset(strat_1_hex, (!is.na(strat_1_hex$isf)))
zoi_psf <- subset(strat_1_hex, (!is.na(strat_1_hex$psf)))
# Summarise subsets
hexmorphoData <- hex_morpho %>% group_by(FID) %>% summarise(bsf=mean(bsf), hre=mean(hre))

hexPSFdata <- zoi_psf %>% group_by(FID) %>% summarise(psf=mean(psf))

hexISFdata <- zoi_isf %>% group_by(FID) %>% summarise(isf=mean(isf))
# Change to dataframes
hexmorphoData <- data.frame("FID"=hexmorphoData$FID, "bsf"=hexmorphoData$bsf, "hre"=hexmorphoData$hre)
hexPSFdata <- data.frame("FID"=hexPSFdata$FID, "psf"=hexPSFdata$psf)
hexISFdata <- data.frame("FID"=hexISFdata$FID, "isf"=hexISFdata$isf)
# Join indicator data to original hex data
hex650m <- full_join(hex650m, hexmorphoData, by="FID")
hex650m <- full_join(hex650m, hexISFdata, by="FID")
hex650m <- full_join(hex650m, hexPSFdata, by="FID")

# Load INSEE population data
inseePOP <- read_sf("C:/Stage_IGN/geoviz/agregation/hexagones/inseePopZoneEtude.shp")
st_crs(inseePOP) <- 2154

# Intersect the INSEE data with the hexagons
hexINSEE <- st_intersection(inseePOP, hex650m)
hexINSEE$men_prop_c <- hexINSEE$r_men_prop*(hexINSEE$c_ind_c/hexINSEE$r_ind_r)
# Summarise data within hexagons
sumHexINSEE <- hexINSEE %>% group_by(FID) %>% summarise(Ind_c=sum(c_ind_c), Men=sum(r_men), Men_surf=sum(r_men_surf), 
                                                        Men_coll=sum(r_men_coll), Men_5ind=sum(r_men_5ind), Men_1ind=sum(r_men_1ind),
                                                        Men_prop=sum(r_men_prop), Men_basr=sum(r_men_basr), Ind_r=sum(r_ind_r),
                                                        Ind_age1=sum(r_ind_age1), Ind_age2=sum(r_ind_age2), Ind_age3=sum(r_ind_age3),
                                                        Ind_age4=sum(r_ind_age4), Ind_age5=sum(r_ind_age5), Ind_age6=sum(r_ind_age6),
                                                        Ind_age7=sum(r_ind_age7), Ind_age8=sum(r_ind_age8), Ind_srf=sum(r_ind_srf))
# Create a separate data frame to join data 
dataHexINSEE <- data.frame("FID"=sumHexINSEE$FID, "Ind_c"=sumHexINSEE$Ind_c, "Men"=sumHexINSEE$Men, "Men_surf"=sumHexINSEE$Men_surf,
                           "Men_coll"=sumHexINSEE$Men_coll, "Men_5ind"=sumHexINSEE$Men_5ind, "Men1ind"=sumHexINSEE$Men_1ind,
                           "Men_prop"=sumHexINSEE$Men_prop, "Men_basr"=sumHexINSEE$Men_basr, "Ind_r"=sumHexINSEE$Ind_r,
                           "Ind_age1"=sumHexINSEE$Ind_age1, "Ind_age2"=sumHexINSEE$Ind_age2, "Ind_age3"=sumHexINSEE$Ind_age3,
                           "ind_age4"=sumHexINSEE$Ind_age4, "Ind_age5"=sumHexINSEE$Ind_age5, "Ind_age6"=sumHexINSEE$Ind_age6,
                           "ind_age7"=sumHexINSEE$Ind_age7, "Ind_age8"=sumHexINSEE$Ind_age8, "Ind_srf"=sumHexINSEE$Ind_srf)

# Join INSEE population data to original hexagons layer
hex650m <- full_join(hex650m, dataHexINSEE, by="FID")
hex650m <- full_join(hex650m, hextempData, by="FID")

st_write(hex650m, "hexMorphoINSEE.geojson")

# Plot data

# Set working directory
setwd("C:/Stage_IGN/geoviz/agregation/hexagones/plots")

# Names of fields
names(hex650m)
# "geometry" "FID"      "surface"  "bsf"      "hre"      "isf"      "psf"      "Ind_c"    "Men"      "Men_surf"
# "Men_coll" "Men_5ind" "Men1ind"  "Men_prop" "Men_basr" "Ind_r"    "Ind_age1" "Ind_age2" "Ind_age3" "ind_age4"
# "Ind_age5" "Ind_age6" "ind_age7" "Ind_age8" "Ind_srf"  "TEB_T1"  

# TEBT1
png("Hex650m_TEB8T1.png", width = 800, height = 800)
ggplot(data = hex650m) + 
  geom_sf(aes(fill = TEB_T1)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "Temp") +
  scale_fill_gradientn(colours = rev(brewer.pal(11, "RdBu")))
dev.off()

# BSF
png("Hex650m_BSF.png", width = 800, height = 800)
ggplot(data = hex650m) + 
  geom_sf(aes(fill = bsf)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "BSF") +
  scale_fill_viridis(option = "magma")
dev.off()

# HRE
png("Hex650m_HRE.png", width = 800, height = 800)
ggplot(data = hex650m) + 
  geom_sf(aes(fill = hre)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "HRE") +
  scale_fill_viridis(option = "viridis")
dev.off()

# ISF
png("Hex650m_ISF.png", width = 800, height = 800)
ggplot(data = hex650m) + 
  geom_sf(aes(fill = isf)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "ISF") +
  scale_fill_gradientn(colours = rev(brewer.pal(9, "YlOrRd")))
dev.off()

# PSF
png("Hex650m_PSF.png", width = 800, height = 800)
ggplot(data = hex650m) + 
  geom_sf(aes(fill = psf)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "PSF") +
  scale_fill_gradientn(colours = brewer.pal(9, "YlGn"))
dev.off()

# Ind_C
png("Hex650m_Ind_C.png", width = 800, height = 800)
ggplot(data = hex650m) + 
  geom_sf(aes(fill = Ind_c)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "Ind_c") +
  scale_fill_gradientn(colours = brewer.pal(9, "Reds"))
dev.off()

# Men
png("Hex650m_Men.png", width = 800, height = 800)
ggplot(data = hex650m) + 
  geom_sf(aes(fill = Men)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "Menages") +
  scale_fill_gradientn(colours = brewer.pal(9, "RdPu"))
dev.off()

# Men_surf
png("Hex650m_Men_surf.png", width = 800, height = 800)
ggplot(data = hex650m) + 
  geom_sf(aes(fill = Men_surf)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "surf. men.") +
  scale_fill_gradientn(colours = brewer.pal(9, "Purples"))
dev.off()

# Men_coll
png("Hex650m_Men_coll.png", width = 800, height = 800)
ggplot(data = hex650m) + 
  geom_sf(aes(fill = Men_coll)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "Men. coll.") +
  scale_fill_gradientn(colours = brewer.pal(9, "PuRd"))
dev.off()

# Men_5ind
png("Hex650m_Men_5ind.png", width = 800, height = 800)
ggplot(data = hex650m) + 
  geom_sf(aes(fill = Men_5ind)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "Men. 5ind.") +
  scale_fill_gradientn(colours = brewer.pal(9, "OrRd"))
dev.off()

# Men_1ind
png("Hex650m_Men_1ind.png", width = 800, height = 800)
ggplot(data = hex650m) + 
  geom_sf(aes(fill = Men1ind)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "Men. 1ind.") +
  scale_fill_gradientn(colours = brewer.pal(9, "Oranges"))
dev.off()

# Men_1ind
png("Hex650m_Men_prop.png", width = 800, height = 800)
ggplot(data = hex650m) + 
  geom_sf(aes(fill = Men_prop)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "Men. prop.") +
  scale_fill_gradientn(colours = brewer.pal(9, "Greens"))
dev.off()

# Men_basr
png("Hex650m_Men_basr.png", width = 800, height = 800)
ggplot(data = hex650m) + 
  geom_sf(aes(fill = Men_basr)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "Men. bas.r.") +
  scale_fill_gradientn(colours = brewer.pal(9, "Greys"))
dev.off()

# Ind_r
png("Hex650m_Ind_r.png", width = 800, height = 800)
ggplot(data = hex650m) + 
  geom_sf(aes(fill = Ind_r)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "Ind. rect.") +
  scale_fill_gradientn(colours = brewer.pal(9, "GnBu"))
dev.off()

# Ind_age1
png("Hex650m_Ind_age1.png", width = 800, height = 800)
ggplot(data = hex650m) + 
  geom_sf(aes(fill = Ind_age1)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "Ind. age1") +
  scale_fill_gradientn(colours = brewer.pal(9, "Blues"))
dev.off()

# Ind_age2
png("Hex650m_Ind_age2.png", width = 800, height = 800)
ggplot(data = hex650m) + 
  geom_sf(aes(fill = Ind_age2)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "Ind. age2") +
  scale_fill_gradientn(colours = brewer.pal(9, "YlGnBu"))
dev.off()

# Ind_age3
png("Hex650m_Ind_age3.png", width = 800, height = 800)
ggplot(data = hex650m) + 
  geom_sf(aes(fill = Ind_age3)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "Ind. age3") +
  scale_fill_gradientn(colours = brewer.pal(9, "PuBu"))
dev.off()

# Ind_age4
png("Hex650m_Ind_age4.png", width = 800, height = 800)
ggplot(data = hex650m) + 
  geom_sf(aes(fill = ind_age4)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "Ind. age4") +
  scale_fill_gradientn(colours = brewer.pal(9, "GnBu"))
dev.off()

# Ind_age5
png("Hex650m_Ind_age5.png", width = 800, height = 800)
ggplot(data = hex650m) + 
  geom_sf(aes(fill = Ind_age5)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "Ind. age5") +
  scale_fill_gradientn(colours = brewer.pal(9, "BuPu"))
dev.off()

# Ind_age6
png("Hex650m_Ind_age6.png", width = 800, height = 800)
ggplot(data = hex650m) + 
  geom_sf(aes(fill = Ind_age6)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "Ind. age6") +
  scale_fill_gradientn(colours = rev(brewer.pal(11, "PiYG")))
dev.off()

# Ind_age7
png("Hex650m_Ind_age7.png", width = 800, height = 800)
ggplot(data = hex650m) + 
  geom_sf(aes(fill = ind_age7)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "Ind. age7") +
  scale_fill_gradientn(colours = rev(brewer.pal(11, "RdYlBu")))
dev.off()

# Ind_age6
png("Hex650m_Ind_age8.png", width = 800, height = 800)
ggplot(data = hex650m) + 
  geom_sf(aes(fill = Ind_age8)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "Ind. age8") +
  scale_fill_gradientn(colours = rev(brewer.pal(11, "RdGy")))
dev.off()

# Ind_srf
png("Hex650m_Ind_srf.png", width = 800, height = 800)
ggplot(data = hex650m) + 
  geom_sf(aes(fill = Ind_srf)) +
  theme_minimal() +
  theme(axis.line =  element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(fill = "Ind. srf.") +
  scale_fill_gradientn(colours = brewer.pal(11, "PuOr"))
dev.off()
