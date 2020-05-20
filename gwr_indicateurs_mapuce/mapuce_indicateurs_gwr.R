# Load necessary packages 
library(cartography)
library(sf)
library(classInt)
library(GWmodel)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

# Set working directory 
setwd("C:/Stage_IGN/indicateurs/gwr_mapuce_indicateurs")

# Load zone of interest (pathway can be modified if different ZOI to be calculated)
zoi <- read_sf("C:/Users/riley/Documents/GitHub/indicateurs_UHI/ventilation_mesoNH_maillage/zone_strat1_full.geojson")
# Fix the projection
st_crs(zoi) <- 2154

names(zoi)

# Drop unnecessary columns and only retain columns useful for calculating and comparing the indicators
drops <- c("vgttn_s", "rt_srfc", "rt_lngr", "trttr_l", "hydr_sr", "hydr_ln",  
           "cntg_st", "mn_dr_s", "h_mean", "h_std", "b_area", "b_vol", "bld_nmb", "min_m_dst", "men_m_dst", "mn_std_",
           "dst_t_c", "hydr_dn", "vgt_dns", "rod_dns", "ext_nv_", "ba", "bgh",      
           "icif", "icio", "id", "local", "pcif", "pcio", "pd", "psc", "typo_mj", "typ_scn")
zoi <- zoi[ , !(names(zoi) %in% drops)]
# Certain USR have duplicates which need to be eliminated
zoi <- zoi[!duplicated(zoi$pk_usr), ]
names(zoi)

# Conversion of ZOI to a spatial object
zoi_spatial<-as(zoi, 'Spatial')
# zoi summary
summary(zoi_spatial)

# Linear models of TEB_T1 and the mapuce indicators
# ml_floor <- lm(zoi_spatial$TEB_T1~zoi_spatial$floor)
# ml_floorRatio <- lm(zoi_spatial$TEB_T1~zoi_spatial$flor_rt)
# ml_compMoyenne <- lm(zoi_spatial$TEB_T1~zoi_spatial$cmpc_mn_n)
# ml_compMoyPonderee <- lm(zoi_spatial$TEB_T1~zoi_spatial$cmpc_mn_w)
# ml_contigBati <- lm(zoi_spatial$TEB_T1~zoi_spatial$cntg_mn)
# ml_volumePassif <- lm(zoi_spatial$TEB_T1~zoi_spatial$p_vl_r_)
# ml_trousBlocs <- lm(zoi_spatial$TEB_T1~zoi_spatial$b_hls__)
# ml_hautMoyBlocs <- lm(zoi_spatial$TEB_T1~zoi_spatial$b_std__)
# ml_moyCompBlocsNonPond <- lm(zoi_spatial$TEB_T1~zoi_spatial$b_m_nw_)
# ml_moyCompBlocsPond <- lm(zoi_spatial$TEB_T1~zoi_spatial$b_m_w_c)
# ml_compBlocsStd <- lm(zoi_spatial$TEB_T1~zoi_spatial$b_std_c)
# ml_densiteBati <- lm(zoi_spatial$TEB_T1~zoi_spatial$bld_dns)

ml_floor <- lm(zoi_spatial$floor~zoi_spatial$TEB_T1)
ml_floorRatio <- lm(zoi_spatial$flor_rt~zoi_spatial$TEB_T1)
ml_compMoyenne <- lm(zoi_spatial$cmpc_mn_n~zoi_spatial$TEB_T1)
ml_compMoyPonderee <- lm(zoi_spatial$cmpc_mn_w~zoi_spatial$TEB_T1)
ml_contigBati <- lm(zoi_spatial$cntg_mn~zoi_spatial$TEB_T1)
ml_volumePassif <- lm(zoi_spatial$p_vl_r_~zoi_spatial$TEB_T1)
ml_trousBlocs <- lm(zoi_spatial$b_hls__~zoi_spatial$TEB_T1)
ml_hautMoyBlocs <- lm(zoi_spatial$b_std__~zoi_spatial$TEB_T1)
ml_moyCompBlocsNonPond <- lm(zoi_spatial$b_m_nw_~zoi_spatial$TEB_T1)
ml_moyCompBlocsPond <- lm(zoi_spatial$b_m_w_c~zoi_spatial$TEB_T1)
ml_compBlocsStd <- lm(zoi_spatial$b_std_c~zoi_spatial$TEB_T1)
ml_densiteBati <- lm(zoi_spatial$bld_dns~zoi_spatial$TEB_T1)
ml_b_vol_m <- lm(zoi_spatial$b_vol_m~zoi_spatial$TEB_T1)

# Summaries of the models
summary(ml_floor) # R2 : 0.01
summary(ml_floorRatio) # R2 : 0.2792 
summary(ml_compMoyenne) # R2 :  0.001137
summary(ml_compMoyPonderee) # R2 :  0.004185
summary(ml_contigBati) # R2 : 0.2273
summary(ml_volumePassif) # R2 : 0.0007837
summary(ml_trousBlocs) # R2 :  0.06756
summary(ml_hautMoyBlocs) # R2 : 0.01267
summary(ml_moyCompBlocsNonPond) # R2 : 0.0942
summary(ml_moyCompBlocsPond) # R2 : 0.05481
summary(ml_compBlocsStd) # R2 : 0.007362
summary(ml_densiteBati) # R2 : 0.1814
summary(ml_b_vol_m) # R2 : 0.007041 => retrouver en dernier mais pas intéressant

# Model residuals 
zoi_spatial$floorResiduals <- ml_floor$residuals
zoi_spatial$fl_rat_Residuals <- ml_floorRatio$residuals
zoi_spatial$compMoy_Residuals <- ml_compMoyenne$residuals
zoi_spatial$compMoyPond_Residuals <- ml_compMoyPonderee$residuals
zoi_spatial$contigBati_Residuals <- ml_contigBati$residuals
zoi_spatial$v_passif_Residuals <- ml_volumePassif$residuals
zoi_spatial$trousBlocs_Residuals <- ml_trousBlocs$residuals
zoi_spatial$hautMoyBlocs_Residuals <- ml_hautMoyBlocs$residuals
zoi_spatial$moyCompBnonPond_Residuals <- ml_moyCompBlocsNonPond$residuals
zoi_spatial$moyCompPond_Residuals <- ml_moyCompBlocsPond$residuals
zoi_spatial$compBlocsStd_Residuals <- ml_compBlocsStd$residuals
zoi_spatial$densidteBati_Residuals <- ml_densiteBati$residuals

# GGPlot2s 
ggplot(ml_floor, aes(x=zoi_spatial$TEB_T1, y=zoi_spatial$floor))+
  geom_point(colour="black", shape="diamond")+geom_smooth(colour="red", method="lm", fill="red")+
  ylab("Sum of floor surfaces")+
  xlab("Temperature TEB_T1")+
  theme_gray()

ggplot(ml_floorRatio, aes(x=zoi_spatial$TEB_T1, y=zoi_spatial$flor_rt))+
  geom_point(colour="black", shape="diamond")+geom_smooth(colour="red", method="lm", fill="red")+
  ylab("Floor surface ratios")+
  xlab("Temperature TEB_T1")+
  theme_gray()

ggplot(ml_compMoyenne, aes(x=zoi_spatial$TEB_T1, y=zoi_spatial$cmpc_mn_n))+
  geom_point(colour="black", shape="diamond")+geom_smooth(colour="red", method="lm", fill="red")+
  ylab("Non-weighted mean building compacity")+
  xlab("Temperature TEB_T1")+
  theme_gray()

ggplot(ml_compMoyPonderee, aes(x=zoi_spatial$TEB_T1, y=zoi_spatial$cmpc_mn_w))+
  geom_point(colour="black", shape="diamond")+geom_smooth(colour="red", method="lm", fill="red")+
  ylab("Weighted mean building compacity")+
  xlab("Temperature TEB_T1")+
  theme_gray()

ggplot(ml_contigBati, aes(x=zoi_spatial$TEB_T1, y=zoi_spatial$cntg_mn))+
  geom_point(colour="black", shape="diamond")+geom_smooth(colour="red", method="lm", fill="red")+
  ylab("Mean building contiguity")+
  xlab("Temperature TEB_T1")+
  theme_gray()

ggplot(ml_volumePassif, aes(x=zoi_spatial$TEB_T1, y=zoi_spatial$p_vl_r_))+
  geom_point(colour="black", shape="diamond")+geom_smooth(colour="red", method="lm", fill="red")+
  ylab("mean passive building volume")+
  xlab("Temperature TEB_T1")+
  theme_gray()

ggplot(ml_trousBlocs, aes(x=zoi_spatial$TEB_T1, y=zoi_spatial$b_hls__))+
  geom_point(colour="black", shape="diamond")+geom_smooth(colour="red", method="lm", fill="red")+
  ylab("Mean areal surface of holes in blocks")+
  xlab("Temperature TEB_T1")+
  theme_gray()

ggplot(ml_hautMoyBlocs, aes(x=zoi_spatial$TEB_T1, y=zoi_spatial$b_std__))+
  geom_point(colour="black", shape="diamond")+geom_smooth(colour="red", method="lm", fill="red")+
  ylab("Mean block height")+
  xlab("Temperature TEB_T1")+
  theme_gray()

ggplot(ml_moyCompBlocsNonPond, aes(x=zoi_spatial$TEB_T1, y=zoi_spatial$b_m_nw_))+
  geom_point(colour="black", shape="diamond")+geom_smooth(colour="red", method="lm", fill="red")+
  ylab("Non-weighted mean block compacity")+
  xlab("Temperature TEB_T1")+
  theme_gray()

ggplot(ml_moyCompBlocsPond, aes(x=zoi_spatial$TEB_T1, y=zoi_spatial$b_m_w_c))+
  geom_point(colour="black", shape="diamond")+geom_smooth(colour="red", method="lm", fill="red")+
  ylab("Weighted mean block compacity")+
  xlab("Temperature TEB_T1")+
  theme_gray()

ggplot(ml_compBlocsStd, aes(x=zoi_spatial$TEB_T1, y=zoi_spatial$b_std_c))+
  geom_point(colour="black", shape="diamond")+geom_smooth(colour="red", method="lm", fill="red")+
  ylab("Mean block compacity standard deviation")+
  xlab("Temperature TEB_T1")+
  theme_gray()

ggplot(ml_densiteBati, aes(x=zoi_spatial$TEB_T1, y=zoi_spatial$bld_dns))+
  geom_point(colour="black", shape="diamond")+geom_smooth(colour="red", method="lm", fill="red")+
  ylab("Building density")+
  xlab("Temperature TEB_T1")+
  theme_gray()

# Histograms of indicator residuals
qplot(zoi_spatial$floorResiduals, geom = "histogram", binwidth = 0.02, main = "Floor residual distribution", xlab = "residual distribution", ylab = "residual count",
      fill=I("blue"), col=I("white"), alpha=(.2))

qplot(zoi_spatial$fl_rat_Residuals, geom = "histogram", binwidth = 0.2, main = "Floor ratio residual distribution", xlab = "residual distribution", ylab = "residual count",
      fill=I("blue"), col=I("white"), alpha=(.2))

qplot(zoi_spatial$compMoy_Residuals, geom = "histogram", binwidth = 0.2, main = "Mean building compacity residual distribution", xlab = "residual distribution", ylab = "residual count",
      fill=I("blue"), col=I("white"), alpha=(.2))

qplot(zoi_spatial$compMoyPond_Residuals, geom = "histogram", binwidth = 0.2, main = "Weighted mean building compacity residual distribution", xlab = "residual distribution", ylab = "residual count",
      fill=I("blue"), col=I("white"), alpha=(.2))

qplot(zoi_spatial$contigBati_Residuals, geom = "histogram", binwidth = 0.2, main = "Mean building contiguity residual distribution", xlab = "residual distribution", ylab = "residual count",
      fill=I("blue"), col=I("white"), alpha=(.2))

qplot(zoi_spatial$v_passif_Residuals, geom = "histogram", binwidth = 0.2, main = "Passive building volume residual distribution", xlab = "residual distribution", ylab = "residual count",
      fill=I("blue"), col=I("white"), alpha=(.2))

qplot(zoi_spatial$trousBlocs_Residuals, geom = "histogram", binwidth = 0.2, main = "Mean areal surface of holes in blocks", xlab = "residual distribution", ylab = "residual count",
      fill=I("blue"), col=I("white"), alpha=(.2))

qplot(zoi_spatial$hautMoyBlocs_Residuals, geom = "histogram", binwidth = 0.2, main = "Mean block height", xlab = "residual distribution", ylab = "residual count",
      fill=I("blue"), col=I("white"), alpha=(.2))

qplot(zoi_spatial$moyCompBnonPond_Residuals, geom = "histogram", binwidth = 0.2, main = "Non-weighted mean block compacity", xlab = "residual distribution", ylab = "residual count",
      fill=I("blue"), col=I("white"), alpha=(.2))

qplot(zoi_spatial$moyCompPond_Residuals, geom = "histogram", binwidth = 0.2, main = "Weighted mean block compacity", xlab = "residual distribution", ylab = "residual count",
      fill=I("blue"), col=I("white"), alpha=(.2))

qplot(zoi_spatial$compBlocsStd_Residuals, geom = "histogram", binwidth = 0.2, main = "Mean block compacity standard deviation", xlab = "residual distribution", ylab = "residual count",
      fill=I("blue"), col=I("white"), alpha=(.2))

qplot(zoi_spatial$densidteBati_Residuals, geom = "histogram", binwidth = 0.2, main = "Building density", xlab = "residual distribution", ylab = "residual count",
      fill=I("blue"), col=I("white"), alpha=(.2))

# Residuals Maps

# Legend values = quantile range 6
qt6<-classIntervals(var=zoi_spatial$fl_rat_Residuals, n=6)
# Legend values equal intervals range 6
#eq6 <- classIntervals(var=zoi_spatial$fl_rat_Residuals, style="equal", n=6)
# Deplacer le zéro
qt6$brks[4] <- 0

#eq6$brks[5] <- 0

# Cartographie thématique des résidus, par quantiles et palette autour de zéro
couleurs<-carto.pal(pal1 = "blue.pal", n1 = 3, pal2 = "red.pal", n2 = 3)
choroLayer(spdf=zoi_spatial, df=zoi_spatial@data, var="fl_rat_Residuals", breaks=qt6$brks, col=couleurs, border=FALSE,
           legend.pos = "topleft", legend.values.rnd = 2, legend.title.txt = "residuals", legend.frame = FALSE)

layoutLayer("TEB T1 Meso-NH~Floor Ratio Residuals Strat_1", tabtitle = T, postitle = "center",
            sources = "Grid: MApUCE, Building data: MApUCE, Météo France", frame = F, scale = 5)

# Habillage avec superposition par les quartiers et communes de l'agglo.
plot(zoi_spatial, col = NA, border = "grey40" , add = TRUE)

north(pos = "topright")
dev.off()

# GWR

# Creation of a distrance matrix for the GWR calculations
dm_zoi <- gw.dist(sp::coordinates(zoi_spatial))

# Bandwidth selection using the cross validation method and the Gaussian kernel
bp_flr <- bw.gwr(floor~TEB_T1, data = zoi_spatial, approach = "CV",kernel = "gaussian", dMat = dm_zoi)
bp_flrRat <- bw.gwr(flor_rt~TEB_T1, data = zoi_spatial, approach = "CV", kernel = "gaussian", dMat = dm_zoi)
bp_cm <- bw.gwr(cmpc_mn_n~TEB_T1, data = zoi_spatial, approach = "CV", kernel = "gaussian", dMat = dm_zoi)
bp_cmp <- bw.gwr(cmpc_mn_w~TEB_T1, data = zoi_spatial, approach = "CV", kernel = "gaussian", dMat = dm_zoi)
bp_contig <- bw.gwr(cntg_mn~TEB_T1, data = zoi_spatial, approach = "CV", kernel = "gaussian", dMat = dm_zoi)
bp_pvol <- bw.gwr(p_vl_r_~TEB_T1, data = zoi_spatial, approach = "CV", kernel = "gaussian", dMat = dm_zoi)
bp_bholes <- bw.gwr(b_hls__~TEB_T1, data = zoi_spatial, approach = "CV", kernel = "gaussian", dMat = dm_zoi)
bp_b_std <- bw.gwr(b_std__~TEB_T1, data = zoi_spatial, approach = "CV", kernel = "gaussian", dMat = dm_zoi)
bp_bcompNW <- bw.gwr(b_m_nw_~TEB_T1, data = zoi_spatial, approach = "CV", kernel = "gaussian", dMat = dm_zoi)
bp_bcompW <- bw.gwr(b_m_w_c~TEB_T1, data = zoi_spatial, approach = "CV", kernel = "gaussian", dMat = dm_zoi)
bp_b_std_c <- bw.gwr(b_std_c~TEB_T1, data = zoi_spatial, approach = "CV", kernel = "gaussian", dMat = dm_zoi)
bp_bld_dns <- bw.gwr(bld_dns~TEB_T1, data = zoi_spatial, approach = "CV", kernel = "gaussian", dMat = dm_zoi)

# GWR according to the automatically calculated bandwidth
g_flr <- gwr.basic(floor~TEB_T1, data = zoi_spatial, bw = bp_flr, kernel = "gaussian", adaptive = F, dMat = dm_zoi)
g_flrRat <- gwr.basic(flor_rt~TEB_T1, data = zoi_spatial, bw = bp_flrRat, kernel = "gaussian", adaptive = F, dMat = dm_zoi)
g_cm <- gwr.basic(cmpc_mn_n~TEB_T1, data = zoi_spatial, bw = bp_cm, kernel = "gaussian", adaptive = F, dMat = dm_zoi)
g_cmp <- gwr.basic(cmpc_mn_w~TEB_T1, data = zoi_spatial, bw = bp_cmp, kernel = "gaussian", adaptive = F, dMat = dm_zoi)
g_contig <- gwr.basic(cntg_mn~TEB_T1, data = zoi_spatial, bw = bp_contig, kernel = "gaussian", adaptive = F, dMat = dm_zoi)
g_pvol <- gwr.basic(p_vl_r_~TEB_T1, data = zoi_spatial, bw = bp_pvol, kernel = "gaussian", adaptive = F, dMat = dm_zoi)
g_bholes <- gwr.basic(b_hls__~TEB_T1, data = zoi_spatial, bw = bp_bholes, kernel = "gaussian", adaptive = F, dMat = dm_zoi)
g_b_std <- gwr.basic(b_std__~TEB_T1, data = zoi_spatial, bw = bp_b_std, kernel = "gaussian", adaptive = F, dMat = dm_zoi)
g_bcompNW <- gwr.basic(b_m_nw_~TEB_T1, data = zoi_spatial, bw = bp_bcompNW, kernel = "gaussian", adaptive = F, dMat = dm_zoi)
g_bcompW <- gwr.basic(b_m_w_c~TEB_T1, data = zoi_spatial, bw = bp_bcompW, kernel = "gaussian", adaptive = F, dMat = dm_zoi)
g_b_std_c <- gwr.basic(b_std_c~TEB_T1, data = zoi_spatial, bw = bp_b_std_c, kernel = "gaussian", adaptive = F, dMat = dm_zoi)
g_bld_dns <- gwr.basic(b_std_c~TEB_T1, data = zoi_spatial, bw = bp_bld_dns, kernel = "gaussian", adaptive = F, dMat = dm_zoi)

# Print GWR results
g_flr
g_flrRat
g_cm
g_cmp
g_contig
g_pvol
g_bholes
g_b_std
g_bcompNW
g_bcompW
g_b_std_c
g_bld_dns

# Sockage des résultats dans une variable, puis dans les données attributaires
rg_flr<-as.data.frame(g_flr$SDF)
zoi_spatial$rg_flr <- rg_flr$TEB_T1
zoi_spatial$r2_flr <- rg_flr$Local_R2

rg_flrRat <- as.data.frame(g_flrRat$SDF)
zoi_spatial$rg_flrRat <- rg_flrRat$TEB_T1
zoi_spatial$r2_flrRat <- rg_flrRat$Local_R2

rg_cm <- as.data.frame(g_cm$SDF)
zoi_spatial$rg_cm <- rg_cm$TEB_T1
zoi_spatial$r2_cm <- rg_cm$Local_R2

rg_cmp <- as.data.frame(g_cmp$SDF)
zoi_spatial$rg_cmp <- rg_cmp$TEB_T1
zoi_spatial$r2_cmp <- rg_cmp$Local_R2

rg_contig <- as.data.frame(g_contig$SDF)
zoi_spatial$rg_contig <- rg_contig$TEB_T1
zoi_spatial$r2_contig <- rg_contig$Local_R2

rg_pvol <- as.data.frame(g_pvol$SDF)
zoi_spatial$rg_pvol <- rg_pvol$TEB_T1
zoi_spatial$r2_pvol <- rg_pvol$Local_R2

rg_bholes <- as.data.frame(g_bholes$SDF)
zoi_spatial$rg_bholes <- rg_bholes$TEB_T1
zoi_spatial$r2_bholes <- rg_bholes$Local_R2

rg_b_std <- as.data.frame(g_b_std$SDF)
zoi_spatial$rg_b_std <- rg_b_std$TEB_T1
zoi_spatial$r2_b_std <- rg_b_std$Local_R2

rg_bcompNW <- as.data.frame(g_bcompNW$SDF)
zoi_spatial$rg_bcompNW <- rg_bcompNW$TEB_T1
zoi_spatial$r2_bcompNW <- rg_bcompNW$Local_R2

rg_bcompW <- as.data.frame(g_bcompW$SDF)
zoi_spatial$rg_bcompW <- rg_bcompW$TEB_T1
zoi_spatial$r2_bcompW <- rg_bcompW$Local_R2

rg_b_std_c <- as.data.frame(g_b_std_c$SDF)
zoi_spatial$rg_b_std_c <- rg_b_std_c$TEB_T1
zoi_spatial$r2_b_std_c <- rg_b_std_c$Local_R2

rg_bld_dns <- as.data.frame(g_bld_dns$SDF)
zoi_spatial$rg_bld_dns <- rg_bld_dns$TEB_T1
zoi_spatial$r2_bld_dns <- rg_bld_dns$Local_R2

# Histogram of all the respective indicator coefficient values relating to the TEB_T1 temperature
qplot(zoi_spatial$rg_flr, geom = "histogram", main = "GWR - Floor~TEB_T1 Coef. distribution", 
      xlab = "coef. distribution", ylab = "coef. count", fill=I("blue"), col=I("white"),
      alpha=(.2))

qplot(zoi_spatial$rg_flrRat, geom = "histogram", main = "GWR - Floor Ratio~TEB_T1 Coef. distribution", 
      xlab = "coef. distribution", ylab = "coef. count", fill=I("blue"), col=I("white"),
      alpha=(.2))

qplot(zoi_spatial$rg_cm, geom = "histogram", main = "GWR - Mean building compacity~TEB_T1 Coef. distribution", 
      xlab = "coef. distribution", ylab = "coef. count", fill=I("blue"), col=I("white"),
      alpha=(.2))

qplot(zoi_spatial$rg_cmp, geom = "histogram", main = "GWR - Weighted mean building compacity~TEB_T1 Coef. distribution", 
      xlab = "coef. distribution", ylab = "coef. count", fill=I("blue"), col=I("white"),
      alpha=(.2))

qplot(zoi_spatial$rg_contig, geom = "histogram", main = "GWR - Mean building contiguity~TEB_T1 Coef. distribution", 
      xlab = "coef. distribution", ylab = "coef. count", fill=I("blue"), col=I("white"),
      alpha=(.2))

qplot(zoi_spatial$rg_pvol, geom = "histogram", main = "GWR - Passive building volume~TEB_T1 Coef. distribution", 
      xlab = "coef. distribution", ylab = "coef. count", fill=I("blue"), col=I("white"),
      alpha=(.2))

qplot(zoi_spatial$rg_bholes, geom = "histogram", main = "GWR - Mean areal surface of holes in blocks~TEB_T1 Coef. distribution", 
      xlab = "coef. distribution", ylab = "coef. count", fill=I("blue"), col=I("white"),
      alpha=(.2))

qplot(zoi_spatial$rg_b_std, geom = "histogram", main = "GWR - Mean block height~TEB_T1 Coef. distribution", 
      xlab = "coef. distribution", ylab = "coef. count", fill=I("blue"), col=I("white"),
      alpha=(.2))

qplot(zoi_spatial$rg_bcompNW, geom = "histogram", main = "GWR - Non-weighted mean block compacity~TEB_T1 Coef. distribution", 
      xlab = "coef. distribution", ylab = "coef. count", fill=I("blue"), col=I("white"),
      alpha=(.2))

qplot(zoi_spatial$rg_bcompW, geom = "histogram", main = "GWR - Weighted mean block compacity~TEB_T1 Coef. distribution", 
      xlab = "coef. distribution", ylab = "coef. count", fill=I("blue"), col=I("white"),
      alpha=(.2))

qplot(zoi_spatial$rg_b_std_c, geom = "histogram", main = "GWR - Mean block compacity standard deviation~TEB_T1 Coef. distribution", 
      xlab = "coef. distribution", ylab = "coef. count", fill=I("blue"), col=I("white"),
      alpha=(.2))

qplot(zoi_spatial$rg_bld_dns, geom = "histogram", main = "GWR - Building density~TEB_T1 Coef. distribution", 
      xlab = "coef. distribution", ylab = "coef. count", fill=I("blue"), col=I("white"),
      alpha=(.2))

# GWR Maps with non adapted bandwidths

# Preparation of fisher-jenks classification for the map legend 
fish_jks7<-classIntervals(zoi_spatial$rg_bld_dns, style="fisher", n=7)
# Deplacer le zéro
fish_jks7$brks[4] <- 0 
hist(zoi_spatial$rg_bld_dns, breaks=fish_jks7$brks)
# Colour prep
couleurs<-carto.pal(pal1 = "green.pal", n1 = 3, pal2 = "wine.pal", n2 = 4)
#couleurs <- brewer.pal(7, "Oranges")

# Dessin de la carte
choroLayer(spdf=zoi_spatial, df=zoi_spatial@data, var="rg_bld_dns", breaks=fish_jks7$brks, col=couleurs, border=FALSE, lwd=0.1, 
           legend.values.rnd = 2, legend.pos="topleft", legend.title.txt = "Coef.")

layoutLayer("Building Density~TEB_T1 (GWR): non-adapted BW", tabtitle = F, postitle = "center",
            sources = "Grid: MApUCE, Building data: MApUCE, Météo France", frame = F, scale = 5)

# Habillage avec superposition par les USR du maillage de MApUCE.
plot(zoi_spatial, col = NA, border = "grey40", add = TRUE)

north(pos = "topright")
dev.off()

# GWR adapted bandwidth, using only the indicators with the greatest adjusted R2 values
bpa_flrRat <- bw.gwr(flor_rt~TEB_T1, data = zoi_spatial, approach = "CV", adaptive=TRUE, dMat = dm_zoi, kernel = "gaussian")
bpa_contig <- bw.gwr(cntg_mn~TEB_T1, data = zoi_spatial, approach = "CV", adaptive=TRUE, dMat = dm_zoi, kernel = "gaussian")

# GWR with adapted bandwidths
ga_flrRat <- gwr.basic(flor_rt~TEB_T1, data = zoi_spatial, bw = bpa_flrRat, adaptive = T)
ga_contig <- gwr.basic(cntg_mn~TEB_T1, data = zoi_spatial, bw = bpa_contig, adaptive = T)

# Print results
ga_flrRat
ga_contig

# Transfer the results to data frames then back into the zoi spatial object
rga_flrRat <- as.data.frame(ga_flrRat$SDF)
zoi_spatial$rga_flrRat <- rga_flrRat$TEB_T1

rga_contig <- as.data.frame(ga_contig$SDF)
zoi_spatial$rga_contig <- rga_contig$TEB_T1

# GWR Maps with non adapted bandwidths

# Preparation of fisher-jenks classification for the map legend 
fish_jks7<-classIntervals(zoi_spatial$rga_flrRat, style="fisher", n=7)
# Deplacer le zéro
fish_jks7$brks[4] <- 0 
hist(zoi_spatial$rga_flrRat, breaks=fish_jks7$brks)
# Colour prep
couleurs<-carto.pal(pal1 = "blue.pal", n1 = 3, pal2 = "red.pal", n2 = 4)
#couleurs <- brewer.pal(7, "Oranges")

# Dessin de la carte
choroLayer(spdf=zoi_spatial, df=zoi_spatial@data, var="rga_flrRat", breaks=fish_jks7$brks, col=couleurs, border=FALSE, lwd=0.1, 
           legend.values.rnd = 2, legend.pos="topleft", legend.title.txt = "Coef.")

layoutLayer("Floor ratio~TEB_T1 (GWR): adapted BW 17 pts", tabtitle = F, postitle = "center",
            sources = "Grid: MApUCE, Building data: MApUCE, Météo France", frame = F, scale = 5)

# Habillage avec superposition par les USR du maillage de MApUCE.
plot(zoi_spatial, col = NA, border = F, add = TRUE)

north(pos = "topright")
dev.off()

# Signifigance tests
signif_test1_flrRat=abs(ga_flrRat$SDF$TEB_T1) -2 * ga_flrRat$SDF$TEB_T1_SE
signif_test2_flrRat=ga_flrRat$SDF$TEB_T1/ga_flrRat$SDF$TEB_T1_SE

signif_test1_contig=abs(ga_contig$SDF$TEB_T1) -2 * ga_contig$SDF$TEB_T1_SE
signif_test2_contig=ga_contig$SDF$TEB_T1/ga_contig$SDF$TEB_T1_SE

summary(signif_test1_flrRat)
summary(signif_test2_flrRat)

summary(signif_test1_contig)
summary(signif_test2_contig)

# Monte Carlo tests
mc_flrRat <- gwr.montecarlo(flor_rt~TEB_T1, data = zoi_spatial, nsims = 99, kernel="gaussian", adaptive = T, bw = bpa_flrRat, dMat = dm_zoi)
mc_contig <- gwr.montecarlo(cntg_mn~TEB_T1, data = zoi_spatial, nsims = 99, adaptive = T, bw = bpa_contig, dMat = dm_zoi)

mc_flrRat
