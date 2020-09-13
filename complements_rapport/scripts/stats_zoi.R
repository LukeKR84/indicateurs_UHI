library(ggplot2)
library(FactoMineR)
library(spdep)
library(ape)
library(sf)
library(cartography)
library(tibble)

# Set working directory 
setwd("C:/Stage_IGN/indicateurs/stats")

# Load zone of interest
zoi <- read_sf("C:/Stage_IGN/indicateurs/stats/zoi.shp")
# Fix the projection
st_crs(zoi) <- 102110
zoi <- st_transform(zoi, 2154)

# Fraction summaries
sum_bsf <- summary(zoi$bsf)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.0000  0.2373  0.3581  0.3503  0.4797  1.0000    2034 
sum_hre <- summary(zoi$hre)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.000   2.419   5.215   5.605   8.422  45.000    2034 
sum_isf <- summary(zoi$isf)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.00000 0.03700 0.05953 0.08991 0.09860 0.99943     109 
# 2e essai (zéros changés en NA)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.0000  0.0401  0.0618  0.0941  0.1012  0.9994     608 
sum_psf <- summary(zoi$psf)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.000   0.016   0.066   0.185   0.232   1.000    8144 

# Get the variance for each fraction
var_bsf <- var(zoi$bsf, na.rm=T)
# [1] 0.02795083
var_hre <- var(zoi$hre, na.rm=T)
# [1] 14.05484
var_isf <- var(zoi$isf, na.rm=T)
# [1] 0.01136105
var_psf <- var(zoi$psf, na.rm =T)
# [1] 0.06805136

# Get the standard deviation for each fraction
sd_bsf <- sd(zoi$bsf, na.rm=T)
# [1] 0.167185
sd_hre <- sd(zoi$hre, na.rm=T)
# [1] 3.748978
sd_isf <- sd(zoi$isf, na.rm=T)
# [1] 0.1065882
sd_psf <- sd(zoi$psf, na.rm=T)
# [1] 0.2608666

# Histograms 

# BSF
hist(zoi$bsf, main = "BSF distribution for the Parisian Region", xlab = "BSF range", ylab = "Number of RSUs",
     col = "red", breaks = 20)
# HRE
hist(zoi$hre, main = "HRE distribution for the Parisian Region", xlab = "HRE range in metres", ylab = "Number of RSUs",
     col = "purple", xlim = c(0, 50), ylim = c(0, 400),   breaks = 200 )
# ISF
hist(zoi$isf, main = "ISF distribution for the Parisian Region", xlab = "ISF range", ylab = "Number of RSUs",
     col = "grey", breaks = 20)
# PSF
hist(zoi$psf, main = "PSF distribution for the Parisian Region", xlab = "PSF range", ylab = "Number of RSUs",
     col = "green", breaks = 20)
# RSU area
hist(zoi$usr_area, main = "Area of the RSUs in metres", col = "orange", xlim = c(0, 200000), breaks = 20)


# Boxplots

# BSF

med_bsf <- median(zoi$bsf, na.rm=T)
# [1] 0.3581075
mean_bsf <- mean(zoi$bsf, na.rm=T)
# [1] 0.3503017
boxplot(zoi$bsf, col = "red", main = "BSF distribution for the Parisian Region", ylab = "BSF range")

# HRE

med_hre <- median(zoi$hre, na.rm=T)
# [1] 5.215132
mean_hre <- mean(zoi$hre, na.rm=T)
# [1] 5.604887
boxplot(zoi$hre, col ="purple", main = "HRE distribution for the Parisian Region", ylab = "HRE range in metres")

# ISF

med_isf <- median(zoi$isf, na.rm=T)
# [1] 0.0595349
mean_isf <- mean(zoi$isf, na.rm=T)
# [1] 0.08990888
boxplot(zoi$isf, col ="grey", main = "ISF distribution for the Parisian Region", ylab = "ISF range")

# PSF

med_psf <- median(zoi$psf, na.rm=T)
# [1] 0.06625202
mean_psf <- mean(zoi$psf, na.rm=T)
# [1] 0.1848537
boxplot(zoi$psf, col = "green", main = "PSF distribution for the Parisian Region", ylab = "PSF range")

##### Moran's I #####

# nbUSR <- poly2nb(pl = zoi, row.names = zoi$pk_usr, snap = 50, queen = TRUE) # First try = fail

nbUSR_2 <- poly2nb(zoi, queen = T)

# moran.test(x= zoi$bsf, listw = nb2listw(nbUSR, style = "W"), na.action=na.exclude) # First try = fail

lw <- nb2listw(nbUSR_2, style = "W") # zero.policy = T removed

## BSF ##

bsf_lag <- lag.listw(lw, zoi$bsf, NAOK = TRUE)

# Regression model of bsf and lag data
M_bsf <- lm(bsf_lag ~ zoi$bsf)
# Plot BSF lag
plot(bsf_lag ~ zoi$bsf, main = "Moran I BSF regression plot", xlab = "BSF range", ylab = "BSF lag", pch=20, asp=1, las=1)
abline(M_bsf, col = "red")
M_bsf
# Call:
#   lm(formula = bsf_lag ~ zoi$bsf)
# 
# Coefficients:
#   (Intercept)      zoi$bsf  
# 0.1433       0.5887  

coef(M_bsf)[2]
# zoi$bsf 
# 0.5886976 

# Long and complicated method of calculating the Moran's I
n <- 599L   # Define the number of simulations
I.r <- vector(length=n)  # Create an empty vector

for (i in 1:n){
  # Randomly shuffle income values
  x <- sample(zoi$bsf, replace=FALSE)
  # Compute new set of lagged values
  x.lag <- lag.listw(lw, x, NAOK = TRUE)
  # Compute the regression slope and store its value
  M.r    <- lm(x.lag ~ x)
  I.r[i] <- coef(M.r)[2]
}
# Plot the histogram of simulated Moran's I values
# then add our observed Moran's I value to the plot
hist(I.r, main=NULL, xlab="Moran's I of the BSF", xlim = c(-0.04, 0.6), las=1)
abline(v=coef(M_bsf)[2], col="red")

# Moran test - quicker and easier
moran.test(zoi$bsf, lw, zero.policy = TRUE ,na.action=na.exclude)

# result :

# Moran I statistic standard deviate = 93.963, p-value < 2.2e-16
# omitted : loads!
# alternative hypothesis: greater
# sample estimates:
#   Moran I statistic       Expectation          Variance 
# 0.6014085433     -0.0001085894      0.0000409808 

# Monte Carlo Simulation

MC_bsf <- moran.mc(zoi$bsf, lw, nsim = 599, zero.policy = TRUE, na.action=na.exclude)
MC_bsf
# result:

# Monte-Carlo simulation of Moran I
# data:  zoi$bsf 
# weights: lw
# omitted : loads!
# number of simulations + 1: 600 
# 
# statistic = 0.60187, observed rank = 600, p-value = 0.001667
# alternative hypothesis: greater

# Plot the distribution
plot(MC_bsf, main="Moran I Monte-Carlo simulation of BSF", xlab = "BSF range", las=1)

## HRE ##

hre_lag <- lag.listw(lw, zoi$hre, NAOK = TRUE)

# Regression model of hre and lag data
M_hre <- lm(hre_lag ~ zoi$hre)
# Plot HRE lag
plot(hre_lag ~ zoi$hre, main = "Moran I HRE regression plot", xlab = "HRE range", ylab = "HRE lag", 
     xlim = c(0,50), ylim = c(0,20), pch=20, asp=1, las=1)
abline(M_hre, col = "red")
M_hre
# Call:
#   lm(formula = hre_lag ~ zoi$hre)
# 
# Coefficients:
#   (Intercept)      zoi$hre  
# 2.0206       0.6362 

coef(M_hre)[2]
# zoi$hre 
# 0.636216 

moran.test(zoi$hre, lw, zero.policy = TRUE ,na.action=na.exclude)
# Results :
# Moran I test under randomisation
# 
# data:  zoi$hre  
# weights: lw 
# omitted: loads

# Moran I statistic standard deviate = 100.9, p-value < 2.2e-16
# alternative hypothesis: greater
# sample estimates:
#   Moran I statistic       Expectation          Variance 
# 6.456694e-01     -1.085894e-04      4.096619e-05 

# Monte-Carlo Simulation
MC_hre <- moran.mc(zoi$hre, lw, nsim = 599, zero.policy = TRUE, na.action=na.exclude)
MC_hre
# Results :
# Monte-Carlo simulation of Moran I
# 
# data:  zoi$hre 
# weights: lw 
# omitted: loads!
# statistic = 0.64616, observed rank = 600, p-value = 0.001667
# alternative hypothesis: greater

# Plot the distribution
plot(MC_hre, main="Moran I Monte-Carlo simulation of HRE", xlab = "HRE range", las=1)

# PSF

psf_lag <- lag.listw(lw, zoi$psf, NAOK = TRUE)
# Regression model of psf and lag data
M_psf <- lm(psf_lag ~ zoi$psf)
# Plot PSF lag

plot(psf_lag ~ zoi$psf, main = "Moran I PSF regression plot", xlab = "PSF range", ylab = "PSF lag", pch=20, asp=1, las=1)
abline(M_psf, col = "red")
M_psf
# Call:
#   lm(formula = psf_lag ~ zoi$psf)
# 
# Coefficients:
#   (Intercept)      zoi$psf  
# 0.04888      0.73330  
coef(M_psf)[2]

moran.test(zoi$psf, lw, zero.policy = TRUE ,na.action=na.exclude)
# Results:
# Moran I test under randomisation
# 
# data:  zoi$psf  
# weights: lw 
# omitted: loads
# Moran I statistic standard deviate = 37.89, p-value < 2.2e-16
# alternative hypothesis: greater
# sample estimates:
#   Moran I statistic       Expectation          Variance 
# 0.5592836307     -0.0003345601      0.0002181405 

# Monte-Carlo Simulation
MC_psf <- moran.mc(zoi$psf, lw, nsim = 599, zero.policy = TRUE, na.action=na.exclude)
MC_psf
# Results:
# Monte-Carlo simulation of Moran I
# 
# data:  zoi$psf 
# weights: lw 
# omitted: loads
# number of simulations + 1: 600 
# 
# statistic = 0.58117, observed rank = 600, p-value = 0.001667
# alternative hypothesis: greater

# Plot the distribution
plot(MC_psf, main="Moran I Monte-Carlo simulation of PSF", xlab = "PSF range", las=1)

# ISF

isf_lag <- lag.listw(lw, zoi$isf, NAOK = TRUE)

# Regression model of bsf and lag data
M_isf <- lm(isf_lag ~ zoi$isf)
# Plot BSF lag
plot(isf_lag ~ zoi$isf, main = "Moran I ISF regression plot", xlab = "ISF range", ylab = "ISF lag", pch=20, asp=1, las=1)
abline(M_isf, col = "red")
M_isf
# Call:
#   lm(formula = isf_lag ~ zoi$isf)
# 
# Coefficients:
#   (Intercept)      zoi$isf  
# 0.0660       0.2281 

moran.test(zoi$isf, lw, zero.policy = TRUE ,na.action=na.exclude)
# Results:
# Moran I test under randomisation
# 
# data:  zoi$isf  
# weights: lw 
# omitted: loads
# Moran I statistic standard deviate = 37.134, p-value < 2.2e-16
# alternative hypothesis: greater
# sample estimates:
#   Moran I statistic       Expectation          Variance 
# 2.175499e-01     -9.396730e-05      3.435254e-05 

# Monte-Carlo Simulation
MC_isf <- moran.mc(zoi$isf, lw, nsim = 599, zero.policy = TRUE, na.action=na.exclude)
MC_isf
# Results
# Monte-Carlo simulation of Moran I
# 
# data:  zoi$isf 
# weights: lw 
# omitted : loads
# number of simulations + 1: 600 
# 
# statistic = 0.21755, observed rank = 600, p-value = 0.001667
# alternative hypothesis: greater

# Plot the distribution
plot(MC_isf, main="Moran I Monte-Carlo simulation of ISF", xlab = "ISF range", las=1)

# Moran's I as a function of a distance band

centroids_zoi <- st_centroid(zoi, of_largest_polygon = FALSE)

# Plot to view the ZOI's geometry with the centroids
plot(zoi$geometry, border = "gray")
plot(centroids_zoi$geometry, border = "blue", add = TRUE)

# coords <- coordinates(centroids_zoi$geometry) # Didn't work

centroids_zoi_wgs4326 <- st_transform(centroids_zoi, 4326)

# Obtain lat and lon columns from centroids
zoi_coords <- do.call(rbind, st_geometry(centroids_zoi)) %>% as_tibble() %>% setNames(c("lon", "lat"))
zoi_coords_2 <- do.call(rbind, st_geometry(centroids_zoi_wgs4326)) %>% as.data.frame() %>% setNames(c("lon", "lat"))

# Convert to numeric columns
zoi_coords_2$lon <- as.numeric(zoi_coords_2$lon)
zoi_coords_2$lat <- as.numeric(zoi_coords_2$lat)

# Define search radius as 500m
s_dist <- dnearneigh(centroids_zoi$geometry, 0, 500) 

# weighted list for the Monte-Carlo simulation

lw_2 <- nb2listw(s_dist, style = "W", zero.policy = T)

# Run the MC simulation for the fractions

Mdist_BSF <- moran.mc(zoi$bsf, lw_2, nsim = 599, zero.policy = T, na.action=na.exclude)
# Results :
# Monte-Carlo simulation of Moran I
# 
# data:  zoi$bsf 
# weights: lw_2 
# omitted: loads
# number of simulations + 1: 600 
# 
# statistic = 0.45172, observed rank = 600, p-value = 0.001667
# alternative hypothesis: greater

plot(Mdist_BSF, main= "Moran I as a function of a distance band - Monte-Carlo simulation of BSF", xlab = "BSF range", las=1)

Mdist_HRE <- moran.mc(zoi$hre, lw_2, nsim = 599, zero.policy = T, na.action=na.exclude)
# Results :
# Monte-Carlo simulation of Moran I
# 
# data:  zoi$hre 
# weights: lw_2 
# omitted: loads
# number of simulations + 1: 600 
# 
# statistic = 0.5717, observed rank = 600, p-value = 0.001667
# alternative hypothesis: greater

plot(Mdist_HRE, main= "Moran I as a function of a distance band - Monte-Carlo simulation of HRE", xlab = "HRE range in metres", las=1)

Mdist_ISF <- moran.mc(zoi$isf, lw_2, nsim = 599, zero.policy = T, na.action=na.exclude)
# Results

# Monte-Carlo simulation of Moran I
# 
# data:  zoi$isf 
# weights: lw_2 
# omitted: loads
# number of simulations + 1: 600 
# 
# statistic = 0.083425, observed rank = 600, p-value = 0.001667
# alternative hypothesis: greater

plot(Mdist_ISF, main= "Moran I as a function of a distance band - Monte-Carlo simulation of ISF", xlab = "ISF range", las=1)

Mdist_PSF <- moran.mc(zoi$psf, lw_2, nsim = 9999, zero.policy = T, na.action=na.exclude) # Si on change le nombre de simulations ça change pas de masse 
# Results :
# Monte-Carlo simulation of Moran I
# 
# data:  zoi$psf 
# weights: lw_2 
# omitted: loads
# number of simulations + 1: 600 
# 
# statistic = 0.43652, observed rank = 600, p-value = 0.001667
# alternative hypothesis: greater

plot(Mdist_PSF, main= "Moran I as a function of a distance band - Monte-Carlo simulation of PSF", xlab = "PSF range", las=1)
