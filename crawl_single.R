#############################################################

#      crawl: fit continuous-time correlated random walk
#             model to animal movement data

# Author: Michaela A. Kratofil, Cascadia Research
# Updated: 03 AUG 2020

#############################################################

## OVERVIEW ##

# For details on the model, read Johnson et al. (2008),
# Continuous-time correlated random walk model for animal
# telemetry data."

# The model coded here is pretty basic, and may not be broadly
# appropriate for all datasets/project objectives, etc. This is
# not a plug 'n chug model! Take the time to understand the model
# and the various specifications that can be customized for 
# your research questions. 

# There are a few very helpful vignettes provided by Josh London
# and Devin Johnson that would be worth reviewing before running
# this code: jmlondon.github.io/crwexampleakbs/analysis.html, 
# Guide to crawl-ing in R (J. London), etc. (Use google).

# This script is set up to fit a crawl model to a single deployment.

# This model has the functionality to use either LC classes or 
# Argos error ellipse information to formulate the error model.
# Here I use error ellipse information (better), so see vignettes
# and package documentation if interested in using LC classes. 

## How it works ##

# Here we will use location data that has already been through the
# Douglas Filter. We'll need to format the data for input into crawl.

# I then create a wrapper function to fit the model; this can be used 
# for one tag or on multiple tags using the purr package. This example
# predicts locations at at 4 hour time interval. 

################################################################

## load packages
library(crawl)
library(sf)
library(tidyverse)
library(purrr)
library(lubridate)
library(ggplot2)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthhires)

## define species batch
spp = "Sb"
tag = "SbTag015"
plot_folder = "SSM/Plots/"

## import Douglas filtered locations, Argos locations ONLY
all <- read.csv("Douglas Filtered/SbTag001-022_DouglasFiltered_KS_r20d3lc2_2020MAYv1.csv", header = T) %>%
  as_tibble()

## subset out one tag
locs <- filter(all, animal == tag)

## review data 
str(locs)
summary(locs)
colnames(locs)

## format datetime, location class
locs$date <- as.POSIXct(locs$date, tz = 'UTC')
head(locs)
attr(locs$date, 'tzone')

locs$LC <- factor(locs$LC, levels = c("DP","L3","L2","L1","L0","LA","LB","LZ"))
summary(locs$LC)

## simple plot
plot(locs$longitud, locs$latitude)

## map location data
world <- ne_countries(scale = 'large', returnclass = 'sf') # get map polygons
degBuffer <- 0.5 # axis scale buffer 

theme_map <- function() { # ggplot map theme 
  theme_bw() +
    theme(panel.background = element_rect(fill = 'white', colour = 'black', size = 1.25),
          axis.text = element_text(colour = 'black'),
          plot.title = element_text(colour = 'black', face = 'bold')) #+
  
}

# map
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(min(locs$longitud, na.rm = T) - degBuffer, max(locs$longitud, na.rm = T) + degBuffer),
           ylim = c(min(locs$latitude, na.rm = T) - degBuffer, max(locs$latitude, na.rm = T) + degBuffer),
           expand = F) +
  geom_path(data = locs, aes(x = as.numeric(as.character(longitud)),
                             y = as.numeric(as.character(latitude))),
            alpha = 0.4, color = 'deepskyblue4') +
  geom_point(data = locs, aes(x = as.numeric(as.character(longitud)),
                              y = as.numeric(as.character(latitude))),
             shape = 21, alpha = 0.4, fill = 'deepskyblue4') +
  theme_map() +
  xlab("") +
  ylab("") +
  annotation_scale(location = 'bl') +
  annotation_north_arrow(location = 'tr', which_north = 'true',
                         style = north_arrow_fancy_orienteering())

           
## project locations for input into crawl (needs to be in meters)
locs_sf <- st_as_sf(locs, coords = c("longitud", "latitude"), crs = 4326) %>%
  st_transform(crs = 32604)
st_crs(locs_sf) # check CRS

## remove deploymenet location 'DP'. Could set error ellipse info = 0 here, but model uses
## all error ellipse info to estimate standard errors around state estimates, so would 
## bias model outputs. 
locs_sf <- filter(locs_sf, LC != "DP")

## create error model covariance matrix using error ellipse info
locs_sf <- crawl::argosDiag2Cov(
  Major = locs_sf$semi_major,
  Minor = locs_sf$semi_minor,
  Orientation = locs_sf$ellipse_orient
) %>% bind_cols(locs_sf, .)

## wrapper function to fit crawl model
fit_crawl <- function(d, fixPar) {
  
  fit <- crawl::crwMLE(
    mov.model = ~ 1, # the process model (correlated random walk) 
    err.model = list( # error model, using info from error covariance matrix from earlier
      x = ~ ln.sd.x + 0,
      y = ~ ln.sd.y + 0,
      rho = ~ error.corr
    ),
    fixPar = fixPar, # fixed parameters (see below)
    data = d,
    Time.name = 'date',
    attempts = 50, # number of times likelihood optimization will be attempted 
    initialSANN = list( # use simulated annealing to obtain starting values (estimating manually can be challenging)
      maxit = 500, trace = 0,
      REPORT = 1)
    )
}

## Specify parameter values and fit model
fixPar = c(1,1,NA,NA) # first 2 parameters estimated by error model, sigma and beta (last 2) are set to NA to be estimated
# If using LC classes for error model, could specify fixed parameters as such (for each LC class)
# fixPar = c(log(250), log(500), log(1500), rep(NA,3), rep(NA,2)), where the log(#) represent expected error associated
# with each class (NA for L0-LB) and last two parameters, sigma, beta, same as before. 
set.seed(123)
fit1 <- fit_crawl(locs_sf, fixPar = fixPar)
fit1 # check parameter estimates 

## Predict locations from fitted model: specify time interval (e.g., hours) or provide a 
## vector of prediction times (e.g., POSIXct values). Here we'll use a 4 hour time interval.
pred1 <- crwPredict(fit1, predTime = "4 hours", return.type = 'flat') # this return type provides speed estimates
pred1_p <- filter(pred1, locType == "p")

## quick plots:
crwPredictPlot(pred1, plotType = 'll') # quick plot of state estimates and confidence intervals
crwPredictPlot(pred1, plotType = 'map') 

## visualize
world_proj <- st_transform(world, crs = 32604) # make world polygons object with same projection as crawl locations

# map
ggplot(data = world_proj) +
  geom_sf() +
  geom_path(data = pred1, aes(x = as.numeric(as.character(mu.x)),
                             y = as.numeric(as.character(mu.y)), color = locType),
            alpha = 0.4) +
  geom_point(data = pred1, aes(x = as.numeric(as.character(mu.x)),
                               y = as.numeric(as.character(mu.y)), fill = locType), shape = 21, alpha = 0.4) +
  coord_sf(xlim = c(min(pred1$mu.x, na.rm = T) - 10000, max(pred1$mu.x, na.rm = T) + 10000),
           ylim = c(min(pred1$mu.y, na.rm = T) - 10000, max(pred1$mu.y, na.rm = T) + 10000),
           expand = F) +
  theme_map() +
  xlab("") +
  ylab("") +
  annotation_scale(location = 'bl') +
  annotation_north_arrow(location = 'tr', which_north = 'true',
                         style = north_arrow_fancy_orienteering())

## get the lat/lon coordinates of the data and save output file. 
pts_sf <- crw_as_sf(pred1, "POINT") # make sf object using crawl helper function 

get_coords <- function(p, d, prj) {

  p_coords <- do.call(rbind, st_geometry(p$geometry)) %>%
    as_tibble() %>% setNames(c("longitud","latitude"))
  p_coords_df <- data.frame(x = p_coords$longitud, y = p_coords$latitude)
  
  inv_proj <- proj4::project(p_coords_df, prj, inverse = T)
  inv_proj_df <- data.frame(latitude = inv_proj$y, longitud = inv_proj$x)
  
  df <- as.data.frame(d)
  f <- cbind(df, inv_proj_df)
  
  return(f)
  
}

## apply function
final <- get_coords(p = pts_sf, d = pred1, prj = "+proj=utm +zone=4 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

## save csv
write.csv(final, "SSM/SbTag015_crawl_4hTimeStep.csv", row.names = F)
