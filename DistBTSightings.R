## Calculate distance between all possible pairs of sightings

## Author: Michaela A. Kratofil, Cascadia Research Collective
## Updated: 16 Feb 2021

## ========================================================================= ##

## This script calculates the distance between all possible pairs of sightings.
## The example in this script uses Kogia sima sightings data (analysis included
## in paper submitted 2021), and includes this analysis applied to various subsets
## of the data. 

## load package libraries 
library(tidyverse)
library(lubridate)
library(gtools)
library(raster)
library(fasterize)
library(ptolemy) # install from github (see jmlondon github page)
library(purrr)
library(sf)
library(sp)
library(rgeos)

## read in sightings data from Kogia catalog; questionable DJM locs removed beforehand
locs <- read.csv("Kogiasima analyses - 2020/Kogiasima_CatalogSightings_forDistBtSight_v3.csv", header = T)

## review
str(locs)
length(unique(locs$ID)) # 174 unique IDs, including stranded 
summary(locs$latitude)
summary(locs$longitude)

#### Distance between sightings by INDIVIDUAL (resightings) ####
## subset IDs that have been resighted
sub <- filter(locs, times_seen > 1)
length(unique(sub$ID)) # 35 individuals that have been resighted 

## remove records with no location (lat/lon)
sub <- filter(sub, !is.na(latitude)) 
length(unique(sub$ID)) # 34 individuals

## make ID into factor and summarize
sub$ID <- as.factor(sub$ID) # make a factor
summary(sub$ID) # IDs with only one location: HIKs053, HIKs065, HIKs128
sub.ID <- filter(sub, ID != "HIKs053") %>%
  filter(ID != "HIKs065") %>%
  filter(ID != "HIKs128")
length(unique(sub.ID$ID)) # 31 individuals resighted, with more than one location available for calculations

## create column for each sighting # by animal
sub.seq <- sub.ID %>%
  group_by(ID) %>%
  mutate(sight_n = row_number())
sub.seq$sight_n <- sub("^", "S", sub.seq$sight_n) # add 'S' in front of it

## create nested tbl object
sub.tbl <- sub.seq %>%
  group_by(ID) %>%
  nest()

## create combination matrix for each animal. use combinations rather than permutations, as the resulting permutation matrix
## will include all possible combinations INCLUDING replicates. For example, in the permutation matrix, both pairs 3-5 and 5-3
## will be present. However, the distance between these two points will be the same, and therefore would be double-counted.
combos <- function(x) {
  n = length(x$sight_n)
  c = combinations(n = n, r = 2, v = x$sight_n, repeats.allowed = F)
  f = as.data.frame(c)
  return(f)
}

## apply the function across all individuals
sub.tbl <- sub.tbl %>%
  mutate(
    pairs = purrr::map(data, ~ combos(x = .x))
  )

## get polygon of islands from ptolemy package
sf_locs <- st_as_sf(sub.seq, coords = c('longitude','latitude'), crs = 4326) %>%
  st_transform(crs = 3750)
land <- ptolemy::extract_gshhg(sf_locs, buffer = 20000, epsg = 3750)
plot(land) # check

## rasterize the polygons
r <- raster(xmn = 800200, xmx = 845918, ymn = 2100000, ymx = 2240000, nrows = 75, ncols = 75)
rland <- fasterize(summarize(land), r)
plot(rland) # check

## now calculate the distance between each set of points
dists <- function(c, d) {
  
  # create columns for lat/lons and distances
  c$lat1 <- NA
  c$lon1 <- NA
  c$lat2 <- NA
  c$lon2 <- NA
  c$dist_m_uc <- NA
  
  for (i in 1:nrow(c)) {
    cat("\rRow ", i, " of ", nrow(c))
    V1 = c[i, "V1"]
    V2 = c[i, "V2"]
    cat("a")
    v1 = filter(d, sight_n == V1)
    v2 = filter(d, sight_n == V2)
    
    cat("\bb")
    c[i, "lat1"] <- v1$latitude
    c[i, "lon1"] <- v1$longitude
    c[i, "lat2"] <- v2$latitude
    c[i, "lon2"] <- v2$longitude
    
    cat("\bc")
    lat1 = c[i, "lat1"]
    lon1 = c[i, "lon1"]
    lat2 = c[i, "lat2"]
    lon2 = c[i, "lon2"]
    
    cat("\bd")
    p1 = st_point(c(lon1, lat1)) %>%
      st_sfc() %>%
      st_sf(crs = "+init=EPSG:4326") %>%
      st_transform(crs = 3750)
    
    cat("\be")
    p2 = st_point(c(lon2, lat2)) %>%
      st_sfc() %>%
      st_sf(crs = "+init=EPSG:4326") %>%
      st_transform(crs = 3750)
    
    cat("\bf")
    # calculate the distance b/t points using the Great Circle Distance according to the Vincenty Ellipsoid method
    #cdf[i, "dist_m"] <- distVincentyEllipsoid(p1 = p1, p2 = p2)
    c[i, "dist_m_uc"] <- st_distance(p1, p2)
  }
  return(c)
}

## apply function across all individuals 
sub.tbl <- sub.tbl %>%
  mutate(
    dist = purrr::map2(pairs, data, ~ dists(.x, .y))
  )

## unnest the data and save 
dist.df <- sub.tbl %>%
  unnest(dist) %>%
  dplyr::select(ID, V1, V2, lat1, lon1, lat2, lon2, dist_m_uc)

dist.df <- bind_rows(dist.df, c)

write.csv(dist.df, "Kogiasima analyses - 2020/Kogiasima_DistBTSightings_byID_2020SEP01.csv", row.names = F)

#### Distance between sightings, all IDs, all sightings ####
## filter out any records without lat/lons
sub.locs <- filter(locs, !is.na(latitude))
sub.locs <- filter(sub.locs, sight_type != 'Final') # 191 records with locations, and not strandings 

## First do all sightings, all islands, not accounting for pseudoreplication

## create column unique to each sighting
seq.locs <- sub.locs %>%
  mutate(sight_n = row_number())
seq.locs$sight_n <- sub("^", "S", seq.locs$sight_n) # add 'S' in front of it

## create matrix of all possible combinations or pairs of sighting locations
options(expressions = 1e5)
locs.combos <- combinations(n = length(seq.locs$sight_n), r = 2, v = seq.locs$sight_n, repeats.allowed = F)
cdf <- as.data.frame(locs.combos)

## get polygon of islands from ptolemy package
sf_locs <- st_as_sf(seq.locs, coords = c('longitude','latitude'), crs = 4326) %>%
  st_transform(crs = 3750)
land <- ptolemy::extract_gshhg(sf_locs, buffer = 20000, epsg = 3750)
plot(land) # check

## rasterize the polygons
r <- raster(extent(land), nrows = 75, ncols = 75)
r <- raster(xmn = 370906.5, xmx = 846235.3, ymn = 2099244, ymx = 2480000, nrows = 75, ncols = 75)
rland <- fasterize(summarize(land), r)
plot(rland) # check

cdf$lat1 <- NA
cdf$lon1 <- NA
cdf$lat2 <- NA
cdf$lon2 <- NA
cdf$dist_m_uc <- NA
cdf$dist_m_c <- NA

## for loop to match lat/lon to each sighting pair and calculate distance between points 
## accounting for intervening land masses 
for (i in 1:nrow(cdf)) {
  cat("\rRow ", i, " of ", nrow(cdf))
  V1 = cdf[i, "V1"]
  V2 = cdf[i, "V2"]
  cat("a")
  v1 = filter(seq.locs, sight_n == V1)
  v2 = filter(seq.locs, sight_n == V2)
  
  cat("\bb")
  cdf[i, "lat1"] <- v1$latitude
  cdf[i, "lon1"] <- v1$longitude
  cdf[i, "lat2"] <- v2$latitude
  cdf[i, "lon2"] <- v2$longitude
  
  cat("\bc")
  lat1 = cdf[i, "lat1"]
  lon1 = cdf[i, "lon1"]
  lat2 = cdf[i, "lat2"]
  lon2 = cdf[i, "lon2"]
  
  cat("\bd")
  p1 = st_point(c(lon1, lat1)) %>%
    st_sfc() %>%
    st_sf(crs = "+init=EPSG:4326") %>%
    st_transform(crs = 3750)
  
  cat("\be")
  p2 = st_point(c(lon2, lat2)) %>%
    st_sfc() %>%
    st_sf(crs = "+init=EPSG:4326") %>%
    st_transform(crs = 3750)
  
  cat("\bf")
  # calculate the distance b/t points using the Great Circle Distance according to the Vincenty Ellipsoid method
  #cdf[i, "dist_m"] <- distVincentyEllipsoid(p1 = p1, p2 = p2)
  cdf[i, "dist_m_uc"] <- st_distance(p1, p2)
  
  cat("\bg")
  ## now calculate distance between points while accounting for 
  pts <- st_sfc(st_point(c(lon1, lat1)), st_point(c(lon2, lat2))) %>%
    st_sf(crs = "+init=EPSG:4326") %>%
    st_transform(crs = 3750)
  
  cat("\bh")
  rland_pts <- rland
  cat("\bi")
  xy <- st_coordinates(pts)
  cat("\bj")
  icell <- cellFromXY(rland, xy)
  cat("\bk")
  rland_pts[icell[2]] <- 2 # assign 2 point a raster value of 2
  cat("\bl")
  d <- gridDistance(rland_pts, origin = 2, omit = 1) # calculate distance to raster value 2
  cat("\bm")
  cdf[i, "dist_m_c"] <- d[icell][1]
}

write.csv(cdf, "Kogiasima analyses - 2020/Kogiasima_DistBTSightings_All_wPseudoreps_2020Sep01_woNAs.csv", row.names = F)

## all sightings, all islands, all IDs, accounting for pseudoreplication (i.e., only one location per encounter)
length(unique(sub.locs$GroupCode)) # 84 unique sightings
pseu.locs <- sub.locs %>%
  distinct(GroupCode, .keep_all = T)

## create column unique to each sighting
pseu.locs <- pseu.locs %>%
  mutate(sight_n = row_number())
pseu.locs$sight_n <- sub("^", "S", pseu.locs$sight_n) # add 'S' in front of it

## create matrix of all possible combinations or pairs of sighting locations
options(expressions = 1e5)
pseu.combos <- combinations(n = length(pseu.locs$sight_n), r = 2, v = pseu.locs$sight_n, repeats.allowed = F)
pdf <- as.data.frame(pseu.combos)

## get polygon of islands from ptolemy package
sf_locs <- st_as_sf(pseu.locs, coords = c('longitude','latitude'), crs = 4326) %>%
  st_transform(crs = 3750)
land <- ptolemy::extract_gshhg(sf_locs, buffer = 20000, epsg = 3750)
plot(land) # check

## rasterize the polygons
r <- raster(extent(land), nrows = 75, ncols = 75)
r <- raster(xmn = 370906.5, xmx = 846235.3, ymn = 2099244, ymx = 2480000, nrows = 75, ncols = 75)
rland <- fasterize(summarize(land), r)
plot(rland) # check

pdf$lat1 <- NA
pdf$lon1 <- NA
pdf$lat2 <- NA
pdf$lon2 <- NA
pdf$dist_m_uc <- NA
pdf$dist_m_c <- NA

## for loop to match lat/lon to each sighting pair and calculate distance between points 
## accounting for intervening land masses 
for (i in 1:nrow(pdf)) {
  cat("\rRow ", i, " of ", nrow(pdf))
  V1 = pdf[i, "V1"]
  V2 = pdf[i, "V2"]
  cat("a")
  v1 = filter(pseu.locs, sight_n == V1)
  v2 = filter(pseu.locs, sight_n == V2)
  
  cat("\bb")
  pdf[i, "lat1"] <- v1$latitude
  pdf[i, "lon1"] <- v1$longitude
  pdf[i, "lat2"] <- v2$latitude
  pdf[i, "lon2"] <- v2$longitude
  
  cat("\bc")
  lat1 = pdf[i, "lat1"]
  lon1 = pdf[i, "lon1"]
  lat2 = pdf[i, "lat2"]
  lon2 = pdf[i, "lon2"]
  
  cat("\bd")
  p1 = st_point(c(lon1, lat1)) %>%
    st_sfc() %>%
    st_sf(crs = "+init=EPSG:4326") %>%
    st_transform(crs = 3750)
  
  cat("\be")
  p2 = st_point(c(lon2, lat2)) %>%
    st_sfc() %>%
    st_sf(crs = "+init=EPSG:4326") %>%
    st_transform(crs = 3750)
  
  cat("\bf")
  # calculate the distance b/t points using the Great Circle Distance according to the Vincenty Ellipsoid method
  #cdf[i, "dist_m"] <- distVincentyEllipsoid(p1 = p1, p2 = p2)
  pdf[i, "dist_m_uc"] <- st_distance(p1, p2)
  
  cat("\bg")
  ## now calculate distance between points while accounting for 
  pts <- st_sfc(st_point(c(lon1, lat1)), st_point(c(lon2, lat2))) %>%
    st_sf(crs = "+init=EPSG:4326") %>%
    st_transform(crs = 3750)
  
  cat("\bh")
  rland_pts <- rland
  cat("\bi")
  xy <- st_coordinates(pts)
  cat("\bj")
  icell <- cellFromXY(rland, xy)
  cat("\bk")
  rland_pts[icell[2]] <- 2 # assign 2 point a raster value of 2
  cat("\bl")
  d <- gridDistance(rland_pts, origin = 2, omit = 1) # calculate distance to raster value 2
  cat("\bm")
  pdf[i, "dist_m_c"] <- d[icell][1]
}

write.csv(pdf, "Kogiasima analyses - 2020/Kogiasima_DistBTSightings_All_noPseudoreps_2020Sep01.csv", row.names = F)


## By island, not accounting for pseudoreplication
locs %>% group_by(Island) %>% tally() # only 1 sighting in Niihau and 1 sighting in Maui, so remove those records
# isl.sub <- filter(locs, Island != 'Niihau') %>%
#   filter(Island != 'Maui') %>%
#   filter(sight_type != 'final') %>%
#   filter(!is.na(latitude))

isl.sub <- filter(pseu.locs, Island != 'Niihau') %>%
  filter(Island != 'Maui') 
  
isl.seq <- isl.sub %>%
  group_by(Island) %>%
  mutate(sight_n = row_number())
isl.seq$sight_n <- sub("^", "S", isl.seq$sight_n) # add 'S' in front of it

sf_locs <- st_as_sf(isl.seq, coords = c("longitude","latitude"), crs = 4326) %>%
  st_transform(crs = 3750)
land <- ptolemy::extract_gshhg(sf_locs, buffer = 20000, epsg = 3750)
plot(land) # check

## rasterize the polygons
r <- raster(extent(land), nrows = 75, ncols = 75)
r <- raster(xmn = 370906.5, xmx = 846235.3, ymn = 2099244, ymx = 2480000, nrows = 75, ncols = 75)
rland <- fasterize(summarize(land), r)
plot(rland) # check

## Do Hawaii first 
h <- filter(isl.seq, Island == "Hawaii")
## create matrix of all possible combinations or pairs of sighting locations
options(expressions = 1e5)
h.combos <- combinations(n = length(h$sight_n), r = 2, v = h$sight_n, repeats.allowed = F)
pdf <- as.data.frame(h.combos)

pdf$lat1 <- NA
pdf$lon1 <- NA
pdf$lat2 <- NA
pdf$lon2 <- NA
pdf$dist_m_uc <- NA
pdf$dist_m_c <- NA

## for loop to match lat/lon to each sighting pair and calculate distance between points 
## accounting for intervening land masses 
for (i in 1:nrow(pdf)) {
  cat("\rRow ", i, " of ", nrow(pdf))
  V1 = pdf[i, "V1"]
  V2 = pdf[i, "V2"]
  cat("a")
  v1 = filter(h, sight_n == V1)
  v2 = filter(h, sight_n == V2)
  
  cat("\bb")
  pdf[i, "lat1"] <- v1$latitude
  pdf[i, "lon1"] <- v1$longitude
  pdf[i, "lat2"] <- v2$latitude
  pdf[i, "lon2"] <- v2$longitude
  
  cat("\bc")
  lat1 = pdf[i, "lat1"]
  lon1 = pdf[i, "lon1"]
  lat2 = pdf[i, "lat2"]
  lon2 = pdf[i, "lon2"]
  
  cat("\bd")
  p1 = st_point(c(lon1, lat1)) %>%
    st_sfc() %>%
    st_sf(crs = "+init=EPSG:4326") %>%
    st_transform(crs = 3750)
  
  cat("\be")
  p2 = st_point(c(lon2, lat2)) %>%
    st_sfc() %>%
    st_sf(crs = "+init=EPSG:4326") %>%
    st_transform(crs = 3750)
  
  cat("\bf")
  # calculate the distance b/t points using the Great Circle Distance according to the Vincenty Ellipsoid method
  #cdf[i, "dist_m"] <- distVincentyEllipsoid(p1 = p1, p2 = p2)
  pdf[i, "dist_m_uc"] <- st_distance(p1, p2)
  
  cat("\bg")
  ## now calculate distance between points while accounting for 
  pts <- st_sfc(st_point(c(lon1, lat1)), st_point(c(lon2, lat2))) %>%
    st_sf(crs = "+init=EPSG:4326") %>%
    st_transform(crs = 3750)
  
  cat("\bh")
  rland_pts <- rland
  cat("\bi")
  xy <- st_coordinates(pts)
  cat("\bj")
  icell <- cellFromXY(rland, xy)
  cat("\bk")
  rland_pts[icell[2]] <- 2 # assign 2 point a raster value of 2
  cat("\bl")
  d <- gridDistance(rland_pts, origin = 2, omit = 1) # calculate distance to raster value 2
  cat("\bm")
  pdf[i, "dist_m_c"] <- d[icell][1]
}

write.csv(pdf, "Effort and sightings/Kogiasima_DistBTSightings_Hawaii_noPseudoreps_2020Sep01.csv", row.names = F)

