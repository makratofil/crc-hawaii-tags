## Drone_DistBearing.R: Determine bearings and distances between points of 
## drone track, write KMLs for each segment.

## Author: Michaela A. Kratofil, Cascadia Research
## Updated: 16 Feb 2021

## ========================================================================= ##

## load packages 
library(dplyr)
library(tidyr)
library(lubridate)
library(sf)
library(geosphere)

## read in drone flight data and review
locs <- read.csv("Kogiasima analyses - 2020/Kogiasima_DroneFlights_Nov2019.csv", header = T)
summary(locs)
str(locs)

## calculate distance and bearing between points
locs$dist <- NA
locs$bearing <- NA

for (i in 1:nrow(locs)) {
  
  lat1 <- locs[i, "latitude"]
  lon1 <- locs[i, "longitude"]
  lat2 <- locs[i + 1, "latitude"]
  lon2 <- locs[i + 1, "longitude"]
  
  p1 = st_point(c(lon1, lat1)) %>%
    st_sfc() %>%
    st_sf(crs = "+init=EPSG:4326") %>%
    st_transform(crs = 3750)
  
  p2 = st_point(c(lon2, lat2)) %>%
    st_sfc() %>%
    st_sf(crs = "+init=EPSG:4326") %>%
    st_transform(crs = 3750)
  
  locs[i, "dist"] <- st_distance(p1, p2)
  locs[i, "bearing"] <- bearing(c(lon1,lat1), c(lon2,lat2))
}

## for a particular segment:
seg <- filter(locs, segment == "S4")
lat1 <- first(seg$latitude)
lon1 <- first(seg$longitude)
lat2 <- last(seg$latitude)
lon2 <- last(seg$longitude)

p1 = st_point(c(lon1, lat1)) %>%
  st_sfc() %>%
  st_sf(crs = "+init=EPSG:4326") %>%
  st_transform(crs = 3750)

p2 = st_point(c(lon2, lat2)) %>%
  st_sfc() %>%
  st_sf(crs = "+init=EPSG:4326") %>%
  st_transform(crs = 3750)

st_distance(p1,p2)
bearing(c(lon1,lat1), c(lon2,lat2))

## save populated file
write.csv(locs, "Kogiasima analyses - 2020/Kogiasima_DroneFlight_Nov2019_DistBearing.csv", row.names = F)
## ================================================================================== ##

## write kmls 
sf_locs <- st_as_sf(locs, coords = c("longitude","latitude"), crs = 4326)
st_write(sf_locs, "Kogiasima analyses - 2020/Kogiasima_DroneFlight_Nov2019.kml", driver = "kml")

sf_lines <- sf_locs %>%
  summarise(do_union = F) %>%
  st_cast("LINESTRING")
st_write(sf_lines, "Kogiasima analyses - 2020/Kogiasima_DroneFlight_Nov2019_lines.kml", driver = "kml")

## for each segment
seg1 <- filter(sf_locs, segment == "S1")
st_write(seg1, "Kogiasima analyses - 2020/Kogiasima_DroneFlight_Nov2019_Seg1.kml", driver = "kml")

seg1_lines <- seg1 %>%
  summarise(do_union = F) %>%
  st_cast("LINESTRING")
st_write(seg1_lines, "Kogiasima analyses - 2020/Kogiasima_DroneFlight_Nov2019_Seg1Lines.kml", driver = "kml")

seg2 <- filter(sf_locs, segment == "S2")
st_write(seg2, "Kogiasima analyses - 2020/Kogiasima_DroneFlight_Nov2019_Seg2.kml", driver = "kml")

seg2_lines <- seg2 %>%
  summarise(do_union = F) %>%
  st_cast("LINESTRING")
st_write(seg2_lines, "Kogiasima analyses - 2020/Kogiasima_DroneFlight_Nov2019_Seg2Lines.kml", driver = "kml")

seg3 <- filter(sf_locs, segment == "S3")
st_write(seg3, "Kogiasima analyses - 2020/Kogiasima_DroneFlight_Nov2019_Seg3.kml", driver = "kml")

seg3_lines <- seg3 %>%
  summarise(do_union = F) %>%
  st_cast("LINESTRING")
st_write(seg3_lines, "Kogiasima analyses - 2020/Kogiasima_DroneFlight_Nov2019_Seg3Lines.kml", driver = "kml")

seg4 <- filter(sf_locs, segment == "S4")
st_write(seg4, "Kogiasima analyses - 2020/Kogiasima_DroneFlight_Nov2019_Seg4.kml", driver = "kml")

seg4_lines <- seg4 %>%
  summarise(do_union = F) %>%
  st_cast("LINESTRING")
st_write(seg4_lines, "Kogiasima analyses - 2020/Kogiasima_DroneFlight_Nov2019_Seg4Lines.kml", driver = "kml")
