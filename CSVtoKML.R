## CSVtoKML.R: Convert csv file/data to KML/KMZ in R

## Author: Michaela A. Kratofil, Cascadia Research
## Updated: 16 Feb 2021

## ========================================================================= ##

## This script shows an example of writing data from a CSV file to a KML file to
## view in Google Earth. There are several ways to do this in R, but this is the 
## way I usually do this. 

## load packages
library(dplyr)
library(lubridate)
library(sf)

## read in location data
gm <- read.csv("Douglas Filtered/GmTag004-231_DouglasFiltered_ArgosOnly_r15d3lc2_2020OCTv4.csv")
summary(gm)
str(gm)
gm$date <- as.POSIXct(gm$date, tz = "UTC")

## subset one animal/tag for example
gm083 <- dplyr::filter(gm, animal == "GmTag083")

## project location data, use crs = 4326 for anything going to Google Earth.
## make sure lon comes before lat in the coords = "" argument
gm083_sf <- st_as_sf(gm083, coords = c("longitud","latitude"), crs = 4326) # single points

## make trackline
gm083_track <- gm083_sf %>%
  summarise(do_union = F) %>%
  st_cast("LINESTRING") # cast to different spatial data type (point to linestring)

## write KML
st_write(gm083_sf, "Directory folder/file_name_points.kml", driver = "kml")
st_write(gm083_track, "Directory folder/file_name_track.kml", driver = "kml")

