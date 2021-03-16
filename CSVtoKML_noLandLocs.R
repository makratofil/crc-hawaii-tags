## CSVtoKML_noLandLocs.R: Generate KML files for individual tags from dataframes/CSVs,
## with land locations removed.

## Author: Michaela A. Kratofil, Cascadia Research
## Updated: 16 Mar 2021

## ================================================================================== ##

## load packages
library(dplyr)
library(sf)

## read in GIS processed location data 
df <- read.csv("GIS output/PcTag001-073_DF_r20d3lc2_ArgosGPS_GIS_2021FEBv1.csv")
str(df)
df$date <- as.POSIXct(df$date, tz = "UTC")
df$animal <- as.factor(df$animal)

## remove all land locations
df_sub <- filter(df, DistToShore != 0) %>%
  select(animal, ptt, date, longitud, latitude, LC, error_radius, ellipse_orient,
         semi_major, semi_minor, LocType)

## if doing a single animal:
tag = "PcTag001"
id <- filter(df_sub, animal == tag) # subset
id_sf <- st_as_sf(id, coords = c("longitud","latitude"), crs = 4326) # project coordinates

# points
id_sf %>% select(Name = date) %>% # use date as the label, and write KML
  st_write(., paste0("KMZ/PcTag001-073_DouglasFiltered_noLandLocs/", tag, "_pts.kml"), driver = "kml")

# lines
lines_sf <- id_sf %>% summarise(do_union = F) %>% st_cast("LINESTRING") %>%
  st_write(., paste0("KMZ/PcTag004-073_DouglasFiltered_noLandLocs/", tag, "_lines.kml"), driver = "kml" )

## if doing a batch of animals:
## write kmls of POINTS and LINESTRINGS (tracklines) for each animal 
for (i in levels(df_sub$animal)) {
  
  id <- filter(df_sub, animal == i)
  
  id_sf <- st_as_sf(id, coords = c("longitud","latitude"), crs = 4326) 
  
  id_sf %>% select(Name = date) %>%
    st_write(., paste0("KMZ/PcTag004-073_DouglasFiltered_noLandLocs/", i, "_pts.kml"), driver = "kml")
  
  lines_sf <- id_sf %>% summarise(do_union = F) %>% st_cast("LINESTRING") %>%
    st_write(., paste0("KMZ/PcTag004-073_DouglasFiltered_noLandLocs/", i, "_lines.kml"), driver = "kml" )
  
}

## In Google Earth: 
# Group points and lines together in a folder for each unique tag, save KMZ file.
# Add new KMZ files to the batch/compiled file for all tags of that spp, save KMZ batch file. 


