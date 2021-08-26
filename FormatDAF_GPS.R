## Format Douglas Argos filtered + FastGPS speed filtered output from
## Movebank for later analyses

## This script is set up to format individual tags

## Author: Michaela A. Kratofil, Cascadia Research
## Updated: 26 Aug 2021

## ========================================================================== ##

## Load packages ##
library(dplyr)
library(lubridate)

## deploy info to add to start of track
dateDeployed <- as.POSIXct("2021-08-11 22:03:00", tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
latDeployed <- 22.1403
lonDeployed <- -159.9301
animalId <- "MdTag020"

## read in data
data <- read.csv("Douglas Filtered/Md/MdTag020_DouglasFiltered_KF_FastGPS_Filtered_test.csv")

## rename columns 
# Animal
colnames(data)[colnames(data)== "individual.local.identifier"] <- "animal"

# PTTID
colnames(data)[colnames(data)== "tag.local.identifier"] <- "ptt"

# format datetime
data$date <- as.POSIXct(data$timestamp, tz = "UTC")

# Latitude 
colnames(data)[colnames(data)=="location.lat"] <- "latitude"

# Longitude 
colnames(data)[colnames(data)=="location.long"] <- "longitud"

# Error radius
colnames(data)[colnames(data) == "argos.error.radius"] <- "error_radius"

# Error ellipse orientation
colnames(data)[colnames(data) == "argos.orientation"] <- "ellipse_orient"

# Error ellipse semi major axis
colnames(data)[colnames(data) == "argos.semi.major"] <- "semi_major"

# Error ellipse semi minor axis
colnames(data)[colnames(data) == "argos.semi.minor"] <- "semi_minor"

# location class
colnames(data)[colnames(data) == "argos.lc"] <- "LC"
data$LC <- paste("L", data$LC, sep = "")

# gps satellite count
colnames(data)[colnames(data) == "gps.satellite.count"] <- "sat_count"

# select columns 
df <- select(data, animal, ptt, date, longitud, latitude, LC, error_radius, ellipse_orient, semi_major, semi_minor,
             sat_count)

# add row with deployment information
df2 <- base::rbind(data.frame(animal = "MdTag020",
                              ptt = 180168,
                              date = dateDeployed,
                              longitud = lonDeployed,
                              latitude = latDeployed,
                              LC = "DP", #df)
                              error_radius = NA,
                              ellipse_orient = NA,
                              semi_major = NA,
                              semi_minor = NA,
                              sat_count = NA), df)

## arrange by date 
df3 <- df2 %>%
  arrange(date)

## make LC = GPS for GPS location classes
df3 <- df3 %>%
  mutate(
    LC = ifelse(LC == "L", "G", LC)
  )


# Save as .csv file 
write.csv(df3, "Douglas Filtered/MdTag020_DouglasFiltered_KF_r10d3lc2_GPSFiltered_r3e1000_Formatted.csv", row.names = F)

# check lc 
sum <- df2 %>%
  group_by(LC) %>%
  tally()

## write kml of merged file 
library(sf)

# pts
df3 %>%
  st_as_sf(., coords = c("longitud","latitude"), crs = 4326) %>%
  st_write(., "KMZ/MdTags/MdTag020_DouglasFiltered_KF_r10d3lc2_GPSFiltered_r3e1000_Formatted.kml", driver = "kml")

# lines
df3 %>%
  st_as_sf(., coords = c("longitud","latitude"), crs = 4326) %>%
  summarise(do_union = F) %>%
  st_cast("LINESTRING") %>%
  st_write(., "KMZ/MdTags/MdTag020_DouglasFiltered_KF_r10d3lc2_GPSFiltered_r3e1000_Formatted_lines.kml", driver = "kml")
