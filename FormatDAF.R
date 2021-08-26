## Format Douglas Argos filtered output from Movebank for rest of analyses

## This script is set up to format individual tags

## Author: Michaela A. Kratofil, Cascadia Research
## Updated: 26 Aug 2021

## ========================================================================== ##

## Load packages ##
library(dplyr)
library(lubridate)

## deploy info to add to start of track
dateDeployed <- as.POSIXct("2021-05-07 19:48:00", tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
latDeployed <- 19.44158
lonDeployed <- -155.9445
animalId <- ("TtTag038")

## read in data
data <- read.csv("Douglas Filtered/Tt/TtTag002_DouglasFiltered_KS_r20d3lc2.csv")

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

# select columns 
df <- select(data, animal, ptt, date, longitud, latitude, LC, error_radius, ellipse_orient, semi_major, semi_minor)

# add row with deployment information
df2 <- base::rbind(data.frame(animal = "TtTag038",
                              ptt = 206182,
                              date = dateDeployed,
                              longitud = lonDeployed,
                              latitude = latDeployed,
                              LC = "DP", #df)
                              error_radius = NA,
                              ellipse_orient = NA,
                              semi_major = NA,
                              semi_minor = NA), df)


# Save as .csv file 
write.csv(df2, "Douglas Filtered/TtTag038_DouglasFiltered_KF_r20d3lc2_Formatted.csv", row.names = F)




