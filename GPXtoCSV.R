## GPXtoCSV.R: Read-in GPX file from research vessel and extract data, save CSV

## Author: Michaela A. Kratofil
## Updated: 16 Feb 2021

## ========================================================================== ##

## There are several ways to do this in R, but this script was based off of this
## example I found on google: rpubs.com/ials2un/gpx1

## load packages
library(lubridate)
library(sf) 
library(XML)
#library(sp)
library(stringr)
library(dplyr)
library(plotKML) # alternative way of reading in GPX file

# create function to help parse GPX file (shifts vectors)
shift.vec <- function (vec, shift) {
  if(length(vec) <= abs(shift)) {
    rep(NA ,length(vec))
  }else{
    if (shift >= 0) {
      c(rep(NA, shift), vec[1:(length(vec)-shift)]) }
    else {
      c(vec[(abs(shift)+1):length(vec)], rep(NA, abs(shift))) } } }

# parse the GPX file
pfile <- htmlTreeParse(file = "Field data/Track-Mathews_200713-200716.gpx", error = function(...) {}, useInternalNodes = T)

# get coordinates and times (other variables can be extracted too)
coords <- xpathSApply(pfile, "//trkpt", xmlAttrs)
times <- xpathSApply(pfile, path = "//trkpt/time", xmlValue)

# extract latitude and longitude from coordinates
lats <- as.numeric(coords["lat",])
lons <- as.numeric(coords["lon",])

# put everything in a dataframe and get rid of old variables
df <- data.frame(lat = lats, lon = lons, time = times)

# adjust the time column
df$time_adj <- str_replace_all(df$time, "[TZ]", " ")

## write KML to check
df %>%
  st_as_sf(., coords = c('lon','lat'), crs = 4326) %>%
  st_write(., "Field data/Track-Mathews_200713-200716.kml", driver = "kml")

## write.csv
write.csv(df, "Directory_folder/file_name.csv", row.names = F)

## ============================================================================ ##
## Another way to read in and extract data from GPX file is using the plotKML pckge
## see package documentation for more details.

# read data
gpx <- readGPX("Field data/Track-Mathews_200713-200716.gpx", waypoints = T)

# get waypoints and make data frame
wpdf <- gpx$waypoints
wpdf$time_adj <- str_replace_all(wpdf$time, "[TZ]", " ")
wpdf$DateTimeUTC <- as.POSIXct(wpdf$time_adj, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
wpdf$DateTimeHST <- with_tz(wpdf$DateTimeUTC, tzone = "Pacific/Honolulu")

# save data
write.csv(wpdf, "Directory_folder/file_name_waypoints.csv", row.names = F)