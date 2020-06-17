######################################################################################
#
#                    HYCOM Eddy vector plots & animations
#
#
#                         Author: Michaela A Kratofil
#                               16 JUN 2020
#
######################################################################################

## DESCRIPTION ##

# This script extracts sea surface height (ssh) and seawater or current velocity data
# (separate vectors, u and v) from an ERDDAP server for a given area and generates daily
# plots with a provided satellite tag track. 

# Before running the script, identify the appropriate datasets on an ERDDAP server
# (e.g., PacIOOS/UofHawaii, default Upwell PFEG) making sure the temporal and spatial
# resolution of the datasets fits those of the tag trackline you're plotting for. 
# - the metadata for the datasets will include information on these

# The first part of the script will extract SSH and current vector variables for a 
# specified spatial area using the package 'rerddapXtracto'. This package can access
# any ERDDAP server given a base URL and dataset name. Here I use Global HYCOM model
# datasets, although in many instances regional model datasets are avaiable. Currently
# don't have code set up to save .csv files of extracted datasets, but can be done so. 

# The extracted variables are then manipulated for use in ggplot and input for creating
# the vector arrows. The package 'metR' is used to generate the vector arrows from the 
# u (eastward) and v (northward) current velocity vectors. 

# At the end of the script, there is an option to generate an animation of series of 
# eddy vector plots using the pacakge 'magick'. Either a .gif or .mp4 file can be saved.

## load packages
library(tidyverse)
library(lubridate)
library(rerddapXtracto)
library(plotdap)
library(ggplot2)
library(mgcv)
library(progress)
library(metR)
library(cmocean)
library(RColorBrewer)
require("rerddap")
require("rerddapXtracto")
require("ggplot2")

# read in douglas filtered batch tag file and subset out tag of interest 
df <- read.csv('Douglas Filtered/GgTag013_DouglasFiltered_KS_r20d3lc2_2020APRv1.csv')

# subset tag
tag <- filter(df, animal == "GgTag013")

# review data
str(tag)
summary(tag)

# format date time column
tag$date <- as.POSIXct(tag$date, tz = "UTC")

# add column for date in yyyy-mm-dd
tag$Date <- date(tag$date)

# remove land locations (if needed)
#noland <- gm225[c(1:39, 42:84, 86:450, 453:478, 481:495, 497, 500:645, 648:703),]                                                                                       ###

# specify range of dates of tag deployment to extract data for
dates <- seq(ymd('2015-04-21'),ymd('2015-05-05'),by='1 day')

## Empty list to hold suv for each day
allData <- list()

## Constants
xpos <- c(-158, -156) # longitudinal boundaries of plot
ypos <- c(19, 21) # latitudinal boundaries of plot
zpos <- c(0., 0.) # depth or altitude boudary for current vector dataset (not needed for SSH dataset)

# make objects for SSH and current vector data
# need to specify the dataset ID and base URL of the ERDDAP server
sshInfo <- info('hawaii_soest_ad7b_021a_08e2', url = 'https://apdrc.soest.hawaii.edu/erddap/') # SSH data
currInfo <- info('hawaii_soest_cca9_a73c_ef70', url = 'https://apdrc.soest.hawaii.edu/erddap/') # current data
uInfo <- info('hawaii_soest_114b_16e8_26f6', url = 'https://apdrc.soest.hawaii.edu/erddap/')
vInfo <- info('hawaii_soest_e97d_9053_f8f8', url = 'https://apdrc.soest.hawaii.edu/erddap/')

# check variable names, dimensions and units
sshInfo
currInfo
uInfo
vInfo


## Add a progress bar with ETA
pb <- progress_bar$new(
  format = "  Computing Estimates [:bar] :percent  \t ETA: :eta \t Elapsed: :elapsed",
  total = length(dates), clear = FALSE, width= 100)

## loop over dates
for (i in 1:length(dates)){
  
  ## Set up dates
  tpos <- c(dates[i], dates[i])
  
  ## Extract
  ssh3D <- rxtracto_3D(sshInfo, parameter = "ssh", xcoord = xpos, ycoord = ypos, tcoord = tpos)
  v3D <- rxtracto_3D(vInfo, parameter = "v", xcoord = xpos, ycoord = ypos, tcoord = tpos, zcoord = zpos, zName = "LEV")
  u3D <- rxtracto_3D(uInfo, parameter = "u", xcoord = xpos, ycoord = ypos, tcoord = tpos, zcoord = zpos, zName = "LEV")
  
  ## Set up s
  s <- as.data.frame(ssh3D$ssh)
  ## Need the variable names paired w/ lat
  ## Don't set names to lat, gets fucked
  ref <- data.frame(var = names(s),
                    lat = ssh3D$latitude)
  #names(s) <- ssh3D$latitude
  s$lon = ssh3D$longitude
  s <- s %>%
    pivot_longer(cols = -c(lon), names_to = 'var', values_to = 'ssh') %>%
    left_join(ref, by = 'var') %>%
    select(-c(var)) %>%
    select(lon, lat, ssh)
  
  ## Set up v
  v <- as.data.frame(v3D$v)
  #names(v) <- v3D$latitude
  v$lon = v3D$longitude
  v <- v %>%
    pivot_longer(cols = -c(lon), names_to = 'lat', values_to = 'northing')
  
  ## Set up v
  u <- as.data.frame(u3D$u)
  #names(u) <- u3D$latitude
  u$lon = u3D$longitude
  u <- u %>%
    pivot_longer(cols = -c(lon), names_to = 'lat', values_to = 'easting')
  
  
  ## Combined w/ lat/long
  suv <- s
  suv$northing <- v$northing
  suv$easting <- u$easting
  suv$lat <- as.numeric(suv$lat)
  suv$lon <- as.numeric(suv$lon)
  
  ## use generalized additive model to estimate grid cells with NAs using lat and lon smoothers
  nas <- filter(suv, is.na(ssh))
  suvMod <- gam(ssh ~ s(lon) + s(lat) + s(lon, lat), gamma = 20, data = suv,
                optimizer = 'outer')
  suvPredict <- predict(suvMod, nas)
  nas$pred <- suvPredict
  
  suv <- left_join(suv, select(nas, pred, lat, lon), by = c("lon", "lat")) %>%
    mutate(ssh = case_when(is.na(ssh) ~ pred,
                            TRUE ~ ssh)) %>%
    select(-c(pred))
  
  
  ## Add the date column to suv
  suv$date <- dates[i]
  
  ## Store the data.frame in a list, will bind them together later
  allData[[i]] <- suv
  
  ## update progress bar
  pb$tick()
}


### suv now has all data for each date in it
suv <- bind_rows(allData)

# set x and y scale limits
xlim = c(min(suv$lon), max(suv$lon))
ylim = c(min(suv$lat), max(suv$lat))

# check the range of SSH values for setting scales of SSH color bar
range(suv$ssh)

# subset tag date and data to highlight
sub <- filter(tag, Date == as.Date("2015-05-05"))
day <- filter(suv, date == as.Date("2015-05-05"))

## plot ##

# read-in Main Hawaiian Islands shapfile
coast <- rgdal::readOGR("Shapefiles", layer = "Coastline")
coast <- sf::st_as_sf(coast)# make sf object
coastr <- sf::st_transform(coast, crs = 4326)

(g <- ggplot() +
    geom_contour_fill(data = day, aes(x = lon, y = lat, z = ssh), bins = 100) +
    geom_vector(data = day, aes(x = lon, y = lat, dx = easting, dy = northing), skip.x = 2,
                skip.y = 1, preserve.dir = TRUE, colour = 'white', size = 0.5,
                show.legend = T) +
    scale_mag("Velocity (m/s)", max = 1.5) +
    geom_path(data = tag, aes(x = longitud, y = latitude), size = .75, alpha = 1, color = "grey60") +
    geom_point(data = tag, aes(x = longitud, y = latitude), color = 'grey55', fill = "grey60",
               shape = 21) +
    geom_path(data = sub, aes(x = longitud, y = latitude), size = 1, color = "black") +
    geom_point(data = sub, aes(x = longitud, y = latitude), color = 'black', fill = "black",
               shape = 21) +
    geom_sf(data = coastr, fill = 'grey80', colour = 'grey28', size = 1.25) +
    coord_sf(xlim = xlim, ylim = ylim, expand = c(0,0)) +
    annotate("rect", xmin = -156.4, xmax = -156.05, ymin = 19.05, ymax = 19.25, alpha = 0.4,
             fill = 'white') +
    annotate("text", x = -156.22, y = 19.15, label = str_wrap("GgTag013: 05 May 2015", 11)) +
    theme_bw(base_size = 12) +
    ylab("Latitude") +
    xlab("Longitude") +
    labs(x = NULL, y = NULL,
         fill = "SSH (m)") +
    scale_fill_gradientn(colours = cmocean('balance')(10), na.value = 'grey', # cmocean balance palette
                         limits = c(0.4, 1)) + 
    theme(axis.text = element_text(color = 'black'),
          rect = element_rect(size = 2),
          legend.key = element_rect(fill = 'grey')) 
)

ggsave("Eddy vector plots and animations/GgTag013/GgTag013_EddyVectorPlot_2015May05.jpg", dpi = 300)

### Create animation of plots, and write video or GIF file ###
library(magick)

# file and folder objects
TagID = tag$animal[1]
folder = "Eddy vector plots and animations/"

# list of files to import
files <- list.files(path = paste0(folder, TagID, "/"), pattern = "EddyVectorPlot",
                    full.names = T, recursive = F)
files # check

# function to read image files using magick's function
read.image <- function(x) {
  
  i <- image_read(x)
  return(i)
  
}

# apply function
images <- lapply(files, read.image)

# group and scale images for full deployment
all <- image_scale(c(images[[1]], images[[2]], images[[3]], images[[4]], images[[5]], images[[6]],
                     images[[7]], images[[8]], images[[9]], images[[10]], images[[11]], images[[12]],
                     images[[13]], images[[14]], images[[15]]))

# write gif file 
image_write_gif(all, paste0(folder, TagID, "/", TagID, "_EddyVectorAnimation_FullDply.gif"),
                  delay = 1)

