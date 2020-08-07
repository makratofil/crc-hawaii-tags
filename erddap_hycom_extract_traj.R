## Extract HYCOM data for tag trajectories:

## Michaela A. Kratofil
## 06 JUL 2020

#############################################

# load packages
library(tidyverse)
library(tidylog)
library(lubridate)
library(rerddapXtracto)
library(plotdap)
library(ggplot2)
library(sf)
library(rgdal)
#library(rnaturalearth)
#library(mgcv)
#library(progress)

## read in GIS processed location data 
gis.locs <- read.csv("GIS output/PmTag012-025_GIS_20200529.csv", header = T) %>%
  as_tibble()

# review data
head(gis.locs)

# select columns of potential interest
gis.locs <- select(gis.locs, animal, ptt, date, longitud, latitude, LC, error_radius,
                   ellipse_orient, semi_major, semi_minor, datetimeUTC, datetimeHST,
                   DistToShore, Island, Source, depth, aspect, slope, 
                   sunrise, sunset, solarNoon, civilDawn, endDawn, civilDusk, startDusk,
                   sunAzimuth, sunAltitude, moonAzimuth, moonAltitude, moonIlluminatedFraction,
                   moonPhase, daylengthHr, daylengthCat, month, season, TagRegion, TagLocality,
                   Sex, Age, Pseudoreplicate)

# format data
gis.locs$animal <- as.factor(gis.locs$animal)
gis.locs$date <- as.POSIXct(gis.locs$date, tz = 'UTC')
attr(gis.locs$date, 'tzone') # check
gis.locs$datetimeUTC <- as.POSIXct(gis.locs$datetimeUTC, tz = 'UTC')
attr(gis.locs$datetimeUTC, 'tzone') # check
gis.locs$datetimeHST <- as.POSIXct(gis.locs$datetimeHST, tz = 'Pacific/Honolulu')
attr(gis.locs$datetimeHST, 'tzone') # check 

# check temporal and spatial extent of data
summary(gis.locs$date)
range(gis.locs$latitude)
range(gis.locs$longitud)

## Find appropriate datasets on any ERDDAP server

# add day/date column
gis.locs$Date_ymd <- date(gis.locs$date)

# subset each tag and run separately. had issues with server timing out
tag <- filter(gis.locs, animal == 'PmTag012')

# set variable information
xpos <- tag$longitud
ypos <- tag$latitude
tpos <- tag$Date_ymd
zpos <- rep(0., length(xpos)) # set altitude to zero 


# get variable info: here I extract variables ssh and u, v, for analyses of large scale eddy systems
sshInfo <- rerddap::info('hawaii_soest_ad7b_021a_08e2', url = 'https://apdrc.soest.hawaii.edu/erddap/')
sshInfo
uInfo <- rerddap::info('hawaii_soest_114b_16e8_26f6', url = 'https://apdrc.soest.hawaii.edu/erddap/')
uInfo
vInfo <- rerddap::info('hawaii_soest_e97d_9053_f8f8', url = 'https://apdrc.soest.hawaii.edu/erddap/')
vInfo

## extract the data

# sea surface height
ssh <- rxtracto(sshInfo, parameter = 'ssh', xcoord = xpos, ycoord = ypos, tcoord = tpos,
                xlen = 0.01, ylen = 0.01, progress_bar = T)

# eastward current velocity
u <- rxtracto(uInfo, parameter = 'u', xcoord = xpos, ycoord = ypos, tcoord = tpos, zcoord = zpos,
             zName = 'LEV', xlen = 0.01, ylen = 0.01, progress_bar = T)

# northward current velocity
v <- rxtracto(vInfo, parameter = 'v', xcoord = xpos, ycoord = ypos, tcoord = tpos, zcoord = zpos,
              zName = 'LEV', xlen = 0.01, ylen = 0.01, progress_bar = T)

# make dataframes from lists and bind together
ssh.df <- as.data.frame(ssh) %>%
  as_tibble() %>%
  select(satellite.date, requested.date, mean.ssh)

v.df <- as.data.frame(v) %>%
  as_tibble() %>%
  select(satellite.date, requested.date, mean.v)

u.df <- as.data.frame(u) %>%
  as_tibble() %>%
  select(satellite.date, requested.date, mean.u)

vars <- bind_cols(ssh.df, v.df) %>%
  bind_cols(., u.df) # check that the dates align

# clean up
vars.df <- select(vars, satellite.date...1, requested.date...2, mean.ssh, mean.v, mean.u)
vars.df <- vars.df %>%
  rename(., satellite.date = satellite.date...1, requested.date = requested.date...2,
         ssh1day = mean.ssh, v1day = mean.v, u1day = mean.u)

# add column for total horizontal velocity magnitude
vars.df <- mutate(vars.df, 
               currMag1day = sqrt(abs((u1day^2) + (v1day^2))))


# bind together
tag.full <- bind_cols(tag, vars.df)

# save individual files
pmtag25 <- tag.full
saveRDS(pmtag25, 'PmTag025_TrajEddyData.rds')
pmtag12 <- readRDS('PmTag012_TrajEddyData.rds')

# bind all together
all.tag.full <- bind_rows(pmtag12, pmtag13) %>%
  bind_rows(., pmtag15) %>%
  bind_rows(., pmtag16) %>%
  bind_rows(., pmtag17) %>%
  bind_rows(., pmtag19) %>%
  bind_rows(., pmtag20) %>%
  bind_rows(., pmtag21) %>%
  bind_rows(., pmtag22) %>%
  bind_rows(., pmtag23) %>%
  bind_rows(., pmtag24) %>%
  bind_rows(., pmtag25)

# write csv and rds files for entire set 
write.csv(all.tag.full, "PmTag012-025_GIS_EddyData.csv", row.names = F)
saveRDS(all.tag.full, "PmTag012-025_EddyData_Traj.rds")

# review
head(all.tag.full)
str(all.tag.full)
summary(all.tag.full$animal)
summary(all.tag.full$ssh1day)

## visualize 

# quick visualization
ggplot() +
  geom_sf(data = coastr) +
  geom_sf(data = sf_lines) +
  geom_sf(data = sf_locs, aes(color = currMag1day)) +
  theme_map() 

