library(moveVis)
library(tidyverse)
library(lubridate)
library(PNWColors)
library(sp)
library(RColorBrewer)
library(viridisLite)

# read in location data
locs <- read.csv("Douglas Filtered/PcTag004-064_DouglasFiltered_KS_r20d3lc2_2020MAYv3.csv", header = T)
#locs <- read.csv("Douglas Filtered/Pc/PcTag065_DouglasFiltered_KF_r20d3lc2_2020May25_v1.csv", header = T)
locs$date <- as.POSIXct(locs$date, tz = "UTC") # format datetime
locs <- dplyr::filter(locs, animal %in% c("PcTag053","PcTag054")) # select pair(s) of tags you want to animate

# subset time period by day (if want)
locs$date_ymd <- lubridate::date(locs$date)

locs_sub <- locs %>%
  filter(between(date_ymd, as.Date("2017-03-09"), as.Date("2017-06-06"))) 


# convert to "move" object
locs_mv <- df2move(locs_sub, proj = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                   x = "longitud", y = "latitude", time = "date", track_id = "animal")


# align move data to uniform time scale
m <- align_move(locs_mv, res = 1, unit = "hours")
m <- sp::spTransform(m, CRS("+init=epsg:3857"))

# color palettes using PNWColors 
pal7 <- pnw_palette(name = "Lake", n = 7, type = "discrete") # for 7 tags
pal5 <- pnw_palette(name = "Starfish", n = 5, type = "discrete") # for 5 tags
pal3 <- pnw_palette(name = "Winter", n = 3, type = "discrete") # for 3 tags
pal2 <- pnw_palette(name = "Winter", n = 2, type = "discrete") # for 2 tags 


vpal7 <- viridis(n = 7, option = "D")
bpal7 <- brewer.pal(n = 7, name = "Dark2")

# create spatial frames with map
frames <- frames_spatial(m, path_colours = pal2,
                         map_service = "osm", map_type = "toner_bg", alpha = 0.5, equidistant = F,
                         path_legend_title = "Tag") %>%
  add_labels(x = "Easting", y = "Northing",
             caption = "Projection: EPSG:3857 WGS 84 / Pseudo-Mercator (axis units: meters)") %>%
  add_timestamps(m, type = "label") %>%
  add_scalebar() %>%
  add_northarrow() 

frames[[50]]

# animate the frames. the time that this will take depends on how many frames you have.
# ex. ~290 frames takes about 17 minutes. 
suggest_formats()
animate_frames(frames, out_file = "Dist between pairs/Plots/PcTag053-54_moveVis_animation.mp4")
