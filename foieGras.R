##########################################################

#     foieGras: fit continous-time random walk or
#         correlated random walk models to
#             animal movement data 

# Michaela A. Kratofil, Cascadia Research
# Updated: 06 AUG 2020

##########################################################

## OVERVIEW ##

# For details on the model, refer to Jonsen et al. (2020), 
# "A continous-time state-space model for rapid quality
# control of argos locations from animal-borne tags. 

# The model coded here is pretty basic, but various user
# defined parameters can be adjusted to suit your data/
# research questions. Take the time to understand the model!

# Ian Jonsen's paper on this model includes code and an
# easy to follow example; I would reccommend reading this. 
# He also has basic vignettes available on his github. 

# This script is set up to fit a foieGras model to several
# deployments at once (or single). 

# This package/model can deal with least-squares or Kalman
# filtered data. The model also includes a psi parameter to
# account for possible consistent underestimation of the 
# Kalman filter-derived location uncertainty. psi re-scales
# all ellipse semi-minor axes, where estimated values > 1
# inflate the uncertainty region around measured locations 
# by lengthening the semi-minor axis. 

## How it works ##

# Here we will use location data that has already been through
# the Douglas Filter. We'll need to format the data for input
# into foieGras.

############################################################

# load packages
library(tidyverse)
library(lubridate)
library(sf)
library(foieGras)
library(ptolemy)
library(ggspatial)

## read in Douglas filtered locations (Argos only)
tbl_locs <- readr::read_csv("Douglas Filtered/FaTag002-011_DouglasFiltered_KS_r15d3lc2_2020MAYv1.csv",
                            col_types = cols(animal = col_character(),
                                             ptt = col_integer(),
                                             date = col_datetime(),
                                             longitud = col_double(),
                                             latitude = col_double(),
                                             LC = col_character(),
                                             error_radius = col_integer(),
                                             ellipse_orient = col_integer(),
                                             semi_major = col_integer(),
                                             semi_minor = col_integer()
                            ))

## review data
str(tbl_locs)
summary(tbl_locs)
length(unique(tbl_locs$animal)) # 10 deployments in this dataset 
tbl_locs$LC <- factor(tbl_locs$LC, levels = c("DP","L3","L2","L1","L0","LA","LB","LZ")) # assign factor levels 
summary(tbl_locs$LC) # check
class(tbl_locs$date) # check class of date is POSIXct or POSIXt
attr(tbl_locs$date, 'tzone') # check TZ of date 


## set up variables to fit FG models
tbl_locs <- tbl_locs %>%
  rename(
    id = animal,
    lc = LC,
    lon = longitud,
    lat = latitude,
    smaj = semi_major,
    smin = semi_minor,
    eor = ellipse_orient
  )

tbl_locs <- select(tbl_locs, id, date, lc, lon, lat, smaj, smin, eor) # select columns

## recode LC classes (foieGras panicks if don't), and make DP location L3
tbl_locs$lc <- recode(tbl_locs$lc, DP = '3', L3 = '3', L2 = '2', L1 = '1',
                L0 = '0', LA = 'A', LB = 'B')
summary(tbl_locs$lc) # check

## assign DP locations error ellipse info
tbl_locs$smaj[is.na(tbl_locs$smaj)] <- 0
tbl_locs$smin[is.na(tbl_locs$smin)] <- 0
tbl_locs$eor[is.na(tbl_locs$eor)] <- 0
summary(tbl_locs) # check

## project locations using an appropriate CRS - if don't project, the fit_ssm
## function will internally project using the world mercator projection. 
## *I've often run into bugs when specifying my own projection, so use default.
## I use the crs EPSG:3750 (NAD83/UTM 4N) here, which works pretty well if data 
## doesn't go too far east of the Big Island. 
sf_locs <- st_as_sf(tbl_locs, coords = c("lon","lat"), crs = 4326) %>%
  st_transform(crs = 3750) 
st_crs(sf_locs) # check

## visualize: create tracklines and map
sf_lines <- sf_locs %>%
  arrange(id, date) %>%
  group_by(id) %>%
  summarise(do_union = F) %>%
  st_cast("MULTILINESTRING")
st_crs(sf_lines) # check

# get coastline data from ptolemy package
map_base <- ptolemy::extract_gshhg(sf_locs, buffer = 50000, epsg = 3750) # extract polygon data for region of loc data
plot(map_base) # check

# map theme function
theme_map <- function() {
  theme_bw() +
    theme(panel.background = element_rect(fill = 'white', colour = 'black', size = 1.25),
          axis.text = element_text(colour = 'black'),
          plot.title = element_text(colour = 'black', face = 'bold')) #+
  
}

# map tracklines
ggplot() +
  annotation_spatial(map_base, fill = 'grey', lwd = 1) +
  layer_spatial(sf_lines, size = 1, aes(color = id)) +
  theme_map() +
  scale_color_viridis_d()

## fit random walk model: we turn spdf (speed filter) off and set a time step of 3 hours. I 
## turned off the 'psi' parameter here.
m1 <- fit_ssm(tbl_locs, model = 'rw', spdf = F, time.step = 3, map = list(psi = factor(NA)))
m1$ssm[[1]] # check model parameters for each tag 

## quick visualization
plot(m1, what = 'fitted', type = 2) # fitted locations
plot(m1, what = 'predicted', type = 2) # predicted locations

## grab predicted locations 
pred1 <- grab(m1, what = 'predicted', as_sf = F)

## If desired, fit move persistence model ** this model is not well fit, just an exmaple of the code
## to fit a move persistence model.
fmp <- m1 %>%
  grab(., "p", as_sf = F) %>%
  select(id, date, lon, lat) %>%
  fit_mpm(., model = "jmpm") # use jmpm to pool variance parameters across all individuals

fmp$mpm[[1]] # can check output


## save predicted location data 
write.csv(pred1, "SSM/FaTag002-011_FG_3hTimeStep_2020AUGv1.csv", row.names = F)
