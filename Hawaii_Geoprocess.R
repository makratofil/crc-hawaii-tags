# Hawaii.R - Perform geoprocessing on positions for odontocetes satellite tagged by Cascadia Research Collective 
# in collaboration with Duke University in 2014 and 2015.
#
# Copyright 2015 David Anderson
# GPL
# use this at your own risk
#
# EDITED: 2020 MAR 21 - removed package 'moonsun', no longer in CRAN repository - MAK
#                     - W_L_WARD: use gIntersects instead of gDistance, otherwise
#                                 does not categorize correctly - MAK
#                     - Commented code throughout - MAK
#         2020 MAR 22 - added columns/function for length of day in hours and categorical - MAK
#                     - added column to compute month - MAK
#                     - added column to compute oceanographic season - MAK
#         2020 MAR 23 - changed file name of script to "Hawaii_Geoprocess.R" - MAK
#         2020 JUN 18 - added column for time of day (ToD)
#         2020 JUL 28 - updated code to be compatible with recent changes in GDAL and PROJ
#                     - converted all sp operations to sf
#                     - added columns for KonaHARPDist, KauaiHARPDist, KonaHARP_NS, KauaiHARP_NS
#
# MAK: Michaela A Kratofil
#
#################################################################################################################

##### DESCRIPTION #####

# This script takes input files including position data and populates them with various geospatial variables.
# The current version of the script (2020 JUL 28) has functionality to deal with file types:

#         - Effort data (GPS data from field projects)
#         - Sightings data (species sightings data from field projects)
#         - Satellite tag data run through state-space model (SSM)
#         - Fastloc GPS data 
#         - "pick", generic location data (?)
#         - Douglas filtered location data 

# There are other random bits of code that can be commented out or disregarded based on the position file
# you are working with. Make sure the position file you are working with has column headings that match the 
# input/functionality written in the script. If using a new file type, add functionality for file type.

#################################################################################################################
#               USER BEWARE!  USER BEWARE!  USER BEWARE!  USER BEWARE!  USER BEWARE! 
#
# I have done all that I can think of to check for errors in this code, but that does not mean that it is
# completely bug-free.  It would be a good idea to check some of the output records with calculations done
# by hand or using a training dataset (of known values). If the input dataset is continually being added to,
# it is a good idea to check and make sure that subsequent runs of the script provide identical results for 
# as previous runs computed.       
#
#################################################################################################################

#### How it works ####

# 1. An option list for the command line is created and populated with all file objects to be run
#     through the main for loop (have variables populated). These include the position file, 
#     all shapefiles, and rasters.
#       a. You should have all the shapefiles and rasters stored in respective folders in your 
#           working directory. 
# 2. Change line #114 opt$species = to the type of file you are using (FastGPS, Sightings, Effort, SSM, pick, DougFilt)
# 3. Change line #115 opt$posfile = to the name of the file you are importing
# 4. Make sure line #144 is written to correctly read in your posfile (directory)
# 5. While you're working through the code, make sure column headings match what the code specifies 
#     for that file type
# 6. Follow the rest of the code (commented out) to format data, import shapefiles and rasters, etc
# 7. The last function is a for loop that will compute and populate all variables
# 8. The output file will be saved as a .csv

#################################################################################################################

# function to load packages
RequirePackages <- function(x){
  for(i in x){
    if(!require(i, character.only=T)) {
      install.packages(i, dependencies=T)
      require(i, character.only=T)
    }
  }
}

# load packages
RequirePackages(c("lubridate", "R.utils", "sp", "sf", "rgdal", "rgeos", "geosphere", "optparse", "oce", "raster", "maptools", "insol", "lunar", "dplyr"))

## Global objects and definitions ##

# projection to use for east coast data

# Proj <- "+init=EPSG:3748"

## option list for the command line ##
option_list <- list(
  make_option(c("-p", "--posfile"), action="store", type="character", help="", default=NULL),
  make_option(c("-S", "--shorefile"), action="store", type="character", help="", default=NULL),
  make_option(c("-d", "--depthH"), action="store", type="character", help="", default=NULL),
  make_option(c("-e", "--depthM"), action="store", type="character", help="", default=NULL),
  make_option(c("-f", "--depthL"), action="store", type="character", help="", default=NULL),
  make_option(c("-s", "--slopeH"), action="store", type="character", help="", default=NULL),
  make_option(c("-t", "--slopeM"), action="store", type="character", help="", default=NULL),
  make_option(c("-u", "--slopeL"), action="store", type="character", help="", default=NULL),
  make_option(c("-D", "--d200m"), action="store", type="character", help="", default=NULL),
  make_option(c("-E", "--EEZ"), action="store", type="character", help="", default=NULL),
  make_option(c("-n", "--navarea"), action="store", type="character", help="", default=NULL)
)

## Functions ##

# Read command line
#opt <- parse_args(OptionParser(option_list=option_list), positional_arguments=T)
opt = data.frame(1)

## Start interactive ##
if(is.null(opt$posfile)) {
  #no position file was supplied on the command line, so let's do this interactively.
  opt$species = "DougFilt" # this will be either: Effort, Sightings, SSM, or FastGPS with current options
  opt$posfile <- "PeTag001-027_DouglasFiltered_ArgosGPS_r15d3lc2_2020JULv1" # file name for the latter
  opt$shorefile <- "FisheriesIslands"
  opt$d200m <- "d200m"
  opt$EEZ <- "EEZ_Hawaii_UTM4N"
  opt$PMRF <- "PMRF_UTM4N"
  opt$fishzone <- "FisheriesZones"
  opt$longlineYearRound <- "longlineYearRound"
  opt$longlineFebSep <- "longlineFebSept"
  #	opt$ramps <- "ramps"
  opt$leeward <- "Leeward"
  opt$studyarea <- "StudyArea"
  opt$honokohau <- "Kaloko-Honokohau"
  opt$kalaupapa <- "Kalaupapa"
  opt$sanctuary <- "Humpback_Sanctuary"
  opt$nwhimnm = "nwhi_mnm_py"
  opt$pmnm = "PMNM_UTM4"
  opt$depthL <- "GebcoDepthWUTM4N.nc"
  opt$depthM <- "FalkorDepthUTM4N.nc"
  opt$depthH <- "multibeamUTM4N.nc"
  opt$aspectL <- "GebcoAspectWUTM4N.nc"
  opt$aspectM <- "FalkorAspectUTM4N.nc"
  opt$aspectH <- "MultibeamAspectUTM4N.nc"
  opt$slopeL <- "GebcoSlopeWUTM4N.nc"
  opt$slopeM <- "FalkorSlopeUTM4N.nc"
  opt$slopeH <- "MultibeamSlopeUTM4N.nc"
}

# Main code

# load posfile and make spatial points of positions
posfile = read.csv(paste0("Douglas Filtered/", opt$posfile, ".csv"), header = T)

# review data
str(posfile)
summary(posfile)
unique(posfile$animal)
length(unique(posfile$animal))
unique(posfile$LC)
length(unique(posfile$LC))
colnames(posfile)

# change column names for function as needed #
if(opt$species == "FastGPS") {
  posfile = subset(posfile, InitType == "FastGPS")
  posfile = posfile[!is.na(posfile$Longitude),]
  names(posfile)[names(posfile) == "Latitude"] = "latitude"
  names(posfile)[names(posfile) == "Longitude"] = "longitud"
} else if (opt$species == "SSM") {
  names(posfile)[names(posfile) == "id"] = "animal"
}

#posfile = subset(posfile, longitud < -100) # if need to adjust longitude

## Add columns to be populated ##
posfile$DistToShore <- NA
posfile$Island <- NA
posfile$DistTo200m <- NA
posfile$W_L_WARD <- NA
posfile$StudyArea <- NA
posfile$FisheryZone <- NA
posfile$ZoneType <- NA
posfile$LonglineExAllYear <- NA
posfile$LonglineExFebSep <- NA
posfile$PMRF <- NA
posfile$EEZ <- NA
posfile$KalaupapaNP <- NA
posfile$KalaupapaDist <- NA
posfile$KalokoHonokohauNP <- NA
posfile$KalokoHonokohauDist <- NA
posfile$HumpbackSanctuary <- NA
posfile$HumpbackSanctuaryDist <- NA
posfile$NWHI_MNM <- NA
posfile$NWHI_MNM_Dist <- NA
posfile$KonaHARPDist <- NA
posfile$KonaHARP_NS <- NA
posfile$KauaiHARPDist <- NA
posfile$KauaiHARP_NS <- NA
posfile$Source <- NA
posfile$depth <- NA
posfile$aspect <- NA
posfile$slope <- NA

## Add datetime columns, check format of datetime in posfile to match what function specifies ##
if(opt$species == "Sa") {
  posfile$SaBigIsl65kmBoundary = NA
  posfile$Sa4Isl20kmBoundary = NA
  posfile$SaOahu20kmBoundary = NA
  posfile$datetimeUTC = as.POSIXct(posfile$date, tz = "UTC")
  #posfile$datetimeUTC = as.POSIXct(paste(posfile$date, posfile$time), tz = "UTC", format = "%m/%d/%Y %H:%M:%S")
  posfile$datetimeHST = as.POSIXct(format(posfile$datetimeUTC, tz="Pacific/Honolulu"), tz="Pacific/Honolulu")
} else if (opt$species == "Effort") {
  posfile$datetimeHST = as.POSIXct(paste(posfile$Date_, posfile$Time.min.and.sec), tz = "Pacific/Honolulu", format = "%m/%d/%Y %H:%M:%S")
  posfile$datetimeUTC = as.POSIXct(format(posfile$datetimeHST, tz="UTC"), tz="UTC")
  
} else if(opt$species == "Sightings") {
  colnames(posfile)[which(names(posfile) == "Start.longitude")] <- "longitud"
  posfile$longitud = 0 - abs(posfile$longitud)
  colnames(posfile)[which(names(posfile) == "Start.latitude")] <- "latitude"
  posfile$datetimeHST = as.POSIXct(posfile$Date, tz = "Pacific/Honolulu", format = "%d-%b-%y") 
  posfile$datetimeUTC = as.POSIXct(format(posfile$datetimeHST, tz="UTC"), tz="UTC")
} else if (opt$species == "SSM") {
  posfile$datetimeUTC = as.POSIXct(posfile$date, tz = "UTC")
  posfile$datetimeHST = as.POSIXct(format(posfile$datetimeUTC, tz="Pacific/Honolulu"), tz="Pacific/Honolulu")
} else if (opt$species == "pick") {
  posfile$datetimeUTC = as.POSIXct(posfile$date, tz = "UTC", format = "%m/%d/%Y %H:%M")
  posfile$datetimeHST = as.POSIXct(format(posfile$datetimeUTC, tz="Pacific/Honolulu"), tz="Pacific/Honolulu")
} else if (opt$species == "FastGPS") {
  posfile$datetimeUTC = as.POSIXct(paste(posfile$Day, posfile$Time), tz = "UTC", format = "%d-%b-%Y %H:%M:%S")
  posfile$datetimeHST = as.POSIXct(format(posfile$datetimeUTC, tz="Pacific/Honolulu"), tz="Pacific/Honolulu")
} else if (opt$species == "DougFilt") {
  posfile$datetimeUTC = as.POSIXct(posfile$date, tz = "UTC")
  posfile$datetimeHST = as.POSIXct(format(posfile$datetimeUTC, tz = "Pacific/Honolulu"), tz = "Pacific/Honolulu")
} else {
  posfile$datetimeUTC = as.POSIXct(paste(posfile$date, posfile$time), tz = "UTC", format = "%m/%d/%Y %H:%M:%S")
  posfile$datetimeHST = as.POSIXct(format(posfile$datetimeUTC, tz="Pacific/Honolulu"), tz="Pacific/Honolulu")
}

## Fill in datetime related columns that will be populated/adjusted by function later ##
posfile$JulianDate = insol::JD(posfile$datetimeUTC)
posfile$sunrise <- posfile$datetimeHST
posfile$sunset <- posfile$datetimeHST
posfile$solarNoon <- posfile$datetimeHST
posfile$civilDawn <- posfile$datetimeHST
posfile$endDawn <- posfile$datetimeHST
posfile$civilDusk <- posfile$datetimeHST
posfile$startDusk <- posfile$datetimeHST
posfile$secNearestSunriseset = as.integer(0)
posfile$sunAzimuth <- NA
posfile$sunAltitude = NA
posfile$moonAzimuth = NA
posfile$moonAltitude = NA
posfile$moonIlluminatedFraction = NA
posfile$moonPhase = as.character(NA)
posfile$daylengthHr = as.numeric(NA)
posfile$daylengthCat = as.character(NA)
posfile$month = NA
posfile$season = as.character(NA)
posfile$ToD = as.character(NA)
#posfile$ClosestRamp <- as.character(NA)

## If working with tag data, if not, disregard ##
tags = read.csv("Summary files/TagIndividual_Info_Jul2020v1.csv", stringsAsFactors = F) # file with info on individuals tagged

# add columns to be populated
posfile$CRC_ID = as.character(NA)
posfile$population = as.character(NA)
posfile$cluster = as.character(NA)
posfile$community = as.character(NA)
posfile$TagRegion = as.character(NA)
posfile$TagLocality = as.character(NA)
posfile$Sex = as.character(NA)
posfile$Age = as.character(NA)
posfile$Pseudoreplicate = as.character(NA)

# populate columns based on individual tag data
rows = nrow(tags)
for (i in 0:rows) {
  cat("\rRow ", i, " of ", rows)
  match = posfile$animal == tags[i, "Animal"]
  posfile[match, "CRC_ID"] = tags[i, "CRC_ID"]
  posfile[match, "population"] = tags[i, "Population"]
  posfile[match, "cluster"] = tags[i, "Cluster"]
  posfile[match, "community"] = tags[i, "Community"]
  posfile[match, "TagRegion"] = tags[i, "Region"]
  posfile[match, "TagLocality"] = tags[i, "Locality"]
  posfile[match, "Age"] = tags[i, "Age"]
  posfile[match, "Sex"] = tags[i, "Sex"]
  posfile[match, "Pseudoreplicate"] = tags[i, "Pseudoreplicate"]
}

## Import shapefiles and reproject using UTM 4N projection (GRS80 ellipsoid)
## EPSG:3750 or "+proj=utm +zone=4 +ellps=GRS80 +units=m +no_defs", see WKT version at spatialreference.org
## Will throw out warning sign for discarded datum

# Shoreline shapefile, for distance to shore 
shore <- readOGR("Shapefiles", opt$shorefile, verbose=F) %>%
  as(., "sf")
shore <- st_transform(shore, 3750) # need to transform the CRS as it does not read in correctly

# Distance to 200m isobath file
d200m <- readOGR("Shapefiles", opt$d200m, verbose=F) %>%
  as(., 'sf')
d200m <- st_transform(d200m, 3750)

# United States Economic Exclusion Zone file
EEZ <- readOGR("Shapefiles", opt$EEZ, verbose=F) %>%
  as(., 'sf')
EEZ <- st_transform(EEZ, 3750)

# Commercial fisheries zones (grid cells) file
FZ <- readOGR("Shapefiles", opt$fishzone, verbose=F) %>%
  as(., 'sf')
FZ <- st_transform(FZ, 3750)

# Pacific Missile Range Facility file
PMRF <- readOGR("Shapefiles", opt$PMRF, verbose=F) %>%
  as(., 'sf')
PMRF <- st_transform(PMRF, 3750)

# Leeward side of islands file
leeward <- readOGR("Shapefiles", opt$leeward, verbose=F) %>%
  as(., 'sf')
leeward <- st_transform(leeward, 3750)

# Study area, polygon on leeward side of Big Island (Honokohau to southern point)
studyarea <- readOGR("Shapefiles", opt$studyarea, verbose=F) %>%
  as(., 'sf')
studyarea <- st_transform(studyarea, 3750)

# Longline exclusion zone all year file
longlineyr <- readOGR("Shapefiles", opt$longlineYearRound, verbose=F) %>%
  as(., 'sf')
longlineyr <- st_transform(longlineyr, 3750)

# Longline exclusion zone February-September file 
longlinefs <- readOGR("Shapefiles", opt$longlineFebSep, verbose=F) %>%
  as(., 'sf')
longlinefs <- st_transform(longlinefs, 3750)

# Kaloko Honokohau National Park file 
honokohau <- readOGR("Shapefiles", opt$honokohau, verbose=F) %>%
  as(., 'sf')
honokohau <- st_transform(honokohau, 3750)

# Kalaupapa National Park file 
kalaupapa <- readOGR("Shapefiles", opt$kalaupapa, verbose=F) %>%
  as(., 'sf')
kalaupapa <- st_transform(kalaupapa, 3750)

# Humpback Sanctuary file 
sanctuary <- readOGR("Shapefiles", opt$sanctuary, verbose=F) %>%
  as(., 'sf')
sanctuary <- st_transform(sanctuary, 3750)

# Northwestern Hawaiian Islands National Monument file 
nwhimnm <- readOGR("Shapefiles", opt$nwhimnm, verbose=F) %>%
  as(., 'sf')
nwhimnm <- st_transform(nwhimnm, 3750)

# Other random shapefiles 
#pmnm = readOGR("Shapefiles", opt$pmnm, verbose=F)
#pmnm <- spTransform(pmnm, CRS("+proj=utm +zone=4 +ellps=GRS80 +units=m +no_defs"))
#ramps <- readOGR(".", opt$ramp, verbose=F)
#ramps <- spTransform(ramps, CRS("+proj=utm +zone=4 +ellps=GRS80 +units=m +no_defs"))

if(opt$species == "Sa") {
  SaBigIsland = readOGR("Shapefiles", "BigIsl65kmSa", verbose=F) %>% as(., 'sf')
  SaBigIsland <- st_transform(SaBigIsland, 3750)
  Sa4Island = readOGR("Shapefiles", "4Isl20kmSa", verbose=F) %>% as(., 'sf')
  Sa4Island <- st_transform(Sa4Island, 3750)
  SaOahu = readOGR("Shapefiles", "Oahu20kmSa4n", verbose=F) %>% as(., 'sf')
  SaOahu <- st_transform(SaOahu, 3750)
}

#x <- st_crs(shore)$proj4string

# DepthL, 30 arc-sec GEBCO
depthL <- raster(paste0("Rasters/", opt$depthL))

# DepthM, 60m Multibeam Falkor
depthM <- raster(paste0("Rasters/", opt$depthM))

# DepthH, 50m HMRG Multibeam GEBCO
depthH <- raster(paste0("Rasters/", opt$depthH))

# Aspect, 30 arc-sec GEBCO
aspectL <- raster(paste0("Rasters/", opt$aspectL))

# Aspect, 60m Multibeam Falkor
aspectM <- raster(paste0("Rasters/", opt$aspectM))

# Aspect, 50m HMRG Multibeam GEBCO
aspectH <- raster(paste0("Rasters/", opt$aspectH))

# Slope, 30 arc-sec GEBCO
slopeL <- raster(paste0("Rasters/", opt$slopeL))

# Slope, 60m Multibeam Falkor
slopeM <- raster(paste0("Rasters/", opt$slopeM))

# Slope, 50m HMRG Multibeam GEBCO
slopeH <- raster(paste0("Rasters/", opt$slopeH))

# For east coast?
#projection(depthL) <- CRS("+init=EPSG:3395")
#depthL.proj <- projectRaster(depthL, crs=Proj)

## Kona and Kauai HARP points
KonaHarp_point <- st_point(c( -156.1691, 19.5547))
KonaHarp_sfc <- st_sfc(KonaHarp_point)
KonaHarp <- st_sf(data.frame(geom=KonaHarp_sfc), crs = "+init=EPSG:4326") %>%
  st_transform(., 3750)

KauaiHarp_point <- st_point(c(-159.8900, 21.9532))
KauaiHarp_sfc <- st_sfc(KauaiHarp_point)
KauaiHarp  <- st_sf(data.frame(geom=KauaiHarp_sfc), crs = "+init=EPSG:4326") %>%
  st_transform(., 3750)

#### This is the main for loop that populates all columns with GIS variables ####
#lon = -157.2 # test points
#lat = 20.5
for (i in 1:nrow(posfile)) {
  cat("\rRow ", i, " of ", nrow(posfile))
  if (opt$species == "Effort") {
    lon = posfile[i, "Long_dd"]
    lat = posfile[i, "Lat_dd"]
  }
  else if (opt$species == "SSM") {
    lon = posfile[i, "lon"]
    lat = posfile[i, "lat"]
  }
  else {
    lon = posfile[i, "longitud"]
    lat = posfile[i, "latitude"]
  }
  if (is.na(lon) | is.na(lat))
    next
  pos <- st_point(c(lon,lat))
  sfc = st_sfc(pos)
  cat("a")
  pos = st_sf(data.frame(geom=sfc), crs = "+init=EPSG:4326")
  cat("\bb")
  pos <- st_transform(pos, 3750)
  cat("\bc")
  dist <- st_distance(pos, shore, by_element = T)
  cat("\bd")
  posfile[i, "DistToShore"] <- min(dist)
  cat("\be")
  posfile[i, "Island"] <- as.character(shore$ISLAND[which.min(dist)])
  cat("\bf")
  posfile[i, "DistTo200m"] <- min(st_distance(pos, d200m))
  cat("\bg")
  if (!st_intersects(pos, leeward, sparse = F)[,1]) {
    posfile[i, "W_L_WARD"] <- "windward"
  }
  else {
    posfile[i, "W_L_WARD"] <- "leeward"
  }
  
  #	dist <- gDistance(pos, ramps, byid=T)
  #	posfile[i, "ClosestRamp"] <- as.character(ramps@data$Boat_ramp[which.min(dist)])
  #	cat("\bf")
  #	for (j in 1:length(dist)) {
  #		posfile[i, as.character(ramps@data$Boat_ramp[j])] <- dist[j]
  #}
  cat("\bh")
  posfile[i, "StudyArea"] <- !st_distance(pos, studyarea)[,1]
  posfile[i, "KalaupapaDist"] = st_distance(pos, kalaupapa)[,1]
  posfile[i, "KalaupapaNP"] <- !posfile[i, "KalaupapaDist"]
  posfile[i, "KalokoHonokohauDist"] = st_distance(pos, honokohau)[,1]
  posfile[i, "KalokoHonokohauNP"] <- !posfile[i, "KalokoHonokohauDist"]
  posfile[i, "HumpbackSanctuaryDist"] = min(st_distance(pos, sanctuary))
  posfile[i, "HumpbackSanctuary"] <- !posfile[i, "HumpbackSanctuaryDist"]
  posfile[i, "NWHI_MNM_Dist"] = st_distance(pos, nwhimnm)[,1]
  posfile[i, "NWHI_MNM"] = !posfile[i, "NWHI_MNM_Dist"]
  posfile[i, "KonaHARPDist"] = st_distance(pos, KonaHarp)[,1]
  posfile[i, "KauaiHARPDist"] = st_distance(pos, KauaiHarp)[,1]
  #posfile[i, "PMNM_Dist"] = gDistance(pos, pmnm)
  #posfile[i, "PMNM"] = !posfile[i, "PMNM_Dist"]
  cat("\bi")
  if (lat > 19.5547) {
    posfile[i, "KonaHARP_NS"] <- "North"
  } else {
    posfile[i, "KonaHARP_NS"] <- "South"
  }
  
  if (lat > 21.9532) {
    posfile[i, "KauaiHARP_NS"] <- "North"
  } else {
    posfile[i, "KauaiHARP_NS"] <- "South"
  }
  cat("\bj")
  EEZover <- sapply(st_intersects(pos, EEZ), function(z) if (length(z) == 0) NA_integer_ else z[1])
  cat("\bk")
  posfile[i, "EEZ"] <- as.character(EEZ$EEZ[EEZover])
  cat("\bl")
  FZover <- sapply(st_intersects(pos, FZ), function(z) if (length(z) == 0) NA_integer_ else z[1])
  cat("\bm")
  posfile[i, "FisheryZone"] <- FZ$AREA_ID[FZover]
  cat("\bn")
  posfile[i, "ZoneType"] <- as.character(FZ$TYPE[FZover])
  cat("\bo")
  posfile[i, "LonglineExAllYear"] <- !is.na(sapply(st_intersects(pos, longlineyr),
                                                   function(z) if (length(z) == 0) NA_integer_ else z[1]))
  cat("\bp")
  posfile[i, "LonglineExFebSep"] <- !is.na(sapply(st_intersects(pos, longlinefs),
                                                  function(z) if (length(z) == 0) NA_integer_ else z[1]))
  cat("\bq")
  posfile[i, "PMRF"] <- !is.na(sapply(st_intersects(pos, PMRF),
                                      function(z) if (length(z) == 0) NA_integer_ else z[1]))
  cat("\br")
  if(opt$species == "Sa") {
    posfile[i, "SaBigIsl65kmBoundary"] = !is.na(sapply(st_intersects(pos, SaBigIsland),
                                                       function(z) if (length(z) == 0) NA_integer_ else z[1]))
    posfile[i, "Sa4Isl20kmBoundary"] = !is.na(sapply(st_intersects(pos, Sa4Island),
                                                     function(z) if (length(z) == 0) NA_integer_ else z[1]))
    posfile[i, "SaOahu20kmBoundary"] = !is.na(sapply(st_intersects(pos, SaOahu),
                                                     function(z) if (length(z) == 0) NA_integer_ else z[1]))
  }
  
  pos <- st_transform(pos, crs = projection(depthH))
  cat("\bs")
  depth <- raster::extract(depthH, pos, method = "bilinear")
  cat("\bt")
  if (!is.na(depth)) {
    posfile[i, "depth"] <- depth
    posfile[i, "Source"] <- "50m HMRG multibeam"
    posfile[i, "aspect"] <- raster::extract(aspectH, pos, method = "bilinear")
    posfile[i, "slope"] <- raster::extract(slopeH, pos, method = "bilinear")
  }
  else {
    depth <- raster::extract(depthM, pos, method="bilinear")
    if (!is.na(depth)) {
      posfile[i, "depth"] <- depth
      posfile[i, "Source"] <- "60m Falkor multibeam"
      posfile[i, "aspect"] <- raster::extract(aspectM, pos, method = "bilinear")
      posfile[i, "slope"] <- raster::extract(slopeM, pos, method = "bilinear")
    }
    else {
      depth <- raster::extract(depthL, pos, method="bilinear")
      if (!is.na(depth)) {
        posfile[i, "depth"] <- depth
        posfile[i, "Source"] <- "30 arc sec GEBCO"
        posfile[i, "aspect"] <- raster::extract(aspectL, pos, method = "bilinear")
        posfile[i, "slope"] <- raster::extract(slopeL, pos, method = "bilinear")
      }
    }
  }
  posmatrix =  matrix(c(lon, lat), nrow=1)
  datetimeHST = posfile[i, "datetimeHST"]
  posfile[i, "sunrise"] = as.POSIXct(sunriset(posmatrix, datetimeHST, direction="sunrise", POSIXct.out=T)$time)
  sunrise = posfile[i, "sunrise"]
  posfile[i, "sunset"] = as.POSIXct(sunriset(posmatrix, datetimeHST, direction="sunset", POSIXct.out=T)$time)
  sunset = posfile[i, "sunset"]
  posfile[i, "solarNoon"] = as.POSIXct(solarnoon(posmatrix, datetimeHST, POSIXct.out=T)$time)
  solarNoon = posfile[i, "solarNoon"]
  posfile[i, "civilDawn"] = as.POSIXct(crepuscule(posmatrix, datetimeHST, solarDep = 6, direction="dawn", POSIXct.out=T)$time)
  posfile[i, "endDawn"] = as.POSIXct(crepuscule(posmatrix, datetimeHST, solarDep = -6, direction="dawn", POSIXct.out=T)$time)
  posfile[i, "civilDusk"] = as.POSIXct(crepuscule(posmatrix, datetimeHST, solarDep = 6, direction="dusk", POSIXct.out=T)$time)
  posfile[i, "startDusk"] = as.POSIXct(crepuscule(posmatrix, datetimeHST, solarDep = -6, direction="dusk", POSIXct.out=T)$time)
  
  lastmidnight = solarNoon - dhours(12)
  nextmidnight = solarNoon + dhours(12)
  
  if (datetimeHST < lastmidnight) {
    posfile[i, "secNearestSunriseset"] = as.integer(0 - as.duration(interval((datetimeHST + (2 * as.duration(interval(datetimeHST, lastmidnight)))), sunrise)))
  }
  else if (datetimeHST < sunrise) {
    posfile[i, "secNearestSunriseset"] = as.integer(0 - as.duration(interval(datetimeHST, sunrise)))
  }
  else if (datetimeHST < solarNoon) {
    posfile[i, "secNearestSunriseset"] = as.integer(as.duration(interval(sunrise, datetimeHST)))
  }
  else if (datetimeHST < sunset) {
    posfile[i, "secNearestSunriseset"] = as.integer(as.duration(interval(datetimeHST, sunset)))
  }
  else if (datetimeHST < nextmidnight) {
    posfile[i, "secNearestSunriseset"] = as.integer(0 - as.duration(interval(sunset, datetimeHST)))
  }
  else {
    posfile[i, "secNearestSunriseset"] = as.integer(0 - as.duration(interval(sunset, (datetimeHST - (2 * as.duration(interval(nextmidnight, datetimeHST)))))))
  }
  
  sunangle = oce::sunAngle(posfile[i, "datetimeUTC"], lon, lat)
  posfile[i, "sunAzimuth"] = sunangle$azimuth
  posfile[i, "sunAltitude"] = sunangle$altitude
  moonangle = oce::moonAngle(posfile[i, "datetimeUTC"], lon, lat)
  posfile[i, "moonAzimuth"] = moonangle$azimuth
  posfile[i, "moonAltitude"] = moonangle$altitude
  posfile[i, "moonIlluminatedFraction"] = moonangle$illuminatedFraction
  posfile[i, "moonPhase"] = lunar.phase(posfile[i, "datetimeUTC"], name = T)
  
  posfile[i, "daylengthHr"] = abs(difftime(sunrise, sunset, units = "hours", tz = "Pacific/Honolulu"))
  daylengthHr = posfile[i, "daylengthHr"]
  if (daylengthHr > 11.75) {
    posfile[i, "daylengthCat"] = "long"
  }
  else if (daylengthHr < 11.25) {
    posfile[i, "daylengthCat"] = "short"
  }
  else {
    posfile[i, "daylengthCat"] = "medium"
  }
  
  posfile[i, "month"] = month(datetimeHST)
  month = posfile[i, "month"]
  if (month %in% c(2:4)) {
    posfile[i, "season"] = "winter"
  }
  else if (month %in% c(5:7)) {
    posfile[i, "season"] = "spring"
  }
  else if (month %in% c(8:10)) {
    posfile[i, "season"] = "summer"
  }
  else {
    posfile[i, "season"] = "fall"
  }
  
  sunrise = posfile[i, 'sunrise']
  sunAltitude = posfile[i, 'sunAltitude']
  sunset = posfile[i, 'sunset']
  
  if(datetimeHST < sunrise & abs(sunAltitude) <= 18) {
    posfile[i, "ToD"] = "Dawn"
  } 
  else if(datetimeHST > sunset & abs(sunAltitude) <= 18) {
    posfile[i, "ToD"] = "Dusk"
  } 
  else if(datetimeHST > sunrise & datetimeHST < sunset) {
    posfile[i, "ToD"] = "Day"
  } 
  else {
    posfile[i, "ToD"] = "Night"
  }
  
}

# Save as .csv file #
write.table(posfile, file=paste0("GIS output/","PeTag001-027_ArgosGPS_", "GIS_", format(Sys.time(), "%Y%m%d"), ".csv"), sep=",", col.names=T, row.names=F)

