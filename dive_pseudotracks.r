## Dive pseudotracks: use position data to assign locations to 
## behavior (dive) data and populate with geospatial variables.

## Originally authored by Dave Anderson, Cascadia Research
## Edited, commented by Michaela A. Kratofil

## Most recent edits: 17 Dec 2020

###################################################################################

## WARNING: With recent changes in r-spatial group of packages, check crs() and 
## projection() of every shapefile, raster, and projected object. Like other scripts,
## I have done what I can to bug check this script but does not mean that it is 
## completely bug free. 

## load packages
library(R.utils)
library(geosphere)
library(lubridate)
library(oce)
library(lunar)
library(maptools)
library(raster)
library(rgdal)
library(rgeos)
library(ncdf4)
library(sf)

# select animal/tag
Animal = "TtTag034"
ptt = "173076"
#TagFile = "PcTag001-066_DouglasFiltered_r20d3lc2_ArgosGPS_2020OCTv2.csv" # file to read in
TagFile = "All_ExpRespTags_thru2020_Crawl_5minStep_ArgosGPS_wLocType.csv" # file to read in

## HST tags or tags that were programmed in HST, so will have behavior data in HST
HSTtags = c("SbTag006", "SbTag007", "SbTag008", "SbTag010", "SbTag011", "SbTag015", "PcTag035", "PcTag037",
            "GmTag070", "GmTag081", "GmTag082")
if(Animal %in% HSTtags) {
  TZ = "Pacific/Honolulu"
} else {
  TZ = "UTC"
}

## global options
BehaviorFile = paste0(Animal, "_", ptt, "-Behavior.csv")
opt = c()
opt$shorefile = "FisheriesIslands"
opt$d200m = "d200m"
opt$EEZ = "EEZ_Hawaii_UTM4N"
opt$PMRF = "PMRF_UTM4N"
opt$fishzone = "FisheriesZones"
opt$longlineYearRound = "longlineYearRound"
opt$longlineFebSep = "longlineFebSept"
#	opt$ramps = "ramps"
opt$leeward = "Leeward"
opt$studyarea = "StudyArea"
opt$honokohau = "Kaloko-Honokohau"
opt$kalaupapa = "Kalaupapa"
opt$sanctuary = "Humpback_Sanctuary"
opt$nwhimnm = "nwhi_mnm_py"
opt$pmnm = "PMNM_UTM4"
opt$depthL = "GebcoDepthWUTM4N.nc"
opt$depthM = "FalkorDepthUTM4N.nc"
opt$depthH = "MultibeamUTM4N.nc"
opt$aspectL = "GebcoAspectWUTM4N.nc"
opt$aspectM = "FalkorAspectUTM4N.nc"
opt$aspectH = "MultibeamAspectUTM4N.nc"
opt$slopeL = "GebcoSlopeWUTM4N.nc"
opt$slopeM = "FalkorSlopeUTM4N.nc"
opt$slopeH = "MultibeamSlopeUTM4N.nc"

## read in position file
position = read.csv(paste0("SSM/", TagFile), header=T) 
## review data
str(position)

## format datetime
#position = within(position, DateTime <- as.POSIXct(datetimeUTC, tz = "UTC"))
position = within(position, DateTime <- as.POSIXct(date, tz = "UTC"))
# position = within(position, DateTime <- as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))

## for crawl data, change name of longitude column in data 
position <- position %>%
  dplyr::rename(
    animal = deployid,
    longitud = longitude
  )

position <- dplyr::filter(position, locType == "p")
position = subset (position, animal == Animal) # subset

## read in behavior file
behavior = read.csv(paste0("Dive behavior/", BehaviorFile), header=T, stringsAsFactors = F)
behavior = read.csv("Dive behavior/TtTag034_173076-Behavior_corrected.csv")
behavior = behavior[behavior$What != "Message", ] # remove message records

## format and add columns to be populated 
behavior = subset(behavior, select = -c(Number.1, Shape.1, DepthMin.1, DepthMax.1, DurationMin.1, DurationMax.1,
                                       Number.2, Shape.2, DepthMin.2, DepthMax.2, DurationMin.2, DurationMax.2)) # if have extra columns
behavior$Start = as.POSIXct(gsub("\\.5", "", behavior$Start), tz=TZ, format="%H:%M:%S %d-%b-%Y")
behavior$End = as.POSIXct(gsub("\\.5", "", behavior$End), tz=TZ, format="%H:%M:%S %d-%b-%Y")
#behavior$Start = as.POSIXct(behavior$Start, tz=TZ)
#behavior$End = as.POSIXct(behavior$End, tz=TZ)
#behavior = subset(behavior, DeployID == Animal)
behavior$latitude = NA
behavior$longitud = NA
behavior$DistToShore = NA
behavior$DistTo200m = NA
behavior$Source = NA
behavior$depth = NA
behavior$aspect = NA
behavior$slope = NA
behavior$datetimeUTC = as.POSIXct(format(behavior$Start, tz="UTC"), tz="UTC")	#These two line have to be set depending on whether the behaviour data 
behavior$datetimeHST = as.POSIXct(format(behavior$Start, tz="Pacific/Honolulu"), tz="Pacific/Honolulu")				#is local time or UTC.
behavior$sunrise <- behavior$datetimeHST
behavior$sunset <- behavior$datetimeHST
behavior$solarNoon <- behavior$datetimeHST
behavior$civilDawn <- behavior$datetimeHST
behavior$endDawn <- behavior$datetimeHST
behavior$isDawn = F
behavior$civilDusk <- behavior$datetimeHST
behavior$startDusk <- behavior$datetimeHST
behavior$isDusk = F
behavior$secNearestSunriseset = as.integer(0)
behavior$sunAzimuth <- NA
behavior$sunAltitude = NA
behavior$moonAzimuth = NA
behavior$moonAltitude = NA
behavior$moonIlluminatedFraction = NA
behavior$moonPhase = NA
#behavior$mfa = NA
behavior$isDawn = as.logical(NA)
behavior$isDusk = as.logical(NA)
behavior$isDay = as.logical(NA)

## for MFAS studies 
# mfa = read.csv("mfablocks.csv", stringsAsFactors = F, colClass = c("start"="numeric", "stop"="numeric", "datetimeStart"="POSIXct", "datetimeStop"="POSIXct"))
# mfa$datetimeStart = force_tz(mfa$datetimeStart, tz = "UTC")
# mfa$datetimeStop = force_tz(mfa$datetimeStop, tz = "UTC")
# mfablocks = interval(mfa$datetimeStart, mfa$datetimeStop)

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

## Import raster files 

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

## First for loop: work way through position file 
for (i in 1:(nrow(position)-1)) {         
  TagID = position[i, "ptt"]
  StartDate = position[i, "DateTime"]
  StartLat = position[i, "latitude"]
  StartLon = position[i, "longitud"]
	StartPt = c(StartLon, StartLat)
	EndDate = position[i+1, "DateTime"]
	EndLat = position[i+1, "latitude"]
	EndLon = position[i+1, "longitud"]
	EndPt = c(EndLon, EndLat)

	Bearing = bearing(StartPt, EndPt)
	Distance = distVincentyEllipsoid(StartPt, EndPt)
	ElapsedTime = as.numeric(as.duration(int_diff(c(StartDate, EndDate))))
	Speed = Distance / ElapsedTime
        
        
	for (d in 1:nrow(behavior)) {
	  cat("\r", i, " ", d, " a")      # output diagnostic count to the console
	
		DiveTagID = behavior[d, "Ptt"]
		DiveStart = behavior[d, "Start"]
		DiveEnd = behavior[d, "End"]

		if (DiveStart < StartDate)      # The dive is not within this segment
			next

		if (DiveStart > EndDate)        # The dive is in the next segment
			break

		cat("\bb")
		# The dive is within the segment
		# so add the lat and lon to the record
		Distance = as.numeric(as.duration(int_diff(c(StartDate, DiveStart)))) * Speed

		cat("\bc")
		p2 <- destPoint(StartPt, Bearing, Distance)
		cat("\bd")
		behavior[d, "latitude"] = p2[2]
		behavior[d, "longitud"] = p2[1]
		posmatrix =  matrix(c(behavior[d, "longitud"], behavior[d, "latitude"]), nrow=1)
		cat("\be")
		lon = behavior[d, "longitud"]
		lat = behavior[d, "latitude"]
	  pos = st_point(c(lon,lat))
	  sfc = st_sfc(pos)
	  pos = st_sf(data.frame(geom=sfc), crs = "+init=EPSG:4326")
	  pos = st_transform(pos, 3750)
		dist = st_distance(pos, shore, by_element = T)
		behavior[d, "DistToShore"] = min(dist)
		behavior[d, "DistTo200m"] = min(st_distance(pos, d200m))
		cat("\bf")
		pos <- st_transform(pos, crs = projection(depthH)) # better to reproject pos to projection of depth raster here
		cat("\bg")
	  depth = raster::extract(depthH, pos, method = "bilinear")
	  if (!is.na(depth)) {
		cat("\bh")
	    behavior[d, "depth"] = depth
	    behavior[d, "Source"] = "50m HMRG multibeam"
	    behavior[d, "aspect"] = raster::extract(aspectH, pos, method = "bilinear")
	    behavior[d, "slope"] = raster::extract(slopeH, pos, method = "bilinear")
	  }
	  else {
		cat("\bi")
	    depth = raster::extract(depthM, pos, method = "bilinear")
		cat("\bj")
	    if (!is.na(depth)) {
		cat("\bk")
	      behavior[d, "depth"] = depth
	      behavior[d, "Source"] = "60m Falkor multibeam"
	      behavior[d, "aspect"] = raster::extract(aspectM, pos, method = "bilinear")
	      behavior[d, "slope"] = raster::extract(slopeM, pos, method = "bilinear")
	    }
	    else {
		cat("\bl")
	      depth = raster::extract(depthL, pos, method = "bilinear")
		cat("\bm")
	      if (!is.na(depth)) {
		cat("\bn")
	        behavior[d, "depth"] = depth
	        behavior[d, "Source"] = "30 arc sec GEBCO"
	        behavior[d, "aspect"] = raster::extract(aspectL, pos, method = "bilinear")
	        behavior[d, "slope"] = raster::extract(slopeL, pos, method = "bilinear")
	      }
	    }
	  }
		cat("\bo")
		datetimeHST = behavior[d, "datetimeHST"]
		behavior[d, "sunrise"] = as.POSIXct(sunriset(posmatrix, datetimeHST, direction="sunrise", POSIXct.out=T)$time)
		cat("\bp")
		sunrise = behavior[d, "sunrise"]
		behavior[d, "sunset"] = as.POSIXct(sunriset(posmatrix, datetimeHST, direction="sunset", POSIXct.out=T)$time)
		cat("\bq")
		sunset = behavior[d, "sunset"]
		behavior[d, "solarNoon"] = as.POSIXct(solarnoon(posmatrix, datetimeHST, POSIXct.out=T)$time)
		cat("\br")
		solarNoon = behavior[d, "solarNoon"]
		behavior[d, "civilDawn"] = as.POSIXct(crepuscule(posmatrix, datetimeHST, solarDep = 6, direction="dawn", POSIXct.out=T)$time)
		behavior[d, "endDawn"] = as.POSIXct(crepuscule(posmatrix, datetimeHST, solarDep = -6, direction="dawn", POSIXct.out=T)$time)
		behavior[d, "civilDusk"] = as.POSIXct(crepuscule(posmatrix, datetimeHST, solarDep = 6, direction="dusk", POSIXct.out=T)$time)
		behavior[d, "startDusk"] = as.POSIXct(crepuscule(posmatrix, datetimeHST, solarDep = -6, direction="dusk", POSIXct.out=T)$time)

		lastmidnight = solarNoon - dhours(12)
		nextmidnight = solarNoon + dhours(12)

		if (datetimeHST < lastmidnight) {
			behavior[d, "secNearestSunriseset"] = as.integer(0 - as.duration(interval((datetimeHST + (2 * as.duration(interval(datetimeHST, lastmidnight)))), sunrise)))
		}
		else if (datetimeHST < sunrise) {
			behavior[d, "secNearestSunriseset"] = as.integer(0 - as.duration(interval(datetimeHST, sunrise)))
		}
		else if (datetimeHST < solarNoon) {
			behavior[d, "secNearestSunriseset"] = as.integer(as.duration(interval(sunrise, datetimeHST)))
		}
		else if (datetimeHST < sunset) {
			behavior[d, "secNearestSunriseset"] = as.integer(as.duration(interval(datetimeHST, sunset)))
		}
		else if (datetimeHST < nextmidnight) {
			behavior[d, "secNearestSunriseset"] = as.integer(0 - as.duration(interval(sunset, datetimeHST)))
		}
		else {
			behavior[d, "secNearestSunriseset"] = as.integer(0 - as.duration(interval(sunset, (datetimeHST - (2 * as.duration(interval(nextmidnight, datetimeHST)))))))
		}

		sunangle = oce::sunAngle(behavior[d, "datetimeUTC"], behavior[d, "longitud"], behavior[d, "latitude"])
		behavior[d, "sunAzimuth"] = sunangle$azimuth
		behavior[d, "sunAltitude"] = sunangle$altitude
		moonangle = oce::moonAngle(behavior[d, "datetimeUTC"], behavior[d, "longitud"], behavior[d, "latitude"])
		behavior[d, "moonAzimuth"] = moonangle$azimuth
		behavior[d, "moonAltitude"] = moonangle$altitude
		behavior[d, "moonIlluminatedFraction"] = moonangle$illuminatedFraction
		behavior[d, "moonPhase"] = lunar.phase(behavior[d, "datetimeUTC"], name = T)
		#behavior[d, "mfa"] = any(behavior[d, "datetimeUTC"] %within% mfablocks)
	}
}

## add dawn, dusk, day variables
behavior$isDawn = (behavior$datetimeHST >= behavior$civilDawn) & (behavior$datetimeHST <= behavior$endDawn)
behavior$isDusk = (behavior$datetimeHST >= behavior$startDusk) & (behavior$datetimeHST <= behavior$civilDusk)
behavior$isDay = (behavior$datetimeHST >= behavior$sunrise) & (behavior$datetimeHST <= behavior$sunset)


## next for loop that splits surface periods/blocks that spanned sunrise or sunset into 2 records,
## as to have one for each day/night. appropriate time/duration is allotted for each record.
## e.g., first record's End time would become the sunrise/sunset time, and a new record will be 
## created starting with sunrise/sunset and ending with the original End time for that record. 
## This is only done for SURFACE periods.
bh = behavior[0,]

for (i in 1:nrow(behavior)) {
  cat("\r", i, "                     ")
  bh = rbind(bh, behavior[i, ])
  dt0 = behavior[i, "datetimeHST"]
  dt1 = as.POSIXct(format(behavior[i, "End"], tz="Pacific/Honolulu"), tz="Pacific/Honolulu")
  sr = behavior[i, "sunrise"]
  ss = behavior[i, "sunset"]
  sunr = F
  if ((dt0 < sr) && (dt1 > sr)) {           # If start < sunrise, and end > sunrise, sunrise is in this segment
    sunr = T
    cat("\r", i, " Sunrise ", nrow(bh))
    bh[nrow(bh), "End"] = as.POSIXct(format(behavior[i, "sunrise"], tz="UTC"), tz="UTC") # make end time record = sunrise
    bh = rbind(bh, behavior[i, ]) # bind back to behavior
    bhrow = nrow(bh)
    bh[bhrow, "Start"] = as.POSIXct(format(behavior[i, "sunrise"], tz="UTC"), tz="UTC") # start of split record = sunrise 
    bh[bhrow, "What"] = "Surface" # surface periods only
    bh[bhrow, "Number"] = NA
    bh[bhrow, "Shape"] = ""
    bh[bhrow, "DepthMin"] = NA
    bh[bhrow, "DepthMax"] = NA
    bh[bhrow, "DurationMin"] = NA
    bh[bhrow, "DurationMax"] = NA
    bh[bhrow, "Shallow"] = NA
    bh[bhrow, "Deep"] = NA
    bh[bhrow, "latitude"] = NA
    bh[bhrow, "longitud"] = NA
    bh[bhrow, "DistToShore"] = NA
    bh[bhrow, "DistTo200m"] = NA
    bh[bhrow, "depth"] = NA
    bh[bhrow, "aspect"] = NA
    bh[bhrow, "slope"] = NA
    bh[bhrow, "datetimeUTC"] = as.POSIXct(format(behavior[i, "sunrise"], tz="UTC"), tz="UTC")
    bh[bhrow, "datetimeHST"] = behavior[i, "sunrise"]
    bh[bhrow, "isDusk"] = F
    bh[bhrow, "secNearestSunriseset"] = 0
    bh[bhrow, "sunAzimuth"] = NA
    bh[bhrow, "sunAltitude"] = NA
    bh[bhrow, "moonAzimuth"] = NA
    bh[bhrow, "moonAltitude"] = NA
    bh[bhrow, "moonIlluminatedFraction"] = NA
    bh[bhrow, "moonPhase"] = NA
    bh[bhrow, "isDay"] = T
    behavior[i, ] = bh[bhrow, ]
  } 
  if ((dt0 < ss) && (dt1 > ss)) {    # if start < sunset, and end > sunset, sunset is in this segment
    cat("\r", i, " Sunset ", nrow(bh))
    bh[nrow(bh), "End"] = as.POSIXct(format(behavior[i, "sunset"], tz="UTC"), tz="UTC")
    bh = rbind(bh, behavior[i, ])
    bhrow = nrow(bh)
    bh[bhrow, "Start"] = as.POSIXct(format(behavior[i, "sunset"], tz="UTC"), tz="UTC")
    if (sunr) {
      bh[bhrow - 1, "End"] = bh[bhrow, "Start"]
    }
    bh[bhrow, "What"] = "Surface"
    bh[bhrow, "Number"] = NA
    bh[bhrow, "Shape"] = ""
    bh[bhrow, "DepthMin"] = NA
    bh[bhrow, "DepthMax"] = NA
    bh[bhrow, "DurationMin"] = NA
    bh[bhrow, "DurationMax"] = NA
    bh[bhrow, "Shallow"] = NA
    bh[bhrow, "Deep"] = NA
    bh[bhrow, "latitude"] = NA
    bh[bhrow, "longitud"] = NA
    bh[bhrow, "DistToShore"] = NA
    bh[bhrow, "DistTo200m"] = NA
    bh[bhrow, "depth"] = NA
    bh[bhrow, "aspect"] = NA
    bh[bhrow, "slope"] = NA
    bh[bhrow, "datetimeUTC"] = as.POSIXct(format(behavior[i, "sunset"], tz="UTC"), tz="UTC")
    bh[bhrow, "datetimeHST"] = behavior[i, "sunset"]
    bh[bhrow, "isDusk"] = F
    bh[bhrow, "secNearestSunriseset"] = 0
    bh[bhrow, "sunAzimuth"] = NA
    bh[bhrow, "sunAltitude"] = NA
    bh[bhrow, "moonAzimuth"] = NA
    bh[bhrow, "moonAltitude"] = NA
    bh[bhrow, "moonIlluminatedFraction"] = NA
    bh[bhrow, "moonPhase"] = NA
    bh[bhrow, "isDay"] = F
  }
}

## calculation record durations
bh$duration = as.duration(bh$Start %--% bh$End)
bh$durationSecs = as.integer(bh$duration)
bh$durationDays = bh$duration / ddays(1)

## save file 
write.csv(bh, paste0("GIS output/Dive pseudotracks/",Animal, "_BehPos_crawl_5minStep_FixedPath_",
                     format(Sys.time(), "%Y%m%d"), ".csv"), row.names=F)


