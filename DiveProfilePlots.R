####################################################################################
#
#
#       Make dive profile plots from dive behavior file
#
#       Original code by David Anderson
#       Edited code (this) by Michaela A. Kratofil
#
#       02 OCT 2020
#
#####################################################################################

#### DESCRIPTION ####

# This script takes the dive behavior file for a tag, (SPLASH-10,
# MK10) which is downloaded from the Wildlife Computers Portal,
# and plots profile of the dives over the duration of the deployment
# and for each 24 hour period, shading night periods.

#### How it works ####

#   1. A function is made to plot the dives per input from the behavior files
#       (these are the -Behavior.csv files).
#           A. DivePlotAll plots all dives over entire deployment
#           B. DivePlotDay plots dives over a specified time period
#               1. Are nearly identical, just a couple of aesthetics changed
#   2. The input file is imported
#   3. The sunrise/sunset times for each day are calculated to determine the 
#       start and ends of the shaded boxes in the plot.
#   4. Code included to plot the entire deployment and a single 24 hour period,
#       which is specified by function arguments
#   5. Code at bottom is provided to save the dive/surface files with times in HST
#       to check plots against the data.

######################################################################################
#               USER BEWARE!  USER BEWARE!  USER BEWARE!  USER BEWARE!  USER BEWARE! 
#
# I have done all that I can think of to check for errors in this code, but that does
# not mean that it is completely bug-free. I highly recommend checking plots agains the 
# raw dive files to make sure the start/end times of dives and shaded boxes align. I
# also advise to check time zones of objects frequently using the tz() function, as this 
# can screw things up. 
#
######################################################################################

# load libraries 
require (ggplot2)
require (lubridate)
require (gridExtra)
require (scales)
library(suncalc)
library(tidyr)
library(dplyr)
library(gganimate)

#### make function to plot all dives that occurred during the entire duration of the deployment ####
# *** NOTE *** If tag was programmed in HST, make sure to change, filetz = "Pacific/Honolulu"
DivePlotAll = function(behavior,
                       title = NULL,
                       gapcolor = "black",
                       plotcolor = "black",
                       filetz = "UTC",
                       plottz = "Pacific/Honolulu",
                       depths = c(-1500, -1000, -500, -250, 0),
                       depthlab = c("1500", "1000", "-500", "-250", "0"),
                       depthlim = NULL,
                       datelim = NULL,
                       nights = NULL,
                       datebreaks = "2 days",
                       lineweight = 0.2,
                       gaps = T,
                       grid = T) {
  
  
  
  behavior$Start = with_tz(as.POSIXct(gsub("\\.5", "", behavior$Start), tz=filetz, format="%H:%M:%S %d-%b-%Y"), tzone="Pacific/Honolulu")
  behavior$End = with_tz(as.POSIXct(gsub("\\.5", "", behavior$End), tz=filetz, format="%H:%M:%S %d-%b-%Y"), tzone="Pacific/Honolulu")
  behavior$DepthMin = 0 - behavior$DepthMin
  messages = subset(behavior, What == "Message")
  surface = subset(behavior, What == "Surface")
  dive = subset(behavior, What == "Dive")
  
  p = ggplot(data = behavior, aes(Start, DepthMin)) + theme_bw() +
    theme(#axis.title.x = element_blank(), 
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.border = element_rect(colour = "black", fill=NA, size=1.5)) +
    coord_cartesian(xlim = datelim)
  if (!grid) {
    p = p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  }
  
  if (!is.null(nights)) {
    if (gaps) {
      p = p + geom_rect(data = nights, mapping = aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = 0),
                        fill = "grey", alpha = 0.5, inherit.aes = F)
    } else {
      p = p + geom_rect(data = nights, mapping = aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                        fill = "grey", alpha = 0.5, inherit.aes = F)
    }
  }
  
  if (!is.null(title)) {
    p = p + ggtitle(title)
  }
  
  datetime = with_tz(as.POSIXct(NA), tzone="Pacific/Honolulu")
  depth = as.numeric(NA)
  grp = as.numeric(NA)
  
  ### NEED TO BE MORE EXPLICIT WITH TIME ZONES AND CONCATENATE.
  ## If you do not tell c() what time zone each element should be in,
  ## it will default to that on on your machine. In most cases this isn't an
  ## issue because we've already set the time zone for the Start column
  ## However as soon as you convert a duration to a numeric and attempt to add
  ## things get panicky. Be explicit in each case and all is Hawaii time
  for (i in 1:nrow(dive)) {
    DiveStart = dive[i, "Start"]
    DiveDur = as.numeric(as.duration(int_diff(c(dive[i, "Start"], dive[i, "End"]))))
    Depth = dive[i, "DepthMin"]
    if (dive[i, "Shape"] == "V") {
      ## ADDED WITH_TZ
      datetime = with_tz(c(datetime, DiveStart, DiveStart + (DiveDur * 0.5), DiveStart + DiveDur), tzone="Pacific/Honolulu")
      depth = c(depth, 0, Depth, 0)
      grp = c(grp, i, i, i)
    } else if (dive[i, "Shape"] == "U") {
      ## ADDED WITH_TZ
      datetime = with_tz(c(datetime, DiveStart, DiveStart + (DiveDur * 0.25), DiveStart + (DiveDur * 0.75), DiveStart + DiveDur), tzone="Pacific/Honolulu")
      depth = c(depth, 0, Depth, Depth, 0)
      grp = c(grp, i, i, i, i)
    } else if (dive[i, "Shape"] == "Square") {
      ## ADDED WITH_TZ
      datetime = with_tz(c(datetime, DiveStart, DiveStart + (DiveDur * 0.4), DiveStart + (DiveDur * 0.6), DiveStart + DiveDur), tzone="Pacific/Honolulu")
      depth = c(depth, 0, Depth, Depth, 0)
      grp = c(grp, i, i, i, i)
    }
  }
  
  dive.df = data.frame(datetime, depth, grp)
  p = p + geom_line(data = dive.df, mapping = aes(datetime, depth, group =  grp), size = lineweight, color = plotcolor)
  
  datetime = with_tz(as.POSIXct(NA), tzone="Pacific/Honolulu")
  depth = as.numeric(NA)
  grp = as.numeric(NA)
  
  for (i in 1:nrow(surface)) {
    datetime = with_tz(c(datetime, surface[i, "Start"], surface[i, "End"]), tzone="Pacific/Honolulu")
    depth = c(depth, 0, 0)
    grp = c(grp, i, i)
  }
  
  surface.df = data.frame(datetime, depth, grp)
  p = p + geom_line(data = surface.df, mapping = aes(datetime, depth, group =  grp), size = lineweight, color = plotcolor)
  
  if (is.null(depthlim)) {
    depth_lower = min(dive$DepthMin) * 1.15
  } else {
    depth_lower = depthlim
  }
  if (gaps) {
    depth_upper = abs(depth_lower) * 0.15
  } else {
    depth_upper = 0
  }
  
  xmin = with_tz(as.POSIXct(NA), tzone="Pacific/Honolulu")
  xmax = with_tz(as.POSIXct(NA), tzone="Pacific/Honolulu")
  
  gapCheck = 0
  for (i in 2:nrow(messages)) {
    
    dt <- difftime( messages[i-1, "End"], messages[i, "Start"], units = 'secs')
    ## Need an indicator that avoids plotting gaps if none exist
    ## Otherwise you get screwed with gaps = TRUE
    if (!is.na(dt)) {
      if (dt < -60) {
        xmin = c(xmin, messages[i-1, "End"])
        xmax = c(xmax, messages[i, "Start"])
        ymin = abs(depth_lower) * 0.05
        ymax = abs(depth_lower) * 0.15
        ## Update if gaps were found
        gapCheck = gapCheck + 1
      }
    }
  }
  
  gapCheck <- ifelse(gapCheck > 0, 1, 0) 
  
  
  ## If you are plotting gaps
  if (gaps & gapCheck) {
    gaps.df = data.frame(xmin, xmax, ymin, ymax)
    gaps.df = gaps.df[!is.na(gaps.df$xmin), ]
    p = p + geom_rect(data = gaps.df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                      fill = gapcolor, inherit.aes = F) +
      scale_y_continuous(breaks = c(depths, mean(c(ymin, ymax))),
                         labels = c(depthlab, "Gaps"),
                         limits = c(depth_lower, depth_upper)) +
      scale_x_datetime(labels = date_format("%Y-%m-%d", tz = "Pacific/Honolulu"), breaks = date_breaks(datebreaks),
                       expand = c(0,0)) +
      ylab("Depth (m)")
  } else {
    p = p + scale_y_continuous(breaks = depths,
                               labels = depthlab,
                               limits = c(depth_lower, depth_upper)) +
      scale_x_datetime(labels = date_format("%Y-%m-%d", tz = "Pacific/Honolulu"), breaks = date_breaks(datebreaks),
                       expand = c(0,0)) +
      ylab("Depth (m)")
  }
  return(p)
}

#### make a function to plot all dives during a specified time period (usually 24 hr) ####
DivePlotDay = function(behavior,
                       title = NULL,
                       gapcolor = "black",
                       plotcolor = "black",
                       filetz = "UTC",
                       plottz = "Pacific/Honolulu",
                       depths = c(-1500, -1000, -500, -250, 0),
                       depthlab = c("-1500", "-1000", "-500", "-250", "0"),
                       depthlim = NULL,
                       datelim = NULL,
                       nights = NULL,
                       datebreaks = "2 days",
                       lineweight = 0.2,
                       gaps = T,
                       grid = T) {
  
  behavior$Start = with_tz(as.POSIXct(gsub("\\.5", "", behavior$Start), tz=filetz, format="%H:%M:%S %d-%b-%Y"), tzone="Pacific/Honolulu")
  behavior$End = with_tz(as.POSIXct(gsub("\\.5", "", behavior$End), tz=filetz, format="%H:%M:%S %d-%b-%Y"), tzone="Pacific/Honolulu")
  behavior$DepthMin = 0 - behavior$DepthMin
  messages = subset(behavior, What == "Message")
  surface = subset(behavior, What == "Surface")
  dive = subset(behavior, What == "Dive")
  
  p = ggplot(data = behavior, aes(Start, DepthMin)) + theme_bw() +
    theme(#axis.title.x = element_blank(), # comment out if want to have x axis titles for daily/24 hour plots
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.border = element_rect(colour = "black", fill=NA, size=1.5)) +
    coord_cartesian(xlim = datelim)
  if (!grid) {
    p = p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  }
  
  if (!is.null(nights)) {
    if (gaps) {
      p = p + geom_rect(data = nights, mapping = aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = 0),
                        fill = "grey", alpha = 0.5, inherit.aes = F)
    } else {
      p = p + geom_rect(data = nights, mapping = aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                        fill = "grey", alpha = 0.5, inherit.aes = F)
    }
  }
  
  if (!is.null(title)) {
    p = p + ggtitle(title)
  }
  
  datetime = with_tz(as.POSIXct(NA), tzone="Pacific/Honolulu")
  depth = as.numeric(NA)
  grp = as.numeric(NA)
  
  ### NEED TO BE MORE EXPLICIT WITH TIME ZONES AND CONCATENATE.
  ## If you do not tell c() what time zone each element should be in,
  ## it will default to that on on your machine. In most cases this isn't an
  ## issue because we've already set the time zone for the Start column
  ## However as soon as you convert a duration to a numeric and attempt to add
  ## things get panicky. Be explicit in each case and all is Hawaii time
  for (i in 1:nrow(dive)) {
    DiveStart = dive[i, "Start"]
    DiveDur = as.numeric(as.duration(int_diff(c(dive[i, "Start"], dive[i, "End"]))))
    Depth = dive[i, "DepthMin"]
    if (dive[i, "Shape"] == "V") {
      ## ADDED WITH_TZ
      datetime = with_tz(c(datetime, DiveStart, DiveStart + (DiveDur * 0.5), DiveStart + DiveDur), tzone="Pacific/Honolulu")
      depth = c(depth, 0, Depth, 0)
      grp = c(grp, i, i, i)
    } else if (dive[i, "Shape"] == "U") {
      ## ADDED WITH_TZ
      datetime = with_tz(c(datetime, DiveStart, DiveStart + (DiveDur * 0.25), DiveStart + (DiveDur * 0.75), DiveStart + DiveDur), tzone="Pacific/Honolulu")
      depth = c(depth, 0, Depth, Depth, 0)
      grp = c(grp, i, i, i, i)
    } else if (dive[i, "Shape"] == "Square") {
      ## ADDED WITH_TZ
      datetime = with_tz(c(datetime, DiveStart, DiveStart + (DiveDur * 0.4), DiveStart + (DiveDur * 0.6), DiveStart + DiveDur), tzone="Pacific/Honolulu")
      depth = c(depth, 0, Depth, Depth, 0)
      grp = c(grp, i, i, i, i)
    }
  }
  
  dive.df = data.frame(datetime, depth, grp)
  p = p + geom_line(data = dive.df, mapping = aes(datetime, depth, group =  grp), size = lineweight, color = plotcolor)
  
  datetime = with_tz(as.POSIXct(NA), tzone="Pacific/Honolulu")
  depth = as.numeric(NA)
  grp = as.numeric(NA)
  
  for (i in 1:nrow(surface)) {
    datetime = with_tz(c(datetime, surface[i, "Start"], surface[i, "End"]), tzone="Pacific/Honolulu")
    depth = c(depth, 0, 0)
    grp = c(grp, i, i)
  }
  
  surface.df = data.frame(datetime, depth, grp)
  p = p + geom_line(data = surface.df, mapping = aes(datetime, depth, group =  grp), size = lineweight, color = plotcolor)
  
  if (is.null(depthlim)) {
    depth_lower = min(dive$DepthMin) * 1.15
  } else {
    depth_lower = depthlim
  }
  if (gaps) {
    depth_upper = abs(depth_lower) * 0.15
  } else {
    depth_upper = 0
  }
  
  xmin = with_tz(as.POSIXct(NA), tzone="Pacific/Honolulu")
  xmax = with_tz(as.POSIXct(NA), tzone="Pacific/Honolulu")
  
  gapCheck = 0
  for (i in 2:nrow(messages)) {
    
    dt <- difftime( messages[i-1, "End"], messages[i, "Start"], units = 'secs')
    ## Need an indicator that avoids plotting gaps if none exist
    ## Otherwise you get screwed with gaps = TRUE
    if (!is.na(dt)) {
      if (dt < -60) {
        xmin = c(xmin, messages[i-1, "End"])
        xmax = c(xmax, messages[i, "Start"])
        ymin = abs(depth_lower) * 0.05
        ymax = abs(depth_lower) * 0.15
        ## Update if gaps were found
        gapCheck = gapCheck + 1
      }
    }
  }
  ## If you are plotting 
  if (gaps & gapCheck) {
    gaps.df = data.frame(xmin, xmax, ymin, ymax)
    gaps.df = gaps.df[!is.na(gaps.df$xmin), ]
    p = p + geom_rect(data = gaps.df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                      fill = gapcolor, inherit.aes = F) +
      scale_y_continuous(breaks = c(depths, mean(c(ymin, ymax))),
                         labels = c(depthlab, "Gaps"),
                         limits = c(depth_lower, depth_upper)) +
      scale_x_datetime(labels = date_format("%H:%M", tz = "Pacific/Honolulu"), breaks = date_breaks(datebreaks),
                       expand = c(0,0)) +
      ylab("Depth (m)")
  } else {
    p = p + scale_y_continuous(breaks = depths,
                               labels = depthlab,
                               limits = c(depth_lower, depth_upper)) +
      scale_x_datetime(labels = date_format("%H:%M", tz = "Pacific/Honolulu"), breaks = date_breaks(datebreaks),
                       expand = c(0,0)) +
      ylab("Depth (m)")
  }
  return(p)
}

#### Read in behavior file and process ####
Tag = "GmTag231"
ptt = "180166"
behavior <- read.csv("Dive behavior/GmTag231_180166_DAP-Behavior_Corrected.csv", header=T, stringsAsFactors = F)
behavior$DepthMin <- behavior$DepthMin*2
behavior$DepthMax <- behavior$DepthMax*2

## Get sunrise and sunset times from suncalc package using Argos locations ##
## OR obtain from dive pseudotrack GIS file for tag ##
LocsFile <- read.csv("Douglas Filtered/GmTag004-231_DouglasFiltered_ArgosOnly_r15d3lc2_2020JULv3.csv", header = T)
locs <- filter(LocsFile, animal == "GmTag231") # filter out tag (Argos only locs here)

# format datetimes
locs$dateUTC <- as.POSIXct(locs$date, tz = "UTC")
locs$dateHST <- with_tz(locs$dateUTC, tzone = "Pacific/Honolulu")
locs$date_ymd <- as.Date(locs$dateUTC, format = "%Y%m%d")

# compute average lat/lon for each day for suncalc
sunriseset <- locs %>%
  select(latitude, longitud, date_ymd) %>%
  group_by(date_ymd) %>%
  summarise(mean(latitude), mean(longitud)) %>%
  rename(lat = "mean(latitude)", lon = "mean(longitud)")

colnames(sunriseset)[colnames(sunriseset) == "date_ymd"] <- "date"

# get sunrise/sunset times
sunriseset <- getSunlightTimes(data = sunriseset, keep = c("sunrise", "sunset"), tz = "Pacific/Honolulu")

# subset locations to plot with dives if want
locs$dayHST <- day(locs$dateHST)
locs_sub <- locs[locs$dayHST %in% c(17:25),]

## Specify xmin and xmax for "nights" in function (shaded boxes). 
## The first value/day for sunrise times needs to be commented out for the math
## to work correctly. 
xmin = as.POSIXct(c(
         "2020-02-12 18:35:00",
         "2020-02-13 18:36:31",
         "2020-02-14 18:37:16",
         "2020-02-15 18:38:03",
         "2020-02-16 18:38:21",
         "2020-02-17 18:38:26",
         "2020-02-18 18:38:00",
         "2020-02-19 18:37:59",
         "2020-02-20 18:38:25",
         "2020-02-21 18:40:36",
         "2020-02-22 18:41:04"),
         tz = "Pacific/Honolulu") 
xmax = as.POSIXct(c(
         #"2020-02-12 07:13:42",
         "2020-02-13 07:13:58",
         "2020-02-14 07:13:31",
         "2020-02-15 07:13:08",
         "2020-02-16 07:12:07",
         "2020-02-17 07:11:08",
         "2020-02-18 07:10:01",
         "2020-02-19 07:09:07",
         "2020-02-20 07:08:20",
         "2020-02-21 07:08:33",
         "2020-02-22 07:07:49",
         with_tz(as.POSIXct(NA), tzone="Pacific/Honolulu")),
         tz = "Pacific/Honolulu") 

# create nights dataframe for plot function
nights = data.frame(xmin, xmax)

### plot dives over entire deployment ###

# specify folder to save plots to #
plot_folder = "Plots/Dive plots/"

# PcTag026
GmTag231.all = DivePlotAll(behavior, title = "GmTag231 Complete", gaps = T, plotcolor = "blue", depthlim = -1500,
                        datebreaks = "1 day", nights = nights, filetz = "UTC", grid = F,
                        gapcolor = "black", depthlab = c("-1500","-1000","-500","-250","0"))


GmTag231.all =  GmTag231.all + theme(axis.text=element_text(size=11, colour = "black"),
                                    axis.title=element_text(size=12)) + xlab("")
GmTag231.all

ggsave(paste0(plot_folder, Tag, "_AllDives_2020Oct2.jpg"), GmTag231.all, device = "jpeg", units = "in", width = 8, height = 4, dpi = 300)

### plot dives for each 24 hour period ##
# change the datelim = to the two dates between 24 hour period
GmTag231.1 = DivePlotDay(behavior, title = "GmTag231, 2/21 - 2/22", gapcolor = "black", plotcolor = "blue", depthlim = -1500,
                      datebreaks = "2 hour", nights = nights,
                      datelim = as.POSIXct(c("2020-02-21 12:00:00", "2020-02-22 12:00:00"), tz = "Pacific/Honolulu"),
                      filetz = "UTC",
                      gaps = T,
                      grid = F, lineweight = 0.3)

GmTag231.1 = GmTag231.1 + theme(axis.text.y=element_text(size=8, colour = "black"),
                                axis.title=element_text(size=12),
                                axis.text.x = element_text(size = 8, colour = "black"))

GmTag231.1 = GmTag231.1 + xlab("Time (HST)")

GmTag231.1

ggsave(paste0(plot_folder, Tag, "_Dives", "2020FEB21-22.jpg"), GmTag231.1, device = "jpeg", units = "in", width = 8,
       height = 4, dpi = 300)


# write csv that includes Start dives in HST to cross reference with plots #
tdives <- behavior[behavior$What == "Dive",]
tdives$Start <- as.POSIXct(tdives$Start, format = "%H:%M:%S %d-%b-%Y", tz = "UTC")
tdives$End <- as.POSIXct(tdives$End, format = "%H:%M:%S %d-%b-%Y", tz = "UTC")
tdives$StartHST <- with_tz(tdives$Start, tzone = "Pacific/Honolulu")

tsurface <- behavior[behavior$What == "Surface",]
tsurface$Start <- as.POSIXct(tsurface$Start, format = "%H:%M:%S %d-%b-%Y", tz = "UTC")
tsurface$End <- as.POSIXct(tsurface$End, format = "%H:%M:%S %d-%b-%Y", tz = "UTC")
tsurface$StartHST <- with_tz(tsurface$Start, tzone = "Pacific/Honolulu")
tsurface$EndHST <- with_tz(tsurface$End, tzone = "Pacific/Honolulu")

write.csv(tsurface, "GmTag231_Behavior_SurfaceOnly_wHST.csv", row.names = F)
write.csv(tdives, "GmTag231_Behavior_DivesOnly_wHST.csv", row.names = F)
