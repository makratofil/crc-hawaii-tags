## DiveSeriesCheck.R: Check time series depth values against behavior data
## depth values (these should be relatively the same).

## For more information on this assessment read the "CRC Tag Deploy SOP" 

## Author: Michaela A. Kratofil, Cascadia Research
## Updated: 16 Feb 2021

#### DESCRIPTION #### ======================================================== ##

# This script takes the dive behavior file for a tag, (SPLASH-10,
# MK10) which is downloaded from the Wildlife Computers Portal,
# and plots profile of the dives over the duration of the deployment
# and for each 24 hour period, shading night periods.

#### How it works ####

#   1. A function is made to plot the dives per input from the behavior files
#      ,depth points from time series files, and shades for the summary periods.
#           A. DivePlotAll plots all dives over entire deployment
#           B. DivePlotDay plots dives over a specified time period
#               1. Are nearly identical, just a couple of aesthetics changed
#   2. The input file is imported
#   3. Code included to plot the entire deployment and a single 24 hour period,
#       which is specified by function arguments

######################################################################################
#               USER BEWARE!  USER BEWARE!  USER BEWARE!  USER BEWARE!  USER BEWARE! 
#
# I have done all that I can think of to check for errors in this code, but that does
# not mean that it is completely bug-free. I highly recommend checking plots again the 
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
library(tidyr)
library(dplyr)

#### make function to plot all dives that occurred during the entire duration of the deployment ####
# *** NOTE *** If tag was programmed in HST, make sure to change, filetz = "Pacific/Honolulu"
DivePlotAll = function(behavior,
                       title = NULL,
                       gapcolor = "black",
                       plotcolor = "black",
                       filetz = "Pacific/Honolulu",
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
Tag = "OoTag046"
behavior <- read.csv("Raw Argos files/Oo/OoTag046/53614-Behavior.csv", header=T, stringsAsFactors = F)
summary(behavior)
str(behavior)

## get series file and format
series <- read.csv("Raw Argos files/Oo/OoTag046/53614-Series.csv")
summary(series)
str(series)
series$datetime <- as.POSIXct(paste(series$Day, series$Time), tz = "Pacific/Honolulu", format = "%d-%b-%Y %H:%M:%S")
series$datetimeUTC <- as.POSIXct(format(series$datetime, tz = "UTC"), tz = "UTC")
series$depthMax <- series$Depth + series$DRange

## get series range file and format
series_r <- read.csv("Raw Argos files/Oo/OoTag046/53614-SeriesRange.csv")
summary(series_r)
str(series_r)
series_r$Start <- as.POSIXct(series_r$Start, format = "%H:%M:%S %d-%b-%Y", tz = "Pacific/Honolulu")
series_r$End <- as.POSIXct(series_r$End, format = "%H:%M:%S %d-%b-%Y", tz = "Pacific/Honolulu")
series_r$plot_date <- series_r$Start + 1*60*60
series_r$maxDepthPE <- series_r$MaxDepth + series_r$MaxDepthAccuracy

## creat dataframe to shade summary periods (adapted from dive profile plot code)
xmin = series_r$Start
xmax = series_r$End
nights = data.frame(xmin, xmax)
sub <- nights %>%
  slice(seq(2, n(), by = 2))

### plot dives over entire deployment ###

# specify folder to save plots to #
plot_folder = "Plots/Dive plots/"

# OoTag046
OoTag046.all = DivePlotAll(behavior, title = "OoTag046 behavior + series depths", gaps = T, plotcolor = "deepskyblue4", depthlim = -500,
                           datebreaks = "1 day", nights = sub, filetz = "Pacific/Honolulu", grid = F,
                           gapcolor = "black", depthlab = c("-500","-250","0"), depths = c(-500,-250,0))


OoTag046.all
OoTag046.all =  OoTag046.all + theme(axis.text=element_text(size=11, colour = "black"),
                                     axis.title=element_text(size=12)) + xlab("") +
  geom_point(data = series, aes(x = datetime, y = (Depth*-1)), shape = 21, color = "black", fill = "goldenrod", alpha = 0.5)+
  geom_point(data = series_r, aes(x = plot_date, y = (maxDepthPE*-1)), shape = 21, color = "black", fill = "red", alpha = 0.6,
             size = 2.5)

OoTag046.all
ggsave(paste0(plot_folder, Tag, "_AllDives_SeriesHST_RangePts.jpg"), OoTag046.all, device = "jpeg", units = "in", width = 10, height = 6, dpi = 300)


### plot dives for each 24 hour period ##
# change the datelim = to the two dates between 24 hour period
OoTag046.1 = DivePlotDay(behavior, title = "OoTag046, 11/1 - 11/2", gapcolor = "black", plotcolor = "deepskyblue4", depthlim = -500,
                         datebreaks = "2 hour", nights = sub,
                         datelim = as.POSIXct(c("2013-11-01 14:00:00", "2013-11-02 14:00:00"), tz = "Pacific/Honolulu"),
                         filetz = "Pacific/Honolulu",
                         gaps = T,
                         grid = F, lineweight = 1.2)

OoTag046.1
OoTag046.1 = OoTag046.1 + theme(axis.text.y=element_text(size=11, colour = "black"),
                                axis.title=element_text(size=12),
                                axis.text.x = element_text(size = 11, colour = "black"))+
  geom_point(data = series, aes(x = datetime, y = (Depth*-1)), shape = 21, color = "black", fill = "goldenrod", alpha = 0.5,
             size = 2) +
  geom_point(data = series_r, aes(x = plot_date, y = (maxDepthPE*-1)), shape = 21, color = "black", fill = "red", alpha = 0.6,
             size = 2.5)

OoTag046.1

ggsave(paste0(plot_folder, Tag, "_Dives", "2013Nov01-02_SeriesHST_RangePts.jpg"), OoTag046.1, device = "jpeg", units = "in", width = 9,
       height = 6, dpi = 300)

OoTag046.2 = DivePlotDay(behavior, title = "OoTag046, 11/2 - 11/3", gapcolor = "black", plotcolor = "deepskyblue4", depthlim = -500,
                         datebreaks = "2 hour", nights = sub,
                         datelim = as.POSIXct(c("2013-11-02 14:00:00", "2013-11-03 14:00:00"), tz = "Pacific/Honolulu"),
                         filetz = "Pacific/Honolulu",
                         gaps = T,
                         grid = F, lineweight = 1.2)

OoTag046.2 = OoTag046.2 + theme(axis.text.y=element_text(size=11, colour = "black"),
                                axis.title=element_text(size=12),
                                axis.text.x = element_text(size = 11, colour = "black"))+
  geom_point(data = series, aes(x = datetime, y = (Depth*-1)), shape = 21, color = "black", fill = "goldenrod", alpha = 0.5,
             size = 2)+
  geom_point(data = series_r, aes(x = plot_date, y = (maxDepthPE*-1)), shape = 21, color = "black", fill = "red", alpha = 0.6,
             size = 2.5)

OoTag046.2

ggsave(paste0(plot_folder, Tag, "_Dives", "2013Nov02-03_SeriesHST_RangePts.jpg"), OoTag046.2, device = "jpeg", units = "in", width = 9,
       height = 6, dpi = 300)

OoTag046.3 = DivePlotDay(behavior, title = "OoTag046, 11/4 - 11/5", gapcolor = "black", plotcolor = "deepskyblue4", depthlim = -500,
                         datebreaks = "2 hour", nights = sub,
                         datelim = as.POSIXct(c("2013-11-04 00:00:00", "2013-11-05 00:00:00"), tz = "Pacific/Honolulu"),
                         filetz = "Pacific/Honolulu",
                         gaps = T,
                         grid = F, lineweight = 1.2)

OoTag046.3 = OoTag046.3 + theme(axis.text.y=element_text(size=11, colour = "black"),
                                axis.title=element_text(size=12),
                                axis.text.x = element_text(size = 11, colour = "black"))+
  geom_point(data = series, aes(x = datetime, y = (Depth*-1)), shape = 21, color = "black", fill = "goldenrod", alpha = 0.5,
             size = 2) +
  geom_point(data = series_r, aes(x = plot_date, y = (maxDepthPE*-1)), shape = 21, color = "black", fill = "red", alpha = 0.6,
             size = 2.5)

OoTag046.3

ggsave(paste0(plot_folder, Tag, "_Dives", "2013Nov04-05_SeriesHST_RangePts.jpg"), OoTag046.3, device = "jpeg", units = "in", width = 9,
       height = 6, dpi = 300)

OoTag046.4 = DivePlotDay(behavior, title = "OoTag046, 11/6 - 11/7", gapcolor = "black", plotcolor = "deepskyblue4", depthlim = -500,
                         datebreaks = "2 hour", nights = sub,
                         datelim = as.POSIXct(c("2013-11-06 00:00:00", "2013-11-07 00:00:00"), tz = "Pacific/Honolulu"),
                         filetz = "Pacific/Honolulu",
                         gaps = T,
                         grid = F, lineweight = 1.2)

OoTag046.4 = OoTag046.4 + theme(axis.text.y=element_text(size=11, colour = "black"),
                                axis.title=element_text(size=12),
                                axis.text.x = element_text(size = 11, colour = "black"))+
  geom_point(data = series, aes(x = datetime, y = (Depth*-1)), shape = 21, color = "black", fill = "goldenrod", alpha = 0.5,
             size = 2)+
  geom_point(data = series_r, aes(x = plot_date, y = (maxDepthPE*-1)), shape = 21, color = "black", fill = "red", alpha = 0.6,
             size = 2.5)

OoTag046.4

ggsave(paste0(plot_folder, Tag, "_Dives", "2013Nov06-07_SeriesHST_RangePts.jpg"), OoTag046.4, device = "jpeg", units = "in", width = 9,
       height = 6, dpi = 300)

