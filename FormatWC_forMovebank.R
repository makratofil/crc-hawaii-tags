## Script for formatting Wildlife Computers Argos data for Movebank (Douglas filter) ##

## Author: Michaela A. Kratofil, Cascadia Research
## Updated: 26 Aug 2021

## ========================================================================== ##

## load libraries
library(lubridate)
library(dplyr)

## import Argos location file downloaded from Wildlife Computers Portal. Use the -Locations.csv file, as this has the error ellipse info
## that you'll want retained. The -Argos.csv file does not include this information.
locs <- read.csv("Raw Argos files/Pc/PcTag074/222020-Locations.csv", header = T) # locations file
summary(locs)

# tag object
tag = "PcTag074"

# remove blank satellite locations (these are Mote locations)
# we don't need User location for Movebank, and will add back in later
unique(locs$Type)
locs <- filter(locs, Type == "Argos")

# get last record info to add into location summary file 
last(locs$Date)
last(locs$Latitude)
last(locs$Longitude)

# check if there are comments regarding unreasonable locations, used to remove, but
# Douglas Filter should remove these
unique(locs$Comment)# check


# adjust date time format: yyyy-mm-dd hh:mm:ss
class(locs$Date)
#?as.POSIXct
locs$Date <- as.POSIXct(locs$Date, format = "%H:%M:%S %d-%b-%Y", tz = "UTC")
tz(locs$Date)

# add dummy variables required for the Douglas filter to run (don't actual matter for distance angle rate filter)
locs$IQ <- rep(11)# quality index; this would be 66 for fastloc gps positions
locs$NMsg <- rep(11) # number of messages; this would be 66 for fastloc gps positions

# remove first location if need to (e.g., if there was a location prior to deployment)
sub <- locs[c(16:302),]


# save new csv
write.csv(sub, paste0("For Douglas Filter/", tag, "_WC_KF_locs.csv"), row.names = F, na = "")


# create function to summarize data/location stats 
summ <- function(x) {
  
  N = x %>%
    group_by(DeployID) %>%
    summarise(N = n())
  
  EndDate = x %>%
    group_by(DeployID) %>%
    summarise(EndDate = dplyr::last(Date))
  
  EndLat = x %>%
    group_by(DeployID) %>%
    summarise(EndLat = dplyr::last(Latitude))
  
  EndLon = x %>%
    group_by(DeployID) %>%
    summarise(EndLon = dplyr::last(Longitude))
  
  LC3 = x %>%
    group_by(DeployID) %>%
    filter(Quality == "3") %>%
    summarise(LC3 = n())
  
  LC2 = x %>%
    group_by(DeployID) %>%
    filter(Quality == "2") %>%
    summarise(LC2 = n())
  
  LC1 = x %>%
    group_by(DeployID) %>%
    filter(Quality == "1") %>%
    summarise(LC1 = n())
  
  LC0 = x %>%
    group_by(DeployID) %>%
    filter(Quality == "0") %>%
    summarise(LC0 = n())
  
  LCA = x %>%
    group_by(DeployID) %>%
    filter(Quality == "A") %>%
    summarise(LCA = n())
  
  LCB = x %>%
    group_by(DeployID) %>%
    filter(Quality == "B") %>%
    summarise(LCB = n())
  
  LCZ = x %>%
    group_by(DeployID) %>%
    filter(Quality == "Z") %>%
    summarise(LCZ = n())
  
  
  f_sum <- left_join(N, EndDate, by = "DeployID") %>%
    left_join(EndLat, by = "DeployID") %>%
    left_join(EndLon, by = "DeployID") %>%
    left_join(LC3, by = "DeployID") %>%
    left_join(LC2, by = "DeployID") %>%
    left_join(LC1, by = "DeployID") %>%
    left_join(LC0, by = "DeployID") %>%
    left_join(LCA, by = "DeployID") %>%
    left_join(LCB, by = "DeployID") %>%
    left_join(LCZ, by = "DeployID") 
}

# apply function 
Sum <- summ(sub)

# save csv
write.csv(Sum, "Summary files/Location info for R/PcTag074_WC_KF_LCSumm.csv", row.names = F)
