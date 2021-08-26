## FormatKalman_forMovebank.R: Format individual Kalman filtered/smoothed data
## reprocessed thru Argos CLS for importing into Movebank to run the Douglas
## Filter

## Author: Michaela A. Kratofil, Cascadia Research
## Updated: 25 August 2021

## ========================================================================== ##

## load libraries
library(lubridate)
library(dplyr)

# read in smoothing csv file for individual tag 
locs <- read.csv("Raw Argos files/Gm/GmTag026/85576_2008-07-09_2008-09-23.smoothing.csv", header = T,
                 sep = ";")

# objects for locs file settings (Argos CLS files)
locs$Loc..date.yyyy.MM.dd.HH.mm.ss <- as.character(locs$Loc..date.yyyy.MM.dd.HH.mm.ss)
deployID = "GmTag026"
tz = "UTC"

# rename/add columns
colnames(locs)[colnames(locs) == "Platform.ID.No."] <- "ptt"
colnames(locs)[colnames(locs) == "Loc..date.yyyy.MM.dd.HH.mm.ss"] <- "date"
colnames(locs)[colnames(locs) == "Loc..quality"] <- "LC"
locs$date <- as.POSIXct(locs$date, format = "%Y/%m/%d %H:%M:%S", tz = 'UTC')
locs$animal <- deployID
locs$IQ <- rep(11)# this would be 66 for fastloc gps positions

# remove first location(s) if need to, eg any at WC office or before deployment
#locs <- locs[c(119:467),]

# create function to summarize data/location stats 
summ <- function(x) {
  
  N = x %>%
    group_by(animal) %>%
    summarise(N = n())
  
  EndDate = x %>%
    group_by(animal) %>%
    summarise(EndDate = dplyr::last(date))
  
  EndLat = x %>%
    group_by(animal) %>%
    summarise(EndLat = dplyr::last(Latitude))
  
  EndLon = x %>%
    group_by(animal) %>%
    summarise(EndLon = ((360 - dplyr::last(Longitude))*-1))
  
  LC3 = x %>%
    group_by(animal) %>%
    filter(LC == "3") %>%
    summarise(LC3 = n())
  
  LC2 = x %>%
    group_by(animal) %>%
    filter(LC == "2") %>%
    summarise(LC2 = n())
  
  LC1 = x %>%
    group_by(animal) %>%
    filter(LC == "1") %>%
    summarise(LC1 = n())
  
  LC0 = x %>%
    group_by(animal) %>%
    filter(LC == "0") %>%
    summarise(LC0 = n())
  
  LCA = x %>%
    group_by(animal) %>%
    filter(LC == "A") %>%
    summarise(LCA = n())
  
  LCB = x %>%
    group_by(animal) %>%
    filter(LC == "B") %>%
    summarise(LCB = n())
  
  LCZ = x %>%
    group_by(animal) %>%
    filter(LC == "Z") %>%
    summarise(LCZ = n())
  
  
  f_sum <- left_join(N, EndDate, by = "animal") %>%
    left_join(EndLat, by = "animal") %>%
    left_join(EndLon, by = "animal") %>%
    left_join(LC3, by = "animal") %>%
    left_join(LC2, by = "animal") %>%
    left_join(LC1, by = "animal") %>%
    left_join(LC0, by = "animal") %>%
    left_join(LCA, by = "animal") %>%
    left_join(LCB, by = "animal") %>%
    left_join(LCZ, by = "animal") 
}

# apply function 
Sum <- summ(locs)

# save summ data
write.csv(Sum, paste0("Summary files/Location info for R/", deployID, "_RawLCSum.csv"), row.names = F)

# save new csv
write.csv(locs, paste0("For Douglas Filter/", deployID, "_Kalman_smooth.csv"), row.names = F)

